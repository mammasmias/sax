! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
! 
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#include "tools_error.h"

!@ MANUAL
module pw_struct_module
use pw_common_module
implicit none
! This module contains the definition of pw_struct and its methods
private
public :: pw_struct,       &
          pw_struct_write, &
          pw_struct_read,  &
          pw_struct_readbcast, &
          pw_struct_bcast, &
          pw_struct_set,   &
          assignment(=),   &
          operator(==)
!@ END MANUAL

!@ MANUAL
type pw_struct
  real a(3,3), b(3,3), a_omega, b_omega
end type pw_struct
! This object contains the informations about the periodic cell
! a(:,1), a(:,2) and a(:,3) :: basis vectors (real space)
! b(:,1), b(:,2) and b(:,3) :: basis vectors (reciprocal space)
! a_omega                   :: cell volume (real space)
! b_omega                   :: cell volume (reciprocal space)
! All quantities in atomic units
!   a       [a.u.]
!   b       [a.u.^-1]
!   a_omega [a.u.^3]
!   b_omega [a.u.^-3]
!@ END MANUAL

!@ MANUAL
interface operator(==)
  module procedure pw_struct_is_equal
end interface
interface assignment(=)
  module procedure pw_struct_copy
end interface
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_struct_write(struct,unit,name)
! Writes an object on a unit
  use iotk_module
  type(pw_struct),  intent(in) :: struct
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
! name is the actual name of the written object
!@ END MANUAL
  character(len=iotk_attlenx) :: attr
  call iotk_write_attr (attr,"type","pw_struct",first=.true.)
  call iotk_write_begin(unit,trim(name),attr=attr)
  call iotk_write_attr (attr,"units","bohr",first=.true.)
  call iotk_write_attr (attr,"a1",struct%a(:,1))
  call iotk_write_attr (attr,"a2",struct%a(:,2))
  call iotk_write_attr (attr,"a3",struct%a(:,3))
  call iotk_write_empty(unit,"direct",attr=attr)
  call iotk_write_attr (attr,"units","bohr",first=.true.)
  call iotk_write_attr (attr,"b1",struct%b(:,1))
  call iotk_write_attr (attr,"b2",struct%b(:,2))
  call iotk_write_attr (attr,"b3",struct%b(:,3))
  call iotk_write_empty(unit,"reciprocal",attr=attr)
  call iotk_write_end  (unit,trim(name))
end subroutine pw_struct_write

!@ MANUAL
subroutine pw_struct_read(struct,unit,name)
! Reads an object from a unit
  use tools_module
  use iotk_module
  type(pw_struct),  intent(out) :: struct
  integer,          intent(in)  :: unit
  character(len=*), intent(in)  :: name
! name is the actual name of the read object
!@ END MANUAL
  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: units,rtype
  logical                     :: found
  real :: a(3,3)
  call iotk_scan_begin(unit,trim(name),attr=attr)
  call iotk_scan_attr (attr,"type",rtype,found=found)
  if(found .and. trim(rtype)/="pw_struct") ERROR("")
  call iotk_scan_empty(unit,"direct",attr=attr)
  call iotk_scan_attr (attr,"units",units,found=found)
  if(.not.found) units="bohr"
  select case(units)
  case("bohr")
    call iotk_scan_attr (attr,"a1",a(:,1))
    call iotk_scan_attr (attr,"a2",a(:,2))
    call iotk_scan_attr (attr,"a3",a(:,3))
  case default
    ERROR("Unknown units "//trim(units))
  end select
  call iotk_scan_end  (unit,trim(name))
  call pw_struct_set(struct,a)
end subroutine pw_struct_read

!@ MANUAL
subroutine pw_struct_readbcast(struct,unit,root,comm,name)
! Reads an object from a unit and broadcasts it
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type(pw_struct),  intent(out) :: struct
  integer,          intent(in)  :: unit,root
  type(ptk_comm),   intent(in)  :: comm
  character(len=*), intent(in)  :: name
!@ END MANUAL
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(root==rank) then
    call pw_struct_read(struct,unit,name)
  end if
  call pw_struct_bcast(struct,root,comm)
end subroutine pw_struct_readbcast
  

!@ MANUAL
subroutine pw_struct_bcast(struct,root,comm)
! Broadcasts an object
  use ptk_module, only : ptk_comm_rank,ptk_bcast,ptk_comm
  type(pw_struct), intent(inout) :: struct
  integer,         intent(in)    :: root
  type(ptk_comm),  intent(in)    :: comm
!@ END MANUAL
  real    :: a(3,3)
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(rank==root) a = struct%a
  call ptk_bcast(a,root,comm)
  if(rank/=root) call pw_struct_set(struct,a)
end subroutine pw_struct_bcast

!@ MANUAL
subroutine pw_struct_set(struct,a)
! Subroutine for struct creation.
! It takes as an argument the a matrix and sets up
! all quantities in struct.
  use num_module
  type(pw_struct), intent(out) :: struct
  real,            intent(in)  :: a(3,3)
  real :: tmp(3,3)
! a(:,1), a(:,2) e a(:,3) :: basis vectors (real space, a.u.)
!@ END MANUAL
  struct%a = a
  struct%a_omega = num_determinant(struct%a)
  if(abs(struct%a_omega)<1e-8) ERROR("")
  tmp = num_inverse(struct%a)
  struct%b = num_2pi * transpose(tmp)
  struct%b_omega = num_determinant(struct%b)
end subroutine pw_struct_set

!@ MANUAL
subroutine pw_struct_copy(new_struct,old_struct)
! Copies an object
! interfaced with operator(=)
  type(pw_struct), intent(out) :: new_struct
  type(pw_struct), intent(in)  :: old_struct
!@ END MANUAL
  call pw_struct_set(new_struct,old_struct%a)
end subroutine pw_struct_copy

!@ MANUAL
function pw_struct_is_equal(struct1,struct2)
! Compares two objects
! interfaced with operator(==)
! NOTE it compares only struct1%a and struct2%a
  logical                     :: pw_struct_is_equal
  type(pw_struct), intent(in) :: struct1,struct2
!@ END MANUAL
  pw_struct_is_equal = all(abs(struct1%a-struct2%a)<1e-8)
end function pw_struct_is_equal

end module pw_struct_module
