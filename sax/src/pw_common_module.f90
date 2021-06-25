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

module pw_common_module
implicit none
private

public :: pw_allocate,pw_deallocate,pw_print_statistics,pw_basis_int_kind,pw_default_fmt

integer, parameter :: pw_basis_int_kind = selected_int_kind(3)
! ATTENZIONE CHE SIA CONSISTENTE CON I KIND DELLA MACCHINA !!

integer, save :: pw_allocated_real
integer, save :: pw_allocated_integer
integer, save :: pw_allocated_complex
integer, save :: pw_allocated_basis_integer

interface pw_allocate
  module procedure pw_allocate_real1
  module procedure pw_allocate_real2
  module procedure pw_allocate_real3
  module procedure pw_allocate_integer1
  module procedure pw_allocate_integer2 
  module procedure pw_allocate_integer3
  module procedure pw_allocate_complex1
  module procedure pw_allocate_complex2
  module procedure pw_allocate_complex3
  module procedure pw_allocate_basis_integer1
  module procedure pw_allocate_basis_integer2
  module procedure pw_allocate_basis_integer3
end interface

interface pw_deallocate
  module procedure pw_deallocate_real1
  module procedure pw_deallocate_real2
  module procedure pw_deallocate_real3
  module procedure pw_deallocate_integer1
  module procedure pw_deallocate_integer2
  module procedure pw_deallocate_integer3
  module procedure pw_deallocate_complex1
  module procedure pw_deallocate_complex2
  module procedure pw_deallocate_complex3
  module procedure pw_deallocate_basis_integer1
  module procedure pw_deallocate_basis_integer2
  module procedure pw_deallocate_basis_integer3
end interface

contains

function pw_default_fmt(unit) result(fmt)
  character(len=3)    :: fmt
  integer, intent(in) :: unit
  character(len=20) :: form,access
  logical           :: opened
  integer           :: iostat
  inquire(unit=unit,opened=opened,form=form,access=access,iostat=iostat)
  IOCHECK(unit,iostat)
  if(form=="FORMATTED" .or. .not. opened) then
    fmt = "txt"
  else if(opened .and. form=="UNFORMATTED") then
    if(access=="SEQUENTIAL") then
      fmt = "bin"
    else if(access=="DIRECT") then
      fmt = "dir"
    else
      ERROR(" ")
    end if
  else
    ERROR(" ")
  end if
end function pw_default_fmt

subroutine pw_allocate_real1(arg)
  real, intent(in) :: arg(:)
  pw_allocated_real = pw_allocated_real + size(arg)
end subroutine pw_allocate_real1

subroutine pw_allocate_real2(arg)
  real, intent(in) :: arg(:,:)
  pw_allocated_real = pw_allocated_real + size(arg)
end subroutine pw_allocate_real2

subroutine pw_allocate_real3(arg)
  real, intent(in) :: arg(:,:,:)
  pw_allocated_real = pw_allocated_real + size(arg)
end subroutine pw_allocate_real3


subroutine pw_allocate_integer1(arg)
  integer, intent(in) :: arg(:)
  pw_allocated_integer = pw_allocated_integer + size(arg)
end subroutine pw_allocate_integer1

subroutine pw_allocate_integer2(arg)
  integer, intent(in) :: arg(:,:)
  pw_allocated_integer = pw_allocated_integer + size(arg)
end subroutine pw_allocate_integer2

subroutine pw_allocate_integer3(arg)
  integer, intent(in) :: arg(:,:,:)
  pw_allocated_integer = pw_allocated_integer + size(arg)
end subroutine pw_allocate_integer3

subroutine pw_allocate_complex1(arg)
  complex, intent(in) :: arg(:)
  pw_allocated_complex = pw_allocated_complex + size(arg)
end subroutine pw_allocate_complex1

subroutine pw_allocate_complex2(arg)
  complex, intent(in) :: arg(:,:)
  pw_allocated_complex = pw_allocated_complex + size(arg)
end subroutine pw_allocate_complex2

subroutine pw_allocate_complex3(arg)
  complex, intent(in) :: arg(:,:,:)
  pw_allocated_complex = pw_allocated_complex + size(arg)
end subroutine pw_allocate_complex3


subroutine pw_allocate_basis_integer1(arg)
  integer(pw_basis_int_kind), intent(in) :: arg(:)
  pw_allocated_basis_integer = pw_allocated_basis_integer + size(arg)
end subroutine pw_allocate_basis_integer1

subroutine pw_allocate_basis_integer2(arg)
  integer(pw_basis_int_kind), intent(in) :: arg(:,:)
  pw_allocated_basis_integer = pw_allocated_basis_integer + size(arg)
end subroutine pw_allocate_basis_integer2

subroutine pw_allocate_basis_integer3(arg)
  integer(pw_basis_int_kind), intent(in) :: arg(:,:,:)
  pw_allocated_basis_integer = pw_allocated_basis_integer + size(arg)
end subroutine pw_allocate_basis_integer3

subroutine pw_deallocate_real1(arg)
  real, intent(in) :: arg(:)
  pw_allocated_real = pw_allocated_real - size(arg)
end subroutine pw_deallocate_real1

subroutine pw_deallocate_real2(arg)
  real, intent(in) :: arg(:,:)
  pw_allocated_real = pw_allocated_real - size(arg)
end subroutine pw_deallocate_real2

subroutine pw_deallocate_real3(arg)
  real, intent(in) :: arg(:,:,:)
  pw_allocated_real = pw_allocated_real - size(arg)
end subroutine pw_deallocate_real3

subroutine pw_deallocate_integer1(arg)
  integer, intent(in) :: arg(:)
  pw_allocated_integer = pw_allocated_integer - size(arg)
end subroutine pw_deallocate_integer1

subroutine pw_deallocate_integer2(arg)
  integer, intent(in) :: arg(:,:)
  pw_allocated_integer = pw_allocated_integer - size(arg)
end subroutine pw_deallocate_integer2

subroutine pw_deallocate_integer3(arg)
  integer, intent(in) :: arg(:,:,:)
  pw_allocated_integer = pw_allocated_integer - size(arg)
end subroutine pw_deallocate_integer3

subroutine pw_deallocate_complex1(arg)
  complex, intent(in) :: arg(:)
  pw_allocated_complex = pw_allocated_complex - size(arg)
end subroutine pw_deallocate_complex1

subroutine pw_deallocate_complex2(arg)
  complex, intent(in) :: arg(:,:)
  pw_allocated_complex = pw_allocated_complex - size(arg)
end subroutine pw_deallocate_complex2

subroutine pw_deallocate_complex3(arg)
  complex, intent(in) :: arg(:,:,:)
  pw_allocated_complex = pw_allocated_complex - size(arg)
end subroutine pw_deallocate_complex3

subroutine pw_deallocate_basis_integer1(arg)
  integer(pw_basis_int_kind), intent(in) :: arg(:)
  pw_allocated_basis_integer = pw_allocated_basis_integer - size(arg)
end subroutine pw_deallocate_basis_integer1

subroutine pw_deallocate_basis_integer2(arg)
  integer(pw_basis_int_kind), intent(in) :: arg(:,:)
  pw_allocated_basis_integer = pw_allocated_basis_integer - size(arg)
end subroutine pw_deallocate_basis_integer2

subroutine pw_deallocate_basis_integer3(arg)
  integer(pw_basis_int_kind), intent(in) :: arg(:,:,:)
  pw_allocated_basis_integer = pw_allocated_basis_integer - size(arg)
end subroutine pw_deallocate_basis_integer3

subroutine pw_print_statistics(unit)
  use ptk_module, only : ptk_comm_rank,ptk_comm_world
  integer, intent(in) :: unit
  integer :: rank
  call ptk_comm_rank(ptk_comm_world,rank)
  write(unit,*) "Pw library statistics"
  write(unit,*) "rank          :",rank
  write(unit,*) "integer       :",pw_allocated_integer
  write(unit,*) "basis integer :",pw_allocated_basis_integer
  write(unit,*) "real          :",pw_allocated_real
  write(unit,*) "complex       :",pw_allocated_complex
  write(unit,*) "Pw library end statistics"
end subroutine pw_print_statistics

end module pw_common_module
