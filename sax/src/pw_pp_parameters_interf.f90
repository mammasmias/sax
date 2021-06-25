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

!@ MANUAL
module pw_pp_parameters_interf
implicit none
private
public :: pw_pp_parameters_init
public :: pw_pp_parameters_destroy
public :: pw_pp_parameters_write
public :: pw_pp_parameters_read
public :: pw_pp_parameters_borrow_basis
public :: pw_pp_parameters_giveback_basis
public :: pw_pp_parameters_borrow_wfc6d
public :: pw_pp_parameters_giveback_wfc6d
public :: pw_pp_parameters_calc_3
public :: pw_pp_parameters_calc_0
!@ END MANUAL

!@ MANUAL
interface pw_pp_parameters_init
subroutine pw_pp_parameters_init_x(pp_parameters,plasmon_energy,broadening,struct,symmlist,qmesh,cutoff,root,comm)
  use pw_pp_parameters_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_pp_parameters),   intent(out) :: pp_parameters
  real,                       intent(in) :: plasmon_energy,broadening 
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
  real,                       intent(in)  :: cutoff
  integer,                    intent(in)  :: root
  type (ptk_comm),            intent(in)  :: comm
!@ END MANUAL
end subroutine pw_pp_parameters_init_x
end interface

interface pw_pp_parameters_borrow_basis
subroutine pw_pp_parameters_borrow_basis_x(pp_parameters,basis,iq)
  use pw_pp_parameters_type
  use pw_basis_module
  implicit none
  type(pw_pp_parameters),                        intent(in)  :: pp_parameters
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
end subroutine pw_pp_parameters_borrow_basis_x
end interface

interface pw_pp_parameters_giveback_basis
subroutine pw_pp_parameters_giveback_basis_x(pp_parameters,basis)
  use pw_pp_parameters_type
  use pw_basis_module
  implicit none
  type(pw_pp_parameters),              intent(in)    :: pp_parameters
  type(pw_basis), pointer :: basis
end  subroutine pw_pp_parameters_giveback_basis_x
end interface

interface pw_pp_parameters_write
subroutine pw_pp_parameters_write_x(pp_parameters,unit,name)
  use pw_pp_parameters_type
  implicit none
  type(pw_pp_parameters),  intent(in) :: pp_parameters
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
end subroutine pw_pp_parameters_write_x
end interface

interface pw_pp_parameters_destroy
subroutine pw_pp_parameters_destroy_x(pp_parameters)
  use pw_pp_parameters_type
  implicit none
  type (pw_pp_parameters), intent(inout) :: pp_parameters
end subroutine pw_pp_parameters_destroy_x
end interface

interface pw_pp_parameters_calc_0
subroutine pw_pp_parameters_calc_x0(pp_parameters,w,coulomb_div_treatment,ecutvcut)
  use pw_w_type
  use pw_pp_parameters_type
  implicit none
  type(pw_pp_parameters), intent(inout) :: pp_parameters
  type(pw_w), intent(in) :: w
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
end subroutine pw_pp_parameters_calc_x0
end interface

interface pw_pp_parameters_calc_3
subroutine pw_pp_parameters_calc_x3(pp_parameters,w,coulomb_div_treatment,ecutvcut)
  use pw_w_type
  use pw_pp_parameters_type
  implicit none
  type(pw_pp_parameters), intent(inout) :: pp_parameters
  type(pw_w), intent(in) :: w
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
end subroutine pw_pp_parameters_calc_x3
end interface

interface pw_pp_parameters_borrow_wfc6d
subroutine pw_pp_parameters_borrow_wfc6d_x(pp_parameters,wfc6d,ik,iomega)
  use pw_pp_parameters_type
  use pw_wfc6d_module
  implicit none
  type(pw_pp_parameters),                        intent(in)  :: pp_parameters
  type(pw_wfc6d), pointer, optional :: wfc6d
  integer,                 optional, intent(in)  :: ik
  integer,                 optional, intent(in)  :: iomega
end subroutine pw_pp_parameters_borrow_wfc6d_x
end interface

interface pw_pp_parameters_giveback_wfc6d
subroutine pw_pp_parameters_giveback_wfc6d_x(pp_parameters,wfc6d)
  use pw_pp_parameters_type
  use pw_wfc6d_module
  implicit none
  type (pw_pp_parameters),              intent(in)    :: pp_parameters
  type (pw_wfc6d), pointer :: wfc6d
end subroutine pw_pp_parameters_giveback_wfc6d_x
end interface

interface pw_pp_parameters_read
subroutine pw_pp_parameters_read_x(pp_parameters,unit,name)
  use pw_pp_parameters_type
  implicit none
  type(pw_pp_parameters),  intent(inout) :: pp_parameters
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
end subroutine pw_pp_parameters_read_x
end interface pw_pp_parameters_read

end module pw_pp_parameters_interf
