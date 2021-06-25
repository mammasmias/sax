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
module pw_wbw_interf
implicit none
private
public :: pw_wbw_calc_3
public :: pw_wbw_write
public :: pw_wbw_borrow_basis
public :: pw_wbw_giveback_basis
public :: pw_wbw_borrow_wfc6d
public :: pw_wbw_giveback_wfc6d
!@ END MANUAL

!@ MANUAL

interface pw_wbw_borrow_basis
subroutine pw_wbw_borrow_basis_x(epsilonLF,basis,iq)
  use pw_epsilon_macroLF_type
  use pw_basis_module
  implicit none
  type(pw_epsilon_macroLF),           intent(in)  :: epsilonLF 
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
end subroutine pw_wbw_borrow_basis_x
end interface

interface pw_wbw_giveback_basis
subroutine pw_wbw_giveback_basis_x(epsilonLF,basis)
  use pw_epsilon_macroLF_type
  use pw_basis_module
  implicit none
  type(pw_epsilon_macroLF),      intent(in)    :: epsilonLF 
  type(pw_basis), pointer :: basis
end  subroutine pw_wbw_giveback_basis_x
end interface

interface pw_wbw_write
subroutine pw_wbw_write_x(epsilonLF,kind,unit,name)
  use pw_epsilon_macroLF_type
  implicit none
  type(pw_epsilon_macroLF),  intent(in) :: epsilonLF 
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name,kind
end subroutine pw_wbw_write_x
end interface

interface pw_wbw_calc_3
subroutine pw_wbw_calc_x3(elf,coulomb_div_treatment,ecutvcut)
  use pw_epsilon_macroLF_module
  implicit none
  type (pw_epsilon_macroLF), intent(inout) :: elf 
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
end subroutine pw_wbw_calc_x3
end interface

interface pw_wbw_borrow_wfc6d
subroutine pw_wbw_borrow_wfc6d_x(epsilonLF,wfc6d,ik,iomega)
  use pw_epsilon_macroLF_type
  use pw_wfc6d_module
  implicit none
  type(pw_epsilon_macroLF),                        intent(in)  :: epsilonLF 
  type(pw_wfc6d), pointer, optional :: wfc6d
  integer,                 optional, intent(in)  :: ik
  integer,                 optional, intent(in)  :: iomega
end subroutine pw_wbw_borrow_wfc6d_x
end interface

interface pw_wbw_giveback_wfc6d
subroutine pw_wbw_giveback_wfc6d_x(epsilonLF,wfc6d)
  use pw_epsilon_macroLF_type
  use pw_wfc6d_module
  implicit none
  type (pw_epsilon_macroLF),     intent(in)    :: epsilonLF
  type (pw_wfc6d), pointer :: wfc6d
end subroutine pw_wbw_giveback_wfc6d_x
end interface

end module pw_wbw_interf
