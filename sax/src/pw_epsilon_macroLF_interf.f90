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

module pw_epsilon_macroLF_interf
implicit none

private
public :: pw_epsilon_macroLF_init
public :: pw_epsilon_macroLF_calc
public :: pw_epsilon_macroLF_borrow_basis
public :: pw_epsilon_macroLF_giveback_basis
public :: pw_epsilon_macroLF_add_dipole
public :: pw_epsilon_macroLF_write
public :: pw_epsilon_macroLF_read
public :: pw_epsilon_macroLF_destroy

interface pw_epsilon_macroLF_init
subroutine pw_epsilon_macroLF_init_x(elf,struct,symmlist,qmesh,omegamax,&
                                     nomega,cutoff,emax,degauss,nkpt,imaginary_axis,root,comm)
  use pw_epsilon_macroLF_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_epsilon_macroLF),   intent(out) :: elf 
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
  real,                       intent(in)  :: cutoff,omegamax
  real,                       intent(in)  :: emax,degauss
  integer,                    intent(in)  :: root,nomega,nkpt
  type (ptk_comm),            intent(in)  :: comm
  logical,                    intent(in)  :: imaginary_axis
end subroutine pw_epsilon_macroLF_init_x
end interface

interface pw_epsilon_macroLF_calc
subroutine pw_epsilon_macroLF_calc_x(elf,states,atoms,broadening)
use ptk_module
use tools_module
use pw_epsilon_macroLF_type
use pw_states_module
use pw_atoms_module
use pw_pseudovelocity_module
use pw_wfc_module
use pw_dipole_module
use pw_field_module
use pw_fft_module

type(pw_epsilon_macroLF), intent(inout) :: elf 
type(pw_states), intent(in)             :: states
type(pw_atoms), intent(in)              :: atoms
real, intent(in)                        :: broadening
end subroutine pw_epsilon_macroLF_calc_x
end interface

interface pw_epsilon_macroLF_borrow_basis
subroutine pw_epsilon_macroLF_borrow_basis_x(elf,basis,iq)
  use pw_epsilon_macroLF_type
  use pw_basis_module
  implicit none
  type(pw_epsilon_macroLF),           intent(in)  :: elf 
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
end subroutine pw_epsilon_macroLF_borrow_basis_x
end interface

interface pw_epsilon_macroLF_giveback_basis
subroutine pw_epsilon_macroLF_giveback_basis_x(elf,basis)
  use pw_epsilon_macroLF_type
  use pw_basis_module
  implicit none
  type(pw_epsilon_macroLF), intent(in)    :: elf 
  type(pw_basis), pointer, optional :: basis
end  subroutine pw_epsilon_macroLF_giveback_basis_x
end interface

interface pw_epsilon_macroLF_add_dipole
subroutine pw_epsilon_macroLF_add_dipole_x(elf,deltae,switch,wfc_dip,velocity)
  use pw_epsilon_macroLF_type
  use pw_wfc_module
  implicit none
  type(pw_epsilon_macroLF), intent(inout) :: elf 
  real,            intent(in)    :: deltae, switch
  type(pw_wfc),    intent(in)    :: wfc_dip
  complex,optional,intent(in)    :: velocity(3) ! needed only if iq=elf%iqgamma
end subroutine pw_epsilon_macroLF_add_dipole_x
end interface

interface pw_epsilon_macroLF_write
subroutine pw_epsilon_macroLF_write_x(elf,unit,name)
  use pw_epsilon_macroLF_type
  implicit none
  type(pw_epsilon_macroLF),  intent(in) :: elf 
  integer,         intent(in) :: unit
  character(len=*),intent(in) :: name
end subroutine pw_epsilon_macroLF_write_x
end interface

interface pw_epsilon_macroLF_read
subroutine pw_epsilon_macroLF_read_x(elf,unit,name)
  use pw_epsilon_macroLF_type
  implicit none
  type(pw_epsilon_macroLF),  intent(inout) :: elf 
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
end subroutine pw_epsilon_macroLF_read_x
end interface 

interface pw_epsilon_macroLF_destroy
subroutine pw_epsilon_macroLF_destroy_x(elf)
  use pw_epsilon_macroLF_type
  implicit none
  type (pw_epsilon_macroLF), intent(inout) :: elf 
end subroutine pw_epsilon_macroLF_destroy_x
end interface

end module pw_epsilon_macroLF_interf

