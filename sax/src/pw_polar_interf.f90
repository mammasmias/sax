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

module pw_polar_interf
implicit none

private
public :: pw_polar_init
public :: pw_polar_calc
public :: pw_polar_borrow_basis
public :: pw_polar_giveback_basis
public :: pw_polar_add_dipole
public :: pw_polar_write
public :: pw_polar_read
public :: pw_polar_destroy

interface pw_polar_init
subroutine pw_polar_init_x(pol,struct,symmlist,qmesh,omegamax,nomega,cutoff,emax,degauss,gw_integration_method,root,comm)
  use pw_polar_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_polar),   intent(out) :: pol
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
!  real,                       intent(in)  :: cutoff,omegamax
  real,                       intent(in)  :: cutoff
  complex,                       intent(in)  :: omegamax
  real,                       intent(in)  :: emax,degauss
  integer,                    intent(in)  :: root,nomega
  type (ptk_comm),            intent(in)  :: comm
  character(len=*), intent(in) :: gw_integration_method
end subroutine pw_polar_init_x
end interface

interface pw_polar_calc
subroutine pw_polar_calc_x(pol,states,atoms)
use ptk_module
use mp_global
use tools_module
use pw_polar_type
use pw_states_module
use pw_atoms_module
use pw_pseudovelocity_module
use pw_wfc_module
use pw_dipole_module
use pw_field_module
use pw_fft_module

type(pw_polar), intent(inout) :: pol
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms
end subroutine pw_polar_calc_x
end interface

interface pw_polar_borrow_basis
subroutine pw_polar_borrow_basis_x(pol,basis,iq)
  use pw_polar_type
  use pw_basis_module
  implicit none
  type(pw_polar),           intent(in)  :: pol
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
end subroutine pw_polar_borrow_basis_x
end interface

interface pw_polar_giveback_basis
subroutine pw_polar_giveback_basis_x(pol,basis)
  use pw_polar_type
  use pw_basis_module
  implicit none
  type(pw_polar), intent(in)    :: pol
  type(pw_basis), pointer, optional :: basis
end  subroutine pw_polar_giveback_basis_x
end interface

interface pw_polar_add_dipole
subroutine pw_polar_add_dipole_x(pol,deltae,weight,wfc_dip,iq,velocity)
  use pw_polar_type
  use pw_wfc_module
  implicit none
  type(pw_polar), intent(inout) :: pol
  real,            intent(in)    :: deltae, weight
  type(pw_wfc),    intent(in)    :: wfc_dip
  integer,         intent(in)    :: iq
  complex,optional,intent(in)    :: velocity(3) ! needed only if iq=pol%iqgamma
end subroutine pw_polar_add_dipole_x
end interface

interface pw_polar_write
subroutine pw_polar_write_x(pol,unit,name)
  use pw_polar_type
  implicit none
  type(pw_polar),  intent(in) :: pol
  integer,         intent(in) :: unit
  character(len=*),intent(in) :: name
end subroutine pw_polar_write_x
end interface

interface pw_polar_read
subroutine pw_polar_read_x(pol,unit,name)
  use pw_polar_type
  implicit none
  type(pw_polar),  intent(inout) :: pol
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
end subroutine pw_polar_read_x
end interface 

interface pw_polar_destroy
subroutine pw_polar_destroy_x(pol)
  use pw_polar_type
  implicit none
  type (pw_polar), intent(inout) :: pol
end subroutine pw_polar_destroy_x
end interface

end module pw_polar_interf

