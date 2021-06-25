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

module pw_epsilon_macro_interf
use pw_epsilon_macro_type
implicit none
private
public :: pw_epsilon_macro_init, &
          pw_epsilon_macro_calc, &
          pw_oscillator_strengh_calc, &
          pw_oscillator_strengh_bis_calc, &
          pw_epsilon_macro_project_on_q, &
          pw_epsilon_macro_spherical_mean, &
          pw_epsilon_macro_read, &
          pw_epsilon_macro_readbcast, &
          pw_epsilon_macro_write, &
          pw_epsilon_macro_add_dipole, &
          pw_epsilon_macro_destroy
       
interface pw_epsilon_macro_init
subroutine pw_epsilon_macro_init_x(epsilon,struct,symmlist,qmesh,omegamax,nomega,cutoff, &
    emax,degauss,energy_shift,broadening,imagomega,root,comm)
  use pw_epsilon_macro_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_common_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use tools_module
  use num_module
  implicit none
  type (pw_epsilon_macro),   intent(out) :: epsilon
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh), target,    intent(in)  :: qmesh
  real,                       intent(in)  :: cutoff,omegamax
  real,                       intent(in)  :: emax, degauss, energy_shift
  integer,                    intent(in)  :: nomega
  real,                       intent(in)  :: broadening
  logical,                    intent(in)  :: imagomega
  type(ptk_comm),             intent(in)  :: comm
  integer,                    intent(in)  :: root

end subroutine pw_epsilon_macro_init_x

subroutine pw_epsilon_macro_init2_x(epsilon,omegamax,nomega,imagomega,root,comm)
  use pw_epsilon_macro_type
  use pw_common_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use tools_module
  use num_module
  implicit none
  type (pw_epsilon_macro),   intent(out) :: epsilon
  real,                       intent(in)  :: omegamax
  integer,                    intent(in)  :: nomega
  logical,                    intent(in)  :: imagomega
  type(ptk_comm),             intent(in)  :: comm
  integer,                    intent(in)  :: root

end subroutine pw_epsilon_macro_init2_x
end interface

interface pw_epsilon_macro_calc
subroutine pw_epsilon_macro_rpa_calc_x(epsilon,states,atoms)
use ptk_module
use mp_global
use tools_module
use pw_epsilon_macro_type
use pw_states_module
use pw_atoms_module
use pw_pseudovelocity_module
use pw_wfc_module
use pw_dipole_module
use pw_kmesh_module
use pw_basis_module
use pw_field_module
use pw_fft_module
use num_module

implicit none

type(pw_epsilon_macro), intent(inout) :: epsilon
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms

end subroutine pw_epsilon_macro_rpa_calc_x

subroutine pw_epsilon_macro_rpaexc_calc_x(epsilon,bse,states,atoms)
use ptk_module
use tools_module
use pw_epsilon_macro_type
use pw_bse_type
use pw_parall_matrix_module
use pw_states_module
use pw_atoms_module
use pw_pseudovelocity_module
use pw_wfc_module
use pw_dipole_module
use pw_kmesh_module
use pw_basis_module
use pw_field_module
use pw_fft_module
use num_module

implicit none

type(pw_epsilon_macro), intent(inout) :: epsilon
type(pw_bse), intent(in) :: bse
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms
end subroutine pw_epsilon_macro_rpaexc_calc_x

end interface

interface pw_oscillator_strengh_calc
subroutine pw_oscillator_strengh_calc_x(nstatemin,nstatemax,bse,states,atoms)
use ptk_module
use tools_module
use pw_bse_type
use pw_parall_matrix_module
use pw_states_module
use pw_atoms_module
use pw_pseudovelocity_module
use pw_wfc_module
use pw_dipole_module
use pw_kmesh_module
use pw_basis_module
use pw_field_module
use pw_fft_module
use num_module

implicit none

integer, intent(in) :: nstatemin, nstatemax
type(pw_bse), intent(in) :: bse
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms
end subroutine pw_oscillator_strengh_calc_x

end interface

interface pw_oscillator_strengh_bis_calc
subroutine pw_oscillator_strengh_bis_calc_x(nstatemin,nstatemax,bse,states,atoms)
use ptk_module
use tools_module
use pw_bse_type
use pw_parall_matrix_module
use pw_states_module
use pw_atoms_module
use pw_pseudovelocity_module
use pw_wfc_module
use pw_dipole_module
use pw_kmesh_module
use pw_basis_module
use pw_field_module
use pw_fft_module
use num_module

implicit none

integer, intent(in) :: nstatemin, nstatemax
type(pw_bse), intent(in) :: bse
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms
end subroutine pw_oscillator_strengh_bis_calc_x

end interface



interface pw_epsilon_macro_add_dipole
subroutine pw_epsilon_macro_add_dipole_rx(macro,deltae,scale,weight,switch,iq,velocity,comm)
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm
  use tools_module
  use num_module
  implicit none
  complex,         intent(inout) :: macro(3,3)
  real,            intent(in)    :: deltae, weight, switch
  complex,         intent(in)    :: scale
  integer,         intent(in)    :: iq
  complex,         intent(in)    :: velocity(3)
  type(ptk_comm),  intent(in)    :: comm

end subroutine pw_epsilon_macro_add_dipole_rx

subroutine pw_epsilon_macro_add_dipole_cx(macro,deltae,scale,weight,switch,iq,velocity,comm)
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm
  use tools_module
  use num_module
  implicit none
  complex,         intent(inout) :: macro(3,3)
  real,            intent(in)    :: deltae, switch
  complex,         intent(in)    :: scale, weight
  integer,         intent(in)    :: iq
  complex,         intent(in)    :: velocity(3)
  type(ptk_comm),  intent(in)    :: comm

end subroutine pw_epsilon_macro_add_dipole_cx
end interface

interface pw_epsilon_macro_project_on_q
subroutine pw_epsilon_macro_project_on_q_x(epsilon,q)
use ptk_module
use tools_module
use pw_epsilon_macro_type
use num_module

implicit none

type(pw_epsilon_macro), intent(inout) :: epsilon
integer, intent(in) :: q(3)

end subroutine pw_epsilon_macro_project_on_q_x
end interface

interface pw_epsilon_macro_spherical_mean
subroutine pw_epsilon_macro_spherical_mean_x(epsilon)
use ptk_module
use tools_module
use pw_epsilon_macro_type
use num_module

implicit none

type(pw_epsilon_macro), intent(inout) :: epsilon

end subroutine pw_epsilon_macro_spherical_mean_x
end interface

interface pw_epsilon_macro_write
subroutine pw_epsilon_macro_write_x(epsilon,unit,name,kindout,fmt)
  use pw_epsilon_macro_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use tools_module
  use num_module
  use iotk_module
  implicit none
  type(pw_epsilon_macro),  intent(in) :: epsilon
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
  character(len=*), optional, intent(in) :: kindout
  character(*), optional, intent(in) :: fmt

end subroutine pw_epsilon_macro_write_x
end interface

interface pw_epsilon_macro_read
subroutine pw_epsilon_macro_read_x(epsilon,unit,name,fmt)
  use pw_epsilon_macro_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_common_module
  use ptk_module, only : ptk_bcast
  use tools_module
  use num_module
  use iotk_module
  implicit none
  type(pw_epsilon_macro),  intent(inout) :: epsilon
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
  character(*), optional, intent(in) :: fmt

end subroutine pw_epsilon_macro_read_x
end interface

interface pw_epsilon_macro_readbcast
subroutine pw_epsilon_macro_readbcast_x(epsilon,unit,name,root,comm,fmt)
  use pw_epsilon_macro_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_common_module
  use ptk_module, only : ptk_bcast, ptk_comm
  use tools_module
  use num_module
  use iotk_module
  implicit none
  type(pw_epsilon_macro),  intent(inout) :: epsilon
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
  integer, intent(in) :: root
  type(ptk_comm), intent(in) :: comm
  character(*), optional, intent(in) :: fmt

end subroutine pw_epsilon_macro_readbcast_x
end interface


interface pw_epsilon_macro_destroy
subroutine pw_epsilon_macro_destroy_x(epsilon)
  use pw_epsilon_macro_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use tools_module
  use num_module
  implicit none
  type (pw_epsilon_macro), intent(inout) :: epsilon
end subroutine pw_epsilon_macro_destroy_x
end interface

end module pw_epsilon_macro_interf


