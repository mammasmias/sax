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

module pw_pptools_interf
implicit none
private

public :: pw_selfinteraction3, pw_selfinteraction0
public :: pw_participationratio
public :: pw_dos
public :: pw_band_1
public :: pw_band_2

interface pw_selfinteraction3
subroutine pw_selfinteraction3_x(states,nbmin,nbmax,ik,qmesh,cutoff,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank 
  use num_module
  use tools_module
  use iotk_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_states_module
  
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
  character(*), intent(in) :: outdir
end subroutine pw_selfinteraction3_x
end interface

interface pw_selfinteraction0
subroutine pw_selfinteraction0_x(states,nbmin,nbmax,ik,qmesh,cutoff,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank 
  use num_module
  use tools_module
  use iotk_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_states_module
  
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
  character(*), intent(in) :: outdir
end subroutine pw_selfinteraction0_x
end interface

interface pw_participationratio
subroutine pw_participationratio_x(states,nbmin,nbmax,ik,qmesh,cutoff,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank 
  use num_module
  use tools_module
  use iotk_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_states_module
  
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
  character(*), intent(in) :: outdir
end subroutine pw_participationratio_x
end interface

interface pw_dos
subroutine pw_dos_x(nomega,omegamin,omegamax,degauss,nkbz,nbmin,nbmax, &
    diagonal,start_from,ionode,outp,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank, ptk_comm_world 
  use num_module
  use tools_module
  use iotk_module
  use pw_QP_module

  integer, intent(in)         :: nomega, nbmin, nbmax, nkbz
  real, intent(in)            :: omegamin,omegamax,degauss
  character(10), intent(in)   :: start_from
  logical, intent(in)         :: diagonal, ionode
  character(256), intent(in)  :: outp
  character(256), intent(in) :: outdir
end subroutine pw_dos_x
end interface

interface pw_band_1
subroutine pw_band_1_x(states,kmesh,k_read,num_k_read,atoms,nbmin,nbmax, &
      file_out,outdir)
  use tools_module
  use pw_states_module
  use num_module
  use pw_wfc_module
  use pw_kmesh_module
  use ptk_module
  use pw_pseudovelocity_module
  use pw_basis_module
  use pw_atoms_module 
  
  type (pw_states),intent(in) :: states
  integer, intent(in)         :: num_k_read, nbmin, nbmax
  real, intent(in)        :: k_read(num_k_read,3)
  type (pw_kmesh),intent(in)  :: kmesh
  type(pw_atoms), intent(in) :: atoms
  character(256), intent(in) :: file_out
  character(256), intent(in) :: outdir
  
end subroutine pw_band_1_x
end interface

interface pw_band_2
subroutine pw_band_2_x(states,kmesh,k_read,num_k_read,nbmin,nbmax,m,n,p,q, &
     file_out,outdir)
  use tools_module
  use pw_states_module
  use num_module
  use pw_kmesh_module
  use ptk_module
  
  type (pw_states),intent(in) :: states
  integer, intent(in)         :: num_k_read, nbmin, nbmax
  real, intent(in)            :: k_read(num_k_read,3)
  type (pw_kmesh),intent(in)  :: kmesh
  character(256), intent(in)  :: file_out
  real, intent(in)            :: m,n,p,q
  character(256), intent(in)  :: outdir
  
end subroutine pw_band_2_x
end interface

end module pw_pptools_interf
