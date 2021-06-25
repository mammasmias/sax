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

module pw_sigma_interf
implicit none
private

public :: pw_sigma_init,pw_sigma_destroy
public :: pw_sigma_c_calc3, pw_sigma_c_calc0, pw_sigma_c_calc
public :: pw_sigma_x_calc3, pw_sigma_x_calc0
public :: pw_hf_calc3, pw_hf_calc0 
public :: pw_sigma_QPenergies_collect, pw_sigma_QPdiag
public :: pw_sigma_QPenergies_firstorder_corr
public :: pw_sigma_read, pw_sigma_write, pw_sigma_bcast,pw_sigma_readbcast
public :: pw_sigma_efermi

interface pw_sigma_init
subroutine pw_sigma_init_x(sigma,states,ik,nbmin,nbmax,diagonal,coulomb_div_treatment, &
           ecutvcut,do_not_alloc)
  use num_module
  use pw_sigma_type
  use pw_states_module
  type (pw_sigma), intent(out) ::sigma
  type (pw_states),intent(in), target :: states
  integer,         intent(in)  :: ik,nbmin,nbmax
  logical,         intent(in)  :: diagonal
  character(len=*), optional,     intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
  logical, optional,        intent(in)  :: do_not_alloc
end subroutine pw_sigma_init_x
end interface

interface pw_sigma_destroy
subroutine pw_sigma_destroy_x(sigma)
  use pw_sigma_type
  type (pw_sigma), intent(inout) :: sigma
end subroutine pw_sigma_destroy_x
end interface

interface pw_sigma_efermi
subroutine pw_sigma_efermi_x(efermi,nelec,states)
use pw_states_module
implicit none
real, intent(out) :: efermi
real, intent(in) :: nelec
type(pw_states), intent(in) :: states
end subroutine pw_sigma_efermi_x
end interface

interface pw_sigma_x_calc3
subroutine pw_sigma_x_calc3_x(sigma,qmesh,cutoff,comm)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
  use ptk_mpi_module, only : ptk_mpi_status_size
  use num_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_coulomb_module
  use tools_module
  use pw_sigma_type
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_kmesh), intent(in) :: qmesh
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
end subroutine pw_sigma_x_calc3_x

end interface

interface pw_sigma_x_calc0
subroutine pw_sigma_x_calc0_x(sigma,qmesh,cutoff,comm)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
  use ptk_mpi_module, only : ptk_mpi_status_size
  use num_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use tools_module
  use pw_sigma_type
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_kmesh), intent(in) :: qmesh
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
end subroutine pw_sigma_x_calc0_x
end interface

interface pw_sigma_c_calc
subroutine pw_sigma_c_sscalc_x(gw_integration_method,sigma,w,cutoff,comm)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
  use ptk_mpi_module, only : ptk_mpi_status_size
  use num_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_w_module
  use tools_module
  use pw_sigma_type
  implicit none
  character(len=*), intent(in) :: gw_integration_method
  type(pw_sigma), intent(inout) :: sigma
  type(pw_w),     intent(in)    :: w
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
end subroutine pw_sigma_c_sscalc_x

subroutine pw_sigma_c_calc_x(sigma,w,omega,cutoff,comm)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
  use ptk_mpi_module, only : ptk_mpi_status_size
  use num_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_w_module
  use tools_module
  use pw_sigma_type
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_w),     intent(in)    :: w
  complex, intent(in) :: omega
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
end subroutine pw_sigma_c_calc_x
end interface


interface pw_sigma_c_calc3
subroutine pw_sigma_c_ppcalc3_x(sigma,dsigma,pp_parameters,cutoff,comm)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
  use ptk_mpi_module, only : ptk_mpi_status_size
  use num_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_pp_parameters_module
  use tools_module
  use pw_sigma_type
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_sigma), intent(inout) :: dsigma
  type(pw_pp_parameters),     intent(in)    :: pp_parameters
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
end subroutine pw_sigma_c_ppcalc3_x
end interface

interface pw_sigma_c_calc3_old
subroutine pw_sigma_c_ppcalc3_old_x(sigma,dsigma,pp_parameters,cutoff,comm)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
  use ptk_mpi_module, only : ptk_mpi_status_size
  use num_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_pp_parameters_module
  use tools_module
  use pw_sigma_type
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_sigma), intent(inout) :: dsigma
  type(pw_pp_parameters),     intent(in)    :: pp_parameters
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
end subroutine pw_sigma_c_ppcalc3_old_x
end interface

interface pw_sigma_c_calc0
subroutine pw_sigma_c_ppcalc0_x(sigma,dsigma,pp_parameters,cutoff,comm)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
  use ptk_mpi_module, only : ptk_mpi_status_size
  use num_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_pp_parameters_module
  use tools_module
  use pw_sigma_type
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_sigma), intent(inout) :: dsigma
  type(pw_pp_parameters),     intent(in)    :: pp_parameters
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
end subroutine pw_sigma_c_ppcalc0_x
end interface

interface pw_sigma_QPenergies_collect
subroutine pw_sigma_QPenergies_collect_x(sigma,QP,first_QPcall,outdir)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, ptk_comm, &
      ptk_bcast
  use num_module
  use iotk_module
  use tools_module
  use pw_parall_matrix_module
  use pw_sigma_type
  use pw_QP_module
  type(pw_sigma), intent(inout) :: sigma
  type(pw_QP), intent(inout) :: QP
  logical, intent(in) :: first_QPcall
  character(len=*) :: outdir
end subroutine pw_sigma_QPenergies_collect_x
end interface

interface pw_sigma_QPenergies_firstorder_corr
subroutine pw_sigma_QPenergies_firstorder_corr_x(sigma,QP,diagonal)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, ptk_comm, &
      ptk_bcast
  use num_module
  use iotk_module
  use tools_module
  use pw_parall_matrix_module
  use pw_sigma_type
  use pw_QP_module
  type(pw_sigma), intent(inout) :: sigma
  type(pw_QP), intent(inout) :: QP
  logical, intent(in) :: diagonal
end subroutine pw_sigma_QPenergies_firstorder_corr_x
end interface


interface pw_sigma_QPdiag
subroutine pw_sigma_QPdiag_x(QP,ik,diagonal)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, ptk_comm, &
      ptk_bcast
  use num_module
  use iotk_module
  use tools_module
  use pw_QP_module
  type(pw_QP), intent(inout) :: QP
  integer, intent(in) :: ik
  logical, intent(in) :: diagonal
end subroutine pw_sigma_QPdiag_x
end interface


interface pw_sigma_write
subroutine pw_sigma_write_x(sigma,unit,name,fmt)
  use iotk_module
  use pw_sigma_type
  type (pw_sigma), intent(in) :: sigma
  integer,         intent(in) :: unit
  character(*),    intent(in) :: name
  character(*), optional, intent(in) ::fmt
end subroutine pw_sigma_write_x
end interface

interface pw_sigma_read
subroutine pw_sigma_read_x(sigma,unit,name)
  use iotk_module
  use pw_sigma_type
  type (pw_sigma), intent(inout) :: sigma
  integer,         intent(in)    :: unit
  character(*),    intent(in)    :: name
end subroutine pw_sigma_read_x
end interface

interface pw_sigma_bcast
subroutine pw_sigma_bcast_x(sigma,root,comm)
  use iotk_module
  use ptk_module, only : ptk_bcast, ptk_comm_rank, ptk_comm
  use pw_sigma_type
  type (pw_sigma), intent(inout) :: sigma
  integer,         intent(in)    :: root
  type(ptk_comm),  intent(in)    :: comm
end subroutine pw_sigma_bcast_x
end interface

interface pw_sigma_readbcast
subroutine pw_sigma_readbcast_x(sigma,unit,name,root,comm)
  use iotk_module
  use ptk_module, only : ptk_comm_rank, ptk_comm
  use pw_sigma_type
  type (pw_sigma), intent(inout) :: sigma
  integer ,        intent(in)    :: unit
  character(*),    intent(in)    :: name
  integer,         intent(in)    :: root
  type(ptk_comm),  intent(in)    :: comm
end subroutine pw_sigma_readbcast_x
end interface


interface pw_hf_calc3
subroutine pw_hf_calc3_x(QP,states,nbmin,nbmax,ik,diagonal,qmesh,cutoff, &
           coulomb_div_treatment,ecutvcut,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank, ptk_comm, ptk_bcast 
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
  use pw_parall_matrix_module
  use pw_states_module
  use pw_QP_module
  
  type(pw_QP), intent(inout) :: QP
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  logical, intent(in) :: diagonal
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
  character(len=*), intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
  character(len=*), intent(in) :: outdir
end subroutine pw_hf_calc3_x
end interface

interface pw_hf_calc0
subroutine pw_hf_calc0_x(QP,states,nbmin,nbmax,ik,diagonal,qmesh,cutoff)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank, ptk_comm, ptk_bcast 
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
  use pw_parall_matrix_module
  use pw_states_module
  use pw_QP_module
  
  type(pw_QP), intent(inout) :: QP
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  logical, intent(in) :: diagonal
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
end subroutine pw_hf_calc0_x
end interface

interface calc_efermi
subroutine calc_efermi_x(omegatail,atail,etail,omega,a,e,efermi,nparticles,energy,fixnparticles,ntail)
  use tools_module
  real, intent(in) :: omegatail
  real, intent(in) :: atail,etail
  real, intent(in) :: omega(:)
  real, intent(in) :: a(:)
  real, intent(in) :: e(:)
  real, intent(out):: efermi
  real, intent(out):: nparticles
  real, intent(out):: energy
  real, intent(in) :: fixnparticles
  integer, intent(in) :: ntail
end subroutine calc_efermi_x
end interface

end module pw_sigma_interf
