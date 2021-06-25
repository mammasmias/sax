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

subroutine pw_sigma_init_x(sigma,states,ik,nbmin,nbmax,diagonal, &
           coulomb_div_treatment,ecutvcut,do_not_alloc)
  use num_module
  use pw_sigma_type
  use pw_states_module
  type (pw_sigma), intent(out) ::sigma
  type (pw_states),intent(in), target :: states
  integer,         intent(in)  :: ik,nbmin,nbmax
  logical,         intent(in)  :: diagonal
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
  logical, optional,        intent(in)  :: do_not_alloc
  

  logical :: do_not_alloc_loc
  character(len=30) :: coulomb_div_treatment_loc
  real :: ecutvcut_loc

  do_not_alloc_loc = .false.
  if(present(do_not_alloc)) do_not_alloc_loc = do_not_alloc
  sigma%do_not_alloc = do_not_alloc_loc

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = trim(coulomb_div_treatment)
  sigma%coulomb_div_treatment = coulomb_div_treatment_loc

  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut
  sigma%ecutvcut = ecutvcut_loc

  sigma%states => states
  if(.not.sigma%do_not_alloc) allocate(sigma%val  (nbmin:nbmax,nbmin:nbmax) )
  sigma%nbmin    = nbmin
  sigma%nbmax    = nbmax
  sigma%ik       = ik
  if(.not.sigma%do_not_alloc) sigma%val = 0.0
  sigma%diagonal = diagonal
end subroutine pw_sigma_init_x

subroutine pw_sigma_destroy_x(sigma)
  use pw_sigma_type
  type (pw_sigma), intent(inout) :: sigma
  nullify(sigma%states)
  if(.not.sigma%do_not_alloc) deallocate(sigma%val)
end subroutine pw_sigma_destroy_x

subroutine pw_sigma_efermi_x(efermi,nelec,states)
use pw_states_module
use pw_sigma_type
implicit none
real, intent(out) :: efermi
real, intent(in) :: nelec
type(pw_states), intent(in) :: states

real :: maxeval, minecond, maxeval_old, minecond_old
integer :: ik

efermi=0.0
maxeval_old=-10000
minecond_old=10000
do ik=1,states%kmesh%nkbz
maxeval=states%e(nint(0.5*nelec),ik)
minecond=states%e(nint(0.5*nelec)+1,ik)
if(maxeval>maxeval_old) maxeval_old=maxeval
if(minecond<minecond_old) minecond_old=minecond
enddo
efermi=0.5*(minecond+maxeval)
end subroutine pw_sigma_efermi_x

subroutine pw_sigma_x_calc3_x(sigma,qmesh,cutoff,comm)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
  use ptk_mpi_module, only : ptk_mpi_status_size
  use mp_global
  use num_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_coulomb_module
  use tools_module
  use pw_sigma_type
  use pw_states_module
  use coulomb_vcut_module, only : vcut_type
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_kmesh), intent(in) :: qmesh
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm

  integer :: ikx ! index of kx   (exchange wfc)
  integer :: idk! index of kx-k (W)
  integer :: dim(3)
  integer :: ibx
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc 
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb)     :: coulomb
  type (pw_wfc)         :: wfc_coulomb
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2
  type (vcut_type) :: vcut
  integer :: ib,ibp
  real    :: scale
  real :: lattice(3,3)
  integer :: system_type

  integer :: iboh

  lattice = num_matmul(sigma%states%struct%b,qmesh%m_inv)

  if(trim(sigma%coulomb_div_treatment)=="vcut_ws") then
    call pw_vcut_init(vcut,sigma%states%struct,qmesh,sigma%ecutvcut)
    call pw_coulomb_init(coulomb,lattice,vcut)
  else
    call pw_coulomb_init(coulomb,lattice)
  endif

  if(trim(sigma%coulomb_div_treatment)=="vcut_ws") then
    system_type=1
  elseif(trim(sigma%coulomb_div_treatment)=="vcut_spherical") then
    system_type=0
  else
    system_type=3 
  endif

if(sigma%diagonal) then
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do


  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    ! AF
    call tools_log("Computing Sigma_x("//TRIM(tools_char(ikx,5))//") ..." ,advance=.false.)
    !
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
!    call pw_coulomb_init(coulomb)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    call pw_coulomb_get(system_type,coulomb,wfc_coulomb)
!    if(idk==pw_kmesh_kibz_index(qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
!    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)


    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    iboh = 1
    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx).or.iboh==(sigma%states%nband_loc+1)) then
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do
        
        do ib = sigma%nbmin, sigma%nbmax
          call num_vemul(dip_wfc2%val,wfc_coulomb%VAL,DIP_WFC(IB)%VAL)
          if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) sigma%val(ib,ib) = sigma%val(ib,ib)- &
             pw_wfc_braket(dip_wfc(ib),dip_wfc2) * scale*sigma%states%occupation(ibx,ikx)*0.5 
        end do !ib
        if(sigma%states%incomplete) iboh=iboh+1
      endif 
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(wfc_coulomb)
    call pw_basis_destroy(basis_coulomb)
    !
    ! AF
    call tools_log("done")
    !
  end do
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

else        
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    ! AF
    call tools_log("Computing Sigma_x("//TRIM(tools_char(ikx,5))//") ..." ,advance=.false.)
    !
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
!    call pw_coulomb_init(coulomb)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    call pw_coulomb_get(system_type,coulomb,wfc_coulomb)
!    if(idk==pw_kmesh_kibz_index(qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
!    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    iboh=1
    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx).or.iboh==(sigma%states%nband_loc+1)) then
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do
        
        do ib = sigma%nbmin, sigma%nbmax
          call num_vemul(dip_wfc2%val,wfc_coulomb%VAL,DIP_WFC(IB)%VAL)
          do ibp = sigma%nbmin,sigma%nbmax
            if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) sigma%val(ibp,ib) = sigma%val(ibp,ib)- &
              pw_wfc_braket(dip_wfc(ibp),dip_wfc2) * scale*sigma%states%occupation(ibx,ikx)*0.5
          end do
        end do !ib
        if(sigma%states%incomplete) iboh=iboh+1
      endif
    end do !ibx
 
    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(wfc_coulomb)
    call pw_basis_destroy(basis_coulomb)
    ! AF
    call tools_log("done")
    !
  end do
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)
  
endif ! end if diagonal

  if(trim(sigma%coulomb_div_treatment)=="vcut_ws") then
    call pw_vcut_destroy(vcut)
  endif
  call pw_coulomb_destroy(coulomb)

! just reduce green function
  call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)
  
end subroutine pw_sigma_x_calc3_x

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
  use pw_coulomb_module
  use tools_module
  use pw_sigma_type
  use pw_states_module
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_kmesh), intent(in) :: qmesh
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm

  integer :: ikx ! index of kx   (exchange wfc)
  integer :: idk! index of kx-k 
  integer :: dim(3)
  integer :: ibx
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb)     :: coulomb
  type (pw_wfc)         :: wfc_coulomb
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2
  integer :: ib,ibp
  real    :: scale

  integer :: iboh

if(sigma%diagonal) then
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    ! AF
    call tools_log("Computing Sigma_x("//TRIM(tools_char(ikx,5))//") ..." ,advance=.false.)
    !
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
    call pw_coulomb_init(coulomb,sigma%states%struct%b)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    call pw_coulomb_get(0,coulomb,wfc_coulomb)
    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    iboh=1
    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx).or.iboh==(sigma%states%nband_loc+1)) then
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do
        
        do ib = sigma%nbmin, sigma%nbmax
          call num_vemul(dip_wfc2%val,wfc_coulomb%VAL,DIP_WFC(IB)%VAL)
          if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) sigma%val(ib,ib) = sigma%val(ib,ib)- &
            pw_wfc_braket(dip_wfc(ib),dip_wfc2) * scale*sigma%states%occupation(ibx,ikx)*0.5
        end do !ib
        if(sigma%states%incomplete) iboh=iboh+1
      endif
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(wfc_coulomb)
    call pw_basis_destroy(basis_coulomb)
    !
    ! AF
    call tools_log("done")
    !
  end do
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

else        
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    ! AF
    call tools_log("Computing Sigma_x("//TRIM(tools_char(ikx,5))//") ..." ,advance=.false.)
    !
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
    call pw_coulomb_init(coulomb,sigma%states%struct%b)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    call pw_coulomb_get(0,coulomb,wfc_coulomb)
    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    iboh=1
    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx).or.iboh==(sigma%states%nband_loc+1)) then
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do
        
        do ib = sigma%nbmin, sigma%nbmax
          call num_vemul(dip_wfc2%val,wfc_coulomb%VAL,DIP_WFC(IB)%VAL)
          do ibp = sigma%nbmin,sigma%nbmax
            if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) sigma%val(ibp,ib) = sigma%val(ibp,ib)- &
              pw_wfc_braket(dip_wfc(ibp),dip_wfc2) * scale*sigma%states%occupation(ibx,ikx)*0.5
          end do
        end do !ib
        if(sigma%states%incomplete) iboh=iboh+1
      endif
    end do !ibx
 
    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(wfc_coulomb)
    call pw_basis_destroy(basis_coulomb)
    !
    ! AF
    call tools_log("done")
    !
  end do
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

endif ! end if diagonal

! just reduce green function
  call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)
  
end subroutine pw_sigma_x_calc0_x

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
  use pw_states_module
  use coulomb_vcut_module
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_sigma), intent(inout) :: dsigma
  type(pw_pp_parameters),     intent(in)    :: pp_parameters
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
  integer :: ikx ! index of kx   (exchange wfc)
  integer :: idk! index of kx-k (W)
  integer :: dim(3)
  integer :: ibx
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb) :: coulomb
  type(pw_wfc) :: wfc_coulomb
  type wfc6dpointer
    type (pw_wfc6d), pointer :: val
  end type wfc6dpointer
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2, dip_wfc3
  type (wfc6dpointer), allocatable :: pp_parameters_matrix(:)
  integer :: iomega,ib,ibp
  real    :: scale
  real :: epsilonibx, epsilonib, epsilonibp
  real :: segnoibx
  integer :: ipw1,ipw2
  complex :: braket_tmp1, braket_tmp2
  real :: lattice(3,3)
  type(vcut_type) :: vcut
  integer :: system_type

  integer :: iboh

! This routine compute the correlation part of the self-energy operator
! projected on states within the plasmon pole model

  lattice = num_matmul(sigma%states%struct%b,pp_parameters%qmesh%m_inv)

  if(trim(sigma%coulomb_div_treatment)=="vcut_ws") then
    call pw_vcut_init(vcut,sigma%states%struct,pp_parameters%qmesh,sigma%ecutvcut)
    call pw_coulomb_init(coulomb,lattice,vcut)
  else
    call pw_coulomb_init(coulomb,lattice)
  endif

  if(trim(sigma%coulomb_div_treatment)=="vcut_ws") then
    system_type=1
  elseif(trim(sigma%coulomb_div_treatment)=="vcut_spherical") then
    system_type=0
  else
    system_type=3
  endif

!  write(0,*)"sigma extern, vl:", num_vl_value(lattice,cutoff)
!  write(0,*)"sigma extern, vkk00:", num_vlr0_plus_vsk0_value(lattice,cutoff)
!  write(0,*)"discontinuity value 0:", pp_parameters%discontinuity_parameters(0,1), &
!     pp_parameters%discontinuity_parameters(0,2)
!  write(0,*)"discontinuity value 1:", pp_parameters%discontinuity_parameters(1,1), &
!     pp_parameters%discontinuity_parameters(1,2)



if(sigma%diagonal) then
  sigma%val(:,:)=0.0
  if(.not.dsigma%do_not_alloc) dsigma%val(:,:)=0.0
  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(pp_parameters_matrix(0:1))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    ! AF
    call tools_log("Computing Sigma_c("//TRIM(tools_char(ikx,5))//") ..." ,advance=.false.)
    !
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(pp_parameters%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,pp_parameters%qmesh%kbz(:,idk),cutoff)
!    call pw_coulomb_init(coulomb,lattice)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    if(system_type==3) then
      call pw_coulomb_get(system_type,coulomb,wfc_coulomb)
    else
      call pw_coulomb_get(system_type,coulomb,wfc_coulomb,lhalf=.true.)
    endif
!    if(idk==pw_kmesh_kibz_index(pp_parameters%qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice)
!    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,1
      call pw_pp_parameters_borrow_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val,idk,iomega=iomega)
    end do
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    call pw_wfc_init(dip_wfc3,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    iboh=1
    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx).or.iboh==(sigma%states%nband_loc+1)) then
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        epsilonibx = sigma%states%e(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do

        if(sigma%states%occupation(ibx,ikx)<1) then
           segnoibx=-1.0 ! conduction
        else
           segnoibx=1.0 ! valence
        endif     
        
        do ib = sigma%nbmin, sigma%nbmax
          epsilonib=sigma%states%e(ib,sigma%ik) 
          npw_small = ubound(pp_parameters_matrix(0)%val%val,1)
          dip_wfc2%val = 0.0
          dip_wfc3%val = 0.0
!-------------------------------------------------------
! test new new 
!          if(idk==pw_kmesh_kibz_index(pp_parameters%qmesh,(/0.0,0.0,0.0/))) then
!             dip_wfc2%val(1) = dip_wfc2%val(1) + &
!                  pp_parameters%discontinuity_parameters(1,1)*&
!                   (1.0/(2.0*pp_parameters%discontinuity_parameters(0,1)))*&
!                   (1.0/(epsilonib+segnoibx*pp_parameters%discontinuity_parameters(0,1)-epsilonibx-&
!                        segnoibx*(0.0,1.0)*pp_parameters%broadening))*dip_wfc(ib)%val(1)* &
!                        (-num_vl_value(lattice,cutoff))
!
!             dip_wfc2%val(1) = dip_wfc2%val(1) + &
!                  pp_parameters%discontinuity_parameters(1,2)*&
!                   (1.0/(2.0*pp_parameters%discontinuity_parameters(0,2)))*&
!                   (1.0/(epsilonib+segnoibx*pp_parameters%discontinuity_parameters(0,2)-epsilonibx-&
!                        segnoibx*(0.0,1.0)*pp_parameters%broadening))*dip_wfc(ib)%val(1)* &
!                        (num_vlr0_plus_vsk0_value(lattice,cutoff))
!
!! if dsigma is allocated -> you want dsigma (first derivative of self-energy
!! operator) to be calculated
!                if(.not.dsigma%do_not_alloc) then
!                   dip_wfc3%val(1) =  dip_wfc3%val(1)+ &
!                   pp_parameters%discontinuity_parameters(1,1)*&
!                     (1.0/(2.0*pp_parameters%discontinuity_parameters(0,1)))* &
!                     (-1.0/(epsilonib+segnoibx*pp_parameters%discontinuity_parameters(0,1)-epsilonibx-&
!                      segnoibx*(0.0,1.0)*pp_parameters%broadening)**2)*&
!                      (-num_vl_value(lattice,cutoff))*dip_wfc(ib)%val(1)
!                   dip_wfc3%val(1) =  dip_wfc3%val(1)+ &
!                   pp_parameters%discontinuity_parameters(1,2)*&
!                     (1.0/(2.0*pp_parameters%discontinuity_parameters(0,2)))* &
!                     (-1.0/(epsilonib+segnoibx*pp_parameters%discontinuity_parameters(0,2)-epsilonibx-&
!                      segnoibx*(0.0,1.0)*pp_parameters%broadening)**2)*&
!                      (num_vlr0_plus_vsk0_value(lattice,cutoff))*dip_wfc(ib)%val(1)
!                endif
!
!             do ipw1=2,npw_small
!                do ipw2=2,npw_small
!                   dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2) + &
!                      pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
!                      (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))*&
!                      (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
!                           segnoibx*(0.0,1.0)*pp_parameters%broadening))*wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)
!
!! if dsigma is allocated -> you want dsigma (first derivative of self-energy
!! operator) to be calculated                
!                   if(.not.dsigma%do_not_alloc) dip_wfc3%val(ipw2) =  dip_wfc3%val(ipw2)+ &
!                      pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
!                       (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
!                     (-1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
!                      segnoibx*(0.0,1.0)*pp_parameters%broadening)**2)*&
!                     wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)  
!                enddo   
!             enddo
!         else
!-------------
!------------
            if(system_type==3) then
            do ipw1=1,npw_small
               do ipw2=1,npw_small
                  dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2) + &
                     pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                     (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))*&
                     (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
                     segnoibx*(0.0,1.0)*pp_parameters%broadening))*wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)

! if dsigma is allocated -> you want dsigma (first derivative of self-energy
! operator) to be calculated                
                if(.not.dsigma%do_not_alloc) dip_wfc3%val(ipw2) =  dip_wfc3%val(ipw2)+ &
                pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                    (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                  (-1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
                   segnoibx*(0.0,1.0)*pp_parameters%broadening)**2)*&
                   wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)  
               enddo   
            enddo
            else
            do ipw1=1,npw_small
               do ipw2=1,npw_small
                  dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2) + &
                     pp_parameters_matrix(1)%val%val(ipw2,ipw1)*wfc_coulomb%val(ipw2) * &
                     (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))*&
                     (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
                     segnoibx*(0.0,1.0)*pp_parameters%broadening))*wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)

! if dsigma is allocated -> you want dsigma (first derivative of self-energy
! operator) to be calculated
                if(.not.dsigma%do_not_alloc) dip_wfc3%val(ipw2) =  dip_wfc3%val(ipw2)+ &
                pp_parameters_matrix(1)%val%val(ipw2,ipw1)*wfc_coulomb%val(ipw2) * &
                    (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                  (-1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
                   segnoibx*(0.0,1.0)*pp_parameters%broadening)**2)*&
                   wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)
               enddo
            enddo
            endif ! endif system_type==3
!----------
!         endif !endif iqgamma for test new new
! ---------
            braket_tmp1=pw_wfc_braket(dip_wfc(ib),dip_wfc2) 
            braket_tmp2=pw_wfc_braket(dip_wfc(ib),dip_wfc3)     
            if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) sigma%val(ib,ib) = sigma%val(ib,ib)+braket_tmp1*scale 
 
! if dsigma is allocated -> you want dsigma to be calculated            
            if(.not.dsigma%do_not_alloc.and.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) then
              dsigma%val(ib,ib) = dsigma%val(ib,ib)+braket_tmp2 *scale
            endif
        end do !ib
        if(sigma%states%incomplete) iboh=iboh+1
      endif
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(dip_wfc3)
    call pw_basis_destroy(basis_coulomb)
    call pw_wfc_destroy(wfc_coulomb)
    do iomega=0,1
      call pw_pp_parameters_giveback_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val)
    end do
    !
    ! AF
    call tools_log("done")
    ! 
  end do
  deallocate(pp_parameters_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

else        
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(pp_parameters_matrix(0:1))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    ! AF
    call tools_log("Computing Sigma_c("//TRIM(tools_char(ikx,5))//") ..." ,advance=.false.)
    !
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(pp_parameters%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,pp_parameters%qmesh%kbz(:,idk),cutoff)
!    call pw_coulomb_init(coulomb,lattice)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    if(system_type==3) then
      call pw_coulomb_get(system_type,coulomb,wfc_coulomb)
    else
      call pw_coulomb_get(system_type,coulomb,wfc_coulomb,lhalf=.true.)
    endif
!    if(idk==pw_kmesh_kibz_index(pp_parameters%qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
!    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,1
      call pw_pp_parameters_borrow_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val,idk,iomega=iomega)
    end do

    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    call pw_wfc_init(dip_wfc3,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    iboh=1
    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx).or.iboh==(sigma%states%nband_loc+1)) then
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        epsilonibx = sigma%states%e(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do

        if(sigma%states%occupation(ibx,ikx)<1) then
           segnoibx=-1.0 ! conduction
        else
           segnoibx=1.0 ! valence
        endif     
      
        do ib = sigma%nbmin, sigma%nbmax
          epsilonib=sigma%states%e(ib,sigma%ik) 
          npw_small = ubound(pp_parameters_matrix(0)%val%val,1)
          dip_wfc2%val = 0.0
          if(system_type==3) then
          do ipw1=1,npw_small
             do ipw2=1,npw_small
                dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2)+ &
                   pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                   (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                   (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-segnoibx*&
                   (0.0,1.0)*pp_parameters%broadening))*&
                    wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)
              enddo   
          enddo

          do ibp = sigma%nbmin,sigma%nbmax
          dip_wfc3%val = 0.0
          epsilonibp=sigma%states%e(ibp,sigma%ik)
          do ipw1=1,npw_small
             do ipw2=1,npw_small
              dip_wfc3%val(ipw2) = dip_wfc3%val(ipw2)+ &
                  pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                  (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                    (1.0/(epsilonibp+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-segnoibx*&
                    (0.0,1.0)*pp_parameters%broadening))*&
                    wfc_coulomb%val(ipw1)*dip_wfc(ibp)%val(ipw1)
              enddo   
          enddo
          braket_tmp1=pw_wfc_braket(dip_wfc(ib),dip_wfc3)
          braket_tmp2=pw_wfc_braket(dip_wfc(ibp),dip_wfc2)      
          if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) then
            sigma%val(ib,ibp) = sigma%val(ib,ibp)+0.25*scale*(braket_tmp1 + &
            conjg(braket_tmp1)+conjg(braket_tmp2)+braket_tmp2)
          endif
          end do !ibp

          else
          do ipw1=1,npw_small
             do ipw2=1,npw_small
                dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2)+ &
                   pp_parameters_matrix(1)%val%val(ipw2,ipw1)*wfc_coulomb%val(ipw2) * &
                   (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                   (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-segnoibx*&
                   (0.0,1.0)*pp_parameters%broadening))*&
                    wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)
              enddo   
          enddo

          do ibp = sigma%nbmin,sigma%nbmax
          dip_wfc3%val = 0.0
          epsilonibp=sigma%states%e(ibp,sigma%ik)
            do ipw1=1,npw_small
               do ipw2=1,npw_small
                dip_wfc3%val(ipw2) = dip_wfc3%val(ipw2)+ &
                    pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                    (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* wfc_coulomb%val(ipw2) * &
                      (1.0/(epsilonibp+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-segnoibx*&
                      (0.0,1.0)*pp_parameters%broadening))*&
                      wfc_coulomb%val(ipw1)*dip_wfc(ibp)%val(ipw1)
                enddo   
            enddo

            braket_tmp1=pw_wfc_braket(dip_wfc(ib),dip_wfc3)
            braket_tmp2=pw_wfc_braket(dip_wfc(ibp),dip_wfc2)      
            if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) then
              sigma%val(ib,ibp) = sigma%val(ib,ibp)+0.25*scale*(braket_tmp1 + &
              conjg(braket_tmp1)+conjg(braket_tmp2)+braket_tmp2)
            endif
          end do !ibp
          endif ! endif system_type==3
        end do !ib
        if(sigma%states%incomplete) iboh=iboh+1
      endif ! end if pw_states_is_local or iboh=...
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(dip_wfc3)
    call pw_basis_destroy(basis_coulomb)
    call pw_wfc_destroy(wfc_coulomb)
    do iomega=0,1
      call pw_pp_parameters_giveback_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val)
    end do
    !
    ! AF
    call tools_log("done")
    !
  end do
  deallocate(pp_parameters_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

endif ! end if diagonal
if(system_type==1) call pw_vcut_destroy(vcut)
call pw_coulomb_destroy(coulomb)

! just reduce green function
  call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)
  if(.not.dsigma%do_not_alloc) call ptk_allreduce_inplace(dsigma%val,ptk_sum,comm)
end subroutine pw_sigma_c_ppcalc3_x

subroutine pw_sigma_c_ppcalc3_old_x(sigma,dsigma,pp_parameters,cutoff,comm)
!--------------
! original version for sax-1.0
!--------------
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
  use pw_states_module
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_sigma), intent(inout) :: dsigma
  type(pw_pp_parameters),     intent(in)    :: pp_parameters
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
  integer :: ikx ! index of kx   (exchange wfc)
  integer :: idk! index of kx-k (W)
  integer :: dim(3)
  integer :: ibx
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb) :: coulomb
  type(pw_wfc) :: wfc_coulomb
  type wfc6dpointer
    type (pw_wfc6d), pointer :: val
  end type wfc6dpointer
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2, dip_wfc3
  type (wfc6dpointer), allocatable :: pp_parameters_matrix(:)
  integer :: iomega,ib,ibp
  real    :: scale
  real :: epsilonibx, epsilonib, epsilonibp
  real :: segnoibx
  integer :: ipw1,ipw2
  complex :: braket_tmp1, braket_tmp2
  real :: lattice(3,3)

  integer :: iboh

! This routine compute the correlation part of the self-energy operator
! projected on states within the plasmon pole model

  lattice = num_matmul(sigma%states%struct%b,pp_parameters%qmesh%m_inv)

if(sigma%diagonal) then
  sigma%val(:,:)=0.0
  if(.not.dsigma%do_not_alloc) dsigma%val(:,:)=0.0
  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(pp_parameters_matrix(0:1))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(pp_parameters%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,pp_parameters%qmesh%kbz(:,idk),cutoff)
    call pw_coulomb_init(coulomb,lattice)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    call pw_coulomb_get(3,coulomb,wfc_coulomb)
    if(idk==pw_kmesh_kibz_index(pp_parameters%qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,1
      call pw_pp_parameters_borrow_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val,idk,iomega=iomega)
    end do
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    call pw_wfc_init(dip_wfc3,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    iboh=1
    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx).or.iboh==(sigma%states%nband_loc+1)) then
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        epsilonibx = sigma%states%e(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do

        if(sigma%states%occupation(ibx,ikx)<1) then
           segnoibx=-1.0 ! conduction
        else
           segnoibx=1.0 ! valence
        endif     
        
        do ib = sigma%nbmin, sigma%nbmax
          epsilonib=sigma%states%e(ib,sigma%ik) 
          npw_small = ubound(pp_parameters_matrix(0)%val%val,1)
          dip_wfc2%val = 0.0
          dip_wfc3%val = 0.0
          do ipw1=1,npw_small
             do ipw2=1,npw_small
             dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2) + &
                  pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                   (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))*&
                   (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
                        segnoibx*(0.0,1.0)*pp_parameters%broadening))*wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)

! if dsigma is allocated -> you want dsigma (first derivative of self-energy
! operator) to be calculated                
                if(.not.dsigma%do_not_alloc) dip_wfc3%val(ipw2) =  dip_wfc3%val(ipw2)+ &
                pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                    (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                  (-1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
                   segnoibx*(0.0,1.0)*pp_parameters%broadening)**2)*&
                   wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)  
              enddo   
          enddo
            braket_tmp1=pw_wfc_braket(dip_wfc(ib),dip_wfc2) 
            braket_tmp2=pw_wfc_braket(dip_wfc(ib),dip_wfc3)     
            if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) sigma%val(ib,ib) = sigma%val(ib,ib)+braket_tmp1*scale 
 
! if dsigma is allocated -> you want dsigma to be calculated            
            if(.not.dsigma%do_not_alloc.and.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) then
              dsigma%val(ib,ib) = dsigma%val(ib,ib)+braket_tmp2 *scale
            endif
        end do !ib
        if(sigma%states%incomplete) iboh=iboh+1
      endif
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(dip_wfc3)
    call pw_basis_destroy(basis_coulomb)
    call pw_wfc_destroy(wfc_coulomb)
    do iomega=0,1
      call pw_pp_parameters_giveback_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val)
    end do
  end do
  deallocate(pp_parameters_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

else        
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(pp_parameters_matrix(0:1))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(pp_parameters%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,pp_parameters%qmesh%kbz(:,idk),cutoff)
    call pw_coulomb_init(coulomb,lattice)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    call pw_coulomb_get(3,coulomb,wfc_coulomb)
    if(idk==pw_kmesh_kibz_index(pp_parameters%qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,1
      call pw_pp_parameters_borrow_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val,idk,iomega=iomega)
    end do

    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    call pw_wfc_init(dip_wfc3,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    iboh=1
    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx).or.iboh==(sigma%states%nband_loc+1)) then
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        epsilonibx = sigma%states%e(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do

        if(sigma%states%occupation(ibx,ikx)<1) then
           segnoibx=-1.0 ! conduction
        else
           segnoibx=1.0 ! valence
        endif     
      
        do ib = sigma%nbmin, sigma%nbmax
          epsilonib=sigma%states%e(ib,sigma%ik) 
          npw_small = ubound(pp_parameters_matrix(0)%val%val,1)
          dip_wfc2%val = 0.0
          do ipw1=1,npw_small
             do ipw2=1,npw_small
                dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2)+ &
                   pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                   (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                   (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-segnoibx*&
                   (0.0,1.0)*pp_parameters%broadening))*&
                    wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)
              enddo   
          enddo

          do ibp = sigma%nbmin,sigma%nbmax
          dip_wfc3%val = 0.0
          epsilonibp=sigma%states%e(ibp,sigma%ik)
            do ipw1=1,npw_small
               do ipw2=1,npw_small
                dip_wfc3%val(ipw2) = dip_wfc3%val(ipw2)+ &
                    pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                    (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                      (1.0/(epsilonibp+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-segnoibx*&
                      (0.0,1.0)*pp_parameters%broadening))*&
                      wfc_coulomb%val(ipw1)*dip_wfc(ibp)%val(ipw1)
                enddo   
            enddo
            braket_tmp1=pw_wfc_braket(dip_wfc(ib),dip_wfc3)
            braket_tmp2=pw_wfc_braket(dip_wfc(ibp),dip_wfc2)      
            if(pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) then
              sigma%val(ib,ibp) = sigma%val(ib,ibp)+0.25*scale*(braket_tmp1 + &
              conjg(braket_tmp1)+conjg(braket_tmp2)+braket_tmp2)
            endif
          end do !ibp
        end do !ib
        if(sigma%states%incomplete) iboh=iboh+1
      endif
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(dip_wfc3)
    call pw_basis_destroy(basis_coulomb)
    call pw_wfc_destroy(wfc_coulomb)
    do iomega=0,1
      call pw_pp_parameters_giveback_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val)
    end do
  end do
  deallocate(pp_parameters_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

endif ! end if diagonal

! just reduce green function
  call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)
  if(.not.dsigma%do_not_alloc) call ptk_allreduce_inplace(dsigma%val,ptk_sum,comm)
end subroutine pw_sigma_c_ppcalc3_old_x

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
  use pw_states_module
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_sigma), intent(inout) :: dsigma
  type(pw_pp_parameters),     intent(in)    :: pp_parameters
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
  integer :: ikx ! index of kx   (exchange wfc)
  integer :: idk! index of kx-k (W)
  integer :: dim(3)
  integer :: ibx
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb) :: coulomb
  type(pw_wfc) :: wfc_coulomb
  type wfc6dpointer
    type (pw_wfc6d), pointer :: val
  end type wfc6dpointer
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2, dip_wfc3
  type (wfc6dpointer), allocatable :: pp_parameters_matrix(:)
  integer :: iomega,ib,ibp
  real    :: scale
  real :: epsilonibx, epsilonib, epsilonibp
  real :: segnoibx
  integer :: ipw1,ipw2
  complex :: braket_tmp1, braket_tmp2
  real :: lattice(3,3)
  integer :: nestep, ie
  real :: ppsexcontrib

! This routine compute the correlation part of the self-energy operator
! projected on states within the plasmon pole model

  lattice = num_matmul(sigma%states%struct%b,pp_parameters%qmesh%m_inv)

if(sigma%diagonal) then
  sigma%val(:,:)=0.0
  if(.not.dsigma%do_not_alloc) dsigma%val(:,:)=0.0
  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(pp_parameters_matrix(0:1))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(pp_parameters%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,pp_parameters%qmesh%kbz(:,idk),cutoff)
    call pw_coulomb_init(coulomb,lattice)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    call pw_coulomb_get(0,coulomb,wfc_coulomb)
    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,1
      call pw_pp_parameters_borrow_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val,idk,iomega=iomega)
    end do
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    call pw_wfc_init(dip_wfc3,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        epsilonibx = sigma%states%e(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do

        if(sigma%states%occupation(ibx,ikx)<1) then
           segnoibx=-1.0 ! conduction
        else
           segnoibx=1.0 ! valence
        endif     
        
        do ib = sigma%nbmin, sigma%nbmax
          epsilonib=sigma%states%e(ib,sigma%ik) 
          npw_small = ubound(pp_parameters_matrix(0)%val%val,1)
          dip_wfc2%val = 0.0
          dip_wfc3%val = 0.0
          do ipw1=1,npw_small
             do ipw2=1,npw_small
                dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2) + &
                     pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                      (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))*&
                      (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
                       segnoibx*(0.0,1.0)*pp_parameters%broadening))*wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)

! if dsigma is allocated -> you want dsigma (first derivative of self-energy
! operator) to be calculated                
                if(.not.dsigma%do_not_alloc) dip_wfc3%val(ipw2) =  dip_wfc3%val(ipw2)+ &
                pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                    (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                  (-1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-&
                  segnoibx*(0.0,1.0)*pp_parameters%broadening)**2)*&
                  wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)  
              enddo   
          enddo
            braket_tmp1=pw_wfc_braket(dip_wfc(ib),dip_wfc2) 
            braket_tmp2=pw_wfc_braket(dip_wfc(ib),dip_wfc3)     
            sigma%val(ib,ib) = sigma%val(ib,ib)+braket_tmp1*scale 
 
! if dsigma is allocated -> you want dsigma to be calculated            
            if(.not.dsigma%do_not_alloc) dsigma%val(ib,ib) = dsigma%val(ib,ib)+braket_tmp2 *scale
        end do !ib
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(dip_wfc3)
    call pw_basis_destroy(basis_coulomb)
    call pw_wfc_destroy(wfc_coulomb)
    do iomega=0,1
      call pw_pp_parameters_giveback_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val)
    end do
  end do
  deallocate(pp_parameters_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

else        
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(pp_parameters_matrix(0:1))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(pp_parameters%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,pp_parameters%qmesh%kbz(:,idk),cutoff)
    call pw_coulomb_init(coulomb,lattice)
    call pw_wfc_init(wfc_coulomb,basis_coulomb)
    call pw_coulomb_get(3,coulomb,wfc_coulomb)
    if(idk==pw_kmesh_kibz_index(pp_parameters%qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
    call pw_coulomb_destroy(coulomb)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,1
      call pw_pp_parameters_borrow_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val,idk,iomega=iomega)
    end do

    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    call pw_wfc_init(dip_wfc3,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        epsilonibx = sigma%states%e(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do

        if(sigma%states%occupation(ibx,ikx)<1) then
           segnoibx=-1.0 ! conduction
        else
           segnoibx=1.0 ! valence
        endif     
      
        do ib = sigma%nbmin, sigma%nbmax
          epsilonib=sigma%states%e(ib,sigma%ik) 
          npw_small = ubound(pp_parameters_matrix(0)%val%val,1)
          dip_wfc2%val = 0.0
          do ipw1=1,npw_small
             do ipw2=1,npw_small
               dip_wfc2%val(ipw2) = dip_wfc2%val(ipw2)+ &
                   pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                   (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                   (1.0/(epsilonib+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-segnoibx*&
                   (0.0,1.0)*pp_parameters%broadening))*&
                    wfc_coulomb%val(ipw1)*dip_wfc(ib)%val(ipw1)
              enddo   
          enddo

          do ibp = sigma%nbmin,sigma%nbmax
          dip_wfc3%val = 0.0
          epsilonibp=sigma%states%e(ibp,sigma%ik)
            do ipw1=1,npw_small
               do ipw2=1,npw_small
                  dip_wfc3%val(ipw2) = dip_wfc3%val(ipw2)+ &
                    pp_parameters_matrix(1)%val%val(ipw2,ipw1)*&
                    (1.0/(2.0*pp_parameters_matrix(0)%val%val(ipw2,ipw1)))* &
                    (1.0/(epsilonibp+segnoibx*pp_parameters_matrix(0)%val%val(ipw2,ipw1)-epsilonibx-segnoibx*&
                    (0.0,1.0)*pp_parameters%broadening))*&
                     wfc_coulomb%val(ipw1)*dip_wfc(ibp)%val(ipw1)
                enddo   
            enddo
            braket_tmp1=pw_wfc_braket(dip_wfc(ib),dip_wfc3)
            braket_tmp2=pw_wfc_braket(dip_wfc(ibp),dip_wfc2)      
            sigma%val(ib,ibp) = sigma%val(ib,ibp)+0.25*scale*(braket_tmp1 + &
              conjg(braket_tmp1)+conjg(braket_tmp2)+braket_tmp2)
          end do !ibp
        end do !ib
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_wfc_destroy(dip_wfc3)
    call pw_basis_destroy(basis_coulomb)
    call pw_wfc_destroy(wfc_coulomb)
    do iomega=0,1
      call pw_pp_parameters_giveback_wfc6d(pp_parameters,pp_parameters_matrix(iomega)%val)
    end do
  end do
  deallocate(pp_parameters_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)
  
endif ! end if diagonal

! just reduce green function
  call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)
  if(.not.dsigma%do_not_alloc) call ptk_allreduce_inplace(dsigma%val,ptk_sum,comm)
end subroutine pw_sigma_c_ppcalc0_x

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
  use pw_states_module
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_w),     intent(in)    :: w
  complex, intent(in) :: omega
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
  integer :: ikx ! index of kx   (exchange wfc)
  integer :: idk! index of kx-k (W)
  integer :: dim(3)
  integer :: ibx
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type wfc6dpointer
    type (pw_wfc6d), pointer :: val
  end type wfc6dpointer
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2
  type (wfc6dpointer), allocatable :: w_matrix(:)
  integer :: iomega,ib,ibp
  real    :: scale
  real :: epsilonx
  complex :: dsigma(sigma%nbmin:sigma%nbmax,sigma%nbmin:sigma%nbmax)
  complex :: dsigma2(sigma%nbmin:sigma%nbmax,sigma%nbmin:sigma%nbmax)
  complex :: braket_tmp

if(sigma%diagonal) then
  sigma%val(:,:)=0.0
  dsigma(:,:)=0.0
  dsigma2(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(w_matrix(0:w%nomega))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(w%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,w%nomega
      call pw_w_borrow_wfc6d(w,w_matrix(iomega)%val,idk,iomega=iomega)
    end do
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        epsilonx = sigma%states%e(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do
        
        do ib = sigma%nbmin, sigma%nbmax
          npw_small = ubound(w_matrix(0)%val%val,1)
          do iomega = 0,w%nomega
            dip_wfc2%val = 0.0
!!            call num_gemv(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small),dip_wfc2%val(1:npw_small),(1.0,0.0),(0.0,0.0))
            dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small))
              braket_tmp=pw_wfc_braket(dip_wfc(ib),dip_wfc2)      
              sigma%val(ib,ib) = sigma%val(ib,ib)- (0.0,1.0)*braket_tmp * scale * &
                   1.0/num_2pi_i*w%weight(iomega)* &
                   ( (omega-epsilonx-(0.0,1.0)*w%omega(iomega))**(-1) + &
                                 (omega-epsilonx+(0.0,1.0)*w%omega(iomega))**(-1) ) 
              dsigma(ib,ib)=dsigma(ib,ib)-braket_tmp * scale * &
                  1.0/num_2pi_i*(0.0,1.0)*w%weight(iomega)* &
                  ((omega-epsilonx-(0.0,1.0)*w%omega(iomega))**(-2) + &
                   (omega-epsilonx+(0.0,1.0)*w%omega(iomega))**(-2))  
             dsigma2(ib,ib)=dsigma2(ib,ib)-braket_tmp *scale* &
                   1.0/num_2pi_i*(0.0,1.0)*w%weight(iomega)* &
                  ((omega-epsilonx-(0.0,1.0)*w%omega(iomega))**(-3) + &
                   (omega-epsilonx+(0.0,1.0)*w%omega(iomega))**(-3))  
          end do !omega
        end do !ib
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_basis_destroy(basis_coulomb)
    do iomega=0,w%nomega
      call pw_w_giveback_wfc6d(w,w_matrix(iomega)%val)
    end do
  end do
  write(0,*)"from pw_sigma_calc"
  do iomega=0,w%nomega
  write(0,*)"freq and weight",w%omega(iomega),w%weight(iomega)
  write(0,*) "sigma",epsilonx,-1.0/num_2pi_i*(0.0,1.0)*w%weight(iomega)* &
             ((omega-epsilonx-(0.0,1.0)*w%omega(iomega))**(-1) + &
                 (omega-epsilonx+(0.0,1.0)*w%omega(0))**(-1))
  write(0,*) "dsigma",epsilonx,-1.0/num_2pi_i*(0.0,1.0)*w%weight(iomega)* &
              ((omega-epsilonx-(0.0,1.0)*w%omega(iomega))**(-2) + &
                 (omega-epsilonx+(0.0,1.0)*w%omega(iomega))**(-2))
  write(0,*) "dsigma2",epsilonx,-1.0/num_2pi_i*(0.0,1.0)*w%weight(iomega)* &
              ((omega-epsilonx-(0.0,1.0)*w%omega(iomega))**(-3) + &
                 (omega-epsilonx-(0.0,1.0)*w%omega(iomega))**(-3) )
  enddo
  deallocate(w_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

else        
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(w_matrix(0:w%nomega))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(w%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,w%nomega
      call pw_w_borrow_wfc6d(w,w_matrix(iomega)%val,idk,iomega=iomega)
    end do
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        epsilonx = sigma%states%e(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do
        
        do ib = sigma%nbmin, sigma%nbmax
          npw_small = ubound(w_matrix(0)%val%val,1)
          do iomega = 0,w%nomega
            dip_wfc2%val = 0.0
!!            call num_gemv(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small),dip_wfc2%val(1:npw_small),(1.0,0.0),(0.0,0.0))
            dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small))
            do ibp = sigma%nbmin,sigma%nbmax
              sigma%val(ibp,ib) = sigma%val(ibp,ib)- pw_wfc_braket(dip_wfc(ibp),dip_wfc2) * scale * &
                   1.0/num_2pi_i*(0.0,1.0)*w%weight(iomega)* &
                   ( (omega-epsilonx-(0.0,1.0)*w%omega(iomega))**(-1) + &
                                 (omega-epsilonx+(0.0,1.0)*w%omega(iomega))**(-1) ) 
            end do !ibp
          end do !omega
        end do !ib
    end do !ibx
 
    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_basis_destroy(basis_coulomb)
    do iomega=0,w%nomega
      call pw_w_giveback_wfc6d(w,w_matrix(iomega)%val)
    end do
  end do
  deallocate(w_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)
endif  

! just reduce green function
  call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)
  call ptk_allreduce_inplace(dsigma,ptk_sum,comm)
  call ptk_allreduce_inplace(dsigma2,ptk_sum,comm)
  
end subroutine pw_sigma_c_calc_x

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
  use pw_states_module
  implicit none
  character(len=*), intent(in) :: gw_integration_method
  type(pw_sigma), intent(inout) :: sigma
  type(pw_w),     intent(in)    :: w
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
  integer :: ikx ! index of kx   (exchange wfc)
  integer :: idk! index of kx-k (W)
  integer :: dim(3)
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type wfc6dpointer
    type (pw_wfc6d), pointer :: val
  end type wfc6dpointer
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2
  type (wfc6dpointer), allocatable :: w_matrix(:)
  integer :: iomega,ib,ibp, ibx
  real    :: scale
  real :: segnoibx
  complex :: braket_tmp

if(gw_integration_method=="sshf") then 
  if(sigma%diagonal) then
    sigma%val(:,:)=0.0
    call pw_field_init(dip_field,sigma%states%struct)
    call pw_field_init(exc_field,sigma%states%struct)
    allocate(wfc_field(sigma%nbmin:sigma%nbmax))
    allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
    allocate(w_matrix(0:w%nomega))
    do ib=sigma%nbmin,sigma%nbmax
      call pw_field_init(wfc_field(ib),sigma%states%struct)
    end do

    k = sigma%states%kmesh%kbz(:,sigma%ik)
    do ikx = 1,ubound(sigma%states%wfc,2)
      kx = sigma%states%kmesh%kbz(:,ikx)
      idk = pw_kmesh_kbz_index(w%qmesh,(k-kx))
      call pw_basis_init(basis_coulomb,sigma%states%struct)
      call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
      dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
      call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
      do iomega=0,w%nomega
        call pw_w_borrow_wfc6d(w,w_matrix(iomega)%val,idk,iomega=iomega)
      end do
      call pw_wfc_init(dip_wfc(:),basis_coulomb)
      call pw_wfc_init(dip_wfc2,basis_coulomb)
      do ib=sigma%nbmin,sigma%nbmax
        call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
        call pw_wfc2field(wfc_field(ib),wfc)
        call pw_states_giveback_wfc(sigma%states,wfc)
      end do

      do ibx = sigma%states%nbmin,sigma%states%nbmax
        if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
          scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
          call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
          call pw_wfc2field(exc_field,exc_wfc)
          call pw_states_giveback_wfc(sigma%states,exc_wfc)
          do ib = sigma%nbmin,sigma%nbmax
            call pw_field_mul(dip_field,exc_field,wfc_field(ib))
            call pw_field2wfc(dip_wfc(ib),dip_field)
          end do

          do ib = sigma%nbmin, sigma%nbmax
            npw_small = ubound(w_matrix(0)%val%val,1)
            dip_wfc2%val = 0.0
            dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(0)%val%val,dip_wfc(ib)%val(1:npw_small))
              braket_tmp=pw_wfc_braket(dip_wfc(ib),dip_wfc2)  
              sigma%val(ib,ib)=sigma%val(ib,ib)-0.5*braket_tmp * scale * &
                sigma%states%occupation(ibx,ikx)
          end do !ib
      end do !ibx

      call pw_wfc_destroy(dip_wfc(:))
      call pw_wfc_destroy(dip_wfc2)
      call pw_basis_destroy(basis_coulomb)
      do iomega=0,w%nomega
        call pw_w_giveback_wfc6d(w,w_matrix(iomega)%val)
      end do
    end do
    deallocate(w_matrix)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_field_destroy(wfc_field(ib))
    end do
    deallocate(wfc_field,dip_wfc)
    call pw_field_destroy(dip_field)
    call pw_field_destroy(exc_field)

  else        
    sigma%val(:,:)=0.0

    call pw_field_init(dip_field,sigma%states%struct)
    call pw_field_init(exc_field,sigma%states%struct)
    allocate(wfc_field(sigma%nbmin:sigma%nbmax))
    allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
    allocate(w_matrix(0:w%nomega))
    do ib=sigma%nbmin,sigma%nbmax
      call pw_field_init(wfc_field(ib),sigma%states%struct)
    end do

    k = sigma%states%kmesh%kbz(:,sigma%ik)
    do ikx = 1,ubound(sigma%states%wfc,2)
      kx = sigma%states%kmesh%kbz(:,ikx)
      idk = pw_kmesh_kbz_index(w%qmesh,(k-kx))
      call pw_basis_init(basis_coulomb,sigma%states%struct)
      call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
      dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
      call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
      do iomega=0,w%nomega
        call pw_w_borrow_wfc6d(w,w_matrix(iomega)%val,idk,iomega=iomega)
      end do
      call pw_wfc_init(dip_wfc(:),basis_coulomb)
      call pw_wfc_init(dip_wfc2,basis_coulomb)
      do ib=sigma%nbmin,sigma%nbmax
        call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
        call pw_wfc2field(wfc_field(ib),wfc)
        call pw_states_giveback_wfc(sigma%states,wfc)
      end do

      do ibx = sigma%states%nbmin,sigma%states%nbmax
        if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
          scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
          call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
          call pw_wfc2field(exc_field,exc_wfc)
          call pw_states_giveback_wfc(sigma%states,exc_wfc)
          do ib = sigma%nbmin,sigma%nbmax
            call pw_field_mul(dip_field,exc_field,wfc_field(ib))
            call pw_field2wfc(dip_wfc(ib),dip_field)
          end do

          do ib = sigma%nbmin, sigma%nbmax
            npw_small = ubound(w_matrix(0)%val%val,1)
            dip_wfc2%val = 0.0
            dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(0)%val%val,dip_wfc(ib)%val(1:npw_small))
            do ibp = sigma%nbmin,sigma%nbmax
              sigma%val(ibp,ib) = sigma%val(ibp,ib)- &
                0.5*pw_wfc_braket(dip_wfc(ibp),dip_wfc2)*scale*sigma%states%occupation(ibx,ikx) 
            end do !ibp
          end do !ib
      end do !ibx
 
      call pw_wfc_destroy(dip_wfc(:))
      call pw_wfc_destroy(dip_wfc2)
      call pw_basis_destroy(basis_coulomb)
      do iomega=0,w%nomega
        call pw_w_giveback_wfc6d(w,w_matrix(iomega)%val)
      end do
    end do
    deallocate(w_matrix)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_field_destroy(wfc_field(ib))
    end do
    deallocate(wfc_field,dip_wfc)
    call pw_field_destroy(dip_field)
    call pw_field_destroy(exc_field)
  
  endif ! end if diagonal

elseif(gw_integration_method=="cohsex") then
  if(sigma%diagonal) then
    sigma%val(:,:)=0.0

    call pw_field_init(dip_field,sigma%states%struct)
    call pw_field_init(exc_field,sigma%states%struct)
    allocate(wfc_field(sigma%nbmin:sigma%nbmax))
    allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
    allocate(w_matrix(0:w%nomega))
    do ib=sigma%nbmin,sigma%nbmax
      call pw_field_init(wfc_field(ib),sigma%states%struct)
    end do

    k = sigma%states%kmesh%kbz(:,sigma%ik)
    do ikx = 1,ubound(sigma%states%wfc,2)
      kx = sigma%states%kmesh%kbz(:,ikx)
      idk = pw_kmesh_kbz_index(w%qmesh,(k-kx))
      call pw_basis_init(basis_coulomb,sigma%states%struct)
      call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
      dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
      call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
      do iomega=0,w%nomega
        call pw_w_borrow_wfc6d(w,w_matrix(iomega)%val,idk,iomega=iomega)
!        w_matrix(iomega)%val%val = real(w_matrix(iomega)%val%val)
      end do
      call pw_wfc_init(dip_wfc(:),basis_coulomb)
      call pw_wfc_init(dip_wfc2,basis_coulomb)
      do ib=sigma%nbmin,sigma%nbmax
        call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
        call pw_wfc2field(wfc_field(ib),wfc)
        call pw_states_giveback_wfc(sigma%states,wfc)
      end do

      do ibx = sigma%states%nbmin,sigma%states%nbmax
        if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
          scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
          call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
          call pw_wfc2field(exc_field,exc_wfc)
          call pw_states_giveback_wfc(sigma%states,exc_wfc)
          do ib = sigma%nbmin,sigma%nbmax
            call pw_field_mul(dip_field,exc_field,wfc_field(ib))
            call pw_field2wfc(dip_wfc(ib),dip_field)
          end do

          if(sigma%states%occupation(ibx,ikx)<1) then
             segnoibx=-1.0 ! conduction
          else
             segnoibx=1.0 ! valence
          endif     

          do ib = sigma%nbmin, sigma%nbmax
            npw_small = ubound(w_matrix(0)%val%val,1)
            do iomega = 0,w%nomega
              dip_wfc2%val = 0.0
              dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(iomega)%val%val(1:npw_small,1:npw_small), &
              dip_wfc(ib)%val(1:npw_small))
                braket_tmp=pw_wfc_braket(dip_wfc(ib),dip_wfc2)      
                sigma%val(ib,ib) = sigma%val(ib,ib)- segnoibx*0.5*braket_tmp * scale 
            end do !omega
          end do !ib
      end do !ibx

      call pw_wfc_destroy(dip_wfc(:))
      call pw_wfc_destroy(dip_wfc2)
      call pw_basis_destroy(basis_coulomb)
      do iomega=0,w%nomega
        call pw_w_giveback_wfc6d(w,w_matrix(iomega)%val)
      end do
    end do
    deallocate(w_matrix)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_field_destroy(wfc_field(ib))
    end do
    deallocate(wfc_field,dip_wfc)
    call pw_field_destroy(dip_field)
    call pw_field_destroy(exc_field)
  
  else        
    sigma%val(:,:)=0.0

    call pw_field_init(dip_field,sigma%states%struct)
    call pw_field_init(exc_field,sigma%states%struct)
    allocate(wfc_field(sigma%nbmin:sigma%nbmax))
    allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
    allocate(w_matrix(0:w%nomega))
    do ib=sigma%nbmin,sigma%nbmax
      call pw_field_init(wfc_field(ib),sigma%states%struct)
    end do
 
    k = sigma%states%kmesh%kbz(:,sigma%ik)
    do ikx = 1,ubound(sigma%states%wfc,2)
      kx = sigma%states%kmesh%kbz(:,ikx)
      idk = pw_kmesh_kbz_index(w%qmesh,(k-kx))
      call pw_basis_init(basis_coulomb,sigma%states%struct)
      call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
      dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
      call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
      do iomega=0,w%nomega
        call pw_w_borrow_wfc6d(w,w_matrix(iomega)%val,idk,iomega=iomega)
      end do
      call pw_wfc_init(dip_wfc(:),basis_coulomb)
      call pw_wfc_init(dip_wfc2,basis_coulomb)
      do ib=sigma%nbmin,sigma%nbmax
        call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
        call pw_wfc2field(wfc_field(ib),wfc)
        call pw_states_giveback_wfc(sigma%states,wfc)
      end do
 
      do ibx = sigma%states%nbmin,sigma%states%nbmax
        if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
          scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
          call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
          call pw_wfc2field(exc_field,exc_wfc)
          call pw_states_giveback_wfc(sigma%states,exc_wfc)
          do ib = sigma%nbmin,sigma%nbmax
            call pw_field_mul(dip_field,exc_field,wfc_field(ib))
            call pw_field2wfc(dip_wfc(ib),dip_field)
          end do

          if(sigma%states%occupation(ibx,ikx)<1) then
             segnoibx=-1.0 ! conduction
          else
             segnoibx=1.0 ! valence
          endif     

          do ib = sigma%nbmin, sigma%nbmax
            npw_small = ubound(w_matrix(0)%val%val,1)
            do iomega = 0,w%nomega
              dip_wfc2%val = 0.0
              dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small))
              do ibp = sigma%nbmin,sigma%nbmax
                sigma%val(ibp,ib) = sigma%val(ibp,ib)- segnoibx*0.5*pw_wfc_braket(dip_wfc(ibp),dip_wfc2) * scale 
              end do !ibp
            end do !omega
          end do !ib
      end do !ibx
 
      call pw_wfc_destroy(dip_wfc(:))
      call pw_wfc_destroy(dip_wfc2)
      call pw_basis_destroy(basis_coulomb)
      do iomega=0,w%nomega
        call pw_w_giveback_wfc6d(w,w_matrix(iomega)%val)
      end do
    end do
    deallocate(w_matrix)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_field_destroy(wfc_field(ib))
    end do
    deallocate(wfc_field,dip_wfc)
    call pw_field_destroy(dip_field)
    call pw_field_destroy(exc_field)

  endif ! end if diagonal
endif ! end gw_integration_method

! just reduce green function
  call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)
end subroutine pw_sigma_c_sscalc_x

subroutine pw_sigma_c_sshfcalc_x(sigma,w,cutoff,comm)
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
  use pw_states_module
  implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_w),     intent(in)    :: w
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm
  integer :: ikx ! index of kx   (exchange wfc)
  integer :: idk! index of kx-k (W)
  integer :: dim(3)
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type wfc6dpointer
    type (pw_wfc6d), pointer :: val
  end type wfc6dpointer
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2
  type (wfc6dpointer), allocatable :: w_matrix(:)
  integer :: iomega,ib,ibp, ibx
  real    :: scale
  real :: segnoibx
  complex :: braket_tmp

if(sigma%diagonal) then
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(w_matrix(0:w%nomega))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(w%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,w%nomega
      call pw_w_borrow_wfc6d(w,w_matrix(iomega)%val,idk,iomega=iomega)
    end do
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do

        do ib = sigma%nbmin, sigma%nbmax
          npw_small = ubound(w_matrix(0)%val%val,1)
          dip_wfc2%val = 0.0
          dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small))
            braket_tmp=pw_wfc_braket(dip_wfc(ib),dip_wfc2)      
            sigma%val(ib,ib) = sigma%val(ib,ib)- 0.5*braket_tmp * scale * &
            sigma%states%occupation(ibx,ikx)
        end do !ib
    end do !ibx

    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_basis_destroy(basis_coulomb)
    do iomega=0,w%nomega
      call pw_w_giveback_wfc6d(w,w_matrix(iomega)%val)
    end do
  end do
  deallocate(w_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

else        
  sigma%val(:,:)=0.0

  call pw_field_init(dip_field,sigma%states%struct)
  call pw_field_init(exc_field,sigma%states%struct)
  allocate(wfc_field(sigma%nbmin:sigma%nbmax))
  allocate(dip_wfc(sigma%nbmin:sigma%nbmax))
  allocate(w_matrix(0:w%nomega))
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_init(wfc_field(ib),sigma%states%struct)
  end do

  k = sigma%states%kmesh%kbz(:,sigma%ik)
  do ikx = 1,ubound(sigma%states%wfc,2)
    kx = sigma%states%kmesh%kbz(:,ikx)
    idk = pw_kmesh_kbz_index(w%qmesh,(k-kx))
    call pw_basis_init(basis_coulomb,sigma%states%struct)
    call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
    dim = pw_field_dim_from_dipole(basis_coulomb,sigma%states%basis(ikx),sigma%states%basis(sigma%ik))
    call pw_field_set_dim(wfc_field,    dim,k=k,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(exc_field,    dim,k=kx,r0=(/0.0,0.0,0.0/))
    call pw_field_set_dim(dip_field,    dim,k=k-kx,r0=(/0.0,0.0,0.0/))
    do iomega=0,w%nomega
      call pw_w_borrow_wfc6d(w,w_matrix(iomega)%val,idk,iomega=iomega)
    end do
    call pw_wfc_init(dip_wfc(:),basis_coulomb)
    call pw_wfc_init(dip_wfc2,basis_coulomb)
    do ib=sigma%nbmin,sigma%nbmax
      call pw_states_borrow_wfc(sigma%states,wfc,ib,sigma%ik)
      call pw_wfc2field(wfc_field(ib),wfc)
      call pw_states_giveback_wfc(sigma%states,wfc)
    end do

    do ibx = sigma%states%nbmin,sigma%states%nbmax
      if(.not.pw_states_is_local(sigma%states,ib=ibx,ik=ikx)) cycle
        scale = 1.0 / sigma%states%struct%a_omega * sigma%states%weight(ibx,ikx)
        call pw_states_borrow_wfc(sigma%states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(sigma%states,exc_wfc)
        do ib = sigma%nbmin,sigma%nbmax
          call pw_field_mul(dip_field,exc_field,wfc_field(ib))
          call pw_field2wfc(dip_wfc(ib),dip_field)
        end do

        do ib = sigma%nbmin, sigma%nbmax
          npw_small = ubound(w_matrix(0)%val%val,1)
          dip_wfc2%val = 0.0
          dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small))
          do ibp = sigma%nbmin,sigma%nbmax
            sigma%val(ibp,ib) = sigma%val(ibp,ib)- &
              0.5*pw_wfc_braket(dip_wfc(ibp),dip_wfc2)*scale*sigma%states%occupation(ibx,ikx) 
          end do !ibp
        end do !ib
    end do !ibx
 
    call pw_wfc_destroy(dip_wfc(:))
    call pw_wfc_destroy(dip_wfc2)
    call pw_basis_destroy(basis_coulomb)
    do iomega=0,w%nomega
      call pw_w_giveback_wfc6d(w,w_matrix(iomega)%val)
    end do
  end do
  deallocate(w_matrix)
  do ib=sigma%nbmin,sigma%nbmax
    call pw_field_destroy(wfc_field(ib))
  end do
  deallocate(wfc_field,dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

endif ! end if diagonal

! just reduce green function
  call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)
  
end subroutine pw_sigma_c_sshfcalc_x

subroutine pw_sigma_QPenergies_collect_x(sigma,QP,first_QPcall,outdir)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, ptk_comm, &
      ptk_bcast, ptk_comm_rank
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

  integer :: ib
  type(ptk_comm) :: comm
  integer :: root
  integer :: myrank

  type(pw_parall_complex_matrix) :: matrix

if(first_QPcall) then

    root=sigma%states%root
    comm=sigma%states%comm

! init of structure parall_matix
    call pw_parall_matrix_init(matrix,sigma%nbmin,sigma%nbmax,root,comm)

! lecture of single-particle hmatrix
    if(matrix%rank==matrix%root) call iotk_open_read(10,file=trim(outdir)//"exp_sp"//trim(iotk_index(sigma%ik)), &
                                      binary=.false.)
    call pw_parall_matrix_read(matrix,10,name="exp_sp"//trim(iotk_index(sigma%ik)))
    if(matrix%rank==matrix%root) call iotk_close_read(10)
! ---------------------------------------------------------------
! fill of sigma
! root collect single_particle hmatrix on sigma%val just the first time the call
! is done
    if(.not.sigma%diagonal) then
      call pw_parall_matrix_collect(sigma%val,matrix,root,comm)
! bcast sigma%exchange
    call ptk_bcast(sigma%val,root,comm)
    endif
            
! ---------------------------------------------------------------
  do ib=sigma%nbmin,sigma%nbmax
    QP%energies(ib,sigma%ik)=real(sigma%val(ib,ib))+real(matrix%eigenval(ib))
  enddo
  if(.not.sigma%diagonal) then
    QP%eigenvec(:,:,sigma%ik)=sigma%val(:,:)
  endif      
! ---------------------------------------------------------------
else
  do ib=sigma%nbmin,sigma%nbmax
     QP%energies(ib,sigma%ik)=QP%energies(ib,sigma%ik)+real(sigma%val(ib,ib))
  enddo
  if(.not.sigma%diagonal) then
     QP%eigenvec(:,:,sigma%ik)=QP%eigenvec(:,:,sigma%ik)+sigma%val(:,:)     
  endif        
endif
if(first_QPcall) call pw_parall_matrix_destroy(matrix)
end subroutine pw_sigma_QPenergies_collect_x

subroutine pw_sigma_QPenergies_firstorder_corr_x(dsigma,QP,diagonal)
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum, ptk_comm, &
      ptk_bcast
  use num_module
  use iotk_module
  use tools_module
  use pw_parall_matrix_module
  use pw_sigma_type
  use pw_QP_module
  type(pw_sigma), intent(in) :: dsigma
  type(pw_QP), intent(inout) :: QP
  logical, intent(in) :: diagonal

  integer :: ib
  type(ptk_comm) :: comm
  integer :: root


if(diagonal) then
! ---------------------------------------------------------------
  do ib=dsigma%nbmin,dsigma%nbmax
    QP%energies(ib,dsigma%ik)=(QP%energies(ib,dsigma%ik)-real(dsigma%val(ib,ib))*dsigma%states%e(ib,dsigma%ik))*&
       1.0/(1.0-real(dsigma%val(ib,ib)))
  enddo
! ---------------------------------------------------------------
endif

end subroutine pw_sigma_QPenergies_firstorder_corr_x


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

  integer :: ib
  type(ptk_comm) :: comm
  integer :: root

if(.not.diagonal) then
! diagonalization      
  call num_he_diag(QP%eigenvec(:,:,ik),QP%energies(:,ik),evonly=.false.)
! ---------------------------------------------------------------
endif
end subroutine pw_sigma_QPdiag_x


subroutine pw_sigma_write_x(sigma,unit,name,fmt)
  use iotk_module
  use pw_sigma_type
  type (pw_sigma), intent(in) :: sigma
  integer,         intent(in) :: unit
  character(*),    intent(in) :: name
  character(*), optional, intent(in) ::fmt

  character(iotk_attlenx) :: attr
  character(50) :: fmt_local
  integer :: ik,ib,ikx,ibx,iomega

  fmt_local = "iotk"
  if(present(fmt)) fmt_local = fmt

  call iotk_write_attr(attr,"type","pw_sigma",first=.true.)
  call iotk_write_begin(unit,name,attr)
  call iotk_write_dat(unit,"val",sigma%val)
  call iotk_write_dat(unit,"diagonal",sigma%diagonal)

  call iotk_write_end(unit,name)
end subroutine pw_sigma_write_x

subroutine pw_sigma_read_x(sigma,unit,name)
  use iotk_module
  use pw_sigma_type
  type (pw_sigma), intent(inout) :: sigma
  integer,         intent(in)    :: unit
  character(*),    intent(in)    :: name

  character(iotk_attlenx) :: attr
  character(iotk_vallenx) :: rtype

  call iotk_scan_begin(unit,name,attr)
  call iotk_scan_attr(attr,"type",rtype,default="pw_sigma")
  if(rtype/="pw_sigma") ERROR("")
  call iotk_scan_dat(unit,"val",sigma%val)
  call iotk_scan_dat(unit,"diagonal",sigma%diagonal)

  call iotk_scan_end(unit,name)
end subroutine pw_sigma_read_x

subroutine pw_sigma_bcast_x(sigma,root,comm)
  use iotk_module
  use ptk_module, only : ptk_bcast, ptk_comm_rank, ptk_comm
  use pw_sigma_type
  type (pw_sigma), intent(inout) :: sigma
  integer,         intent(in) :: root
  type(ptk_comm),  intent(in) :: comm

  call ptk_bcast(sigma%val,root,comm)
  call ptk_bcast(sigma%diagonal,root,comm)
end subroutine pw_sigma_bcast_x


subroutine pw_sigma_readbcast_x(sigma,unit,name,root,comm)
  use iotk_module
  use ptk_module, only : ptk_comm_rank, ptk_comm
  use pw_sigma_type
  use pw_sigma_interf, only : pw_sigma_read, pw_sigma_bcast
  type (pw_sigma), intent(inout) :: sigma
  integer,         intent(in)    :: unit
  character(*),    intent(in)    :: name
  integer,         intent(in)    :: root
  type(ptk_comm),  intent(in)    :: comm

  integer :: rank

  call ptk_comm_rank(comm,rank)
  if(rank==root) call pw_sigma_read(sigma,unit,name)
  call pw_sigma_bcast(sigma,root,comm)

end subroutine pw_sigma_readbcast_x


subroutine pw_hf_calc3_x(QP,states,nbmin,nbmax,ik,diagonal,qmesh,cutoff, &
           coulomb_div_treatment,ecutvcut,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm, ptk_bcast 
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
  use pw_QP_module
  use pw_states_module
  use coulomb_vcut_module, only : vcut_type
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

  integer :: ib, ibp, ibx
  integer :: ikx, idk
  integer :: dim(3)
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb)     :: coulomb
  type (pw_wfc)         :: wfc_coulomb
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2
  real    :: scale
  real :: lattice(3,3)
  real :: a_omega

  complex, dimension(nbmin:nbmax,nbmin:nbmax) :: hf
  real, dimension(nbmin:nbmax) :: hf_eigenval

  integer :: myrank
  type(ptk_comm) :: comm
  integer :: root

  type(pw_parall_complex_matrix) :: matrix

  real :: ecutvcut_loc
  integer :: system_type
  type (vcut_type) :: vcut
  
  ecutvcut_loc = 0.01
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut

  root=states%root
  comm=states%comm

! init of structure parall_matrix
  call pw_parall_matrix_init(matrix,nbmin,nbmax,root,comm)
! lecture of single-particle hmatrix  
  if(matrix%rank==matrix%root) call iotk_open_read(10,file=trim(outdir)//"exp_sp"//trim(iotk_index(ik)),&
                                     binary=.false.)
  call pw_parall_matrix_read(matrix,10,name="exp_sp"//trim(iotk_index(ik)))
  if(matrix%rank==matrix%root) call iotk_close_read(10)

  hf(:,:) =(0.0,0.0)
  hf_eigenval(:)=0.0


  lattice=num_matmul(states%struct%b,qmesh%m_inv)

  
  if(trim(coulomb_div_treatment)=="vcut_ws") then
    call pw_vcut_init(vcut,states%struct,qmesh,ecutvcut_loc)
    call pw_coulomb_init(coulomb,lattice,vcut)
  else
    call pw_coulomb_init(coulomb,lattice)
  endif

  if(trim(coulomb_div_treatment)=="vcut_ws") then
    system_type=1
  elseif(trim(coulomb_div_treatment)=="vcut_spherical") then
    system_type=0
  else
    system_type=3
  endif


if(.not.diagonal) then

  call pw_field_init(dip_field,states%struct)
  call pw_field_init(exc_field,states%struct)
  allocate(wfc_field(nbmin:nbmax))
  allocate(dip_wfc(nbmin:nbmax))
  do ib=nbmin,nbmax
     call pw_field_init(wfc_field(ib),states%struct)
  enddo
  k = states%kmesh%kbz(:,ik)
  do ikx=1,ubound(states%wfc,2)
     kx = states%kmesh%kbz(:,ikx)
     idk = pw_kmesh_kbz_index(qmesh,(k-kx))
     call pw_basis_init(basis_coulomb,states%struct)
     call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
     call pw_wfc_init(wfc_coulomb,basis_coulomb)
     call pw_coulomb_get(system_type,coulomb,wfc_coulomb)
!     if(idk==pw_kmesh_kibz_index(qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
     dim = pw_field_dim_from_dipole(basis_coulomb,states%basis(ikx), &
      states%basis(ik))
     call pw_field_set_dim(wfc_field, dim, k=k,r0=(/0.0,0.0,0.0/))
     call pw_field_set_dim(exc_field, dim, k=kx,r0=(/0.0,0.0,0.0/))
     call pw_field_set_dim(dip_field, dim, k=k-kx,r0=(/0.0,0.0,0.0/))
     call pw_wfc_init(dip_wfc(:),basis_coulomb)
     call pw_wfc_init(dip_wfc2,basis_coulomb)
     do ib=nbmin,nbmax
        call pw_states_borrow_wfc(states,wfc,ib,ik)
        call pw_wfc2field(wfc_field(ib),wfc)
        call pw_states_giveback_wfc(states,wfc)
     enddo

     do ibx=states%nbmin,states%nbmax
        if(.not.pw_states_is_local(states,ib=ibx,ik=ikx)) cycle
        scale=1.0/states%struct%a_omega*states%weight(ibx,ikx)
        call pw_states_borrow_wfc(states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(states,exc_wfc)
        do ib=nbmin,nbmax
           call pw_field_mul(dip_field,exc_field,wfc_field(ib))
           call pw_field2wfc(dip_wfc(ib),dip_field)
        enddo
        do ib=nbmin,nbmax
           call num_vemul(dip_wfc2%val,wfc_coulomb%val,dip_wfc(ib)%val)
           do ibp=nbmin,nbmax 
              hf(ibp,ib)=hf(ibp,ib)-pw_wfc_braket(dip_wfc(ibp),dip_wfc2) * &
                    states%occupation(ibx,ikx)*0.5*scale
           enddo  
        enddo     
     enddo
   
     call pw_wfc_destroy(dip_wfc(:))
     call pw_wfc_destroy(dip_wfc2)
     call pw_wfc_destroy(wfc_coulomb)
     call pw_basis_destroy(basis_coulomb)
  enddo   
  do ib=nbmin,nbmax
     call pw_field_destroy(wfc_field(ib))
  enddo
  deallocate(wfc_field)
  deallocate(dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

! just reduce green function   
  call ptk_allreduce_inplace(hf,ptk_sum,states%comm) 
! -------------------------------------------------------
! root collect single-particle hmatrix on hf matrix
  call pw_parall_matrix_collect(hf,matrix,root,comm)
! -------------------------------------------------------
! bcast hf matrix
  call ptk_bcast(hf,root,comm)
! -------------------------------------------------------
! diagonalization 
  call num_he_diag(hf,hf_eigenval,evonly=.false.)
! fill of QP  
  QP%energies(:,ik)=hf_eigenval(:)
  QP%eigenvec(:,:,ik)=hf(:,:)  
! -------------------------------------------------------

else

  call pw_field_init(dip_field,states%struct)
  call pw_field_init(exc_field,states%struct)
  allocate(wfc_field(nbmin:nbmax))
  allocate(dip_wfc(nbmin:nbmax))
  do ib=nbmin,nbmax
     call pw_field_init(wfc_field(ib),states%struct)
  enddo
  k = states%kmesh%kbz(:,ik)
  do ikx=1,ubound(states%wfc,2)
     kx = states%kmesh%kbz(:,ikx)
     idk = pw_kmesh_kbz_index(qmesh,(k-kx))
     call pw_basis_init(basis_coulomb,states%struct)
     call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
     call pw_wfc_init(wfc_coulomb,basis_coulomb)
     call pw_coulomb_get(system_type,coulomb,wfc_coulomb)
!     if(idk==pw_kmesh_kibz_index(qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
     dim = pw_field_dim_from_dipole(basis_coulomb,states%basis(ikx), &
      states%basis(ik))
     call pw_field_set_dim(wfc_field, dim, k=k,r0=(/0.0,0.0,0.0/))
     call pw_field_set_dim(exc_field, dim, k=kx,r0=(/0.0,0.0,0.0/))
     call pw_field_set_dim(dip_field, dim, k=k-kx,r0=(/0.0,0.0,0.0/))
     call pw_wfc_init(dip_wfc(:),basis_coulomb)
     call pw_wfc_init(dip_wfc2,basis_coulomb)
     do ib=nbmin,nbmax
        call pw_states_borrow_wfc(states,wfc,ib,ik)
        call pw_wfc2field(wfc_field(ib),wfc)
        call pw_states_giveback_wfc(states,wfc)
     enddo

     do ibx=states%nbmin,states%nbmax
        if(.not.pw_states_is_local(states,ib=ibx,ik=ikx)) cycle
        scale=1.0/states%struct%a_omega*states%weight(ibx,ikx)
        call pw_states_borrow_wfc(states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(states,exc_wfc)
        do ib=nbmin,nbmax
           call pw_field_mul(dip_field,exc_field,wfc_field(ib))
           call pw_field2wfc(dip_wfc(ib),dip_field)
        enddo
        do ib=nbmin,nbmax
           call num_vemul(dip_wfc2%val,wfc_coulomb%val,dip_wfc(ib)%val)
           hf(ib,ib)=hf(ib,ib)-pw_wfc_braket(dip_wfc(ib),dip_wfc2) * &
                 states%occupation(ibx,ikx)*0.5*scale
        enddo     
     enddo
        
     call pw_wfc_destroy(dip_wfc(:))
     call pw_wfc_destroy(dip_wfc2)
     call pw_wfc_destroy(wfc_coulomb)
     call pw_basis_destroy(basis_coulomb)
  enddo   
  do ib=nbmin,nbmax
     call pw_field_destroy(wfc_field(ib))
  enddo
  deallocate(wfc_field)
  deallocate(dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

! just reduce green function   
  call ptk_allreduce_inplace(hf,ptk_sum,states%comm) 
! -------------------------------------------------------
! for diagonal option we do not need to collect
! matrix%eigenval are known by all procs
! -------------------------------------------------------
! QP
  do ib=nbmin,nbmax
     QP%energies(ib,ik)=real(hf(ib,ib))+matrix%eigenval(ib)
  enddo   

endif 
! ----------------------------------------------------- 
! destruction of structure parall_matrix
call pw_parall_matrix_destroy(matrix)

if(system_type==1) call pw_vcut_destroy(vcut)
call pw_coulomb_destroy(coulomb)

end subroutine pw_hf_calc3_x

subroutine pw_hf_calc0_x(QP,states,nbmin,nbmax,ik,diagonal,qmesh,cutoff)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm, ptk_bcast 
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
  use pw_QP_module
  use pw_states_module
  type(pw_QP), intent(inout) :: QP
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  logical, intent(in) :: diagonal
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff

  integer :: ib, ibp, ibx
  integer :: ikx, idk
  integer :: dim(3)
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb)     :: coulomb
  type (pw_wfc)         :: wfc_coulomb
  type (pw_field), allocatable :: wfc_field(:)
  type (pw_wfc),   allocatable :: dip_wfc(:)
  type (pw_wfc) :: dip_wfc2
  real    :: scale
  real :: lattice(3,3)
  real :: a_omega

  complex, dimension(nbmin:nbmax,nbmin:nbmax) :: hf
  real, dimension(nbmin:nbmax) :: hf_eigenval

  integer :: myrank
  type(ptk_comm) :: comm
  integer :: root

  type(pw_parall_complex_matrix) :: matrix


  root=states%root
  comm=states%comm

! init of structure parall_matrix
  call pw_parall_matrix_init(matrix,nbmin,nbmax,root,comm)

! lecture of single-particle hmatrix  
  if(matrix%rank==matrix%root) call iotk_open_read(10,file="exp_sp"//trim(iotk_index(ik)),binary=.false.)
  call pw_parall_matrix_read(matrix,10,name="exp_sp"//trim(iotk_index(ik)))
  if(matrix%rank==matrix%root) call iotk_close_read(10)


  hf(:,:) =(0.0,0.0)
  hf_eigenval(:)=0.0
  
if(.not.diagonal) then

  lattice=num_matmul(states%struct%b,qmesh%m_inv)

  call pw_field_init(dip_field,states%struct)
  call pw_field_init(exc_field,states%struct)
  allocate(wfc_field(nbmin:nbmax))
  allocate(dip_wfc(nbmin:nbmax))
  do ib=nbmin,nbmax
     call pw_field_init(wfc_field(ib),states%struct)
  enddo
  k = states%kmesh%kbz(:,ik)
  do ikx=1,ubound(states%wfc,2)
     kx = states%kmesh%kbz(:,ikx)
     idk = pw_kmesh_kbz_index(qmesh,(k-kx))
     call pw_basis_init(basis_coulomb,states%struct)
     call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
     call pw_coulomb_init(coulomb,states%struct%b)
     call pw_wfc_init(wfc_coulomb,basis_coulomb)
     call pw_coulomb_get(0,coulomb,wfc_coulomb)
     call pw_coulomb_destroy(coulomb)
     dim = pw_field_dim_from_dipole(basis_coulomb,states%basis(ikx), &
      states%basis(ik))
     call pw_field_set_dim(wfc_field, dim, k=k,r0=(/0.0,0.0,0.0/))
     call pw_field_set_dim(exc_field, dim, k=kx,r0=(/0.0,0.0,0.0/))
     call pw_field_set_dim(dip_field, dim, k=k-kx,r0=(/0.0,0.0,0.0/))
     call pw_wfc_init(dip_wfc(:),basis_coulomb)
     call pw_wfc_init(dip_wfc2,basis_coulomb)
     do ib=nbmin,nbmax
        call pw_states_borrow_wfc(states,wfc,ib,ik)
        call pw_wfc2field(wfc_field(ib),wfc)
        call pw_states_giveback_wfc(states,wfc)
     enddo

     do ibx=states%nbmin,states%nbmax
        if(.not.pw_states_is_local(states,ib=ibx,ik=ikx)) cycle
        scale=1.0/states%struct%a_omega*states%weight(ibx,ikx)
        call pw_states_borrow_wfc(states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(states,exc_wfc)
        do ib=nbmin,nbmax
           call pw_field_mul(dip_field,exc_field,wfc_field(ib))
           call pw_field2wfc(dip_wfc(ib),dip_field)
        enddo
        do ib=nbmin,nbmax
           call num_vemul(dip_wfc2%val,wfc_coulomb%val,dip_wfc(ib)%val)
           do ibp=nbmin,nbmax 
              hf(ibp,ib)=hf(ibp,ib)-pw_wfc_braket(dip_wfc(ibp),dip_wfc2) * &
                    states%occupation(ibx,ikx)*0.5*scale
           enddo  
        enddo     
     enddo
   
     call pw_wfc_destroy(dip_wfc(:))
     call pw_wfc_destroy(dip_wfc2)
     call pw_wfc_destroy(wfc_coulomb)
     call pw_basis_destroy(basis_coulomb)
  enddo   
  do ib=nbmin,nbmax
     call pw_field_destroy(wfc_field(ib))
  enddo
  deallocate(wfc_field)
  deallocate(dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

! just reduce green function   
  call ptk_allreduce_inplace(hf,ptk_sum,states%comm) 
! -------------------------------------------------------
! root collect single-particle hmatrix on hf matrix
  call pw_parall_matrix_collect(hf,matrix,root,comm)
! -------------------------------------------------------
! bcast hf matrix
  call ptk_bcast(hf,root,comm)
! -------------------------------------------------------
! diagonalization  
  call num_he_diag(hf,hf_eigenval,evonly=.false.)
! fill of QP  
  QP%energies(:,ik)=hf_eigenval(:)
  QP%eigenvec(:,:,ik)=hf(:,:)  
! -------------------------------------------------------

else

  lattice=num_matmul(states%struct%b,qmesh%m_inv)

  call pw_field_init(dip_field,states%struct)
  call pw_field_init(exc_field,states%struct)
  allocate(wfc_field(nbmin:nbmax))
  allocate(dip_wfc(nbmin:nbmax))
  do ib=nbmin,nbmax
     call pw_field_init(wfc_field(ib),states%struct)
  enddo
  k = states%kmesh%kbz(:,ik)
  do ikx=1,ubound(states%wfc,2)
     kx = states%kmesh%kbz(:,ikx)
     idk = pw_kmesh_kbz_index(qmesh,(k-kx))
     call pw_basis_init(basis_coulomb,states%struct)
     call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
     call pw_coulomb_init(coulomb,states%struct%b)
     call pw_wfc_init(wfc_coulomb,basis_coulomb)
     call pw_coulomb_get(0,coulomb,wfc_coulomb)
     call pw_coulomb_destroy(coulomb)
     dim = pw_field_dim_from_dipole(basis_coulomb,states%basis(ikx), &
      states%basis(ik))
     call pw_field_set_dim(wfc_field, dim, k=k,r0=(/0.0,0.0,0.0/))
     call pw_field_set_dim(exc_field, dim, k=kx,r0=(/0.0,0.0,0.0/))
     call pw_field_set_dim(dip_field, dim, k=k-kx,r0=(/0.0,0.0,0.0/))
     call pw_wfc_init(dip_wfc(:),basis_coulomb)
     call pw_wfc_init(dip_wfc2,basis_coulomb)
     do ib=nbmin,nbmax
        call pw_states_borrow_wfc(states,wfc,ib,ik)
        call pw_wfc2field(wfc_field(ib),wfc)
        call pw_states_giveback_wfc(states,wfc)
     enddo

     do ibx=states%nbmin,states%nbmax
        if(.not.pw_states_is_local(states,ib=ibx,ik=ikx)) cycle
        scale=1.0/states%struct%a_omega*states%weight(ibx,ikx)
        call pw_states_borrow_wfc(states,exc_wfc,ibx,ikx)
        call pw_wfc2field(exc_field,exc_wfc)
        call pw_states_giveback_wfc(states,exc_wfc)
        do ib=nbmin,nbmax
           call pw_field_mul(dip_field,exc_field,wfc_field(ib))
           call pw_field2wfc(dip_wfc(ib),dip_field)
        enddo
        do ib=nbmin,nbmax
           call num_vemul(dip_wfc2%val,wfc_coulomb%val,dip_wfc(ib)%val)
           hf(ib,ib)=hf(ib,ib)-pw_wfc_braket(dip_wfc(ib),dip_wfc2) * &
                 states%occupation(ibx,ikx)*0.5*scale
        enddo     
     enddo
   
     call pw_wfc_destroy(dip_wfc(:))
     call pw_wfc_destroy(dip_wfc2)
     call pw_wfc_destroy(wfc_coulomb)
     call pw_basis_destroy(basis_coulomb)
  enddo   
  do ib=nbmin,nbmax
     call pw_field_destroy(wfc_field(ib))
  enddo
  deallocate(wfc_field)
  deallocate(dip_wfc)
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)

! just reduce green function   
  call ptk_allreduce_inplace(hf,ptk_sum,states%comm) 
! -------------------------------------------------------
! for diagonal option we do not need to collect
! matrix%eigenval are known by all procs
! -------------------------------------------------------
! QP
  do ib=nbmin,nbmax
     QP%energies(ib,ik)=hf(ib,ib)+matrix%eigenval(ib)
  enddo   

endif 
! ----------------------------------------------------- 
! destruction of structure parall_matrix
call pw_parall_matrix_destroy(matrix)
 
end subroutine pw_hf_calc0_x

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
  integer :: ifermi,ie
  real :: asum(ubound(a,1))
  real :: esum(ubound(a,1))
  real :: c0,c1,c2
  real :: nparticles_tail,tail_alpha,tail_beta
  integer :: iter
  if(size(a) /= size(omega)) ERROR("")
  if(size(e) /= size(omega)) ERROR("")
! fitto con
! a = alpha / (beta - omega)**3
  tail_beta  = omega(size(omega))
  do iter = 1 , 100
    tail_alpha = atail * (tail_beta - omegatail)**3
    tail_beta  = omega(1) + exp(log(tail_alpha/a(1))/3.0)
  end do
! e' il modo piu' cretino ma efficacie di interpolare
  write(0,*) tail_alpha,tail_beta,atail,omegatail,omega(1),etail
call tools_log("SUM")
  asum=0.0
  esum=0.0
  asum(1) = 0.5 * tail_alpha /(tail_beta-omega(1))**2
  select case(ntail)
  case(2)
    esum(1) = - 0.5 * tail_alpha * (tail_beta-2*omega(1)) / (tail_beta-omega(1))**2 / atail / omegatail * etail
  case(3)
    esum(1) = asum(1) / atail * etail
  end select
  do ie=2,ubound(a,1)
    asum(ie) = asum(ie-1) + 0.5*(a(ie) + a(ie-1)) * (omega(ie)-omega(ie-1))
    esum(ie) = esum(ie-1) + 0.5*(e(ie) + e(ie-1)) * (omega(ie)-omega(ie-1))
  end do
  do ie = 1 , ubound(a,1)
    if(asum(ie) > fixnparticles) exit
  end do
  if(ie<=ubound(a,1)) then
    ifermi = ie - 1
    c0 = asum(ifermi) - fixnparticles
    c1 = a(ifermi)
    c2 = 0.5 * (a(ifermi+1)-a(ifermi)) / (omega(ifermi+1)-omega(ifermi))
    if(c1**2-4.0*c0*c2 <= 0.0) ERROR("")
    efermi = omega(ifermi) + (-c1+sqrt(c1**2-4.0*c0*c2))/(2.0*c2)
  else
! In this case,  integrate over the full domain
    efermi = omega(ubound(omega,1))
    ifermi = ubound(a,1) - 1
  end if
  
call tools_log("SUM1")
  nparticles = asum(ifermi) + a(ifermi) * (efermi-omega(ifermi))  &
    + 0.5 * (a(ifermi+1)-a(ifermi)) / (omega(ifermi+1)-omega(ifermi)) * (efermi-omega(ifermi))**2
  energy     = esum(ifermi) + e(ifermi) * (efermi-omega(ifermi))  &
    + 0.5 * (e(ifermi+1)-e(ifermi)) / (omega(ifermi+1)-omega(ifermi)) * (efermi-omega(ifermi))**2
end subroutine calc_efermi_x
