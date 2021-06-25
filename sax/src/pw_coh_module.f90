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
module pw_coh_module
use pw_basis_module
use pw_struct_module
use pw_wfc_module
use pw_w_module
use ptk_module
use pw_hartree_module
use pw_common_module
implicit none
! This module contains the object pw_hartree and its methods
private
public :: pw_vcoh_calc
public :: pw_sigmacoh_calc
public :: pw_sigmasex_calc
!@ END MANUAL

contains

subroutine pw_vcoh_calc(w,vcoh,comm)
! this routine computes the COH potential

type(pw_w), intent(in) :: w
type(pw_hartree), intent(out) :: vcoh
type(ptk_comm), intent(in) :: comm

integer :: K1min, K1max, K2min, K2max, K3min, K3max
complex, allocatable :: tmp(:,:,:)
integer :: owner, location
integer:: iq, ipw,jpw,npw
integer :: g1, g2, g3, gprime1, gprime2, gprime3
integer :: g_gprime1, g_gprime2, g_gprime3

integer :: ierr

! all proc allocate all basis(iq) ... uuuu not too smart

K1min = vcoh%basis%gmin(1) - vcoh%basis%gmax(1)
K1max = vcoh%basis%gmax(1) - vcoh%basis%gmin(1) 
K2min = vcoh%basis%gmin(2) - vcoh%basis%gmax(2)
K2max = vcoh%basis%gmax(2) - vcoh%basis%gmin(2)
K3min = vcoh%basis%gmin(3) - vcoh%basis%gmax(3)
K3max = vcoh%basis%gmax(3) - vcoh%basis%gmin(3)

allocate(tmp(K1min:K1max,K2min:K2max,K3min:K3max), STAT = ierr)
if(ierr/=0) ERROR("Not enough mem for tmp array in vcoh")

tmp(:,:,:) = 0.0

do iq=1,w%qmesh%nkibz
    owner = w%where_q(1,iq)
    location = w%where_q(2,iq)
    if(w%rank /= owner ) cycle
    npw = w%basis(iq)%npw

    do jpw = 1,npw
       gprime1=w%basis(iq)%g(1,jpw)
       gprime2=w%basis(iq)%g(2,jpw)
       gprime3=w%basis(iq)%g(3,jpw)
       do ipw = 1,npw
          g1=w%basis(iq)%g(1,ipw)
          g2=w%basis(iq)%g(2,ipw)
          g3=w%basis(iq)%g(3,ipw)
          g_gprime1 = g1-gprime1
          g_gprime2 = g2-gprime2
          g_gprime3 = g3-gprime3
  

          tmp(g_gprime1,g_gprime2,g_gprime3) = tmp(g_gprime1,g_gprime2,g_gprime3) + &
             w%val(location,0)%val(ipw,jpw)
       enddo
    enddo

enddo

call ptk_allreduce_inplace(tmp,ptk_sum,comm)

npw=vcoh%basis%npw

do ipw=1,npw
   g1=vcoh%basis%g(1,ipw)
   g2=vcoh%basis%g(2,ipw)
   g3=vcoh%basis%g(3,ipw)
   vcoh%wfc%val(ipw)=tmp(g1,g2,g3)
enddo

deallocate(tmp)

end subroutine pw_vcoh_calc

subroutine pw_sigmacoh_calc(sigma,vcoh,cutoff,comm)

use tools_module
use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
use pw_wfc_module
use pw_field_module
use pw_fft_module
use pw_hartree_module
use pw_kmesh_module
use pw_sigma_type
use pw_states_module
use pw_dipole_module
implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_hartree), intent(in) :: vcoh
  real,           intent(in)    :: cutoff
  type(ptk_comm), intent(in) :: comm

  integer :: ib1, ib2
  real :: scale
  type(pw_wfc)     :: wfc_tmp1
  type(pw_wfc),pointer :: wfc_tmp
  type(pw_basis), pointer :: basis_tmp



if(sigma%diagonal) then
    call pw_states_borrow_basis(sigma%states,basis_tmp,ik=sigma%ik)
    call pw_wfc_init(wfc_tmp1,basis_tmp)
    do ib1=sigma%nbmin, sigma%nbmax
        if(pw_states_is_local(sigma%states,ib1,sigma%ik)) then
          scale = 1.0/(sigma%states%struct%a_omega) * sigma%states%weight(ib1,sigma%ik)
          call pw_states_borrow_wfc(sigma%states,wfc_tmp,ib=ib1,ik=sigma%ik)
          call pw_vloc_apply(wfc_tmp1,vcoh%wfc,wfc_tmp)
          sigma%val(ib1,ib1) = scale * 0.5 * pw_wfc_braket(wfc_tmp,wfc_tmp1)
          call pw_states_giveback_wfc(sigma%states,wfc_tmp)
!write(0,*) "coh sigmaval: ", sigma%val(ib1,ib1)
        else
          call pw_states_borrow_wfc(sigma%states,wfc_tmp,ib=ib1,ik=sigma%ik)
          call pw_states_giveback_wfc(sigma%states,wfc_tmp)
        endif

    enddo
    call pw_wfc_destroy(wfc_tmp1)
    call pw_states_giveback_basis(sigma%states,basis_tmp)

else
    call pw_states_borrow_basis(sigma%states,basis_tmp,ik=sigma%ik)
    call pw_wfc_init(wfc_tmp1,basis_tmp)
!    allocate(wfc_tmp2(sigma%nbmin,sigma%nbmax))
!    call pw_wfc_init(wfc_tmp2,basis_tmp)
!    do ib1=sigma%nbmin, sigma%nbmax
!       call pw_states_borrow_wfc(sigma%states,wfc_tmp1,ib=ib1,ik=sigma%ik)
!       wfc_tmp2(ib)%val(:) = wfc_tmp1%val(:)
!    endd

    do ib1=sigma%nbmin, sigma%nbmax
        if(pw_states_is_local(sigma%states,ib1,sigma%ik)) then
          scale = 1.0/(sigma%states%struct%a_omega) * sigma%states%weight(ib1,sigma%ik)
          call pw_states_borrow_wfc(sigma%states,wfc_tmp,ib=ib1,ik=sigma%ik)
          call pw_vloc_apply(wfc_tmp1,vcoh%wfc,wfc_tmp)
          call pw_states_giveback_wfc(sigma%states,wfc_tmp)

          do ib2=sigma%nbmin, sigma%nbmax
             scale = 1.0/(sigma%states%struct%a_omega) * sigma%states%weight(ib2,sigma%ik)
write(0,*) "ib1, ib2: ", ib1, ib2
             call pw_states_borrow_wfc(sigma%states,wfc_tmp,ib=ib2,ik=sigma%ik)

             sigma%val(ib2,ib1) = scale * 0.5 * pw_wfc_braket(wfc_tmp,wfc_tmp1)
             call pw_states_giveback_wfc(sigma%states,wfc_tmp)
!write(0,*) "coh sigmaval: ", sigma%val(ib2,ib1)
          enddo

        else
          call pw_states_borrow_wfc(sigma%states,wfc_tmp,ib=ib1,ik=sigma%ik)
          call pw_states_giveback_wfc(sigma%states,wfc_tmp)
          do ib2=sigma%nbmin, sigma%nbmax
             call pw_states_borrow_wfc(sigma%states,wfc_tmp,ib=ib2,ik=sigma%ik)
             call pw_states_giveback_wfc(sigma%states,wfc_tmp)
          enddo

        endif

    enddo


    call pw_wfc_destroy(wfc_tmp1)
    call pw_states_giveback_basis(sigma%states,basis_tmp)
endif

call ptk_allreduce_inplace(sigma%val,ptk_sum,comm)

end subroutine pw_sigmacoh_calc

subroutine pw_sigmasex_calc(sigma,w,cutoff,comm)
use tools_module
use ptk_module, only : ptk_allreduce_inplace,ptk_sum, &
    ptk_comm_rank, ptk_comm_size, ptk_comm, ptk_send, &
    ptk_recv
use num_module
use pw_wfc_module
use pw_field_module
use pw_fft_module
use pw_w_module
use pw_kmesh_module
use pw_sigma_type
use pw_states_module
use pw_wfc6d_module
implicit none
  type(pw_sigma), intent(inout) :: sigma
  type(pw_w), intent(in) :: w
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
  complex, allocatable :: sigma_tmp(:,:) 


if(sigma%diagonal) then
    allocate(sigma_tmp(sigma%nbmin:sigma%nbmax,sigma%nbmin:sigma%nbmax))
    sigma_tmp(:,:) = 0.0
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
            do iomega = 0,w%nomega
              dip_wfc2%val = 0.0
              dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small))
                braket_tmp=pw_wfc_braket(dip_wfc(ib),dip_wfc2)
                sigma_tmp(ib,ib) = sigma_tmp(ib,ib)- braket_tmp * scale * &
                sigma%states%occupation(ibx,ikx) * 0.5
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
    allocate(sigma_tmp(sigma%nbmin:sigma%nbmax,sigma%nbmin:sigma%nbmax))
    sigma_tmp(:,:) = 0.0
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
            do iomega = 0,w%nomega
              dip_wfc2%val = 0.0
              dip_wfc2%val(1:npw_small) = num_matmul(w_matrix(iomega)%val%val,dip_wfc(ib)%val(1:npw_small))
              do ibp = sigma%nbmin, sigma%nbmax
                braket_tmp=pw_wfc_braket(dip_wfc(ibp),dip_wfc2)
                sigma_tmp(ibp,ib) = sigma_tmp(ibp,ib)- braket_tmp * scale * &
                sigma%states%occupation(ibx,ikx) * 0.5
              enddo
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

call ptk_allreduce_inplace(sigma_tmp,ptk_sum,comm)

sigma%val(:,:) = sigma%val(:,:) + sigma_tmp(:,:)

deallocate(sigma_tmp)

end subroutine pw_sigmasex_calc

end module pw_coh_module

