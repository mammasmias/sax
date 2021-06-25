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

module pw_pseudovelocity_module
use pw_basis_module
use pw_wfc_module
implicit none

private

public :: pw_pseudovelocity, &
          pw_pseudovelocity_init, &
          pw_pseudovelocity_destroy, &
          pw_pseudovelocity_apply, &
          pw_pseudovelocity_braket

integer, parameter :: lmax = 3

type pw_pseudovelocity
  type(pw_basis), pointer :: basis
  integer                 :: nproj
  type(pw_wfc),   pointer :: proj(:)
  type(pw_wfc),   pointer :: dproj(:,:)
  real,           pointer :: d(:)
end type pw_pseudovelocity

contains

subroutine pw_pseudovelocity_init(velocity,basis,atoms)
  use pw_atoms_module
  use num_module
  use pw_pseudo_module
  use tools_module
  type(pw_pseudovelocity), intent(out) :: velocity
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  type(pw_pseudo), pointer :: pseudo
  integer :: nproj,iatom,ibeta,iproj,nbeta,ipw,npw,l,m
  complex :: tmp,tmp_vec(3)
  
  real :: b(3,3),kg(3),pos(3)
  real :: q(3,basis%npw),modq(basis%npw)
  complex :: struct_fact(basis%npw)
  complex, allocatable :: xlylm(:,:,:)
  complex, allocatable :: dxlylm(:,:,:,:)
  real    :: fl(basis%npw)
  real    :: dfl(3,basis%npw)

  allocate(xlylm(basis%npw,-lmax:lmax,0:lmax),dxlylm(3,basis%npw,-lmax:lmax,0:lmax))
  velocity%basis => basis
  b = basis%struct%b
  npw = basis%npw
  do ipw=1,npw
    kg = basis%k + real(basis%g(:,ipw))
    q(:,ipw) = num_matmul(b,kg)
    modq(ipw) = sqrt(sum(q(:,ipw)**2))
  end do
  xlylm = 0.0
  do l=0,lmax
    do m=-l,l
      do ipw=1,npw
        xlylm(ipw,m,l) = num_xlylm(q(:,ipw),l,m)
        dxlylm(:,ipw,m,l) = num_xlylm_grad(q(:,ipw),l,m)
      end do
    end do
  end do
  iproj = 0
  do iatom=1,atoms%natoms
    pseudo => atoms%pseudo(atoms%type_map(iatom))
    iproj = iproj + sum(2*pseudo%lbeta(:)+1)
  end do
  nproj = iproj
  velocity%nproj = nproj
  allocate(velocity%proj(nproj))
  allocate(velocity%dproj(3,nproj))
  call pw_wfc_init(velocity%proj,basis)
  call pw_wfc_init(velocity%dproj,basis)
  allocate(velocity%d(nproj))
  iproj=0
  do iatom=1,atoms%natoms
    pseudo => atoms%pseudo(atoms%type_map(iatom))
    nbeta = pseudo%nbeta
    pos = atoms%positions(:,iatom)
    do ipw=1,npw
      struct_fact(ipw) = exp(-num_2pi_i*dot_product(real(basis%g(:,ipw)),pos))
    end do
    do ibeta=1,nbeta
      l = pseudo%lbeta(ibeta)
      do ipw=1,npw
        fl(ipw) = num_interpolation_calc(pseudo%interpolation(ibeta),modq(ipw),3)
        if(modq(ipw) < 0.0000001) then
          dfl(:,ipw) = q(:,ipw) * 0.5 * &
                  num_interpolation_calc_der(pseudo%interpolation(ibeta),modq(ipw),3,ider=2)
        else
          dfl(:,ipw) = q(:,ipw)/modq(ipw) * &
                  num_interpolation_calc_der(pseudo%interpolation(ibeta),modq(ipw),3,ider=1)
        end if
      end do
      do m=-l,l
        iproj = iproj + 1
        velocity%d(iproj) = pseudo%d(ibeta)
! componente normale
        do ipw=1,npw
          tmp = (0.0,-1.0)**l * xlylm(ipw,m,l) * &
                num_interpolation_calc(pseudo%interpolation(ibeta),modq(ipw),3) * &
                struct_fact(ipw)
          velocity%proj(iproj)%val(ipw) = tmp
        end do
! componenti derivate
        do ipw=1,npw
          tmp_vec = (0.0,-1.0)**l * struct_fact(ipw) * &
                    (xlylm(ipw,m,l) * dfl(:,ipw) + dxlylm(:,ipw,m,l) * fl(ipw))
          velocity%dproj(1,iproj)%val(ipw) = tmp_vec(1)
          velocity%dproj(2,iproj)%val(ipw) = tmp_vec(2)
          velocity%dproj(3,iproj)%val(ipw) = tmp_vec(3)
        end do
      end do
    end do
  end do
  if(iproj /= nproj) ERROR(tools_char(iproj,4))
  deallocate(xlylm,dxlylm)
end subroutine pw_pseudovelocity_init

subroutine pw_pseudovelocity_apply(wfc_new,velocity,wfc)
  type (pw_wfc),   intent(inout) :: wfc_new(3)
  type (pw_pseudovelocity), intent(in)    :: velocity
  type (pw_wfc),   intent(in)    :: wfc
  integer :: iproj
  type (pw_wfc) :: wfc_tmp(3)
  type (pw_wfc) :: wfc_dtmp(3)
  complex :: projection(velocity%nproj)
  complex :: dprojection(3,velocity%nproj)

  real :: d
  do iproj=1,velocity%nproj
    d = velocity%d(iproj) / wfc_new(1)%basis%struct%a_omega
    projection(iproj) = pw_wfc_braket(velocity%proj(iproj),wfc) * d
    dprojection(1,iproj) = pw_wfc_braket(velocity%dproj(1,iproj),wfc) * d
    dprojection(2,iproj) = pw_wfc_braket(velocity%dproj(2,iproj),wfc) * d
    dprojection(3,iproj) = pw_wfc_braket(velocity%dproj(3,iproj),wfc) * d
  end do
  call pw_wfc_init(wfc_tmp,velocity%basis)
  call pw_wfc_init(wfc_dtmp,velocity%basis)
  wfc_new(1)%val = 0.0
  wfc_new(2)%val = 0.0
  wfc_new(3)%val = 0.0
  do iproj=1,velocity%nproj
    wfc_tmp(1) = velocity%proj(iproj)
    wfc_tmp(2) = velocity%proj(iproj)
    wfc_tmp(3) = velocity%proj(iproj)
    wfc_dtmp(:) = velocity%dproj(:,iproj)
    call pw_wfc_scale(wfc_tmp(1),dprojection(1,iproj))
    call pw_wfc_scale(wfc_tmp(2),dprojection(2,iproj))
    call pw_wfc_scale(wfc_tmp(3),dprojection(3,iproj))
    call pw_wfc_scale(wfc_dtmp(1),projection(iproj))
    call pw_wfc_scale(wfc_dtmp(2),projection(iproj))
    call pw_wfc_scale(wfc_dtmp(3),projection(iproj))
    wfc_new(1)%val = wfc_new(1)%val + wfc_tmp(1)%val + wfc_dtmp(1)%val
    wfc_new(2)%val = wfc_new(2)%val + wfc_tmp(2)%val + wfc_dtmp(2)%val
    wfc_new(3)%val = wfc_new(3)%val + wfc_tmp(3)%val + wfc_dtmp(3)%val
  end do
  call pw_wfc_destroy(wfc_tmp)
  call pw_wfc_destroy(wfc_dtmp)
end subroutine pw_pseudovelocity_apply

subroutine pw_pseudovelocity_destroy(velocity)
  type (pw_pseudovelocity), intent(inout) :: velocity
  call pw_wfc_destroy(velocity%proj)
  call pw_wfc_destroy(velocity%dproj)
  deallocate(velocity%proj)
  deallocate(velocity%dproj)
  deallocate(velocity%d)
end subroutine pw_pseudovelocity_destroy

function pw_pseudovelocity_braket(velocity,bra,ket)
  type (pw_pseudovelocity), intent(in) :: velocity
  type (pw_wfc), intent(in) :: bra,ket
  complex :: pw_pseudovelocity_braket(3)
  type (pw_wfc) :: wfc_tmp(3)
  call pw_wfc_init(wfc_tmp,bra%basis)
  call pw_pseudovelocity_apply(wfc_tmp,velocity,ket)
  pw_pseudovelocity_braket(1) = pw_wfc_braket(bra,wfc_tmp(1))
  pw_pseudovelocity_braket(2) = pw_wfc_braket(bra,wfc_tmp(2))
  pw_pseudovelocity_braket(3) = pw_wfc_braket(bra,wfc_tmp(3))
  call pw_wfc_destroy(wfc_tmp)
end function pw_pseudovelocity_braket

end module pw_pseudovelocity_module

