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

subroutine pw_vnloc_init_x1(vnloc,basis,atoms)
  use pw_atoms_module
  use num_module
  use pw_pseudo_module
  use tools_module
  use pw_vnloc_type
  use pw_basis_module
  use pw_wfc_module
  
  type(pw_vnloc), intent(out) :: vnloc
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  type(pw_pseudo), pointer :: pseudo
  integer :: nproj,iatom,ibeta,iproj,nbeta,ipw,npw,l,m,itype
  complex :: tmp

  integer, parameter :: lmax= 3 
  
  real :: b(3,3),kg(3),pos(3),qmax
  real, allocatable :: q(:,:),modq(:)
  complex, allocatable :: struct_fact(:)
  complex, allocatable :: xlylm(:,:,:)
  complex, allocatable :: proj(:,:,:)
  real, allocatable    :: fl(:)

  vnloc%basis => basis
  if(.not.associated(vnloc%basis,basis)) ERROR(" ")
  b = basis%struct%b
  npw = basis%npw

  allocate(q(1:3,1:npw))
  allocate(modq(1:npw))
  allocate(struct_fact(1:npw))
  allocate(xlylm(1:npw,-lmax:lmax,0:lmax))
  allocate(proj(1:npw,-lmax:lmax,0:lmax))
  allocate(fl(1:npw))

  q(:,:)=0.0
  modq(:)=0.0
  struct_fact(:)=0.0
  xlylm(:,:,:)=0.0
  proj(:,:,:)=0.0
  fl(:)=0.0

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
      end do
    end do
  end do
  iproj = 0
  do iatom=1,atoms%natoms
    pseudo => atoms%pseudo(atoms%type_map(iatom))
    iproj = iproj + sum(2*pseudo%lbeta(:)+1)
  end do
  nproj = iproj
  vnloc%nproj = nproj
  allocate(vnloc%proj(nproj))
  call pw_wfc_init(vnloc%proj,basis)
  allocate(vnloc%d(nproj))
  iproj=0
  do itype = 1,atoms%ntypes
    pseudo => atoms%pseudo(itype)
    nbeta = pseudo%nbeta
    do ibeta=1,nbeta
      l  = pseudo%lbeta(ibeta)
      do ipw=1,npw
        fl(ipw) = num_interpolation_calc(pseudo%interpolation(ibeta),modq(ipw),3)
      end do
      do m=-l,l
        proj(:,m,l) = (0.0,-1.0)**l * xlylm(:,m,l) * fl(:)
      end do
    end do
    do iatom = 1,atoms%natoms
      if(atoms%type_map(iatom) /= itype) cycle
      pos = atoms%positions(:,iatom)
      struct_fact = exp(-num_2pi_i*num_matmul(pos,real(basis%g(:,:))))
      do ibeta=1,nbeta
        l  = pseudo%lbeta(ibeta)
        do m=-l,l
          iproj = iproj + 1
          vnloc%d(iproj) = pseudo%d(ibeta)
          vnloc%proj(iproj)%val = proj(:,m,l) * struct_fact
        end do
      end do
    end do
  end do
  if(iproj /= nproj) ERROR(tools_char(iproj,4))

  nullify(pseudo)

  deallocate(q)
  deallocate(modq)
  deallocate(struct_fact)
  deallocate(xlylm)
  deallocate(proj)
  deallocate(fl)

end subroutine pw_vnloc_init_x1

subroutine pw_vnloc_init_x2(vnloc,basis,atoms,iatom)
  use pw_atoms_module
  use num_module
  use pw_pseudo_module
  use tools_module
  use pw_vnloc_type
  use pw_basis_module
  use pw_wfc_module
  
  type(pw_vnloc), intent(out) :: vnloc
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  integer, intent(in) :: iatom
  type(pw_pseudo), pointer :: pseudo
  integer :: nproj,ibeta,iproj,nbeta,ipw,npw,l,m,itype
  complex :: tmp

  integer, parameter :: lmax= 3
  
  real :: b(3,3),kg(3),pos(3),qmax
  real, allocatable :: q(:,:),modq(:)
  complex, allocatable :: struct_fact(:)
  complex, allocatable :: xlylm(:,:,:)
  complex, allocatable :: proj(:,:,:)
  real, allocatable    :: fl(:)

  vnloc%basis => basis
  if(.not.associated(vnloc%basis,basis)) ERROR(" ")
  b = basis%struct%b
  npw = basis%npw

  allocate(q(1:3,1:npw))
  allocate(modq(1:npw))
  allocate(struct_fact(1:npw))
  allocate(xlylm(1:npw,-lmax:lmax,0:lmax))
  allocate(proj(1:npw,-lmax:lmax,0:lmax))
  allocate(fl(1:npw))

  q(:,:)=0.0
  modq(:)=0.0
  struct_fact(:)=0.0
  xlylm(:,:,:)=0.0
  proj(:,:,:)=0.0
  fl(:)=0.0

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
      end do
    end do
  end do
  iproj = 0

  pseudo => atoms%pseudo(atoms%type_map(iatom))
  if(.not.associated(pseudo,atoms%pseudo(atoms%type_map(iatom)))) ERROR(" ")

  iproj = iproj + sum(2*pseudo%lbeta(:)+1)
 
  nproj = iproj
 
!  write(0,*)"from pw_vnloc_init_x2, nproj", nproj
!  write(0,*)"from pw_vnloc_init_x2, iatom", iatom
  
  vnloc%nproj = nproj
  allocate(vnloc%proj(nproj))
  call pw_wfc_init(vnloc%proj,basis)
  allocate(vnloc%d(nproj))

  iproj=0
  itype = atoms%type_map(iatom)
!  write(0,*)"from pw_vnloc_init_x2, itype", itype
  nbeta = pseudo%nbeta
  do ibeta=1,nbeta
     l  = pseudo%lbeta(ibeta)
     do ipw=1,npw
        fl(ipw) = num_interpolation_calc(pseudo%interpolation(ibeta),modq(ipw),3)
     end do
     do m=-l,l
        proj(:,m,l) = (0.0,-1.0)**l * xlylm(:,m,l) * fl(:)
     end do
  end do
  pos = atoms%positions(:,iatom)
  struct_fact = exp(-num_2pi_i*num_matmul(pos,real(basis%g(:,:))))
  do ibeta=1,nbeta
     l  = pseudo%lbeta(ibeta)
     do m=-l,l
        iproj = iproj + 1
        vnloc%d(iproj) = pseudo%d(ibeta)
        vnloc%proj(iproj)%val = proj(:,m,l) * struct_fact
     end do
  end do
 
  if(iproj /= nproj) ERROR(tools_char(iproj,4))

  nullify(pseudo)

  deallocate(q)
  deallocate(modq)
  deallocate(struct_fact)
  deallocate(xlylm)
  deallocate(proj)
  deallocate(fl)

end subroutine pw_vnloc_init_x2

subroutine pw_vnloc_apply_x(wfc_new,vnloc,wfc)
  use pw_vnloc_type
  use pw_wfc_module
  
  type (pw_wfc),   intent(inout) :: wfc_new
  type (pw_vnloc), intent(in)    :: vnloc
  type (pw_wfc),   intent(in)    :: wfc
  integer :: iproj
  type (pw_wfc), allocatable :: wfc_tmp(:)
  complex, allocatable :: projection(:)

  integer :: nproj

  nproj=vnloc%nproj

  allocate(wfc_tmp(1:nproj))
  allocate(projection(1:nproj))

  projection(:)=0.0

  do iproj=1,nproj
    projection(iproj) = pw_wfc_braket(vnloc%proj(iproj),wfc)
  end do
  projection = projection * vnloc%d / wfc_new%basis%struct%a_omega
  call pw_wfc_init(wfc_tmp,vnloc%basis)
  wfc_tmp = vnloc%proj
  do iproj=1,nproj
    call pw_wfc_scale(wfc_tmp(iproj),projection(iproj))
  end do
  call pw_wfc_sum(wfc_new,wfc_tmp)
  call pw_wfc_destroy(wfc_tmp)
  deallocate(wfc_tmp)
  deallocate(projection)
end subroutine pw_vnloc_apply_x

subroutine pw_vnloc_destroy_x(vnloc)
  use pw_vnloc_type
  use pw_wfc_module
  
  type (pw_vnloc), intent(inout) :: vnloc
  call pw_wfc_destroy(vnloc%proj)
  deallocate(vnloc%proj)
  deallocate(vnloc%d)
  nullify(vnloc%basis)
end subroutine pw_vnloc_destroy_x
