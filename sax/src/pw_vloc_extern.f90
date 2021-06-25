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

subroutine pw_vloc_calc_x1(vloc,atoms)
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  use pw_atoms_module
  use tools_module
  use num_module
  type (pw_wfc),   intent(inout) :: vloc
  type (pw_atoms), intent(in)    :: atoms

  integer :: ipw,iatom,npw,itype
  real :: b(3,3)
  integer :: imesh,nmesh
  real    :: z,modg,a_omega
  real    :: pos(3)
  real,    allocatable :: vg(:),rv2z(:),r(:)
  complex, allocatable :: sf(:)
  integer(pw_basis_int_kind), allocatable :: g(:,:)

  if(any(vloc%basis%k/=0.0))ERROR("")

  b = vloc%basis%struct%b
  a_omega = vloc%basis%struct%a_omega
  npw = vloc%basis%npw
  allocate(g(3,1:npw))
  allocate(vg(1:npw))
  allocate(sf(1:npw))
  g = vloc%basis%g

  vloc%val = 0.0

  do itype=1,atoms%ntypes
    z = atoms%pseudo(itype)%z
    nmesh = atoms%pseudo(itype)%nmesh
    allocate(rv2z(nmesh))
    allocate(r(nmesh))
    r = atoms%pseudo(itype)%mesh
    rv2z = (r * atoms%pseudo(itype)%vloc + 2.0*z) * atoms%pseudo(itype)%wmesh
! CASO SPECIALE G=0,0,0
    if(.not. all(g(:,1)==0)) ERROR("")
    vg(1) = num_4pi/a_omega * num_simpson(rv2z*r)
! LOOP
    do ipw=2,npw
      modg = sqrt(sum(num_matmul(b,real(g(:,ipw)))**2))
      vg(ipw)  = - num_8pi*z/(modg**2)/a_omega &
                 + num_4pi/modg/a_omega * num_simpson(rv2z*sin(modg*r))
    end do
    do iatom=1,atoms%natoms
      if(atoms%type_map(iatom) /= itype) cycle
! Calcolo il fattore di struttura
      pos = atoms%positions(:,iatom)
      sf = exp(-num_2pi_i*num_matmul(pos,real(g)))
      vloc%val = vloc%val + sf * vg
    end do
    deallocate(rv2z)
    deallocate(r)
  end do
  deallocate(sf)
  deallocate(vg)
  deallocate(g)

  
#if 0
  do iatom=1,atoms%natoms
write(0,*) iatom
    z = atoms%pseudo(atoms%type_map(iatom))%z
    pos = atoms%positions(:,iatom)
    nmesh = atoms%pseudo(atoms%type_map(iatom))%nmesh
    allocate(rv2z(nmesh))
    allocate(r(nmesh))
    r = atoms%pseudo(atoms%type_map(iatom))%mesh
    rv2z = atoms%pseudo(atoms%type_map(iatom))%wmesh * (r * &
           atoms%pseudo(atoms%type_map(iatom))%vloc +2.0*z)
    do ipw = 1,vloc%basis%npw
      g = vloc%basis%g(:,ipw)
      if(all(g==0)) then
        vg = num_4pi/a_omega * num_simpson(rv2z*r)
        tmp = vg ! Structure factor not needed
        vloc%val(ipw) = vloc%val(ipw) + tmp
      else
        modg = sqrt(sum(num_matmul(b,g)**2))
        vg = - num_8pi*z/(modg**2)/a_omega + &
               num_4pi/modg/a_omega * num_simpson(rv2z*sin(modg*r))
                  ! Structure factor
        tmp = vg * exp(-num_2pi_i*dot_product(g,pos))
        vloc%val(ipw) = vloc%val(ipw) + tmp
      end if
    end do
    deallocate(rv2z)
    deallocate(r)
  end do
write(0,*) "+++"
#endif
end subroutine pw_vloc_calc_x1

subroutine pw_vloc_calc0_x1(vloc,atoms)
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  use pw_atoms_module
  use tools_module
  use num_module
  type (pw_wfc),   intent(inout) :: vloc
  type (pw_atoms), intent(in)    :: atoms

  integer :: ipw,iatom,npw,itype
  real :: b(3,3)
  integer :: imesh,nmesh
  real    :: z,modg,a_omega
  real    :: pos(3)
  real,    allocatable :: vg(:),rv2z(:),r(:)
  complex, allocatable :: sf(:)
  integer(pw_basis_int_kind), allocatable :: g(:,:)

  if(any(vloc%basis%k/=0.0))ERROR("")

  b = vloc%basis%struct%b
  a_omega = vloc%basis%struct%a_omega
  npw = vloc%basis%npw
  allocate(g(3,1:npw))
  allocate(vg(1:npw))
  allocate(sf(1:npw))
  g = vloc%basis%g

  vloc%val = 0.0

  do itype=1,atoms%ntypes
    z = atoms%pseudo(itype)%z
    nmesh = atoms%pseudo(itype)%nmesh
    allocate(rv2z(nmesh))
    allocate(r(nmesh))
    r = atoms%pseudo(itype)%mesh
    rv2z = (r * atoms%pseudo(itype)%vloc + 2.0*z) * atoms%pseudo(itype)%wmesh
! CASO SPECIALE G=0,0,0
    if(.not. all(g(:,1)==0)) ERROR("")
    vg(1) = num_4pi/a_omega * num_simpson(rv2z*r)
! LOOP
    do ipw=2,npw
      modg = sqrt(sum(num_matmul(b,real(g(:,ipw)))**2))
      vg(ipw)  = &
                 + num_4pi/modg/a_omega * num_simpson(rv2z*sin(modg*r))
    end do
    do iatom=1,atoms%natoms
      if(atoms%type_map(iatom) /= itype) cycle
! Calcolo il fattore di struttura
      pos = atoms%positions(:,iatom)
      sf = exp(-num_2pi_i*num_matmul(pos,real(g)))
      vloc%val = vloc%val + sf * vg
    end do
    deallocate(rv2z)
    deallocate(r)
  end do
  deallocate(sf)
  deallocate(vg)
  deallocate(g)
end subroutine pw_vloc_calc0_x1

subroutine pw_vloc_corr_x1(vloc,atoms,vcut)
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  use pw_atoms_module
  use tools_module
  use num_module
  use coulomb_vcut_module, only : vcut_type, vcut_get
  type (pw_wfc),   intent(inout) :: vloc
  type (pw_atoms), intent(in)    :: atoms
  type (vcut_type), intent(in) :: vcut

  integer :: ipw,iatom,npw,itype
  real :: b(3,3)
  integer :: imesh,nmesh
  real    :: z,modg,a_omega
  real    :: pos(3)
  real,    allocatable :: vg(:)
  complex, allocatable :: sf(:)
  integer(pw_basis_int_kind), allocatable :: g(:,:)
  real :: kg(3)

  if(any(vloc%basis%k/=0.0))ERROR("")

  b = vloc%basis%struct%b
  a_omega = vloc%basis%struct%a_omega
  npw = vloc%basis%npw
  allocate(g(3,1:npw))
  allocate(vg(1:npw))
  allocate(sf(1:npw))
  g = vloc%basis%g

  do itype=1,atoms%ntypes
    z = atoms%pseudo(itype)%z
    do ipw=1,npw
      kg = num_matmul(b,real(g(:,ipw)))
      vg(ipw)  =  - z/a_omega * vcut_get(vcut,kg)
    end do
    do iatom=1,atoms%natoms
      if(atoms%type_map(iatom) /= itype) cycle
! Calcolo il fattore di struttura
      pos = atoms%positions(:,iatom)
      sf = exp(-num_2pi_i*num_matmul(pos,real(g)))
      vloc%val = vloc%val + sf * vg
    end do
  end do
  deallocate(sf)
  deallocate(vg)
  deallocate(g)

end subroutine pw_vloc_corr_x1

subroutine pw_vloc_calc_x2(vloc,atoms,iatom)
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  use pw_atoms_module
  use tools_module
  use num_module
  type (pw_wfc),   intent(inout) :: vloc
  type (pw_atoms), intent(in)    :: atoms
  integer, intent(in) :: iatom      

  integer :: ipw,npw,itype
  real :: b(3,3)
  integer :: imesh,nmesh
  real    :: z,modg,a_omega
  real    :: pos(3)
  real,    allocatable :: vg(:),rv2z(:),r(:)
  complex, allocatable :: sf(:)
  integer(pw_basis_int_kind), allocatable :: g(:,:)

  if(any(vloc%basis%k/=0.0))ERROR("")

  b = vloc%basis%struct%b
  a_omega = vloc%basis%struct%a_omega
  npw = vloc%basis%npw
  allocate(g(3,npw))
  allocate(vg(npw))
  allocate(sf(npw))
  g = vloc%basis%g

  vloc%val = 0.0

  itype=atoms%type_map(iatom)

  z = atoms%pseudo(itype)%z
  nmesh = atoms%pseudo(itype)%nmesh
  allocate(rv2z(nmesh))
  allocate(r(nmesh))
  r = atoms%pseudo(itype)%mesh
  rv2z = (r * atoms%pseudo(itype)%vloc + 2.0*z) * atoms%pseudo(itype)%wmesh
  ! CASO SPECIALE G=0,0,0
  if(.not. all(g(:,1)==0)) ERROR("")
  vg(1) = num_4pi/a_omega * num_simpson(rv2z*r)
  ! LOOP
  do ipw=2,npw
     modg = sqrt(sum(num_matmul(b,real(g(:,ipw)))**2))
     vg(ipw)  = - num_8pi*z/(modg**2)/a_omega &
          + num_4pi/modg/a_omega * num_simpson(rv2z*sin(modg*r))
  end do
  ! Calcolo il fattore di struttura
  pos = atoms%positions(:,iatom)
  sf = exp(-num_2pi_i*num_matmul(pos,real(g)))
  vloc%val = vloc%val + sf * vg
  deallocate(rv2z)
  deallocate(r)
  deallocate(sf)
  deallocate(vg)
  deallocate(g)
  
  
#if 0
  write(0,*) iatom
  z = atoms%pseudo(atoms%type_map(iatom))%z
  pos = atoms%positions(:,iatom)
  nmesh = atoms%pseudo(atoms%type_map(iatom))%nmesh
  allocate(rv2z(nmesh))
  allocate(r(nmesh))
  r = atoms%pseudo(atoms%type_map(iatom))%mesh
  rv2z = atoms%pseudo(atoms%type_map(iatom))%wmesh * (r * &
       atoms%pseudo(atoms%type_map(iatom))%vloc +2.0*z)
  do ipw = 1,vloc%basis%npw
     g = vloc%basis%g(:,ipw)
     if(all(g==0)) then
        vg = num_4pi/a_omega * num_simpson(rv2z*r)
        tmp = vg ! Structure factor not needed
        vloc%val(ipw) = vloc%val(ipw) + tmp
     else
        modg = sqrt(sum(num_matmul(b,g)**2))
        vg = - num_8pi*z/(modg**2)/a_omega + &
             num_4pi/modg/a_omega * num_simpson(rv2z*sin(modg*r))
        ! Structure factor
        tmp = vg * exp(-num_2pi_i*dot_product(g,pos))
        vloc%val(ipw) = vloc%val(ipw) + tmp
     end if
  end do
  deallocate(rv2z)
  deallocate(r)
  write(0,*) "+++"
#endif
end subroutine pw_vloc_calc_x2
