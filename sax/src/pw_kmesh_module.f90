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
module pw_kmesh_module
use pw_common_module
use pw_symmlist_module
implicit none
private
public :: pw_kmesh, &
          pw_kmesh_init, &
          pw_kmesh_destroy, &
          pw_kmesh_set_symmlist, &
          pw_kmesh_set_dim, &
          pw_kmesh_write, &
          pw_kmesh_read, &
          pw_kmesh_kbz_index, &
          pw_kmesh_kibz_index, &
          pw_kmesh_kibz_isymm, &
          pw_kmesh_bcast,      &
          pw_kmesh_readbcast
!@ END MANUAL

!@ MANUAL
type pw_kmesh
  type(pw_symmlist), pointer :: symmlist ! 
  integer                    :: m(3,3)
  real                       :: m_inv(3,3)
  real                       :: k0(3)
  integer                    :: nkbz,nkibz
  real,              pointer :: kbz(:,:), kibz(:,:), kibz_weight(:)
end type pw_kmesh
! NOTA i vettori di base per la mesh si ottengono moltiplicando matmul(m_inv,struct%b)
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_kmesh_init(kmesh,symmlist)
! Inizializza 
  type (pw_kmesh),              intent(out) :: kmesh
  type (pw_symmlist), optional, intent(in)  :: symmlist
!@ END MANUAL
  kmesh%nkbz = 0
  kmesh%nkibz = 0
  allocate(kmesh%kbz(3,0))
  call pw_allocate(kmesh%kbz)
  allocate(kmesh%kibz(3,0))
  call pw_allocate(kmesh%kibz)
  allocate(kmesh%kibz_weight(0))
  call pw_allocate(kmesh%kibz_weight)
  nullify(kmesh%symmlist)
  if(present(symmlist)) call pw_kmesh_set_symmlist(kmesh,symmlist)
  kmesh%m  = 0
  kmesh%m_inv  = 0.0
  kmesh%k0 = 0.0
end subroutine pw_kmesh_init

subroutine pw_kmesh_destroy(kmesh)
  type (pw_kmesh), intent(inout) :: kmesh
  kmesh%nkbz = 0
  kmesh%nkibz = 0
  if(.not.associated(kmesh%kbz)) ERROR("")
  if(.not.associated(kmesh%kibz)) ERROR("")
  if(.not.associated(kmesh%kibz_weight)) ERROR("")
  call pw_deallocate(kmesh%kbz)
  deallocate(kmesh%kbz)
  call pw_deallocate(kmesh%kibz)
  deallocate(kmesh%kibz)
  call pw_deallocate(kmesh%kibz_weight)
  deallocate(kmesh%kibz_weight)
  nullify(kmesh%symmlist)
end subroutine pw_kmesh_destroy

subroutine pw_kmesh_set_symmlist(kmesh,symmlist)
  type (pw_kmesh),            intent(inout) :: kmesh
  type (pw_symmlist), target, intent(in)  :: symmlist
  if(.not.associated(kmesh%kbz)) ERROR("")
  if(.not.associated(kmesh%kibz)) ERROR("")
  if(.not.associated(kmesh%kibz_weight)) ERROR("")
  kmesh%symmlist => symmlist
end subroutine pw_kmesh_set_symmlist

subroutine pw_kmesh_read(kmesh,unit,fmt,name)
  use tools_module
  use iotk_module
  type (pw_kmesh),        intent(inout) :: kmesh
  integer,                intent(in)    :: unit
  character(*), optional, intent(in) :: fmt,name
  character(20) :: fmt_local
  real    :: kshift(3)
  integer :: m(3,3),nk(3)
  logical :: found_nk
  logical :: found_m1
  logical :: found_m2
  logical :: found_m3
  character(len=iotk_attlenx) :: attr
  if(.not.associated(kmesh%kbz)) ERROR("")
  if(.not.associated(kmesh%kibz)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  if(.not.associated(kmesh%kibz_weight)) ERROR("")
  if(present(fmt)) fmt_local=fmt
  if(.not.present(fmt)) fmt_local=pw_default_fmt(unit)
  select case (fmt_local)
  case("iotk")
    if(.not.present(name)) ERROR("")
    call iotk_scan_begin(unit,trim(name))
    call iotk_scan_empty(unit,"mesh",attr)
    call iotk_scan_attr (attr,"nk",nk,found=found_nk)
    call iotk_scan_attr (attr,"m1",m(:,1),found=found_m1)
    call iotk_scan_attr (attr,"m2",m(:,2),found=found_m2)
    call iotk_scan_attr (attr,"m3",m(:,3),found=found_m3)
    if(found_m1 .neqv. found_m2) ERROR("")
    if(found_m2 .neqv. found_m3) ERROR("")
    if(found_m1 .neqv. .not. found_nk) ERROR("")
    if(found_nk) then
      m(:,1) = (/nk(1),0,0/)
      m(:,2) = (/0,nk(2),0/)
      m(:,3) = (/0,0,nk(3)/)
    end if
    call iotk_scan_attr (attr,"shift",kshift,default=(/0.0,0.0,0.0/))
    call iotk_scan_end  (unit,trim(name))
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
  call pw_kmesh_set_dim(kmesh,m,kshift)
end subroutine pw_kmesh_read

!@ MANUAL
subroutine pw_kmesh_bcast(kmesh,root,comm)
! Effettua il broadcast di un oggetto kmesh
  use ptk_module, only : ptk_comm_rank,ptk_bcast,ptk_comm
  use tools_module
  type(pw_kmesh), intent(inout) :: kmesh
  integer,        intent(in)    :: root
  type(ptk_comm), intent(in)    :: comm
! kmesh :: oggetto di cui fare il broadcast
! root   :: processo di root
! comm   :: communicator
!@ END MANUAL
  real    :: k0(3)
  integer :: m(3,3)
  integer :: myrank
  call ptk_comm_rank(comm,myrank)
  if(myrank==root) k0 = kmesh%k0
  if(myrank==root) m  = kmesh%m
  call ptk_bcast(k0,root,comm)
  call ptk_bcast(m,root,comm)
  if(myrank/=root) call pw_kmesh_set_dim(kmesh,m,k0)
end subroutine pw_kmesh_bcast


!@ MANUAL
subroutine pw_kmesh_readbcast(kmesh,unit,root,comm,fmt,name)
! Root legge da file un oggetto di tipo pw_kmesh
! e ne fa il broadcast
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type(pw_kmesh),         intent(out) :: kmesh
  integer,                intent(in)  :: unit,root
  type(ptk_comm),         intent(in)  :: comm
  character(*), optional, intent(in)  :: fmt,name
! kmesh :: oggetto da leggere
! unit   :: unita'
! fmt    :: formato
!           "txt" testo standard
!           "log" formato di log
!           "bin" formato binario
!@ END MANUAL
  integer :: rank
  character(50) :: fmt_local,name_local
  call ptk_comm_rank(comm,rank)
  if(root==rank) then
    if(present(fmt))      fmt_local=fmt
    if(.not.present(fmt)) fmt_local=pw_default_fmt(unit)
    if(present(name))      name_local=name
    if(.not.present(name)) name_local=""
    call pw_kmesh_read(kmesh,unit,fmt_local,name_local)
  end if
  call pw_kmesh_bcast(kmesh,root,comm)
end subroutine pw_kmesh_readbcast


subroutine pw_kmesh_set_dim(kmesh,m,k0)
  use pw_qtools_module
  use num_module
  type (pw_kmesh), intent(inout) :: kmesh
  integer,         intent(in)    :: m(3,3)
  real,            intent(in)    :: k0(3)
  integer :: ikbz,ikibz,nkbz,nkibz
  real    :: kbz(3)
  if(.not.associated(kmesh%kbz)) ERROR("")
  if(.not.associated(kmesh%kibz)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  if(.not.associated(kmesh%kibz_weight)) ERROR("")
  kmesh%m  = m
  kmesh%k0 = k0
  kmesh%m_inv = num_inverse(real(m))
  nkbz  = num_determinant(kmesh%m)
  nkibz = pw_qtools_mesh_ibz_count(m,k0,kmesh%symmlist%symm)
  kmesh%nkbz  = nkbz
  kmesh%nkibz = nkibz
  call pw_deallocate(kmesh%kbz)
  deallocate(kmesh%kbz)
  call pw_deallocate(kmesh%kibz)
  deallocate(kmesh%kibz)
  call pw_deallocate(kmesh%kibz_weight)
  deallocate(kmesh%kibz_weight)
  allocate(kmesh%kbz(3,nkbz))
  call pw_allocate(kmesh%kbz)
  allocate(kmesh%kibz(3,nkibz))
  call pw_allocate(kmesh%kibz)
  allocate(kmesh%kibz_weight(nkibz))
  call pw_allocate(kmesh%kibz_weight)
  call pw_qtools_mesh(kmesh%kbz,m,k0)
  call pw_qtools_mesh_ibz(kmesh%kibz,m,k0,kmesh%symmlist%symm)
  kmesh%kibz_weight = 0.0
  do ikbz=1,nkbz
    kbz = kmesh%kbz(:,ikbz)
    ikibz = pw_kmesh_kibz_index(kmesh,kbz)
    kmesh%kibz_weight(ikibz) = kmesh%kibz_weight(ikibz) + 1.0/nkbz
  end do
end subroutine pw_kmesh_set_dim

function pw_kmesh_kbz_index(kmesh,k)
  use pw_qtools_module
  integer                     :: pw_kmesh_kbz_index
  type (pw_kmesh), intent(in) :: kmesh
  real,            intent(in) :: k(3)
  integer :: ikbz,nkbz
  if(.not.associated(kmesh%kbz)) ERROR("")
  if(.not.associated(kmesh%kibz)) ERROR("")
  if(.not.associated(kmesh%kibz_weight)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  nkbz = kmesh%nkbz
  do ikbz = 1,nkbz
    if(pw_qtools_same_q(k,kmesh%kbz(:,ikbz))) then
      pw_kmesh_kbz_index = ikbz
      return
    end if
  end do
  WARNING("")
  pw_kmesh_kbz_index = 0
end function pw_kmesh_kbz_index

function pw_kmesh_kibz_index(kmesh,k)
  use pw_qtools_module
  integer                     :: pw_kmesh_kibz_index
  type (pw_kmesh), intent(in) :: kmesh
  real,            intent(in) :: k(3)
  integer :: ikibz,nkibz,isymm
  if(.not.associated(kmesh%kbz)) ERROR("")
  if(.not.associated(kmesh%kibz)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  nkibz = kmesh%nkibz
  do ikibz = 1,nkibz
    call pw_qtools_equivalent(kmesh%kibz(:,ikibz),k,kmesh%symmlist%symm,isymm)
    if(isymm>0) then
      pw_kmesh_kibz_index = ikibz
      return
    end if
  end do
!  write(0,*) "attento:",k,"su",nkibz
!  write(0,"(3f15.9)") kmesh%kibz
  WARNING("")
  pw_kmesh_kibz_index = 0
end function pw_kmesh_kibz_index

function pw_kmesh_kibz_isymm(kmesh,k)
  use pw_qtools_module
  integer                     :: pw_kmesh_kibz_isymm
  type (pw_kmesh), intent(in) :: kmesh
  real,            intent(in) :: k(3)
  integer :: ikibz,nkibz,isymm
  if(.not.associated(kmesh%kbz)) ERROR("")
  if(.not.associated(kmesh%kibz)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  nkibz = kmesh%nkibz
  do ikibz = 1,nkibz
    call pw_qtools_equivalent(kmesh%kibz(:,ikibz),k,kmesh%symmlist%symm,isymm)
    if(isymm>0) then
      pw_kmesh_kibz_isymm = isymm
      return
    end if
  end do
!  write(0,*) "attento:",k,"su",nkibz
!  write(0,"(3f15.9)") kmesh%kibz
  WARNING("")
  pw_kmesh_kibz_isymm = 0

end function pw_kmesh_kibz_isymm

subroutine pw_kmesh_write(kmesh,unit,fmt)
  use iotk_module
  type (pw_kmesh), intent(in) :: kmesh
  integer,         intent(in) :: unit
  character(*),    intent(in) :: fmt
  integer :: ik
  character(iotk_attlenx) :: attr

  if(.not.associated(kmesh%kbz)) ERROR("")
  if(.not.associated(kmesh%kibz)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  if(.not.associated(kmesh%symmlist)) ERROR("")
  select case(fmt)
  case("log")
    write(unit,"(a)")    "%PW_KMESH_log"
    write(unit,"(a,3i5)")     "m",  kmesh%m(1,:)
    write(unit,"(a,3i5)")     "m",  kmesh%m(2,:)
    write(unit,"(a,3i5)")     "m",  kmesh%m(3,:)
    write(unit,"(a,3f15.9)")  "k0",  kmesh%k0
    write(unit,"(a,i5)") "nkbz",kmesh%nkbz
    do ik = 1,kmesh%nkbz
      write(unit,"(3f15.9)") kmesh%kbz(:,ik)
    end do
    write(unit,"(a,i5)") "nkibz",kmesh%nkibz
    do ik = 1,kmesh%nkibz
      write(unit,"(4f15.9)") kmesh%kibz(:,ik),kmesh%kibz_weight(ik)
    end do
    write(unit,"(a)")    "%END_PW_KMESH_log"
  case("pwscf_lxkcry")
    write(unit,"(i5)") kmesh%nkibz
    do ik = 1,kmesh%nkibz
      write(unit,"(4f15.9)") kmesh%kibz(:,ik),kmesh%kibz_weight(ik)
    end do
  case("pwscf")
    write(unit,"(a)") "K_POINTS crystal"
    write(unit,"(i5)") kmesh%nkibz
    do ik = 1,kmesh%nkibz
      write(unit,"(4f15.9)") kmesh%kibz(:,ik),kmesh%kibz_weight(ik)
    end do
  case("want")
    call iotk_write_attr(attr, "units", "crystal", first=.TRUE.)
    call iotk_write_dat(unit,"VKPT",kmesh%kbz(:,:),columns=3, attr=attr)
  case default
    ERROR("Unrecognized fmt - "//fmt)
  end select
end subroutine pw_kmesh_write

end module pw_kmesh_module
