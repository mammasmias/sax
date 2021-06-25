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

module pw_QP_module
implicit none
private

public :: pw_QP, pw_QP_init, pw_QP_read, pw_QP_destroy, &
     pw_QP_write, pw_QP_readbcast, pw_QP_bcast

type pw_QP
   integer :: nbmax, nbmin
   logical :: diagonal
   real, pointer :: energies(:,:)
   complex, pointer :: eigenvec(:,:,:)
end type pw_QP

contains

subroutine pw_QP_read(QP,nk,unit,name,fmt)
  use tools_module
  use num_module
  use iotk_module
  use pw_common_module
  
  character(*), intent(in) :: name
  integer, intent(in) :: unit
  type(pw_QP), intent(inout) :: QP
  integer, intent(in) :: nk
  character(*), optional, intent(in) :: fmt

  integer :: ik
  integer :: nbmin,nbmax
  logical :: diagonal
  integer :: iostat
  character(20) :: fmt_local
  complex, allocatable :: aux(:)

  if(present(fmt)) fmt_local=fmt
  if(.not.present(fmt)) fmt_local="iotk"

  select case(fmt_local)
  case("iotk")
    call iotk_scan_begin(unit,trim(name))
    call iotk_scan_dat(unit,"nbmin",nbmin)
    if(nbmin/=QP%nbmin) ERROR("QP nbmin not consistent")
    call iotk_scan_dat(unit,"nbmax",nbmax)
    if(nbmax/=QP%nbmax) ERROR("QP nbmax not consistent")
    call iotk_scan_dat(unit,"diagonal",diagonal)
    do ik=1,nk
      call iotk_scan_dat(unit,"energies"//tools_char(ik,3),QP%energies(:,ik))
    enddo
    do ik=1,nk
      call iotk_scan_dat(unit,"eigenvec"//tools_char(ik,3),QP%eigenvec(:,:,ik))
    enddo
    call iotk_scan_end(unit,trim(name))
  case("want")
    call iotk_scan_begin(unit,trim(name))
    call iotk_scan_dat(unit,"nbmin",nbmin)
    call iotk_scan_dat(unit,"nbmax",nbmax)
    call iotk_scan_dat(unit,"diagonal", diagonal)
    if(diagonal) then
      allocate(aux(nbmin:nbmax))
      do ik=1,nk
        aux(:)=0.0
        call iotk_scan_dat(unit,"KPT"//trim(iotk_index(ik)),aux(:))
        QP%energies(:,ik)=real(aux(:))
      enddo
      deallocate(aux)
    else
      do ik=1,nk
        call iotk_scan_dat(unit,"KPT"//trim(iotk_index(ik)),QP%eigenvec(:,:,ik))
      enddo
    endif
    call iotk_scan_end(unit,trim(name))
   
  end select
     
end subroutine pw_QP_read

subroutine pw_QP_readbcast(QP,nk,unit,name,root,comm,fmt)
  use ptk_module, only : ptk_comm_rank,ptk_comm
! Root reads QP and broadcasts it
  type(pw_QP), intent(out) :: QP
  integer,         intent(in)    :: unit, root
  integer,         intent(in) ::  nk
  type(ptk_comm),  intent(in) :: comm
  character(*), intent(in) :: name
  character(*), optional, intent(in) :: fmt

  integer :: ierr, rank

  call ptk_comm_rank(comm,rank)
  if(root==rank) then
     call pw_QP_read(QP,nk,unit,trim(name),fmt)
  endif
  call pw_QP_bcast(QP,root,comm)
end subroutine pw_QP_readbcast

subroutine pw_QP_bcast(QP,root,comm)
  use ptk_module, only : ptk_comm_rank,ptk_comm, ptk_bcast
! bcast a QP object
  type(pw_QP),    intent(inout) :: QP
  integer,        intent(in)    :: root
  type(ptk_comm), intent(in)    :: comm

  integer :: ierr, myrank

  call ptk_bcast(QP%energies,root,comm)
  call ptk_bcast(QP%eigenvec,root,comm)
end subroutine pw_QP_bcast

subroutine pw_QP_init(QP,nk,nbmin,nbmax,diagonal)
  use pw_common_module
  type(pw_QP), intent(out) :: QP
  integer, intent(in) :: nk,nbmin,nbmax
  logical, intent(in) :: diagonal
  
  integer :: ib

  allocate(QP%energies(nbmin:nbmax,nk))
  QP%energies(:,:)=0.0
  QP%nbmin=nbmin
  QP%nbmax=nbmax
  QP%diagonal=diagonal
  allocate(QP%eigenvec(nbmin:nbmax,nbmin:nbmax,nk))
  QP%eigenvec(:,:,:)=(0.0,0.0)
  do ib=nbmin,nbmax
     QP%eigenvec(ib,ib,:)=(1.0,0.0)
  enddo

end subroutine pw_QP_init

subroutine pw_QP_destroy(QP)
  type(pw_QP), intent(inout) :: QP
  deallocate(QP%energies)
  deallocate(QP%eigenvec)
end subroutine pw_QP_destroy

subroutine pw_QP_write(QP,nk,unit,name,fmt)
  use iotk_module
  use tools_module
  use num_module

  type(pw_QP), intent(in) :: QP
  integer, intent(in) :: nk
  integer, intent(in) :: unit
  character(*), intent(in) :: name
  character(*), optional, intent(in) :: fmt

  integer :: ib,ik
  character(20) :: fmt_local
  complex, allocatable :: aux(:)
  complex :: tmp

  if(present(fmt)) fmt_local=fmt
  if(.not.present(fmt)) fmt_local="iotk"
  
  select case(fmt_local)
  case("iotk")
    call iotk_write_begin(unit,trim(name))
    call iotk_write_dat(unit,"nbmin",QP%nbmin)
    call iotk_write_dat(unit,"nbmax",QP%nbmax)
    call iotk_write_dat(unit,"diagonal",QP%diagonal)
    do ik=1,nk
      call iotk_write_dat(unit,"energies"//tools_char(ik,3),QP%energies(:,ik))
    enddo
    do ik=1,nk
       call iotk_write_dat(unit,"eigenvec"//tools_char(ik,3),QP%eigenvec(:,:,ik))
    enddo
    call iotk_write_end(unit,trim(name))
  case("want")
    call iotk_write_begin(unit,trim(name))
    if(QP%diagonal) then
      allocate(aux(QP%nbmin:QP%nbmax))
      do ik=1,nk
        aux(:)=0.0
        aux(:)=QP%energies(:,ik)
        call iotk_write_dat(unit,"KPT"//trim(iotk_index(ik)),aux(:))
      enddo
      deallocate(aux)
    else
      do ik=1,nk
        call iotk_write_dat(unit,"KPT"//trim(iotk_index(ik)),QP%eigenvec(:,:,ik))
      enddo
    endif
    call iotk_write_end(unit,trim(name))

  end select

end subroutine pw_QP_write
end module pw_QP_module
