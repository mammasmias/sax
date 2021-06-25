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
module sax_to_want_module
implicit none
! This module contains conversion format SaX-WanT
private
public :: sax_to_want, &
          sax_to_want_Heff_calc, &
          sax_to_want_Heff_write, &
          sax_to_want_Heff_read
!@ END MANUAL

contains

subroutine sax_to_want(unit,QP,kmesh,states) 
use pw_QP_module
use pw_kmesh_module
use iotk_module
use pw_states_module

type(pw_QP), intent(in) :: QP
type(pw_kmesh), intent(in) :: kmesh
integer, intent(in) :: unit
type(pw_states), intent(in) :: states
character(iotk_attlenx) :: attr

complex, allocatable :: Heff(:,:)
logical :: ldynamical
integer :: ik, dim 
integer :: nbnd, iband_start, iband_end
integer :: nspin
character*20 :: analyticity

!dim=(QP%nbmax-QP%nbmin)+1

!allocate(Heff(1:dim,1:dim))
allocate(Heff(QP%nbmin:QP%nbmax,QP%nbmin:QP%nbmax))

ldynamical=.false.
nspin = 1
Heff(:,:)=0.0
nbnd = QP%nbmax-QP%nbmin +1
analyticity = "time-ordered"

call iotk_write_attr(attr,"nbnd",nbnd,first=.true.)
call iotk_write_attr(attr,"nkpts",kmesh%nkbz)
call iotk_write_attr(attr,"nspin",nspin)
call iotk_write_attr(attr,"iband_start",QP%nbmin)
call iotk_write_attr(attr,"iband_end",QP%nbmax)
call iotk_write_attr(attr,"band_diagonal",QP%diagonal)
call iotk_write_attr(attr,"dynamical",ldynamical)
call iotk_write_attr(attr,"analyticity",trim(analyticity))
call iotk_write_empty(unit,"DATA",attr)

call pw_kmesh_write(kmesh,unit,"want")

call iotk_write_attr(attr,"units", "eV", first=.true.)
call iotk_write_begin(unit,"OPR", attr=attr)
do ik=1,kmesh%nkbz
  Heff(:,:)=0.0
  call sax_to_want_Heff_calc(Heff,QP,ik,states)
  call sax_to_want_Heff_write(Heff,QP%nbmin,QP%nbmax,ik,QP%diagonal,unit)
enddo
call iotk_write_end(unit,"OPR")
deallocate(Heff)
end subroutine sax_to_want  

subroutine sax_to_want_Heff_calc(Heff,QP,ik,states)
use pw_QP_module
use pw_kmesh_module
use num_module
use pw_states_module

complex, intent(inout) :: Heff(:,:)
type(pw_QP), intent(in) :: QP
integer, intent(in) :: ik
type(pw_states), intent(in) :: states

complex, allocatable :: Heff_tmp(:,:), Heff_tmp2(:,:) 
integer :: ib1, ib2


allocate(Heff_tmp(QP%nbmin:QP%nbmax,QP%nbmin:QP%nbmax))

Heff_tmp(:,:)=0.0
Heff(:,:)=0.0

do ib1=QP%nbmin,QP%nbmax
  Heff(ib1,ib1)=QP%energies(ib1,ik)
enddo

Heff_tmp(:,:) = num_matmul(Heff(:,:),transpose(QP%eigenvec(:,:,ik)))
Heff(:,:) = num_matmul(conjg(QP%eigenvec(:,:,ik)),Heff_tmp(:,:))

do ib1=QP%nbmin,QP%nbmax
  Heff(ib1,ib1)=Heff(ib1,ib1)-states%e(ib1,ik)
enddo

end subroutine sax_to_want_Heff_calc

subroutine sax_to_want_Heff_write(Heff,nbmin,nbmax,ik,diagonal,unit,fmt)
  use iotk_module
  use tools_module
  use num_module

  complex, intent(in) :: Heff(:,:)
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  logical, intent(in) :: diagonal
  integer, intent(in) :: unit
  character(*), optional, intent(in) :: fmt

  integer :: ib
  character(20) :: fmt_local
  complex, allocatable :: aux(:)
  complex :: tmp

  if(present(fmt)) fmt_local=fmt
  if(.not.present(fmt)) fmt_local="want"
  

  select case(fmt_local)
  case("want")

    if(diagonal) then
      allocate(aux(nbmin:nbmax))
      aux(:)=0.0
      do ib=nbmin,nbmax
        aux(ib)=Heff(ib,ib)
      enddo
      call iotk_write_dat(unit,"KPT"//trim(iotk_index(ik)),aux(:) * num_ry2ev)
      deallocate(aux)
    else
      call iotk_write_dat(unit,"KPT"//trim(iotk_index(ik)),Heff(:,:) * num_ry2ev)
    endif
  end select

end subroutine sax_to_want_Heff_write

subroutine sax_to_want_Heff_read(Heff,nbmin,nbmax,ik,diagonal,unit,fmt)
  use iotk_module
  use tools_module
  use num_module

  complex, intent(inout) :: Heff(:,:)
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  logical, intent(in) :: diagonal
  integer, intent(in) :: unit
  character(*), optional, intent(in) :: fmt

  integer :: ib
  character(20) :: fmt_local
  complex :: tmp

  complex, allocatable :: aux(:)

  if(present(fmt)) fmt_local=fmt
  if(.not.present(fmt)) fmt_local="want"
  
  select case(fmt_local)
  case("want")
    if(diagonal) then
      allocate(aux(nbmin:nbmax))
      aux(:)=0.0
      call iotk_scan_dat(unit,"KPT"//trim(iotk_index(ik)),aux(:))
      do ib=nbmin,nbmax
        Heff(ib,ib)=aux(ib)/num_ry2ev
      enddo
    else
      Heff(:,:)=0.0
      call iotk_scan_dat(unit,"KPT"//trim(iotk_index(ik)),Heff(:,:))
      Heff(:,:)=Heff(:,:)/num_ry2ev
    endif
  case default
    ERROR("Wrong fmt in sax_to_want")
  end select

end subroutine sax_to_want_Heff_read

end module sax_to_want_module
