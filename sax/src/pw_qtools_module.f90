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
module pw_qtools_module
use pw_struct_module
use pw_symm_module
implicit none
private
public :: pw_qtools_same_q, &
          pw_qtools_equivalent, &
          pw_qtools_q2ibz, &
          pw_qtools_mesh, &
          pw_qtools_mesh_ibz, &
          pw_qtools_mesh_ibz_count
!@ END MANUAL

contains

!@ MANUAL
function pw_qtools_same_q(q1,q2)
! Capisce se due punti q (nella base del reticolo)
! sono equivalenti a meno di un vettore di reticolo
! in pratica guarda se la differenza e' un vettore
! di interi
  logical          :: pw_qtools_same_q
  real, intent(in) :: q1(3),q2(3)
!@ END MANUAL
  real should_be_int(3)
  should_be_int = q2 - q1
  if(all(abs(should_be_int-nint(should_be_int))<1.0d-8)) then
    pw_qtools_same_q=.true.
  else
    pw_qtools_same_q=.false.
  end if
end function pw_qtools_same_q

!@ MANUAL
subroutine pw_qtools_equivalent(q1,q2,symm,isymm)
! Trova la simmetria che porta q1 in q2
! guardando nella lista di simmetrie symm(:)
! fornisce in output l'indice isymm della simmetria
! in questione
  use num_module
  real,           intent(in)  :: q1(3),q2(3)
  type (pw_symm), intent(in)  :: symm(:)
  integer,        intent(out) :: isymm
!@ END MANUAL
  real :: q1_new(3)
  real should_be_int(3)
  integer is
  isymm=0
  do is=1,size(symm)
    q1_new = num_matmul(real(symm(is)%grot),q1)
    should_be_int = q2 - q1_new
    if(all(abs(should_be_int-nint(should_be_int))<1.0d-6))then
      isymm = is
      return
    end if
  end do
end subroutine pw_qtools_equivalent

!@ MANUAL
subroutine pw_qtools_q2ibz(qibz,nqibz,q,symm)
! porta l'insieme di punti q(1:3,:) nella zona irriducibile
! individuata dalla lista di simmetrie symm(:)
! nqibz e' il numero di punti ottenuti nella zona irredicubile
!   (sempre <= al numero di punti in input)
! e qibz e' l'insieme di punti ridotti
  real,           intent(out) :: qibz(:,:)
  integer,        intent(out) :: nqibz
  real,           intent(in)  :: q(:,:)
  type (pw_symm), intent(in)  :: symm(:)
!@ END MANUAL
  integer iq,iqibz,isymm
  logical equivalent
  if(ubound(qibz,1)/=3) ERROR("")
  if(ubound(q,1)/=3) ERROR("")
  nqibz=0
  do iq=1,ubound(q,2)
    equivalent=.false.
    do iqibz=1,nqibz
      call pw_qtools_equivalent(q(:,iq),qibz(:,iqibz),symm,isymm)
      if(isymm>0) then
        equivalent=.true.
        exit
      end if
    end do
    if(.not.equivalent) then
      nqibz=nqibz+1
      qibz(:,nqibz) = q(:,iq)
    end if
  end do
end subroutine pw_qtools_q2ibz

subroutine pw_qtools_little_group(q,symm,mask)
  use num_module
  real, intent(in) :: q(3)
  type (pw_symm), intent(in)  :: symm(:)
  logical, intent(out) :: mask(:)
  real should_be_int(3)
  integer isymm
  if(size(mask)/=size(symm)) ERROR("")
  mask = .false.
  do isymm=1,size(symm)
    should_be_int = q - num_matmul(real(symm(isymm)%grot),q)
    if(all(abs(should_be_int-nint(should_be_int))<1.0d-8))then
      mask(isymm) = .true.
    end if
  end do
end subroutine pw_qtools_little_group

!@ MANUAL
subroutine pw_qtools_mesh(q,m,shift)
  use num_module
! Genera una mesh di n(1)*n(2)*n(3) punti
! shiftati di un vettore shift(:)
  real,    intent(out) :: q(:,:)
  integer, intent(in)  :: m(3,3)
  real,    intent(in)  :: shift(3)
!@ END MANUAL
  logical :: found
  integer i1,i2,i3,l,lprev
  real :: m_inv(3,3)
  real try(3)
  if(ubound(q,1)/=3) ERROR("")
  if(ubound(q,2)<num_determinant(m)) ERROR("")
  l=0
  m_inv = num_inverse(real(m))
  do i3=1,m(3,3)
    do i2=1,m(2,2)
      do i1=1,m(1,1)
        try = num_matmul(m_inv,(shift + ((/i1,i2,i3/)-1)))
        found = .false.
        do lprev=1,l
          if(pw_qtools_same_q(try,q(:,lprev))) found = .true.
          if(found) exit
        end do
        if(found) cycle
        l = l + 1
        q(:,l) = try
      end do
    end do
  end do
end subroutine pw_qtools_mesh

!@ MANUAL
function pw_qtools_mesh_ibz_count(m,shift,symm)
  use num_module
! Conta i punti che si ottengono portando nella zona
! irriducibile una mesh di n(1)*n(2)*n(3) punti
! shiftati di un vettore shift(:)
  integer pw_qtools_mesh_ibz_count
  integer,       intent(in)  :: m(3,3)
  real,          intent(in)  :: shift(3)
  type(pw_symm), intent(in)  :: symm(:)
!@ END MANUAL
  real, allocatable :: qtmp(:,:)
  real, allocatable :: qibztmp(:,:)
  integer :: nqibz
  allocate(qtmp(3,num_determinant(m)))
  allocate(qibztmp(3,num_determinant(m)))
  call pw_qtools_mesh(qtmp,m,shift)
  call pw_qtools_q2ibz(qibztmp,nqibz,qtmp,symm)
  pw_qtools_mesh_ibz_count = nqibz
  deallocate(qtmp)
  deallocate(qibztmp)
end function pw_qtools_mesh_ibz_count

!@ MANUAL
subroutine pw_qtools_mesh_ibz(q,m,shift,symm)
  use num_module
! Genera una mesh nella zona irriducibile
  real,          intent(out) :: q(:,:)
  integer,       intent(in)  :: m(3,3)
  real,          intent(in)  :: shift(3)
  type(pw_symm), intent(in)  :: symm(:)
!@ END MANUAL
  integer :: nqibz
  real, allocatable :: qtmp(:,:)
  real, allocatable :: qibztmp(:,:)
  allocate(qtmp(3,num_determinant(m)))
  allocate(qibztmp(3,num_determinant(m)))
  call pw_qtools_mesh(qtmp,m,shift)
  call pw_qtools_q2ibz(qibztmp,nqibz,qtmp,symm)
  q = qibztmp(:,1:nqibz)
  deallocate(qtmp)
  deallocate(qibztmp)
end subroutine pw_qtools_mesh_ibz

end module pw_qtools_module
