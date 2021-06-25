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
module pw_wfc6d_module
use pw_common_module
use pw_basis_module
implicit none
private
public :: pw_wfc6d, &
          pw_wfc6d_init, &
          pw_wfc6d_destroy, &
          pw_wfc6d_read, &
          pw_wfc6d_write, &
          pw_wfc6d_add_from_wfc, &
          pw_wfc6d_matrix_element, &
          pw_wfc6d_send, &
          pw_wfc6d_recv, &
          pw_wfc6d_bcast
!@ END MANUAL

!@ MANUAL
type pw_wfc6d
  type(pw_basis), pointer :: basis1,basis2
  integer                 :: npw1,npw2
  complex,        pointer :: val(:,:)
end type pw_wfc6d
!@ END MANUAL

interface pw_wfc6d_init
  module procedure pw_wfc6d_init000
  module procedure pw_wfc6d_init111
end interface
interface pw_wfc6d_destroy
  module procedure pw_wfc6d_destroy0
  module procedure pw_wfc6d_destroy1
  module procedure pw_wfc6d_destroy2
end interface

contains

!@ MANUAL
subroutine pw_wfc6d_init000(wfc6d,basis1,basis2)
  type(pw_wfc6d),                   intent(out) :: wfc6d
  type(pw_basis), optional, intent(in)  :: basis1,basis2
!@ END MANUAL
  if(present(basis2) .and. .not. present(basis1)) ERROR("")
  nullify(wfc6d%basis1)
  nullify(wfc6d%basis2)
  wfc6d%npw1 = 0
  wfc6d%npw2 = 0
  allocate(wfc6d%val(0,0))
  call pw_allocate(wfc6d%val)
  if(present(basis1) .and. present(basis2)) call pw_wfc6d_set_basis(wfc6d,basis1,basis2)
  if(present(basis1) .and. .not. present(basis2)) call pw_wfc6d_set_basis(wfc6d,basis1,basis1)
end subroutine pw_wfc6d_init000

subroutine pw_wfc6d_init111(wfc6d,basis1,basis2)
  type(pw_wfc6d),                   intent(out) :: wfc6d(:)
  type(pw_basis), optional, intent(in)  :: basis1(:),basis2(:)
  integer :: i,n
  if(.not. present(basis1) .and. present(basis2)) ERROR("")
  n = size(wfc6d)
  if(present(basis1)) then
    if(size(basis1)/=n) ERROR("")
  end if
  if(present(basis2)) then
    if(size(basis2)/=n) ERROR("")
  end if
  do i=1,n
    if(.not.present(basis1)) then
      call pw_wfc6d_init(wfc6d(i))
    end if
    if(present(basis1) .and. .not. present(basis2)) then
      call pw_wfc6d_init(wfc6d(i),basis1(i))
    end if
    if(present(basis1) .and. present(basis2)) then
      call pw_wfc6d_init(wfc6d(i),basis1(i),basis2(i))
    end if
  end do
end subroutine pw_wfc6d_init111

!@ MANUAL
subroutine pw_wfc6d_destroy0(wfc6d)
  type(pw_wfc6d), intent(inout) :: wfc6d
!@ END MANUAL
  if(.not.associated(wfc6d%val)) ERROR("")
  call pw_deallocate(wfc6d%val)
  deallocate(wfc6d%val)
end subroutine pw_wfc6d_destroy0

subroutine pw_wfc6d_destroy1(wfc6d)
  type(pw_wfc6d), intent(inout) :: wfc6d(:)
  integer :: i
  do i=1,size(wfc6d)
    call pw_wfc6d_destroy0(wfc6d(i))
  end do
end subroutine pw_wfc6d_destroy1

subroutine pw_wfc6d_destroy2(wfc6d)
  type(pw_wfc6d), intent(inout) :: wfc6d(:,:)
  integer :: i,j
  do j=1,ubound(wfc6d,2)
    do i=1,ubound(wfc6d,1)
      call pw_wfc6d_destroy0(wfc6d(i,j))
    end do
  end do
end subroutine pw_wfc6d_destroy2


!@ MANUAL
subroutine pw_wfc6d_set_basis(wfc6d,basis1,basis2)
! Imposta la base in un oggetto wfc6d e alloca di conseguenza val
  type(pw_wfc6d),         intent(inout) :: wfc6d
  type(pw_basis), target, intent(in)    :: basis1,basis2
!@ END MANUAL
  if(.not.associated(wfc6d%val)) ERROR("")
  wfc6d%basis1 => basis1
  wfc6d%basis2 => basis2
  if(wfc6d%npw1==basis1%npw .and. &
     wfc6d%npw2==basis2%npw) return
  wfc6d%npw1 = basis1%npw
  wfc6d%npw2 = basis2%npw
  call pw_deallocate(wfc6d%val)
  deallocate(wfc6d%val)
  allocate(wfc6d%val(wfc6d%npw1,wfc6d%npw2))
  call pw_allocate(wfc6d%val)
  wfc6d%val=0
end subroutine pw_wfc6d_set_basis

!@ MANUAL
subroutine pw_wfc6d_write(wfc6d,unit,fmt,rec,name)
  use num_module
  use iotk_module
! Subroutine per scrivere su file
  type(pw_wfc6d),         intent(in) :: wfc6d
  integer,                intent(in) :: unit
  character(*), optional, intent(in) :: fmt
  integer,      optional, intent(in) :: rec
  character(*), optional, intent(in) :: name
! wfc6d:: oggetto da scrivere
! unit :: unita' di scrittura
! fmt  :: formato (opzionale)
! rec  :: record (opzionale)
! fmt puo' assumere i seguenti valori:
! "txt" : formato ASCII
! "bin" : formato binario per accesso sequenziale
! "dir" : formato binario per accesso diretto
! "test001" : formato che da anche informazioni sulla griglia
! "test002" : formato che da anche informazioni sulla griglia
! "test003" : formato che da anche informazioni sulla griglia
! se fmt non e' presente viene dedotto da un inquire sull'unita'
! se fmt e' "dir" deve essere presente anche rec
!@ END MANUAL
  integer :: ipw1,ipw2,npw1,npw2
  integer       :: iostat
  character(20) :: fmt_local
  character(iotk_attlenx) :: attr

  real :: k1(3), k2(3), b1(3,3), b2(3,3)
  if(.not.associated(wfc6d%basis1)) ERROR("")
  if(.not.associated(wfc6d%basis2)) ERROR("")
  if(.not.associated(wfc6d%val)) ERROR("")
  if(present(fmt)) fmt_local = fmt
  if(.not.present(fmt)) fmt_local = pw_default_fmt(unit)
  npw1 = wfc6d%npw1
  npw2 = wfc6d%npw2
  k1 = wfc6d%basis1%k
  k2 = wfc6d%basis2%k
  b1 = wfc6d%basis1%struct%b
  b2 = wfc6d%basis2%struct%b
  select case (fmt_local)
  case ("txt")
    write(unit,"(a)",iostat=iostat) "%PW_WFC6D_formatted"
    if(iostat/=0) ERROR("")
    write(unit,"(a,i8)",iostat=iostat) "npw1 ",npw1
    write(unit,"(a,i8)",iostat=iostat) "npw2 ",npw2
    if(iostat/=0) ERROR("")
    do ipw2 = 1,npw2
      do ipw1 = 1,npw1
        write(unit,*,iostat=iostat) wfc6d%val(ipw1,ipw2)
        if(iostat/=0) ERROR("")
      end do
    end do
  case ("bin")
    write(unit,iostat=iostat) npw1,npw2,wfc6d%val
    if(iostat/=0) ERROR("")
  case ("dir")
    write(unit,rec=rec,iostat=iostat) npw1,npw2,wfc6d%val
    if(iostat/=0) ERROR("")
  case ("test001")
    write(unit,"(a)",iostat=iostat) "%PW_WFC6D_test001"
    write(unit,"(a,i8)",iostat=iostat) "npw1",npw1
    write(unit,"(a,i8)",iostat=iostat) "npw2",npw2
    write(unit,"(a,3f15.9)",iostat=iostat) "k1",k1
    write(unit,"(a,3f15.9)",iostat=iostat) "k2",k2
    do ipw2 = 1,npw2
      do ipw1 = 1,npw1
        write(unit,"(3i4,a,3i4,2f15.9)",iostat=iostat) &
     wfc6d%basis1%g(:,ipw1)," ",wfc6d%basis2%g(:,ipw2),&
     wfc6d%val(ipw1,ipw2)
        if(iostat/=0) ERROR("")
      end do
    end do
  case ("test002")
    write(unit,"(a)",iostat=iostat) "%PW_WFC6D_test001"
    write(unit,"(a,i8)",iostat=iostat) "npw1",npw1
    write(unit,"(a,i8)",iostat=iostat) "npw2",npw2
    write(unit,"(a,3f15.9)",iostat=iostat) "k1",k1
    write(unit,"(a,3f15.9)",iostat=iostat) "k2",k2
    do ipw2 = 1,npw2
      do ipw1 = 1,npw1
        write(unit,"(3i4,a,3i4,4f15.9)",iostat=iostat) &
     wfc6d%basis1%g(:,ipw1+1)," ",wfc6d%basis2%g(:,ipw2+1),&
     sum(num_matmul(b1,real((wfc6d%basis1%g(:,ipw1))+k1)**2)), &
     sum(num_matmul(b2,real((wfc6d%basis2%g(:,ipw2))+k2)**2)), &
     wfc6d%val(ipw1,ipw2)
        if(iostat/=0) ERROR("")
      end do
    end do
  case ("test003")
    write(unit,"(a)",iostat=iostat) "%PW_WFC6D_test001"
    write(unit,"(a,i8)",iostat=iostat) "npw1",npw1
    write(unit,"(a,i8)",iostat=iostat) "npw2",npw2
    write(unit,"(a,3f15.9)",iostat=iostat) "k1",k1
    write(unit,"(a,3f15.9)",iostat=iostat) "k2",k2
    do ipw2 = 1,npw2
      do ipw1 = 1,npw1
        write(unit,"(3i4,a,3i4,5f15.9)",iostat=iostat) &
     wfc6d%basis1%g(:,ipw1+1)," ",wfc6d%basis2%g(:,ipw2+1),&
     sum(num_matmul(b1,real((wfc6d%basis1%g(:,ipw1))+k1)**2)), &
     sum(num_matmul(b2,real((wfc6d%basis2%g(:,ipw2))+k2)**2)), &
     wfc6d%val(ipw1,ipw2), &
     abs(wfc6d%val(ipw1,ipw2))
        if(iostat/=0) ERROR("")
      end do
    end do
  case ("iotk")
    if(.not. present(name)) ERROR("")
    call iotk_write_attr (attr,"type","pw_wfc6d",first=.true.)
    call iotk_write_begin(unit,trim(name),attr)
    call iotk_write_attr (attr,"npw1",npw1,first=.true.)
    call iotk_write_attr (attr,"npw2",npw2)
    call iotk_write_empty(unit,"info",attr)
    call iotk_write_dat  (unit,"val",wfc6d%val)
    call iotk_write_end  (unit,trim(name))
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
end subroutine pw_wfc6d_write

!@ MANUAL
subroutine pw_wfc6d_read(wfc6d,unit,fmt,rec,name)
  use iotk_module
! Subroutine per leggere da file
  type(pw_wfc6d),         intent(inout) :: wfc6d
  integer,                intent(in)    :: unit
  character(*), optional, intent(in)    :: fmt
  integer,      optional, intent(in)    :: rec
  character(*), optional, intent(in)    :: name
! wfc6d:: oggetto da leggere
! unit :: unita' di scrittura
! fmt  :: formato (opzionale)
! rec  :: record (opzionale)
! fmt puo' assumere i seguenti valori:
! "txt" : formato ASCII
! "bin" : formato binario per accesso sequenziale
! "dir" : formato binario per accesso diretto
! se fmt non e' presente viene dedotto da un inquire sull'unita'
! se fmt e' "dir" deve essere presente anche rec
!@ END MANUAL
  integer :: ipw1,ipw2,npw1,npw2
  integer       :: iostat
  character(20) :: fmt_local
  character(30) :: dummy
  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype

  if(.not.associated(wfc6d%basis1)) ERROR("")
  if(.not.associated(wfc6d%basis2)) ERROR("")
  if(.not.associated(wfc6d%val)) ERROR("")
  if(present(fmt)) fmt_local = fmt
  if(.not.present(fmt)) fmt_local = pw_default_fmt(unit)
  select case (fmt_local)
  case ("txt")
    read(unit,*,iostat=iostat) dummy
    if(iostat/=0) ERROR("")
    if(dummy/="%PW_WFC6D_formatted") ERROR("")
    read(unit,*,iostat=iostat) dummy,npw1
    if(iostat/=0) ERROR("")
    if(dummy/="npw1") ERROR("")
    read(unit,*,iostat=iostat) dummy,npw2
    if(iostat/=0) ERROR("")
    if(dummy/="npw2") ERROR("")
    if(npw1/=wfc6d%npw1) ERROR("")
    if(npw2/=wfc6d%npw2) ERROR("")
    do ipw2 = 1,npw2
      do ipw1 = 1,npw1
        read(unit,*,iostat=iostat) wfc6d%val(ipw1,ipw2)
        if(iostat/=0) ERROR("")
      end do
    end do
  case ("bin")
    read(unit,iostat=iostat) npw1,npw2,wfc6d%val
    if(iostat/=0) ERROR("")
  case ("dir")
    read(unit,rec=rec,iostat=iostat) npw1,npw2,wfc6d%val
    if(iostat/=0) ERROR("")
  case ("iotk")
    if(.not. present(name)) ERROR("")
    call iotk_scan_begin(unit,trim(name),attr)
    call iotk_scan_attr (attr,"type",rtype)
    if(trim(rtype)/="pw_wfc6d") ERROR("")
    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr (attr,"npw1",npw1)
    call iotk_scan_attr (attr,"npw2",npw2)
    if(npw1/=wfc6d%npw1) ERROR("")
    if(npw2/=wfc6d%npw2) ERROR("")
    call iotk_scan_dat  (unit,"val",wfc6d%val)
    call iotk_scan_end  (unit,trim(name))
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
end subroutine pw_wfc6d_read

subroutine pw_wfc6d_add_from_wfc(wfc6d,scale,wfc)
  use pw_wfc_module
  use num_module
  type(pw_wfc6d), intent(inout) :: wfc6d
  complex,        intent(in)    :: scale
  type(pw_wfc),   intent(in)    :: wfc
  complex                       :: val(size(wfc%val))
  if(.not.associated(wfc6d%val)) ERROR("")
  if(.not.associated(wfc6d%basis1)) ERROR("")
  if(.not.associated(wfc6d%basis2)) ERROR("")
  if(.not.associated(wfc%val)) ERROR("")
  if(.not.associated(wfc%basis)) ERROR("")
  if(.not.(wfc%basis==wfc6d%basis1)) ERROR("")
  if(.not.(wfc%basis==wfc6d%basis2)) ERROR("")
  val= conjg(wfc%val)
  call num_gerc(scale,val,val,wfc6d%val)
!
! performs wfc6d%val(i,j)=wfc6d%val(i,j)+scale*val(i)*conjg(val(j))
!
end subroutine pw_wfc6d_add_from_wfc

function pw_wfc6d_matrix_element(wfc6d,wfc1,wfc2)
  use pw_wfc_module
  use num_module
  complex :: pw_wfc6d_matrix_element
  type(pw_wfc6d), intent(in) :: wfc6d
  type(pw_wfc),   intent(in) :: wfc1, wfc2
  complex :: val(wfc1%npw)
  integer :: npw1,npw2
  if(.not.associated(wfc6d%val)) ERROR("")
  if(.not.associated(wfc6d%basis1)) ERROR("")
  if(.not.associated(wfc6d%basis2)) ERROR("")
  if(.not.associated(wfc1%val)) ERROR("")
  if(.not.associated(wfc1%basis)) ERROR("")
  if(.not.associated(wfc2%val)) ERROR("")
  if(.not.associated(wfc2%basis)) ERROR("")
  npw1 = min(wfc1%basis%npw,wfc6d%basis1%npw)
  npw2 = min(wfc2%basis%npw,wfc6d%basis2%npw)
  if(npw1/=wfc6d%basis1%npw) ERROR("Not implemented yet")
  if(npw2/=wfc6d%basis2%npw) ERROR("Not implemented yet")
!! CONTROLLO INUTILE MA SICURO !!
  if(any(wfc1%basis%g(:,1:npw1)/=wfc6d%basis1%g(:,1:npw1))) then
    call pw_basis_write(wfc1%basis,600)
    call pw_basis_write(wfc6d%basis1,601)
    ERROR("")
  end if
  if(any(wfc2%basis%g(:,1:npw2)/=wfc6d%basis1%g(:,1:npw2))) ERROR("")
!! FINE CONTROLLO INUTILE MA SICURO !!
  call num_gemv(wfc6d%val,wfc2%val(1:npw2),val(1:npw1))
  pw_wfc6d_matrix_element = dot_product(wfc1%val(1:npw1),val(1:npw1))
end function pw_wfc6d_matrix_element

subroutine pw_wfc6d_send(wfc6d,dest,tag,comm)
  use ptk_module, only : ptk_send,ptk_comm
  type (pw_wfc6d), intent(in) :: wfc6d
  integer,         intent(in) :: dest,tag
  type (ptk_comm), intent(in) :: comm
  call ptk_send(wfc6d%val,dest,tag,comm)
end subroutine pw_wfc6d_send

subroutine pw_wfc6d_recv(wfc6d,source,tag,comm)
  use ptk_module, only : ptk_recv,ptk_comm
  type (pw_wfc6d), intent(inout) :: wfc6d
  integer,         intent(in)    :: source,tag
  type (ptk_comm), intent(in)    :: comm
  call ptk_recv(wfc6d%val,source,tag,comm)
end subroutine pw_wfc6d_recv

subroutine pw_wfc6d_bcast(wfc6d,root,comm)
  use ptk_module, only : ptk_bcast,ptk_comm
  type (pw_wfc6d), intent(inout) :: wfc6d
  integer,         intent(in)    :: root
  type (ptk_comm), intent(in)    :: comm
  call ptk_bcast(wfc6d%val,root,comm)
end subroutine pw_wfc6d_bcast

end module pw_wfc6d_module
