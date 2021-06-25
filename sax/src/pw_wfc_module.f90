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
module pw_wfc_module
use pw_basis_module
use pw_common_module
use lasi_module, only : lasi_scal
implicit none
private
public :: pw_wfc,           &
          pw_wfc_init,      &
          pw_wfc_destroy,   &
          pw_wfc_set_basis, &
          pw_wfc_maxiolength,  &
          pw_wfc_iolength,  &
          pw_wfc_write,     &
          pw_wfc_read,      &
          pw_wfc_braket,    &
          assignment(=),    &
          operator(==),     &
          pw_wfc_random,    &
          pw_wfc_random_phase,    &
          pw_wfc_fix_phase, &
          pw_wfc_scale,     &
          pw_wfc_norm,      &
          pw_wfc_normalize, &
          pw_wfc_send,      &
          pw_wfc_recv,      &
          pw_wfc_bcast,      &
          pw_wfc_allreduce, &
          pw_wfc_allreduce_inplace, &
          pw_wfc_change_basis,&
          pw_wfc_change_cutoff,&
          pw_wfc_calc_hartree, &
          pw_wfc_kinetic,     &
          pw_wfc_sort,      &
           pw_wfc_sum,      &
          pw_wfc_add,      &
          pw_wfc_axpy,     &
          pw_wfc_p,         &
          pw_wfc_p_braket,  &
          pw_wfc_coulomb,   &
          pw_wfc_mul,       &
          pw_wfc_conjg
!@ END MANUAL

!@ MANUAL
type pw_wfc
  type(pw_basis), pointer :: basis
  integer                 :: npw
  complex,        pointer :: val(:)
! This object contains a wavefunction in reciprocal space
! basis :: pointer to the associated basis grid
! npw   :: number of plane waves; redundant (it is size(%val))
! val   :: Fourier coefficients of the wavefunction, (1:npw)
end type pw_wfc
!@ END MANUAL

!@ MANUAL
interface operator(==)
  module procedure pw_wfc_is_equal
end interface
interface assignment(=)
  module procedure pw_wfc_copy0
  module procedure pw_wfc_copy1
end interface
interface pw_wfc_init
  module procedure pw_wfc_init00
  module procedure pw_wfc_init10
  module procedure pw_wfc_init11
  module procedure pw_wfc_init20
end interface
interface pw_wfc_destroy
  module procedure pw_wfc_destroy0
  module procedure pw_wfc_destroy1
  module procedure pw_wfc_destroy2
end interface
interface pw_wfc_set_basis
  module procedure pw_wfc_set_basis00
  module procedure pw_wfc_set_basis10
  module procedure pw_wfc_set_basis21
end interface
interface pw_wfc_change_basis
  module procedure pw_wfc_change_basis00
  module procedure pw_wfc_change_basis10
end interface
interface pw_wfc_scale
  module procedure pw_wfc_scale_r
  module procedure pw_wfc_scale_c
end interface

!@ END MANUAL

contains

!@ MANUAL
subroutine pw_wfc_init00(wfc,basis)
! Initializes a wfc object
  type(pw_wfc),                     intent(out) :: wfc
  type(pw_basis), target, optional, intent(in)  :: basis
! basis :: basis to be pointed to. it can be omitted and assigned later
!@ END MANUAL
  nullify(wfc%basis)
  wfc%npw   = 0
  allocate(wfc%val(0))
  call pw_allocate(wfc%val)
  if(present(basis)) call pw_wfc_set_basis(wfc,basis)
end subroutine pw_wfc_init00

subroutine pw_wfc_init10(wfc,basis)
! Initializes an array of wfc objects, all af them pointing to the same basis
  type(pw_wfc),                     intent(out) :: wfc(:)
  type(pw_basis), target, optional, intent(in)  :: basis
! basis :: basis to be pointed to. it can be omitted and assigned later
  integer :: i
  do i=1,size(wfc)
    if(present(basis)) then
      call pw_wfc_init(wfc(i),basis)
    else
      call pw_wfc_init(wfc(i))
    end if
  end do
end subroutine pw_wfc_init10

subroutine pw_wfc_init20(wfc,basis)
! Initializes a matrix of wfc objects, all af them pointing to the same basis
  type(pw_wfc),                     intent(out) :: wfc(:,:)
  type(pw_basis), target, optional, intent(in)  :: basis
! basis :: basis to be pointed to. it can be omitted and assigned later
  integer :: i,j
  do j=1,ubound(wfc,2)
    do i=1,ubound(wfc,1)
      if(present(basis)) then
        call pw_wfc_init(wfc(i,j),basis)
      else
        call pw_wfc_init(wfc(i,j))
      end if
    end do
  end do
end subroutine pw_wfc_init20

subroutine pw_wfc_init11(wfc,basis)
! Initializes an array of wfc objects, pointing to different basis
  type(pw_wfc),           intent(out) :: wfc(:)
  type(pw_basis), target, intent(in)  :: basis(:)
! basis :: basis to be pointed to.
  integer :: i
  if(size(wfc)/=size(basis)) ERROR("")
  do i=1,size(wfc)
    call pw_wfc_init(wfc(i),basis(i))
  end do
end subroutine pw_wfc_init11

!@ MANUAL
subroutine pw_wfc_destroy0(wfc)
! Destroys an object
  type(pw_wfc), intent(inout) :: wfc
!@ END MANUAL
!  if(.not.associated(wfc%val)) ERROR(" non e associato")
!  call pw_deallocate(wfc%val)
  if(associated(wfc%val)) deallocate(wfc%val)
!   if(associated(wfc%val)) write(0,*) "sono associato wfc"
end subroutine pw_wfc_destroy0

subroutine pw_wfc_destroy1(wfc)
! Destroys an array of objects
  type(pw_wfc), intent(inout) :: wfc(:)
  integer :: i
  do i=1,size(wfc)
    call pw_wfc_destroy(wfc(i))
  end do
end subroutine pw_wfc_destroy1

subroutine pw_wfc_destroy2(wfc)
! Destroys a matrix of objects
  type(pw_wfc), intent(inout) :: wfc(:,:)
  integer :: i,j
  do j=1,ubound(wfc,2)
    do i=1,ubound(wfc,1)
      call pw_wfc_destroy(wfc(i,j))
    end do
  end do
end subroutine pw_wfc_destroy2

!@ MANUAL
subroutine pw_wfc_set_basis00(wfc,basis)
! Set the basis pointer for a wfc object
  type(pw_wfc),           intent(inout) :: wfc
  type(pw_basis), target, intent(in)    :: basis
!@ END MANUAL
  if(.not.associated(wfc%val)) ERROR("")
  wfc%basis => basis
  if(wfc%npw==basis%npw) return
  wfc%npw = basis%npw
  call pw_deallocate(wfc%val)
  deallocate(wfc%val)
  allocate(wfc%val(wfc%npw))
  call pw_allocate(wfc%val)
  wfc%val = 0.0
end subroutine pw_wfc_set_basis00

subroutine pw_wfc_set_basis10(wfc,basis)
! Set the basis pointer for an array of wfc objects
  type(pw_wfc),           intent(inout) :: wfc(:)
  type(pw_basis), target, intent(in)    :: basis
  integer ::n,i
  n=size(wfc)
  do i=1,n
    call pw_wfc_set_basis(wfc(i),basis)
  end do
end subroutine pw_wfc_set_basis10

subroutine pw_wfc_set_basis21(wfc,basis)
  type(pw_wfc),           intent(inout) :: wfc(:,:)
  type(pw_basis), target, intent(in)    :: basis(:)
  integer :: i,j
  if(ubound(wfc,2) /= ubound(basis,1)) ERROR("")
  do j=1,ubound(wfc,2)
    do i=1,ubound(wfc,1)
      if(.not.associated(wfc(i,j)%val)) then
        write(0,*) "i,j",i,j
        ERROR("")
      end if
      call pw_wfc_set_basis(wfc(i,j),basis(j))
    end do
  end do
end subroutine pw_wfc_set_basis21

!@ MANUAL
subroutine pw_wfc_change_basis00(wfc,basis)
! Cambia la base associata ad una wfc in maniera
! 'soft' (senza riallocare memoria e senza modificare
! la wfc). Serve per sostituire una base con una sua simmetrica
  type(pw_wfc),           intent(inout) :: wfc
  type(pw_basis), target, intent(in)    :: basis
!@ END MANUAL
  if(.not.associated(wfc%val)) ERROR("")
  if(wfc%npw/=basis%npw) ERROR("")
  wfc%basis => basis
end subroutine pw_wfc_change_basis00

subroutine pw_wfc_change_basis10(wfc,basis)
  type(pw_wfc),           intent(inout) :: wfc(:)
  type(pw_basis), target, intent(in)    :: basis
  integer :: i,n
  n=size(wfc)
  do i=1,n
    call pw_wfc_change_basis(wfc(i),basis)
  end do
end subroutine pw_wfc_change_basis10

!@ MANUAL
subroutine pw_wfc_change_cutoff(wfc,basis)
! Serve per cambiare il cutoff sostituendo la base
! Il punto k non deve cambiare
! L'ordine delle due basi deve essere identico,
! altrimenti non funziona
  type(pw_wfc),           intent(inout) :: wfc
  type(pw_basis), target, intent(in)    :: basis
!@ END MANUAL
  integer :: npwmin
  complex :: new_val(basis%npw)
  new_val = 0.0
  npwmin = min(basis%npw,wfc%basis%npw)
  new_val(1:npwmin) = wfc%val(1:npwmin)
  call pw_wfc_set_basis(wfc,basis)
  wfc%val = new_val
end subroutine pw_wfc_change_cutoff

!@ MANUAL
function pw_wfc_iolength(wfc)
! Subroutine che restituisce la lunghezza minima di un record che possa
! contenere la funzione in questione
! tale numero puo' essere utilizzato per aprire un file ad accesso diretto
! es:
!     open(unit=1,file="wfcfile",access="direct",recl=pw_wfc_iolength(wfc))
! nota che le wfc associate ad una stessa base hanno tutte la stessa lunghezza
  integer                  :: pw_wfc_iolength
  type(pw_wfc), intent(in) :: wfc
!@ END MANUAL
  integer iolength
  inquire(iolength=iolength) wfc%npw,wfc%val
  pw_wfc_iolength = iolength
end function pw_wfc_iolength

!@ MANUAL
function pw_wfc_maxiolength(wfc)
! Subroutine che restituisce la lunghezza minima di un record che possa
! contenere la piu' grande tra le funzioni in questione
! tale numero puo' essere utilizzato per aprire un file ad accesso diretto
! es:
!     open(unit=1,file="wfcfile",access="direct",recl=pw_wfc_iolength(wfc))
! nota che le wfc associate ad una stessa base hanno tutte la stessa lunghezza
  integer                  :: pw_wfc_maxiolength
  type(pw_wfc), intent(in) :: wfc(:)
!@ END MANUAL
  integer iolength(size(wfc)),i
  do i=1,size(wfc)
    iolength(i) = pw_wfc_iolength(wfc(i))
  end do
  pw_wfc_maxiolength = maxval(iolength)
end function pw_wfc_maxiolength

!@ MANUAL
subroutine pw_wfc_write(wfc,unit,fmt,rec,name)
  use iotk_module
! Subroutine per scrivere su file
  type(pw_wfc),           intent(in) :: wfc
  integer,                intent(in) :: unit
  character(*), optional, intent(in) :: fmt
  integer,      optional, intent(in) :: rec
  character(*), optional, intent(in) :: name
! wfc  :: oggetto da scrivere
! unit :: unita' di scrittura
! fmt  :: formato (opzionale)
! rec  :: record (opzionale)
! fmt puo' assumere i seguenti valori:
! "txt" : formato ASCII
! "bin" : formato binario per accesso sequenziale
! "dir" : formato binario per accesso diretto
! "test001" : formato che da anche informazioni sulla griglia
! se fmt non e' presente viene dedotto da un inquire sull'unita'
! se fmt e' "dir" deve essere presente anche rec
!@ END MANUAL
  integer :: ipw,npw
  integer       :: iostat
  character(20) :: fmt_local
  character(len=iotk_attlenx) :: attr
  if(.not.associated(wfc%basis)) ERROR("")
  if(.not.associated(wfc%val)) ERROR("")
  if(present(fmt)) fmt_local = fmt
  if(.not.present(fmt)) fmt_local = pw_default_fmt(unit)
  npw = wfc%npw
  select case (fmt_local)
  case ("txt")
    write(unit,"(a)",iostat=iostat) "%PW_WFC_formatted"
    if(iostat/=0) ERROR("")
    write(unit,"(a,i8)",iostat=iostat) "npw ",npw
    if(iostat/=0) ERROR("")
    do ipw = 1,npw
      write(unit,*,iostat=iostat) wfc%val(ipw)
      if(iostat/=0) ERROR("")
    end do
  case ("bin")
    write(unit,iostat=iostat) npw,wfc%val
    if(iostat/=0) ERROR("")
  case ("dir")
    if(.not.present(rec)) ERROR("")
    write(unit,rec=rec,iostat=iostat) npw,wfc%val
    if(iostat/=0) ERROR("")
  case ("test001")
    write(unit,"(a)",iostat=iostat) "%PW_WFC_test001"
    write(unit,"(a,i8)",iostat=iostat) "npw",npw
    write(unit,"(a,3f15.9)",iostat=iostat) "k",wfc%basis%k
    write(unit,"(a,3f15.9)",iostat=iostat) "r0",wfc%basis%r0
    write(unit,"(a,l5)",iostat=iostat) "conjg",wfc%basis%conjg
    do ipw = 1,npw
      write(unit,"(3i5,3f15.9)") wfc%basis%g(:,ipw+1), &
          wfc%val(ipw),abs(wfc%val(ipw)**2)
    end do
  case ("iotk")
    if(.not. present(name)) ERROR("")
    call iotk_write_attr (attr,"type","pw_wfc",first=.true.)
    call iotk_write_begin(unit,trim(name),attr)
    call iotk_write_attr (attr,"npw",npw,first=.true.)
    call iotk_write_empty(unit,"info",attr)
    call iotk_write_dat  (unit,"val",wfc%val)
    call iotk_write_end  (unit,trim(name))
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
end subroutine pw_wfc_write

!@ MANUAL
subroutine pw_wfc_read(wfc,unit,fmt,rec,eband,name)
  use iotk_module
! Subroutine per leggere una pw_wfc
  type(pw_wfc),           intent(inout) :: wfc
  integer,                intent(in)    :: unit
  character(*), optional, intent(in) :: fmt
  integer,      optional, intent(in) :: rec
  real,         optional, intent(out) :: eband
  character(*), optional, intent(in) :: name
! wfc  :: oggetto da leggere
! unit :: unita' di lettura
! fmt  :: formato (opzionale)
! rec  :: record (opzionale)
! eband:: energie (se leggi in formato "pw104")
! fmt puo' assumere i seguenti valori:
! "txt"   : formato ASCII
! "bin"   : formato binario per accesso sequenziale
! "dir"   : formato binario per accesso diretto
! "pw104" : formato dei file wfc prodotti dal pw104 (versione modificata da Alice)
! se fmt non e' presente viene dedotto da un inquire sull'unita'
! se fmt e' "dir" deve essere presente anche rec
!@ END MANUAL
  integer :: ipw,npw
  character(20) :: dummy
  integer       :: iostat
  character(20) :: fmt_local
  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
! ATTENZIONE
  complex       :: val(wfc%npw)
! The explicit kind (8) could cause portability problems
  real(8)        :: eband_tmp
! questo e' il formato PW104
  if(.not.associated(wfc%basis)) ERROR("")
  if(.not.associated(wfc%val)) ERROR("")
  if(present(fmt)) fmt_local=fmt
  if(.not.present(fmt)) fmt_local=pw_default_fmt(unit)
  if(present(eband)) eband=0.0
  select case (fmt_local)
  case ("txt")
    read(unit,*,iostat=iostat) dummy
    if(iostat/=0) ERROR("")
    if(dummy/="%PW_WFC_formatted") ERROR("")
    read(unit,*,iostat=iostat) dummy,npw
    if(dummy/="npw") ERROR("")
    if(iostat/=0) ERROR("")
    if(npw/=wfc%npw) ERROR("")
    do ipw=1,npw
      read(unit,*,iostat=iostat) wfc%val(ipw)
      if(iostat/=0) ERROR("")
    end do
  case ("bin")
    read(unit,iostat=iostat) npw,wfc%val
    if(iostat/=0) ERROR("")
    if(npw/=wfc%npw) ERROR("")
  case ("pw104")
    read(unit,iostat=iostat) npw,eband_tmp
    if(iostat/=0) ERROR("")
    if(npw/=wfc%npw) ERROR("")
    read(unit,iostat=iostat) val
    wfc%val = val
    if(present(eband)) eband = eband_tmp
  case ("dir")
    if(.not.present(rec)) ERROR("")
    read(unit,rec=rec,iostat=iostat) npw,wfc%val
    if(iostat/=0) ERROR("")
    if(npw/=wfc%npw) ERROR("")
  case ("pw_punch")
    if(.not.present(rec)) ERROR("")
    call iotk_scan_dat  (unit,"Wfc"//iotk_index(rec),wfc%val)
  case ("iotk")
    if(.not. present(name)) ERROR("")
    call iotk_scan_begin(unit,trim(name),attr)
    call iotk_scan_attr (attr,"type",rtype)
    if(trim(rtype)/="pw_wfc") ERROR("")
    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr (attr,"npw",npw)
    if(npw/=wfc%npw) ERROR("")
    call iotk_scan_dat  (unit,"val",wfc%val)
    call iotk_scan_end  (unit,trim(name))
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
end subroutine pw_wfc_read

!@ MANUAL
subroutine pw_wfc_copy0(new_wfc,old_wfc)
! Subroutine per copiare una wfc
  type(pw_wfc), intent(out) :: new_wfc
  type(pw_wfc), intent(in)  :: old_wfc
  integer :: ipw
!@ END MANUAL
  if(.not.associated(old_wfc%basis)) ERROR("")
  if(.not.associated(old_wfc%val)) ERROR("")
  if(.not.associated(new_wfc%basis)) ERROR("")
  if(.not.associated(new_wfc%val)) ERROR("")
  if(.not. new_wfc%basis==old_wfc%basis) ERROR("")
  do ipw=1,new_wfc%npw
    new_wfc%val(ipw) = old_wfc%val(ipw)
  enddo
end subroutine pw_wfc_copy0

subroutine pw_wfc_copy1(new_wfc,old_wfc)
  type(pw_wfc), intent(out) :: new_wfc(:)
  type(pw_wfc), intent(in)  :: old_wfc(:)
  integer ::i,n
  n=size(new_wfc)
  if(n/=size(old_wfc)) ERROR("")
  do i=1,n
    call pw_wfc_copy0(new_wfc(i),old_wfc(i))
  end do
end subroutine pw_wfc_copy1

!@ MANUAL
function pw_wfc_is_equal(wfc1,wfc2)
! Subroutine per confrontare due wfc
  logical                  :: pw_wfc_is_equal
  type(pw_wfc), intent(in) :: wfc1,wfc2
!@ END MANUAL
  pw_wfc_is_equal = (wfc1%basis == wfc2%basis) .and. &
                    all(abs(wfc1%val-wfc2%val)<1e-7)
end function pw_wfc_is_equal

!@ MANUAL
subroutine pw_wfc_random(wfc)
! Subroutine che inizializza a caso i valori di una wfc
! (parte reale e parte immaginaria sono numeri casuali tra -1 e 1)
  type(pw_wfc), intent(inout) :: wfc
!@ END MANUAL
  real :: tmp(wfc%npw*2)
  call random_number(tmp)
  wfc%val=cmplx(tmp(1:wfc%npw)*2.0-1.0,tmp(wfc%npw+1:2*wfc%npw)*2.0-1.0)
end subroutine pw_wfc_random

!@ MANUAL
subroutine pw_wfc_random_phase(wfc)
! Subroutine che moltiplica una wfc per una fase casuale
  use num_module
  type(pw_wfc), intent(inout) :: wfc
!@ END MANUAL
  real                        :: phase
  complex                     :: scale
  call random_number(phase)
  scale = exp(num_2pi_i * phase)
  wfc%val = wfc%val * scale
end subroutine pw_wfc_random_phase

!@ MANUAL
subroutine pw_wfc_fix_phase(wfc)
! Subroutine che fissa la fase di una wfc con un criterio preciso
  use num_module
  type(pw_wfc), intent(inout) :: wfc
!@ END MANUAL
  complex                     :: scale
  complex :: valsum
  valsum = sum(wfc%val)
  scale = abs(valsum) / valsum
  wfc%val = wfc%val * scale
end subroutine pw_wfc_fix_phase

subroutine pw_wfc_gaussian(wfc,sigma)
  use num_module
  type(pw_wfc), intent(inout) :: wfc
  real,         intent(in)    :: sigma
  integer :: ipw
  real    :: vec(3),b(3,3)
  b = wfc%basis%struct%b
  do ipw = 0,wfc%basis%npw-1
    vec = num_matmul(b,real(wfc%basis%g(:,ipw)))
    wfc%val(ipw)   = exp(-(sum(vec**2))/(2*sigma**2))
  end do
end subroutine pw_wfc_gaussian

!@ MANUAL
function pw_wfc_braket(bra,ket)
! Subroutine per il prodotto scalare di due funzioni d'onda
  use num_module
  complex :: pw_wfc_braket
  type(pw_wfc), intent(in) :: bra,ket
!@ END MANUAL
  if(.not.bra%basis == ket%basis) &
     ERROR("braket with different grids not implemented")
  pw_wfc_braket = dot_product(bra%val,ket%val)
end function pw_wfc_braket

!@ MANUAL
subroutine pw_wfc_scale_r(wfc,scale)
! Subroutine per riscalare di un fattore reale una funzione d'onda
  use num_module
  type(pw_wfc), intent(inout) :: wfc
  real,         intent(in)    :: scale
!@ END MANUAL
  call lasi_scal(wfc%npw,scale,wfc%val,1)
end subroutine pw_wfc_scale_r

!@ MANUAL
subroutine pw_wfc_scale_c(wfc,scale)
! Subroutine per riscalare di un fattore complesso una funzione d'onda
  use num_module
  type(pw_wfc), intent(inout) :: wfc
  complex,      intent(in)    :: scale
!@ END MANUAL
  call lasi_scal(wfc%npw,scale,wfc%val,1)
end subroutine pw_wfc_scale_c

!@ MANUAL
function pw_wfc_norm(wfc)
! Subroutine che restituisce la norma di una funzione d'onda
! definita come sqrt( sum_G |c(G)|^2 )
  use num_module
  real                     :: pw_wfc_norm
  type(pw_wfc), intent(in) :: wfc
!@ END MANUAL
  pw_wfc_norm = num_nrm2(wfc%val)
end function pw_wfc_norm

!@ MANUAL
subroutine pw_wfc_normalize(wfc)
! Subroutine che normalizza una funzione d'onda
  type(pw_wfc), intent(inout) :: wfc
!@ END MANUAL
  real :: norm
  norm = pw_wfc_norm(wfc)
  call pw_wfc_scale(wfc,1.0/norm)
end subroutine pw_wfc_normalize

!@ MANUAL
subroutine pw_wfc_send(wfc,dest,tag,comm)
! Spedisce una funzione d'onda
  use ptk_module, only : ptk_send,ptk_comm
  type (pw_wfc),   intent(in) :: wfc
  integer,         intent(in) :: dest,tag
  type (ptk_comm), intent(in) :: comm
!@ END MANUAL
  call ptk_send(wfc%val,dest,tag,comm)
end subroutine pw_wfc_send

!@ MANUAL
subroutine pw_wfc_recv(wfc,source,tag,comm)
! Riceve una funzione d'onda
  use ptk_module, only : ptk_recv,ptk_comm
  type (pw_wfc),   intent(inout) :: wfc
  integer,         intent(in)    :: source,tag
  type (ptk_comm), intent(in)    :: comm
!@ END MANUAL
  call ptk_recv(wfc%val,source,tag,comm)
end subroutine pw_wfc_recv

subroutine pw_wfc_bcast(wfc,root,comm)
  use ptk_module, only : ptk_bcast,ptk_comm
  type (pw_wfc),   intent(inout) :: wfc
  integer,         intent(in)    :: root
  type (ptk_comm), intent(in)    :: comm
  integer :: npw
!!! Check
  npw = wfc%npw
  call ptk_bcast(npw,root,comm)
  if(npw/=wfc%npw) ERROR("")
  call ptk_bcast(wfc%val,root,comm)
end subroutine pw_wfc_bcast

!@ MANUAL
subroutine pw_wfc_allreduce(wfc_send,wfc_recv,comm)
! Riduce una funzione d'onda in parallelo
  use ptk_module, only : ptk_allreduce,ptk_sum,ptk_comm
  type (pw_wfc),   intent(in)    :: wfc_send
  type (pw_wfc),   intent(inout) :: wfc_recv
  type (ptk_comm), intent(in)    :: comm
!@ END MANUAL
  call ptk_allreduce(wfc_send%val,wfc_recv%val,ptk_sum,comm)
end subroutine pw_wfc_allreduce

!@ MANUAL
subroutine pw_wfc_allreduce_inplace(wfc,comm)
! Riduce una funzione d'onda in parallelo
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm
  type (pw_wfc),   intent(inout) :: wfc
  type (ptk_comm), intent(in)    :: comm
!@ END MANUAL
  call ptk_allreduce_inplace(wfc%val,ptk_sum,comm)
end subroutine pw_wfc_allreduce_inplace


!@ MANUAL
subroutine pw_wfc_sort(wfc,index)
! Riordina una funzione d'onda usando index
  type(pw_wfc), intent(inout) :: wfc
  integer,      intent(in)    :: index(:)
!@ END MANUAL
  integer :: npw, ipw1
  complex :: wfc_tmp_val(1:wfc%npw) 
  npw = wfc%npw
  wfc_tmp_val(:)=0.0
  if(size(index)<npw) ERROR("")
  do ipw1=1,npw
     wfc_tmp_val(ipw1)=wfc%val(index(ipw1))
  enddo
  wfc%val(:) = wfc_tmp_val(:)
end subroutine pw_wfc_sort

!@ MANUAL
subroutine pw_wfc_calc_hartree(hartree,density)
  use num_module
! Calcola il potenziale di Hartree dalla densita'
  type(pw_wfc), intent(inout) :: hartree
  type(pw_wfc), intent(in)    :: density
!@ END MANUAL
  integer :: ipw,npw
  real    :: b(3,3),vg

  if(any(density%basis%k /= (/0.0,0.0,0.0/))) ERROR("")
  if(.not. hartree%basis == density%basis) ERROR("")
  npw = hartree%basis%npw
  
  b = hartree%basis%struct%b

  if(any(int(density%basis%g(:,1))/=(/0,0,0/))) ERROR("")
  do ipw=2,npw ! da due per saltare il vettore nullo !
    vg =  num_8pi/sum(num_matmul(b,real(density%basis%g(:,ipw))**2))
    hartree%val(ipw) = density%val(ipw) * vg
  end do
end subroutine pw_wfc_calc_hartree

!@ MANUAL
subroutine pw_wfc_sum(new_wfc,old_wfc)
! Somma le old_wfc in new_wfc
  type (pw_wfc), intent(inout) :: new_wfc
  type (pw_wfc), intent(in)    :: old_wfc(:)
!@ END MANUAL
  integer :: i,n,npw
  n = size(old_wfc)
  npw = new_wfc%npw
  new_wfc%val = 0.0
  do i=1,n
    if(old_wfc(i)%npw /= npw) ERROR("")
    new_wfc%val = new_wfc%val + old_wfc(i)%val
  end do
end subroutine pw_wfc_sum

subroutine pw_wfc_add(wfc,wfc1)
  type (pw_wfc), intent(inout) :: wfc
  type (pw_wfc), intent(in)    :: wfc1
  integer :: npw
  npw = wfc%npw
  if(wfc1%npw /= npw) ERROR("")
  wfc%val = wfc%val + wfc1%val
end subroutine pw_wfc_add

!@ MANUAL
subroutine pw_wfc_kinetic(new_wfc,old_wfc)
  use num_module
! Applica l'energia cinetica
  type (pw_wfc), intent(inout) :: new_wfc
  type (pw_wfc), intent(in)    :: old_wfc
!@ END MANUAL
  integer :: ipw
  real    :: kinetic
  real    :: b(3,3),kg(3)
  if(new_wfc%npw /= old_wfc%npw) ERROR("")
  b = new_wfc%basis%struct%b
  do ipw = 1,old_wfc%npw
    kg = old_wfc%basis%g(:,ipw)+old_wfc%basis%k
    kinetic = sum(num_matmul(b,kg)**2)
    new_wfc%val(ipw) = old_wfc%val(ipw) * kinetic
  end do
end subroutine pw_wfc_kinetic

!@ MANUAL
subroutine pw_wfc_p(new_wfc,old_wfc)
  use num_module
! Applica l'operatore p
! NB new_wfc(1) e' p_x, new_wfc(2) e' p_y, new_wfc(3) e' p_z
  type (pw_wfc), intent(inout) :: new_wfc(3)
  type (pw_wfc), intent(in)    :: old_wfc
!@ END MANUAL
  integer :: ipw
  real    :: kg(3),p(3),b(3,3)
  b = old_wfc%basis%struct%b
  do ipw = 1,old_wfc%npw
    kg = old_wfc%basis%g(:,ipw)+old_wfc%basis%k
    p  = 2.0 * num_matmul(b,kg)
! il 2.0 serve perche' H_k=-\nabla^2 (Rydberg)
    new_wfc(1)%val(ipw) = old_wfc%val(ipw) * p(1)
    new_wfc(2)%val(ipw) = old_wfc%val(ipw) * p(2)
    new_wfc(3)%val(ipw) = old_wfc%val(ipw) * p(3)
  end do
end subroutine pw_wfc_p

!@ MANUAL
function pw_wfc_p_braket(bra,ket)
! Effettua il valore di aspettazione di p
! pw_wfc_p_braket(:) e' p(:), dove p(1) e' p_x etc.
  complex                   :: pw_wfc_p_braket(3)
  type (pw_wfc), intent(in) :: bra,ket
!@ END MANUAL
  type(pw_wfc) :: wfc_tmp(3)
  call pw_wfc_init(wfc_tmp,bra%basis)
  call pw_wfc_p(wfc_tmp,ket)
  pw_wfc_p_braket(1) = pw_wfc_braket(bra,wfc_tmp(1))
  pw_wfc_p_braket(2) = pw_wfc_braket(bra,wfc_tmp(2))
  pw_wfc_p_braket(3) = pw_wfc_braket(bra,wfc_tmp(3))
  call pw_wfc_destroy(wfc_tmp)
end function pw_wfc_p_braket

subroutine pw_wfc_axpy(y,a,x)
  use num_module
  use lasi_module, only : lasi_axpy
  type (pw_wfc), intent(inout) :: y
  complex,       intent(in)    :: a
  type (pw_wfc), intent(in)    :: x
!  call num_axpy(a,x%val,y%val)
  if(size(x%val)/=size(y%val)) ERROR("")
  call lasi_axpy(n=size(x%val),ca=a,cx=x%val,incx=1,cy=y%val,incy=1)
end subroutine pw_wfc_axpy

subroutine pw_wfc_coulomb(wfc,opt)
  use num_module
  type (pw_wfc), intent(inout) :: wfc
  character(*),  intent(in)    :: opt
  integer :: ipw,npw
  real    :: gpq2,b(3,3),kg(3)
  integer :: iopt 
  npw = wfc%npw
  b = wfc%basis%struct%b
  select case(opt)
  case("full")
    iopt = 1
  case("half")
    iopt = 2
  case("inverse")
    iopt = 3
  case default
    ERROR("")
  end select
  do ipw=1,npw
    kg   = wfc%basis%g(:,ipw)+wfc%basis%k
    gpq2 = sum(num_matmul(b,kg)**2)
    if(gpq2 < 0.00000001) cycle
    if(iopt==1) wfc%val(ipw) = num_8pi / gpq2
    if(iopt==2) wfc%val(ipw) = num_sqrt8pi / sqrt(gpq2)
    if(iopt==3) wfc%val(ipw) = gpq2 / num_8pi
  end do
end subroutine pw_wfc_coulomb

subroutine pw_wfc_mul(res,wfc1,wfc2)
  use num_module
  type (pw_wfc), intent(inout) :: res
  type (pw_wfc), intent(in)    :: wfc1,wfc2
  integer :: npw
  npw = res%npw
  if(npw/=wfc1%npw) ERROR("")
  if(npw/=wfc2%npw) ERROR("")
  call num_vemul(res%val,wfc1%val,wfc2%val)
end subroutine pw_wfc_mul

subroutine pw_wfc_conjg(wfc)
  type (pw_wfc), intent(inout) :: wfc
  if(any(abs(wfc%basis%k)>0.000001)) ERROR("")
  wfc%val = conjg(wfc%val)
end subroutine pw_wfc_conjg
  

end module pw_wfc_module
