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

subroutine pw_basis_write_external(basis,unit,fmt,rec,name)
  use iotk_module
  use pw_basis_module
  implicit none
  type(pw_basis),         intent(in) :: basis
  integer,                intent(in) :: unit
  character(100),         intent(in) :: fmt
  integer,                intent(in) :: rec
  character(iotk_namlenx),intent(in) :: name
  integer :: iostat
  character(iotk_attlenx) :: attr
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  select case (trim(fmt))
  case("dir")
    if(rec<0) ERROR("")
    write(unit,rec=rec,iostat=iostat) basis%npw,basis%k,basis%r0,basis%conjg,&
                                      basis%g
    IOCHECK(unit,iostat)
  case("iotk")
    call iotk_write_attr(attr,"type","pw_basis",first=.true.)
    call iotk_write_begin(unit,trim(name),attr)
    call iotk_write_attr(attr,"npw",basis%npw,first=.true.)
    call iotk_write_attr(attr,"k",basis%k)
    call iotk_write_attr(attr,"r0",basis%r0)
    call iotk_write_attr(attr,"conjg",basis%conjg)
    call iotk_write_empty(unit,"info",attr)
    call iotk_write_dat  (unit,"g",basis%g,columns=3)
    call iotk_write_end(unit,trim(name))
  case default
    ERROR("Unrecognized fmt - "//trim(fmt))
  end select
end subroutine pw_basis_write_external

!@ MANUAL
subroutine pw_basis_read_external(basis,unit,fmt,rec,name)
  use pw_basis_module
  use iotk_module
  implicit none
! Subroutine per scrivere su file una base
! il formato fmt, se assente, viene dedotto con un inquire sull'unita' logica
  type(pw_basis), intent(inout) :: basis
  integer,        intent(in)    :: unit
  character(100), intent(in)    :: fmt
  integer,        intent(in)    :: rec
  character(iotk_namlenx),intent(in) :: name
! basis :: base da leggere
! unit  :: unita' da cui leggere
! fmt   :: formato
! rec   :: record
! sono possibili i seguenti fmt:
!   "txt"   -> file ascii
!   "bin"   -> file binario
!   "dir"   -> file binario sequenziale
!   "pw104" -> file tipo 'grid' prodotto da pw104 [versione modificata da Alice Ruini]
!   "launch"-> file launch.dat ! nb rec e' il numero del punto k
!@ END MANUAL
  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  integer :: npw
  integer       :: iostat
  integer, allocatable :: g4(:,:)
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  select case (trim(fmt))
  case("dir")
    if(rec<0) ERROR("")
    read(unit,rec=rec,iostat=iostat) npw
    IOCHECK(unit,iostat)
    call pw_basis_set_npw(basis,npw)
    read(unit,rec=rec,iostat=iostat) npw,basis%k,basis%r0,basis%conjg,basis%g
    IOCHECK(unit,iostat)
  case("pw104")
    read(unit,iostat=iostat) npw
    IOCHECK(unit,iostat)
    call pw_basis_set_npw(basis,npw)
    basis%k  = 0
    basis%r0 = 0
    basis%conjg = .false.
    allocate(g4(3,npw))
    read(unit,iostat=iostat) g4
    IOCHECK(unit,iostat)
    basis%g=g4
    deallocate(g4)
   case("pw_punch")
     call iotk_scan_begin(unit,"Kpoint"//iotk_index(rec),attr)
     call iotk_scan_attr (attr,"npw",npw)
     call pw_basis_set_npw(basis,npw)
     basis%k  = 0
     basis%r0 = 0
     basis%conjg = .false.
     call iotk_scan_dat  (unit,"grid",basis%g)
     call iotk_scan_end  (unit,"Kpoint"//iotk_index(rec))
  case("iotk")
    call iotk_scan_begin(unit,trim(name),attr)
    call iotk_scan_attr(attr,"type",rtype)
    if(trim(rtype)/="pw_basis") ERROR("")
    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr(attr,"npw",npw)
    call pw_basis_set_npw(basis,npw)
    call iotk_scan_attr(attr,"k",basis%k)
    call iotk_scan_attr(attr,"r0",basis%r0)
    call iotk_scan_attr(attr,"conjg",basis%conjg)
    call iotk_scan_dat  (unit,"g",basis%g)
    call iotk_scan_end(unit,trim(name))
  case default
    ERROR("Unrecognized fmt - "//trim(fmt))
  end select
  call pw_basis_set_g_extr(basis)
end subroutine pw_basis_read_external

subroutine pw_basis_bcast_external(basis,root,comm)
  use ptk_module, only : ptk_comm_rank,ptk_bcast,ptk_comm
  use pw_basis_module
  implicit none
  type(pw_basis), intent(inout) :: basis
  integer,        intent(in)    :: root
  type(ptk_comm), intent(in)    :: comm
! QUESTA SUBROUTINE PUO' ESSERE OTTIMIZZATA MOLTO
! DIMINUENDO IL NUMERO DI CHIAMATE A MPI
  integer :: myrank,npw
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  call ptk_comm_rank(comm,myrank)
  npw = basis%npw
  call ptk_bcast(npw,root,comm)
  if(myrank/=root) then
    call pw_basis_set_npw(basis,npw)
  end if
  call ptk_bcast(basis%k,root,comm)
  call ptk_bcast(basis%r0,root,comm)
  call ptk_bcast(basis%conjg,root,comm)
  call ptk_bcast(basis%g,root,comm)
  call ptk_bcast(basis%gmax,root,comm)
  call ptk_bcast(basis%gmin,root,comm)
end subroutine pw_basis_bcast_external


