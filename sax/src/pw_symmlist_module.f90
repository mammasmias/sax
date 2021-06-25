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
module pw_symmlist_module
use pw_symm_module
use pw_struct_module
implicit none
private
public :: pw_symmlist,            &
          pw_symmlist_init,       &
          pw_symmlist_destroy,    &
          pw_symmlist_set_struct, &
          pw_symmlist_set_nsymm,  &
          pw_symmlist_read,       &
          pw_symmlist_write,      &
          pw_symmlist_bcast,      &
          pw_symmlist_readbcast,  &
          pw_symmlist_only_identity
!@ END MANUAL

!@ MANUAL
type pw_symmlist
  integer                   :: nsymm
  type (pw_symm),   pointer :: symm(:)
  type (pw_struct), pointer :: struct
end type pw_symmlist
! nsymm  :: numero di simmetrie
! symm   :: array con le simmetrie
! struct :: puntatore alla struttura
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_symmlist_init(symmlist,struct)
  type (pw_symmlist),                 intent(out) :: symmlist
  type (pw_struct), optional, intent(in)  :: struct
!@ END MANUAL
  symmlist%nsymm=0
  allocate(symmlist%symm(0))
  nullify(symmlist%struct)
  if(present(struct)) call pw_symmlist_set_struct(symmlist,struct)
end subroutine pw_symmlist_init

!@ MANUAL
subroutine pw_symmlist_destroy(symmlist)
  type (pw_symmlist), intent(inout) :: symmlist
!@ END MANUAL
  symmlist%nsymm=0
  if(.not.associated(symmlist%symm)) ERROR("")
  deallocate(symmlist%symm)
end subroutine pw_symmlist_destroy

!@ MANUAL
subroutine pw_symmlist_only_identity(symmlist)
  type (pw_symmlist), intent(inout) :: symmlist
!@ END MANUAL
  call pw_symmlist_set_nsymm(symmlist,1)
  call pw_symm_identity(symmlist%symm(1))
end subroutine pw_symmlist_only_identity


!@ MANUAL
subroutine pw_symmlist_set_struct(symmlist,struct)
  type (pw_symmlist),       intent(inout) :: symmlist
  type (pw_struct), target, intent(in)    :: struct
!@ END MANUAL
  if(.not.associated(symmlist%symm)) ERROR("")
  symmlist%struct => struct
end subroutine pw_symmlist_set_struct

!@ MANUAL
subroutine pw_symmlist_set_nsymm(symmlist,nsymm)
  type (pw_symmlist), intent(inout) :: symmlist
  integer,            intent(in)    :: nsymm
!@ END MANUAL
  integer :: isymm
  if(.not.associated(symmlist%symm))   ERROR("")
  if(.not.associated(symmlist%struct)) ERROR("")
  do isymm=1,symmlist%nsymm
    call pw_symm_destroy(symmlist%symm(isymm))
  end do
  symmlist%nsymm = nsymm
  deallocate(symmlist%symm)
  allocate(symmlist%symm(nsymm))
  do isymm=1,symmlist%nsymm
    call pw_symm_init(symmlist%symm(isymm),symmlist%struct)
  end do
end subroutine pw_symmlist_set_nsymm

!@ MANUAL
subroutine pw_symmlist_read(symmlist,unit,fmt)
  use pw_common_module
  type (pw_symmlist),     intent(inout) :: symmlist
  integer,                intent(in)    :: unit
  character(*), optional, intent(in)    :: fmt
! symmlist : oggetto da leggere
! unit     : unita' di scrittura
! fmt      : formato
!           "txt" testo standard
!           "bin" formato binario
!@ END MANUAL
  character(20) :: fmt_local
  character(20) :: dummy
  integer :: iostat,nsymm,isymm
  if(present(fmt)) fmt_local = fmt
  if(.not.present(fmt)) fmt_local = pw_default_fmt(unit)
  select case(fmt_local)
  case("txt")
    read(unit,*,iostat=iostat) dummy
    if(iostat/=0) ERROR("")
    if(dummy/="%PW_SYMMLIST") ERROR("")
    read(unit,*,iostat=iostat) dummy,nsymm
    if(iostat/=0) ERROR("")
    if(dummy/="nsymm") ERROR("")
    call pw_symmlist_set_nsymm(symmlist,nsymm)
    do isymm=1,nsymm
      call pw_symm_read(symmlist%symm(isymm),unit,"txt")
    end do
    read(unit,*,iostat=iostat) dummy
    if(iostat/=0) ERROR("")
    if(dummy/="%END_PW_SYMMLIST") ERROR("")
  case("bin")
    read(unit,iostat=iostat) nsymm
    if(iostat/=0) ERROR("")
    call pw_symmlist_set_nsymm(symmlist,nsymm)
    do isymm=1,nsymm
      call pw_symm_read(symmlist%symm(isymm),unit,"bin")
    end do
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
end subroutine pw_symmlist_read

!@ MANUAL
subroutine pw_symmlist_write(symmlist,unit,fmt)
  use pw_common_module
  type (pw_symmlist),     intent(in) :: symmlist
  integer,                intent(in) :: unit
  character(*), optional, intent(in) :: fmt
! symmlist : oggetto da scrivere
! unit     : unita' di scrittura
! fmt      : formato
!           "txt" testo standard
!           "log" formato di log
!           "bin" formato binario
!@ END MANUAL
  integer :: iostat,nsymm,isymm
  character(20) :: fmt_local
  if(present(fmt)) fmt_local = fmt
  if(.not.present(fmt)) fmt_local = pw_default_fmt(unit)
  nsymm = symmlist%nsymm
  select case(fmt_local)
  case("txt")
    write(unit,"(a)",   iostat=iostat) "%PW_SYMMLIST"
    if(iostat/=0) ERROR("")
    write(unit,"(a,i5)",iostat=iostat) "nsymm",nsymm
    if(iostat/=0) ERROR("")
    do isymm=1,nsymm
      call pw_symm_write(symmlist%symm(isymm),unit,"txt")
    end do
    write(unit,"(a)",   iostat=iostat) "%END_PW_SYMMLIST"
    if(iostat/=0) ERROR("")
  case("bin")
    write(unit,iostat=iostat) nsymm
    if(iostat/=0) ERROR("")
    do isymm=1,nsymm
      call pw_symm_write(symmlist%symm(isymm),unit,"bin")
    end do
  case("log")
    write(unit,"(a)",   iostat=iostat) "*** List of symmetries ***"
    write(unit,"(a,i5)",iostat=iostat) "Number of symmetries : ",nsymm
    write(unit,"(a)",   iostat=iostat) "List of symmetries :"
    do isymm=1,nsymm
      call pw_symm_write(symmlist%symm(isymm),unit,"log")
    end do
    write(unit,"(a)",   iostat=iostat) "*** End list of symmetries ***"
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
end subroutine pw_symmlist_write

!@ MANUAL
subroutine pw_symmlist_bcast(symmlist,root,comm)
! Effettua il broadcast di un oggetto symmlist
  use ptk_module, only : ptk_comm_rank,ptk_bcast,ptk_comm
  type (pw_symmlist), intent(inout) :: symmlist
  integer,            intent(in)    :: root
  type (ptk_comm),    intent(in)    :: comm
!@ END MANUAL
  integer :: isymm,nsymm,rank
  if(.not.associated(symmlist%symm))   ERROR("")
  if(.not.associated(symmlist%struct)) ERROR("")
  nsymm = symmlist%nsymm
  call ptk_comm_rank(comm,rank)
  call ptk_bcast(nsymm,root,comm)
  if(root/=rank) call pw_symmlist_set_nsymm(symmlist,nsymm)
  do isymm=1,nsymm
    call pw_symm_bcast(symmlist%symm(isymm),root,comm)
  end do
end subroutine pw_symmlist_bcast

!@ MANUAL
subroutine pw_symmlist_readbcast(symmlist,unit,root,comm,fmt)
! Root legge ed effettua il broadcast di un oggetto symmlist
  use pw_common_module
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type(pw_symmlist),      intent(out) :: symmlist
  integer,                intent(in)  :: unit,root
  type (ptk_comm),        intent(in)  :: comm
  character(*), optional, intent(in)  :: fmt
!@ END MANUAL
  character(20) :: fmt_local
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(root==rank) then
    if(present(fmt))      fmt_local=fmt
    if(.not.present(fmt)) fmt_local=pw_default_fmt(unit)
    call pw_symmlist_read(symmlist,unit,fmt_local)
  end if
  call pw_symmlist_bcast(symmlist,root,comm)
end subroutine pw_symmlist_readbcast

end module pw_symmlist_module
