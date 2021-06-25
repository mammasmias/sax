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
module tools_mallinfo_module
implicit none
! This module contains tools for obtain information about
! dynamic memory allocation.
private
public :: tools_mallinfo_print
!@ END MANUAL

contains

!@ MANUAL
subroutine tools_mallinfo_print(unit,fmt,msg)
  use ptk_module, only : ptk_comm_rank,ptk_comm_world
! This subroutine writes on unit 'unit' information about
! the dynamic memory allocation, together with message 'msg'
! and the rank of the calling process
! Different formats are allowed:
! fmt = "short" (default)
!   a oneline output containing:
!   rank arena hblkhd usmblks fsmblks uordblks fordblks msg
! fmt = "long"
!   a more descriptve output
  integer,                    intent(in) :: unit
  character(len=*), optional, intent(in) :: fmt
  character(len=*), optional, intent(in) :: msg
!@ END MANUAL
! NOTE that the kind here could cause portability problems:
  integer(kind=8) :: arena, hblkhd, usmblks, fsmblks, uordblks, fordblks
  integer   :: rank
  character(len=40) :: fmt_local,msg_local
  call ptk_comm_rank(ptk_comm_world,rank)
  fmt_local = "short"
  msg_local = ""
  if(present(fmt)) fmt_local = fmt
  if(present(msg)) msg_local = msg
  call clib_mallinfo(arena,hblkhd,usmblks,fsmblks,uordblks,fordblks) 
  select case(fmt_local)
  case("short")
    write(unit,"(i3,6i12,a)") rank, arena, hblkhd, usmblks, fsmblks, uordblks, fordblks, &
                              trim(msg_local)
  case("long")
    write(unit,*) "Memory statistics:"
#ifdef __PARA
    write(unit,*) "rank          :",rank,"in MPI_Common_world"
#endif
    write(unit,*) "total space in arena            :",arena
    write(unit,*) "space in holding block headers  :",hblkhd
    write(unit,*) "space in small blocks in use    :",usmblks
    write(unit,*) "space in free small blocks      :",fsmblks
    write(unit,*) "space in ordinary blocks in use :",uordblks
    write(unit,*) "space in free ordinary blocks   :",fordblks
    write(unit,*) "message                         :",trim(msg_local)
    write(unit,*) "End memory statistics"
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select
end subroutine tools_mallinfo_print

end module tools_mallinfo_module

