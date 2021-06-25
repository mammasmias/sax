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

!@ MANUAL
subroutine tools_er(msg,file,line,fatal)
  use ptk_module, only : ptk_comm_rank,ptk_comm_world,ptk_abort
  implicit none
! Error handler
  character(len=*), intent(in) :: msg,file
  integer,          intent(in) :: line,fatal
! msg is the message
! file is the name of the file containing the calling routine
! line is the line number containing the calling routine
! fatal is:
! 0 => the error is fatal
! 1 => the error is non fatal
! 2 => this is a debug information
! This subroutine is tipically interfaced through some macro
! defined in tools_error.h
!@ END MANUAL
  integer :: myrank
  select case(fatal)
  case (0)
    write(0,*) "ERROR   : ",trim(msg)
  case (1)
    write(0,*) "WARNING : ",trim(msg)
  case (2)
    write(0,*) "DEBUG   : ",trim(msg)
  end select
  call ptk_comm_rank(comm=ptk_comm_world,rank=myrank)
  write(0,*) "from pe : ",myrank," in COMMON_WORLD"
  write(0,*) "in file : ",trim(file)
  write(0,*) "at line : ",line
  if(fatal==0) then
    call ptk_abort
  end if
end subroutine tools_er

!@ MANUAL
subroutine tools_ioerr(iostat,unit,file,line)
  implicit none
! I/O Error handler
  character(*), intent(in) :: file
  integer,      intent(in) :: iostat,line,unit
! iostat is the error code returned by the i/o operation
! unit is the number of the logical i'o unit
! file is the name of the file containing the calling routine
! line is the line number containing the calling routine
! This subroutine is tipically interfaced through some macro
! defined in tools_error.h
! (this subroutine calls tools_er)
!@ END MANUAL
  character(300) :: msg,name
  character(10)  :: unitstr,iostatstr
  logical        :: named
  write(iostatstr,'(i10)') iostat
  write(unitstr,'(i10)') unit
  inquire(unit=unit,named=named)
  inquire(unit=unit,name=name)
  if(.not. named) name="UNNAMED"
  msg = "IO error, code "//trim(iostatstr)//" unit "//trim(unitstr)//" file "//trim(name)
  call tools_er(msg,file,line,0)
end subroutine tools_ioerr


