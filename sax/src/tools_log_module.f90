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
module tools_log_module
implicit none
! Module containing tools for logging
private
public :: tools_log
public :: tools_log_unit
public :: tools_log_set_file
!@ END MANUAL

integer,            save :: log_unit = -1
character(len=256), save :: logfile

! Private parameters:
character(len=3), parameter :: logfile_default = "log"
logical,            save :: last_advanced = .true.

contains

!@ MANUAL
subroutine tools_log_set_file(file)
! Sets the logfile to 'file'.
! It can be invoked only once (renaming is not allowed)
! Note that if a log subroutine is used when this subroutine
! has not been called yet, the logfile is set to 'logfile_default'
! and no subsequent change is allowed
  use tools_char_module
  use ptk_module, only : ptk_comm_rank,ptk_comm_size,ptk_comm_world
  use iotk_module, only : iotk_free_unit
  character(len=*), intent(in) :: file
!@ END MANUAL
  integer :: rank,size,values(8)
  character(500):: line
  if(log_unit==-1) then
    if(len(file)>100) ERROR("")
    call ptk_comm_rank(ptk_comm_world,rank)
    call ptk_comm_size(ptk_comm_world,size)
    if(rank>0) then
      logfile = file//"."//tools_char(rank,3)//"-"//tools_char(size,3)
    else
      logfile = file
    end if
    call iotk_free_unit(log_unit)
    open(file=trim(logfile),unit=log_unit,position="append")
    write(log_unit,"(a)") "##################################################"
    call date_and_time(values=values)
    line = "Today is "//tools_char(values(3),2)//"/"// &
            tools_char(values(2),2)//"/"// &
            tools_char(values(1),4)
    write(log_unit,"(a)") trim(line)
  else
    WARNING("Once opened, the log file cannot be changed")
  end if
end subroutine tools_log_set_file


!@MANUAL
subroutine tools_log(msg,advance)
! Writes on logfile the message 'msg'
! The timestamp is added at the beginning of each line.
  use tools_char_module
  character(len=*),           intent(in) :: msg
  logical,          optional, intent(in) :: advance
! The maximum lenght is 300.
! If advance is present and .false., a new line is not appended
! and in the next write the date is not prepended
!@ END MANUAL
  integer :: values(8)
  character(3) :: advance_loc
  character(500):: line
  if(log_unit<0) call tools_log_set_file(logfile_default)
  if(len(msg)>300) ERROR("")
  if(last_advanced) then
    call date_and_time(values=values)
    line = tools_char(values(5),2)//":"// &
           tools_char(values(6),2)//":"// &
           tools_char(values(7),2)//"."// &
           tools_char(values(8),3)//":  "//msg
  else
    line = msg
  end if
  advance_loc = "yes"
  if(present(advance)) then
    if(.not.advance) advance_loc = "no"
  end if
  if(advance_loc == "yes" ) last_advanced = .true.
  if(advance_loc == "no"  ) last_advanced = .false.
  write(log_unit,"(a)",advance=advance_loc) trim(line)
  close(log_unit)
  open(file=trim(logfile),unit=log_unit,position="append")
end subroutine tools_log

!@ MANUAL
function tools_log_unit()
! This function returns the actual number of the log unit.
! It can be used to write to this unit bypassing the logging system.
! e.g., to write on the log file, you should use
! write(tools_log_unit(),*) "pippo"
! Note that, due to problem with recursive IO in fortran,
! it is not allowed to use it as the first writing.
! It is preferrable to first assign it to a variable,
! then using it, e.g.
! logunit = tools_log_unit()
! write(logunit,*) "pippo"
  integer :: tools_log_unit
!@ END MANUAL
  if(log_unit<0) call tools_log_set_file(logfile_default)
  tools_log_unit = log_unit
end function tools_log_unit

end module tools_log_module

