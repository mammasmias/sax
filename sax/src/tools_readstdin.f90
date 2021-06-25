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
module tools_readstdin_module
! module with tool to read file names from stdin with a given
! convention
! valid inputs (from stdin) are in the form
! "name > value"
! multiple names are allowed

implicit none
private
save
public :: tools_readstdin

!@ END MANUAL

type argument
  type (argument), pointer :: next
  character(1024) :: name
  character(1024) :: val
end type argument

logical                  :: arguments_initialized = .false.
type (argument), pointer :: arguments

contains

!@ MANUAL
function tools_readstdin(name)
! returns the value associated to name in stdin
! e.g., if stdin in "pippo > pluto" then
! tools_readstdin("pippo") returns "pluto"
! if the argument is not found, the routine fails.
  character(len=*), intent(in) :: name
  character(len=1024) :: tools_readstdin
!@ END MANUAL
  character(len=1024) :: line
  integer             :: iostat,pos
  type (argument), pointer :: this
  if(.not.arguments_initialized) then
    arguments_initialized = .true.
    nullify(arguments)
    do
      read(*,"(a)",iostat=iostat) line
      if(iostat<0) exit
      if(iostat>0) ERROR("")
      pos = scan(line,">")
      if(pos==0) cycle
      if(pos==1 .or. pos==len(line)) ERROR("")
      allocate(this)
      this%name = adjustl(line(1:pos-1))
      this%val  = adjustl(line(pos+1:))
      this%next => arguments
      arguments => this
    end do
  end if
  this => arguments
  do
    if(.not. associated(this)) exit
    this => this%next
  end do
  this => arguments
  do
    if(trim(name)==trim(this%name)) exit
    if(.not. associated(this%next)) ERROR("")
    this => this%next
  end do
  tools_readstdin = this%val
end function tools_readstdin

end module tools_readstdin_module

