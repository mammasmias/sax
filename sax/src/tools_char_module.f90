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
module tools_char_module
implicit none
! This module contains tools for character manipulation
private
public :: tools_char,      &
          tools_uppercase, &
          tools_lowercase
!@ END MANUAL

! local strings
character(len=26), parameter :: upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
character(len=26), parameter :: lower = "abcdefghijklmnopqrstuvwxyz"
!

contains

!@ MANUAL
function tools_char(i,len)
! It returns a string containing the number 'i'
! written in decimal format of length 'len',
! left-padded with 0.
  integer, intent(in) :: i,len
  character(len=len)  :: tools_char
! Maximum value for len is 100
!@ END MANUAL
  character(len=8) :: fmt
  if(len>=100 .or. len<=0) ERROR("")
  write(fmt,"(a,i2.2,a,i2.2,a)") "(i",len,".",len,")"
  write(tools_char,fmt) i
end function tools_char

!@ MANUAL
function tools_uppercase(char)
! convert a string to uppercase
  character(len=*), intent(in) :: char
  character(len=len(char))     :: tools_uppercase
!@ END MANUAL
  integer :: i,pos
  do i = 1,len(char)
    pos = index(lower,char(i:i))
    if(pos==0) then
      tools_uppercase(i:i) = char(i:i)
    else
      tools_uppercase(i:i) = upper(pos:pos)
    end if
  end do
end function tools_uppercase

!@ MANUAL
function tools_lowercase(char)
! convert a string to lowercase
  character(len=*), intent(in) :: char
  character(len=len(char))     :: tools_lowercase
!@ END MANUAL
  integer :: i,pos
  do i = 1,len(char)
    pos = index(upper,char(i:i))
    if(pos==0) then
      tools_lowercase(i:i) = char(i:i)
    else
      tools_lowercase(i:i) = lower(pos:pos)
    end if
  end do
end function tools_lowercase

end module tools_char_module

