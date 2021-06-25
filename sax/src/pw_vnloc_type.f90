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

module pw_vnloc_type
use pw_wfc_module
use pw_basis_module
implicit none
private
public :: pw_vnloc

type pw_vnloc
      type(pw_basis), pointer :: basis
      integer                 :: nproj
      type(pw_wfc), pointer   :: proj(:)
      real, pointer           :: d(:)
end type pw_vnloc      
end module pw_vnloc_type
