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

module pw_vnloc_interf
implicit none

private
public :: pw_vnloc_init
public :: pw_vnloc_apply
public :: pw_vnloc_destroy

interface pw_vnloc_init
subroutine pw_vnloc_init_x1(vnloc,basis,atoms)
  use pw_atoms_module
  use num_module
  use pw_pseudo_module
  use tools_module
  use pw_vnloc_type
  use pw_basis_module

  type(pw_vnloc), intent(out) :: vnloc
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  
end subroutine pw_vnloc_init_x1

subroutine pw_vnloc_init_x2(vnloc,basis,atoms,iatom)
  use pw_atoms_module
  use num_module
  use pw_pseudo_module
  use tools_module
  use pw_vnloc_type
  use pw_basis_module

  type(pw_vnloc), intent(out) :: vnloc
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  integer, intent(in) :: iatom

end subroutine pw_vnloc_init_x2
end interface pw_vnloc_init

interface pw_vnloc_apply
subroutine pw_vnloc_apply_x(wfc_new,vnloc,wfc)
  use pw_vnloc_type
  use pw_wfc_module

  type(pw_wfc), intent(inout) :: wfc_new
  type(pw_vnloc), intent(in)  :: vnloc
  type(pw_wfc), intent(in)    :: wfc

end subroutine pw_vnloc_apply_x  
end interface pw_vnloc_apply

interface pw_vnloc_destroy
subroutine pw_vnloc_destroy_x(vnloc)
  use pw_vnloc_type
  use pw_wfc_module

  type(pw_vnloc), intent(inout) :: vnloc
  
end subroutine pw_vnloc_destroy_x
end interface pw_vnloc_destroy
end module pw_vnloc_interf
