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

module pw_vloc_interf
implicit none

private
public :: pw_vloc_calc, pw_vloc_corr, pw_vloc_calc0


interface pw_vloc_calc
subroutine pw_vloc_calc_x1(vloc,atoms)
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  use pw_atoms_module
  use tools_module
  use num_module
  type (pw_wfc),   intent(inout) :: vloc
  type (pw_atoms), intent(in)    :: atoms
end subroutine pw_vloc_calc_x1

subroutine pw_vloc_calc_x2(vloc,atoms,iatom)
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  use pw_atoms_module
  use tools_module
  use num_module
  type (pw_wfc),   intent(inout) :: vloc
  type (pw_atoms), intent(in)    :: atoms
  integer, intent(in) :: iatom
end subroutine pw_vloc_calc_x2
end interface

interface pw_vloc_calc0
subroutine pw_vloc_calc0_x1(vloc,atoms)
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  use pw_atoms_module
  use tools_module
  use num_module
  type (pw_wfc),   intent(inout) :: vloc
  type (pw_atoms), intent(in)    :: atoms
end subroutine pw_vloc_calc0_x1
end interface

interface pw_vloc_corr
subroutine pw_vloc_corr_x1(vloc,atoms,vcut)
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  use pw_atoms_module
  use tools_module
  use num_module
  use coulomb_vcut_module, only : vcut_type
  type (pw_wfc),   intent(inout) :: vloc
  type (pw_atoms), intent(in)    :: atoms
  type (vcut_type), intent(in) :: vcut
end subroutine pw_vloc_corr_x1
end interface

end module pw_vloc_interf
