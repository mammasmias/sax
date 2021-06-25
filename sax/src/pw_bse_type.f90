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

module pw_bse_type
use pw_states_type
use pw_atoms_module
use pw_parall_matrix_type, only : pw_parall_complex_matrix
implicit none
private
public :: pw_bse

type pw_bse
  type(pw_states), pointer :: states 
  type(pw_atoms), pointer :: atoms
  integer          :: ntrans
  integer, pointer :: iktrans(:),ib1trans(:)
  integer, pointer :: ib2trans(:)
  integer :: nbvmin, nbvmax,nbcmin,nbcmax
  integer :: nkmin,nkmax
  type(pw_parall_complex_matrix) :: matrix
  integer :: spin
  logical :: find_trans
  real :: emax
  real :: emin
end type pw_bse

end module pw_bse_type
