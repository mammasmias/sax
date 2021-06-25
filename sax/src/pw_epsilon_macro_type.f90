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

module pw_epsilon_macro_type
use pw_struct_module
use pw_symmlist_module
use pw_kmesh_module
use pw_common_module
use ptk_module, only : ptk_comm

implicit none
private

public :: pw_epsilon_macro

type pw_epsilon_macro
  type (pw_struct), pointer    :: struct
  type (pw_symmlist), pointer  :: symmlist
  type (pw_kmesh), pointer     :: qmesh
  real                :: cutoff,omegamax
  real                :: emax, degauss, energy_shift
  integer             :: nomega
  real                :: broadening
  logical             :: imaginary_axis
  complex, pointer    :: macroscopic(:,:,:)
  real, pointer       :: omega(:)
  real, pointer       :: weight(:)
  integer             :: iqgamma
  type(ptk_comm)      :: comm
  integer             :: root
end type pw_epsilon_macro

end module pw_epsilon_macro_type
