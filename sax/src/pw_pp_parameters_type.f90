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
module pw_pp_parameters_type
use pw_struct_module
use pw_symmlist_module
use pw_kmesh_module
use pw_wfc_module
use pw_wfc6d_module
use pw_basis_module
use pw_common_module
use ptk_module, only : ptk_comm
implicit none
private
public :: pw_pp_parameters
!@ END MANUAL

!@ MANUAL
! here is defined the type for plasmon pole parameters
! the type is similar to W
type pw_pp_parameters
  type (pw_struct),   pointer :: struct
  type (pw_symmlist), pointer :: symmlist
  type (pw_kmesh),    pointer :: qmesh
  real                        :: cutoff
  integer                     :: root
  type (ptk_comm)             :: comm
  real                        :: plasmon_energy, broadening       
  integer                     :: npe,rank
  integer,            pointer :: where_q(:,:)
!                                (1,i) --> processore che contiene il punto q i-esimo
!                                (2,i) --> locazione del punto q i-esimo
!  complex                    :: discontinuity_parameters(0:1,1:2) ! not in use by now with vcut
  type (pw_basis),    pointer :: basis(:)
  type (pw_wfc6d),    pointer :: val(:,:)
! for val(:,:) the first dimension is q the second goes from 0 to 1
! val(:,0) contains plasmon pole energies
! val(:,1) contains plasmon pole weights**2
  integer                     :: iqgamma
end type pw_pp_parameters
! Si tratta di un oggetto parallelo
!@ END MANUAL

end module pw_pp_parameters_type

