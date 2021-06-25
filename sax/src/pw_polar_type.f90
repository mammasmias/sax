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

module pw_polar_type
use pw_struct_module
use pw_symmlist_module
use pw_kmesh_module
use pw_basis_module
use pw_wfc6d_module
use pw_wfc_module
use ptk_module, only : ptk_comm
implicit none
private
public :: pw_polar

type pw_polar
  type (pw_struct),   pointer :: struct
  type (pw_symmlist), pointer :: symmlist
  type (pw_kmesh),    pointer :: qmesh
  real                        :: cutoff
  integer                     :: root
  type (ptk_comm)             :: comm
  integer                     :: npe,rank
  integer,            pointer :: where_q(:,:)
!                                (1,i) --> processore che contiene il punto q i-esimo
!                                (2,i) --> locazione del punto q i-esimo
  type (pw_basis),    pointer :: basis(:)
  type (pw_wfc6d),    pointer :: val(:,:)
  logical                     :: val_initialized
  integer                     :: iqgamma
  type (pw_wfc),      pointer :: gradients_l(:,:)
  type (pw_wfc),      pointer :: gradients_r(:,:)
  complex,            pointer :: macroscopic(:,:,:)
!  real,               pointer :: omega(:),weight(:)
  complex,               pointer :: omega(:)
  real,               pointer :: weight(:)
  integer                     :: nomega
  real                        :: emax ! energy cutoff for allowed valence-conduction transitions
  real                        :: degauss
! Si tratta di un oggetto parallelo
end type pw_polar

end module pw_polar_type

