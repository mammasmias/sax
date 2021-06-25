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
module pw_w_type
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
public :: pw_w
!@ END MANUAL

!@ MANUAL
type pw_w
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
  complex,            pointer :: macroscopic(:,:,:)
! NB epsilon_M e' 8*pi*macroscopic
! W(q) = 1 / q_alpha * macroscopic_alpha_beta * q_beta
! NB per iomega = -1,
! W(q) = q_alpha * macroscopic_alpha_beta * q_beta / q**4
  type (pw_wfc),      pointer :: lambda(:,:)
  type (pw_wfc),      pointer :: rho(:,:)
  logical                     :: smooth
  real                        :: v0 ! Valore di v_bare(q=0) associato con lo smooth
  integer                     :: nomega
!  real                        :: omegamax
  complex                      :: omegamax
!  real,               pointer :: omega(:),weight(:)
  complex,               pointer :: omega(:)
  real,                 pointer :: weight(:)

  character(len=30) :: coulomb_div_treatment
  real             :: ecutvcut
end type pw_w
! Si tratta di un oggetto parallelo
!@ END MANUAL

end module pw_w_type

