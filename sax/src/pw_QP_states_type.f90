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

module pw_QP_states_type
  use pw_struct_module
  use pw_kmesh_module
  use pw_basis_module
  use pw_wfc_module
  use pw_common_module
  use pw_states_module
  use ptk_module, only : ptk_comm
  implicit none
  private
  public :: pw_QP_states

!@ MANUAL
  type pw_QP_states
  type (pw_struct),   pointer :: struct
  type (pw_kmesh),    pointer :: kmesh
  integer                     :: nbmin,nbmax
  integer                     :: root
  type (ptk_comm)             :: comm
  integer                     :: npe,rank
  integer,            pointer :: where_band(:,:)
!                                (1,i) -->
!                                processore che
!                                contiene la banda i
!                                (2,i) --> locazione
!                                della banda i
  type (pw_basis),    pointer :: basis(:)
  type (pw_wfc),      pointer :: wfc(:,:)
  real,               pointer :: e(:,:)
  real,               pointer :: occupation(:,:) 
  real,               pointer :: weight(:,:) 
end type pw_QP_states
!@ END MANUAL
end module pw_QP_states_type

