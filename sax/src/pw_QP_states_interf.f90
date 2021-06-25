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

module pw_QP_states_interf
implicit none

private
public :: pw_QP_states_init
public :: pw_QP_states_destroy
public :: pw_QP_states_is_local
public :: pw_QP_states_borrow_basis
public :: pw_QP_states_giveback_basis
public :: pw_QP_states_borrow_wfc
public :: pw_QP_states_giveback_wfc
public :: pw_QP_states_get_cutoff


interface pw_QP_states_init
subroutine pw_QP_states_init_x(QP_states,states,QP,root,comm)
  use pw_QP_states_type
  use pw_QP_module
  use pw_states_type
  use pw_struct_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_states),        intent(out) :: QP_states
  type (pw_QP),               intent(in)  :: QP
  type (pw_states), target,   intent(in)  :: states
  integer,                    intent(in)  :: root
  type (ptk_comm),            intent(in)  :: comm
end subroutine pw_QP_states_init_x
end interface

interface pw_QP_states_destroy
subroutine pw_QP_states_destroy_x(QP_states)
  use pw_states_type
  implicit none
  type (pw_states), intent(inout) :: QP_states
end subroutine pw_QP_states_destroy_x
end interface

interface pw_QP_states_is_local
function pw_QP_states_is_local_x(QP_states,ib,ik)
  use pw_QP_states_type
  implicit none
  logical :: pw_QP_states_is_local_x
  type (pw_QP_states), intent(in) :: QP_states
  integer, intent(in) :: ib,ik
end function pw_QP_states_is_local_x
end interface

interface pw_QP_states_borrow_basis
subroutine pw_QP_states_borrow_basis_x(QP_states,basis,ik)
  use pw_QP_states_type
  use pw_basis_module
  implicit none
  type(pw_QP_states),                   intent(in)  :: QP_states
  type(pw_basis), pointer, optional :: basis
  integer,                 optional, intent(in)  :: ik
end subroutine pw_QP_states_borrow_basis_x
end interface

interface pw_QP_states_giveback_basis
subroutine pw_QP_states_giveback_basis_x(QP_states,basis)
  use pw_QP_states_type
  use pw_basis_module
  implicit none
  type(pw_QP_states),         intent(in)    :: QP_states
  type(pw_basis), pointer :: basis
end subroutine pw_QP_states_giveback_basis_x
end interface

interface pw_QP_states_borrow_wfc
subroutine pw_QP_states_borrow_wfc_x(QP_states,wfc,ib,ik)
  use pw_QP_states_type
  use pw_wfc_module
  implicit none
  type(pw_QP_states),       intent(in)            :: QP_states
  type(pw_wfc), pointer, optional :: wfc
  integer,               optional, intent(in)  :: ib,ik
end subroutine pw_QP_states_borrow_wfc_x
end interface

interface pw_QP_states_giveback_wfc
subroutine pw_QP_states_giveback_wfc_x(QP_states,wfc)
  use pw_QP_states_type
  use pw_wfc_module
  implicit none
  type(pw_QP_states), intent(in) :: QP_states  
  type(pw_wfc), pointer, optional :: wfc
end subroutine pw_QP_states_giveback_wfc_x
end interface

interface pw_QP_states_get_cutoff
function pw_QP_states_get_cutoff_x(states)
  use pw_QP_states_type
  implicit none
  real :: pw_QP_states_get_cutoff_x
  type(pw_QP_states), intent(in) :: states 
end function pw_QP_states_get_cutoff_x
end interface
end module pw_QP_states_interf
