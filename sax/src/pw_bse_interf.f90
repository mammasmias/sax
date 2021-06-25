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
module pw_bse_interf
implicit none
private
public :: pw_bse_init
public :: pw_bse_destroy
public :: pw_bse_write
public :: pw_bse_readbcast
public :: pw_bse_macro3
public :: pw_bse_macro0
public :: pw_bse_calc3
public :: pw_bse_calc0
public :: pw_bse_x_calc
public :: pw_bse_transpose
!@ END MANUAL

interface pw_bse_init
subroutine pw_bse_init_x(bse,states,atoms,emin,emax,spin,root,comm,find_trans)
  use pw_states_type
  use pw_bse_type
  use pw_atoms_module
  use ptk_module, only : ptk_comm, ptk_comm_size, ptk_comm_rank
  implicit none
  type (pw_bse), intent(out) :: bse
  type (pw_states), target, intent(in) :: states
  type(pw_atoms), target, intent(in) :: atoms
  real,          intent(in)  :: emin,emax
  integer, intent(in) :: spin 
  integer, intent(in) :: root
  type(ptk_comm), intent(in) :: comm
  logical, optional, intent(in) :: find_trans
end subroutine pw_bse_init_x
end interface

interface pw_bse_destroy
subroutine pw_bse_destroy_x(bse)
  use pw_bse_type
  implicit none
  type (pw_bse), intent(inout) :: bse
end subroutine pw_bse_destroy_x
end interface

interface pw_bse_write
subroutine pw_bse_write_x(bse,unit,name)
  use pw_bse_type
  implicit none
  type (pw_bse), intent(in) :: bse
  integer,       intent(in) :: unit
  character(*),  intent(in) :: name
end subroutine pw_bse_write_x
end interface

interface pw_bse_readbcast
subroutine pw_bse_readbcast_x(bse,unit,name,root,comm,not_allocated)
  use pw_bse_type
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_bse), intent(inout) :: bse
  type (ptk_comm), intent(in) :: comm
  integer, intent(in) :: root
  integer,       intent(in)    :: unit
  character(*),  intent(in)    :: name
  logical, optional, intent(in) :: not_allocated
end subroutine pw_bse_readbcast_x
end interface

interface pw_bse_macro3
subroutine pw_bse_macro_x3(bse,epsilon,energy_shift,qmesh,cutoff1,cutoff2, &
	coulomb_div_treatment,ecutvcut)
  use pw_bse_type
  use pw_w_type
  use pw_epsilon_macro_module
  use pw_kmesh_module
  implicit none
  type(pw_bse), intent(inout) :: bse
  type(pw_epsilon_macro), intent(in) :: epsilon
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff1, cutoff2
  real, intent(in) :: energy_shift
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
end subroutine pw_bse_macro_x3
end interface

interface pw_bse_macro0
subroutine pw_bse_macro_x0(bse,epsilon,energy_shift,qmesh,cutoff1,cutoff2)
  use pw_bse_type
  use pw_w_type
  use pw_epsilon_macro_module
  use pw_kmesh_module
  implicit none
  type(pw_bse), intent(inout) :: bse
  type(pw_epsilon_macro), intent(in) :: epsilon
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff1, cutoff2
  real, intent(in) :: energy_shift
end subroutine pw_bse_macro_x0
end interface


interface pw_bse_calc3
subroutine pw_bse_calc_x3(bse,w,energy_shift,cutoff,coulomb_div_treatment,ecutvcut)
  use pw_bse_type
  use pw_w_type
  implicit none
  type(pw_bse), intent(inout) :: bse
  type(pw_w), intent(in) :: w
  real, intent(in) :: energy_shift
  real, intent(in) :: cutoff
  character(len=*), optional :: coulomb_div_treatment
  real, optional  :: ecutvcut
end subroutine pw_bse_calc_x3
end interface

interface pw_bse_calc0
subroutine pw_bse_calc_x0(bse,w,energy_shift,cutoff)
  use pw_bse_type
  use pw_w_type
  implicit none
  type(pw_bse), intent(inout) :: bse
  type(pw_w), intent(in) :: w
  real, intent(in) :: energy_shift
  real, intent(in) :: cutoff
end subroutine pw_bse_calc_x0
end interface  

interface pw_bse_x_calc
subroutine pw_bse_x_calc_x(bse,qmesh,cutoff,coulomb_div_treatment,ecutvcut)
  use pw_bse_type
  use pw_kmesh_module
  implicit none
  type(pw_bse), intent(inout) :: bse
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
end subroutine pw_bse_x_calc_x
end interface

interface pw_bse_transpose
subroutine pw_bse_transpose_x(bse,root,comm)
  use pw_bse_type
  use num_la_parall_module, only : num_parall_c_transpose
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_bse),   intent(inout) :: bse
  integer, intent(in) :: root
  type (ptk_comm), intent(in)    :: comm
end subroutine pw_bse_transpose_x
end interface

end module pw_bse_interf
