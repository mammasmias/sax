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

module pw_states_interf
implicit none

private
public :: pw_states_init
public :: pw_states_init_basis_wfc
public :: pw_states_destroy
public :: pw_states_destroy_basis_wfc
public :: pw_states_get_cutoff
public :: pw_states_is_local
public :: pw_trans_is_local
public :: pw_states_read
public :: pw_states_write
public :: pw_states_convert_from_newp
public :: pw_states_convert_from_pw104
public :: pw_states_occupation
public :: pw_states_time_reversal
public :: pw_states_borrow_basis
public :: pw_states_giveback_basis
public :: pw_states_borrow_wfc
public :: pw_states_giveback_wfc
public :: pw_states_density
public :: pw_states_calc_expectation
public :: pw_states_calc_sp_hmatrix


interface pw_states_init
subroutine pw_states_init_x(states,struct,symmlist,kmesh,nbmin,nbmax,root,comm,do_not_alloc,superparall)
  use pw_states_type
  use pw_smearing_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_states),           intent(out) :: states
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: kmesh
  integer,                    intent(in)  :: nbmin,nbmax,root
  type (ptk_comm),            intent(in)  :: comm
  logical, optional,          intent(in)  :: do_not_alloc
  logical, optional,          intent(in)  :: superparall
end subroutine pw_states_init_x

subroutine pw_states_init_smearing_x(states,smearing,struct,symmlist,kmesh,nbmin,nbmax,root,comm)
  use pw_states_type
  use pw_smearing_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_states),           intent(out) :: states
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: kmesh
  integer,                    intent(in)  :: nbmin,nbmax,root
  type (ptk_comm),            intent(in)  :: comm
  type (pw_smearing), target, intent(in)  :: smearing
end subroutine pw_states_init_smearing_x

end interface

interface pw_states_init_basis_wfc
subroutine pw_states_init_basis_wfc_x(states,struct,symmlist,kmesh,nbmin,nbmax,root,comm)
  use pw_states_type
  use pw_smearing_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_states),           intent(out) :: states
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: kmesh
  integer,                    intent(in)  :: nbmin,nbmax,root
  type (ptk_comm),            intent(in)  :: comm
end subroutine pw_states_init_basis_wfc_x
end interface


interface pw_states_destroy
subroutine pw_states_destroy_x(states)
  use pw_states_type
  implicit none
  type (pw_states), intent(inout) :: states
end subroutine pw_states_destroy_x
end interface

interface pw_states_destroy_basis_wfc
subroutine pw_states_destroy_basis_wfc_x(states)
  use pw_states_type
  implicit none
  type (pw_states), intent(inout) :: states
end subroutine pw_states_destroy_basis_wfc_x
end interface

interface pw_states_get_cutoff
function pw_states_get_cutoff_x(states)
  use pw_states_type
  implicit none
  real :: pw_states_get_cutoff_x
  type (pw_states), intent(in) :: states
end function pw_states_get_cutoff_x
end interface 

interface pw_states_is_local
function pw_states_is_local_x(states,ib,ik)
  use pw_states_type
  implicit none
  logical :: pw_states_is_local_x
  type (pw_states), intent(in) :: states
  integer, intent(in) :: ib,ik
end function pw_states_is_local_x
end interface

interface pw_trans_is_local
function pw_trans_is_local_x(trans,ib,jb,rank)
  implicit none
  logical :: pw_trans_is_local_x
  integer, pointer, intent(in) :: trans(:,:)
  integer, intent(in) :: ib,jb,rank
end function pw_trans_is_local_x
end interface

interface pw_states_read
subroutine pw_states_read_x(states,unit,name)
  use pw_states_type
  implicit none
  type (pw_states), intent(inout) :: states
  integer,          intent(in)    :: unit
  character(*),     intent(in)    :: name
end subroutine pw_states_read_x
end interface

interface pw_states_write
subroutine pw_states_write_x(states,unit,name)
  use pw_states_type
  implicit none
  type (pw_states), intent(in) :: states
  integer,          intent(in) :: unit
  character(*),     intent(in) :: name
end subroutine pw_states_write_x
end interface

interface pw_states_convert_from_newp
subroutine pw_states_convert_from_newp_x(states,file,unit,nelec,name)
  use pw_states_type
  use pw_smearing_module
  implicit none
  type (pw_states), intent(in) :: states ! serve per dedurre i vari parametri (nbande, mesh, etc)
  character(len=*), intent(in) :: file   ! punch file
  integer,          intent(in) :: unit   ! unita' di scrittura
  real,             intent(in) :: nelec
  character(len=*), intent(in) :: name
end subroutine pw_states_convert_from_newp_x
end interface
    
! NOTA:
! fatta sulla falsariga di pw_states_convert_from_newp, ma non ancora debuggata, OCCHIO!
interface pw_states_convert_from_pw104
subroutine pw_states_convert_from_pw104_x(states,unit,nelec,name)
  use pw_states_type
  use pw_smearing_module
  implicit none
  type (pw_states), intent(in) :: states ! serve per dedurre i vari parametri (nbande, mesh, etc)
  integer,          intent(in) :: unit
  real,             intent(in) :: nelec
  character(len=*), intent(in) :: name
end subroutine pw_states_convert_from_pw104_x
end interface

interface pw_states_occupation
subroutine pw_states_occupation_x(occupation,weights,eband,efermi,nelec,smearing)
  use pw_smearing_module
  implicit none
  real, intent(out) :: occupation(:,:)
  real, intent(in)  :: eband(:,:),weights(:,:)
  real, intent(in)  :: nelec
  real, intent(out) :: efermi
  type(pw_smearing), intent(in) :: smearing
end subroutine pw_states_occupation_x
end interface

interface pw_states_time_reversal
subroutine pw_states_time_reversal_x(states)
  use pw_states_type
  implicit none
  type(pw_states), intent(inout) :: states
end subroutine pw_states_time_reversal_x
end interface

interface pw_states_borrow_basis
subroutine pw_states_borrow_basis_x(states,basis,ik)
  use pw_states_type
  use pw_basis_module
  implicit none
  type(pw_states),                   intent(in)  :: states
  type(pw_basis), pointer, optional :: basis
  integer,                 optional, intent(in)  :: ik
end subroutine pw_states_borrow_basis_x
end interface

interface pw_states_giveback_basis
subroutine pw_states_giveback_basis_x(states,basis)
  use pw_states_type
  use pw_basis_module
  implicit none
  type(pw_states),         intent(in)    :: states
  type(pw_basis), pointer :: basis
end subroutine pw_states_giveback_basis_x
end interface

interface pw_states_borrow_wfc
subroutine pw_states_borrow_wfc_x(states,wfc,ib,ik)
  use pw_states_type
  use pw_wfc_module
  implicit none
  type(pw_states),       intent(in)            :: states
  type(pw_wfc), pointer, optional :: wfc
  integer,               optional, intent(in)  :: ib,ik
end subroutine pw_states_borrow_wfc_x
end interface

interface pw_states_giveback_wfc
subroutine pw_states_giveback_wfc_x(states,wfc)
  use pw_states_type
  use pw_wfc_module
  implicit none
  type(pw_states), intent(in) :: states  
  type(pw_wfc), pointer, optional :: wfc
end subroutine pw_states_giveback_wfc_x
end interface

interface pw_states_density
subroutine pw_states_density_x(states,density)
  use pw_states_type
  use pw_density_module
  implicit none
  type (pw_states),  intent(in)    :: states
  type (pw_density), intent(inout) :: density
end subroutine pw_states_density_x
end interface

interface pw_states_calc_expectation
subroutine pw_states_calc_expect_x(states,nbmin,nbmax,diagonal,atoms,density)
  use pw_states_type
  use pw_density_module
  use pw_atoms_module
  implicit none
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  logical, intent(in) :: diagonal
  type(pw_atoms),  intent(in)    :: atoms
  type(pw_density),intent(in)    :: density
end subroutine pw_states_calc_expect_x
end interface

interface pw_states_calc_sp_hmatrix
subroutine pw_states_calc_sp_hmatrix_x(system_type,states,nbmin,nbmax,diagonal,atoms,density,outdir)
  use pw_states_type
  use pw_density_module
  use pw_atoms_module
  implicit none
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  logical, intent(in) :: diagonal
  type(pw_atoms), intent(in) :: atoms
  type(pw_density), intent(in) :: density
  character(*), intent(in) :: outdir
  integer, intent(in) :: system_type
end subroutine pw_states_calc_sp_hmatrix_x
end interface

interface pw_states_calc_expect_sp
subroutine pw_states_calc_expect_sp_x(exp_sp,nbmin,nbmax,states,atoms,density)
  use pw_states_type
  use pw_QP_module
  use pw_density_module
  use pw_atoms_module

  implicit none
 
  type(pw_QP), intent(out) :: exp_sp
  integer, intent(in) :: nbmin,nbmax
  type(pw_states), intent(in) :: states
  type(pw_atoms), intent(in) :: atoms
  type(pw_density), intent(in) :: density
end subroutine pw_states_calc_expect_sp_x
end interface

interface pw_states_calc_expect_hartree
subroutine pw_states_calc_expect_hartree_x(exp_hartree,nbmin,nbmax,states,atoms,density)
  use pw_states_type
  use pw_QP_module
  use pw_density_module
  use pw_atoms_module

  implicit none

  type(pw_QP), intent(out) :: exp_hartree
  integer, intent(in) :: nbmin,nbmax
  type(pw_states), intent(in) :: states
  type(pw_atoms), intent(in) :: atoms
  type(pw_density), intent(in) :: density
  
end subroutine pw_states_calc_expect_hartree_x
end interface

interface pw_states_calc_expect_vxc
subroutine pw_states_calc_expect_vxc_x(exp_vxc,nbmin,nbmax,states,atoms,density)
  use pw_states_type
  use pw_QP_module
  use pw_density_module
  use pw_atoms_module

  implicit none

  type(pw_QP), intent(out) :: exp_vxc
  integer, intent(in) :: nbmin, nbmax
  type(pw_states), intent(in) :: states
  type(pw_atoms), intent(in) :: atoms
  type(pw_density), intent(in) :: density

end subroutine pw_states_calc_expect_vxc_x
end interface

end module pw_states_interf
