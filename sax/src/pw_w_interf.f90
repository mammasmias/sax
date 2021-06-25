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
module pw_w_interf
implicit none
private
public :: pw_w_init
public :: pw_w_destroy
public :: pw_w_calc_0
public :: pw_w_calc_3
public :: pw_w_write
public :: pw_w_read
public :: pw_w_smooth_3
public :: pw_w_cutoff_3
public :: pw_w_borrow_basis
public :: pw_w_giveback_basis
public :: pw_w_borrow_wfc6d
public :: pw_w_giveback_wfc6d
!@ END MANUAL

!@ MANUAL
interface pw_w_init
subroutine pw_w_init_x(w,struct,symmlist,qmesh,omegamax,nomega,cutoff,gw_integration_method,root,comm,do_not_alloc)
  use pw_w_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_w),   intent(out) :: w
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
!  real,                       intent(in)  :: cutoff,omegamax
  real,                       intent(in)  :: cutoff
  complex,                       intent(in)  :: omegamax
  logical,          optional, intent(in)  :: do_not_alloc
  character(len=*),           intent(in)  :: gw_integration_method
  integer,                    intent(in)  :: root,nomega
  type (ptk_comm),            intent(in)  :: comm
! Costruttore. Nota che e' parallelo, dunque tutti i processori appartenenti
! a comm devono chiamarlo contemporaneamente con gli stessi parametri.
!@ END MANUAL
end subroutine pw_w_init_x
subroutine pw_w_init_tail_x(w,struct,symmlist,qmesh,iomega_tail,cutoff,root,comm,do_not_alloc)
  use pw_w_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_w),   intent(out) :: w
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
  integer,                    intent(in)  :: iomega_tail
  real,                       intent(in)  :: cutoff
  logical,          optional, intent(in)  :: do_not_alloc
  integer,                    intent(in)  :: root
  type (ptk_comm),            intent(in)  :: comm
! Costruttore. Nota che e' parallelo, dunque tutti i processori appartenenti
! a comm devono chiamarlo contemporaneamente con gli stessi parametri.
!@ END MANUAL
end subroutine pw_w_init_tail_x
end interface

interface pw_w_borrow_basis
subroutine pw_w_borrow_basis_x(w,basis,iq)
  use pw_w_type
  use pw_basis_module
  implicit none
  type(pw_w),                        intent(in)  :: w
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
end subroutine pw_w_borrow_basis_x
end interface

interface pw_w_giveback_basis
subroutine pw_w_giveback_basis_x(w,basis)
  use pw_w_type
  use pw_basis_module
  implicit none
  type(pw_w),              intent(in)    :: w
  type(pw_basis), pointer :: basis
end  subroutine pw_w_giveback_basis_x
end interface

interface pw_w_write
subroutine pw_w_write_x(w,unit,name)
  use pw_w_type
  implicit none
  type(pw_w),  intent(in) :: w
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
end subroutine pw_w_write_x
end interface

interface pw_w_destroy
subroutine pw_w_destroy_x(w)
  use pw_w_type
  implicit none
  type (pw_w), intent(inout) :: w
end subroutine pw_w_destroy_x
end interface

interface pw_w_calc_0
subroutine pw_w_calc_x0(w,polar,coulomb_div_treatment,ecutvcut,dealloc_polar)
  use pw_w_type
  use pw_polar_module
  implicit none
  type (pw_w), intent(inout) :: w
  type (pw_polar), intent(inout) :: polar
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
  logical, optional, intent(in) :: dealloc_polar
end subroutine pw_w_calc_x0
end interface

interface pw_w_calc_3
subroutine pw_w_calc_x3(w,polar,dealloc_polar)
  use pw_w_type
  use pw_polar_module
  implicit none
  type (pw_w), intent(inout) :: w
  type (pw_polar), intent(inout) :: polar
  logical, optional, intent(in) :: dealloc_polar
end subroutine pw_w_calc_x3
end interface

interface pw_w_smooth_3
subroutine pw_w_smooth_x3(w) ! Elimina tutte le discontinuita'
  use pw_w_type
  implicit none
  type (pw_w), intent(inout) :: w
end subroutine pw_w_smooth_x3
end interface

interface pw_w_cutoff_3
subroutine pw_w_cutoff_x3(w,coulomb_div_treatment,ecutvcut) ! Symm e cutoff
  use pw_w_type
  implicit none
  type (pw_w), intent(inout) :: w
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
end subroutine pw_w_cutoff_x3
end interface


interface pw_w_borrow_wfc6d
subroutine pw_w_borrow_wfc6d_x(w,wfc6d,ik,iomega)
  use pw_w_type
  use pw_wfc6d_module
  implicit none
  type(pw_w),                        intent(in)  :: w
  type(pw_wfc6d), pointer, optional :: wfc6d
  integer,                 optional, intent(in)  :: ik
  integer,                 optional, intent(in)  :: iomega
end subroutine pw_w_borrow_wfc6d_x
end interface

interface pw_w_giveback_wfc6d
subroutine pw_w_giveback_wfc6d_x(w,wfc6d)
  use pw_w_type
  use pw_wfc6d_module
  implicit none
  type (pw_w),              intent(in)    :: w
  type (pw_wfc6d), pointer :: wfc6d
end subroutine pw_w_giveback_wfc6d_x
end interface

interface pw_w_read
subroutine pw_w_read_x(w,unit,name)
  use pw_w_type
  implicit none
  type(pw_w),  intent(inout) :: w
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
end subroutine pw_w_read_x

subroutine pw_w_read_tail_x(w,iomega_tail,unit,name)
  use pw_w_type
  implicit none
  type(pw_w),  intent(inout) :: w
  integer, intent(in) :: iomega_tail
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
end subroutine pw_w_read_tail_x
end interface pw_w_read
end module pw_w_interf
