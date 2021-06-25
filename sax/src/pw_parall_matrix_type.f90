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

module pw_parall_matrix_type
use ptk_module, only : ptk_comm

implicit none
private
public :: pw_parall_complex_matrix
public :: pw_parall_real_matrix

type pw_parall_complex_matrix
  integer, pointer :: where_line(:,:)
  complex, pointer :: val(:,:)
  real, pointer :: eigenval(:)
  integer :: npe, rank
  integer :: root
  type(ptk_comm) :: comm
  integer :: m_dim_locmax
  integer :: m_dim_loc
  integer :: m_dim
  integer :: imin,imax
  logical :: incomplete
end type pw_parall_complex_matrix

type pw_parall_real_matrix
  integer, pointer :: where_line(:,:)
  real, pointer :: val(:,:)
  real, pointer :: eigenval(:)
  integer :: npe, rank
  integer :: root
  type(ptk_comm) :: comm
  integer :: m_dim_locmax
  integer :: m_dim_loc
  integer :: m_dim
  integer :: imin,imax
  logical :: incomplete
end type pw_parall_real_matrix

end module pw_parall_matrix_type
