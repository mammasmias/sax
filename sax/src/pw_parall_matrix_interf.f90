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

module pw_parall_matrix_interf
implicit none

private
public :: pw_parall_matrix_init, pw_parall_matrix_destroy
public :: pw_parall_matrix_write, pw_parall_matrix_read
public :: pw_parall_matrix_distr, pw_parall_matrix_collect
public :: pw_parall_matrix_borrow_line, pw_parall_matrix_giveback_line

interface pw_parall_matrix_init
subroutine pw_parall_complex_m_init_x(matrix,imin,imax,root,comm)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_complex_matrix
  type (pw_parall_complex_matrix), intent(out) :: matrix
  integer, intent(in) :: imin,imax
  integer, intent(in) :: root
  type(ptk_comm),intent(in) :: comm
end subroutine pw_parall_complex_m_init_x

subroutine pw_parall_real_m_init_x(matrix,imin,imax,root,comm)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_real_matrix
  type (pw_parall_real_matrix), intent(out) :: matrix
  integer, intent(in) :: imin, imax
  integer, intent(in) :: root
  type(ptk_comm), intent(in) :: comm
end subroutine pw_parall_real_m_init_x
end interface

interface pw_parall_matrix_destroy
subroutine pw_parall_complex_m_destroy_x(matrix)
  use pw_parall_matrix_type, only : pw_parall_complex_matrix

  type(pw_parall_complex_matrix), intent(inout) :: matrix
end subroutine pw_parall_complex_m_destroy_x

subroutine pw_parall_real_m_destroy_x(matrix)
  use pw_parall_matrix_type, only : pw_parall_real_matrix

  type(pw_parall_real_matrix), intent(inout) :: matrix
end subroutine pw_parall_real_m_destroy_x 
end interface

interface pw_parall_matrix_write
subroutine pw_parall_complex_m_write_x(matrix,unit,name)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_complex_matrix
  type (pw_parall_complex_matrix), intent(in) :: matrix
  integer, intent(in) :: unit
  character(*), intent(in) :: name
end subroutine pw_parall_complex_m_write_x

subroutine pw_parall_real_m_write_x(matrix,unit,name)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_real_matrix
  type (pw_parall_real_matrix), intent(in) :: matrix
  integer, intent(in) :: unit
  character(*), intent(in) :: name
end subroutine pw_parall_real_m_write_x
end interface

interface pw_parall_matrix_read
subroutine pw_parall_complex_m_read_x(matrix,unit,name)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_complex_matrix
  type (pw_parall_complex_matrix), intent(inout) :: matrix
  integer, intent(in) :: unit
  character(*), intent(in) :: name
end subroutine pw_parall_complex_m_read_x

subroutine pw_parall_real_m_read_x(matrix,unit,name)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_real_matrix
  type (pw_parall_real_matrix), intent(inout) :: matrix
  integer, intent(in) :: unit
  character(*), intent(in) :: name
end subroutine pw_parall_real_m_read_x
end interface

interface pw_parall_matrix_distr
subroutine pw_parall_complex_m_distr_x(fullmatrix,matrix,root,comm)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_complex_matrix
  type (pw_parall_complex_matrix), intent(inout) :: matrix
  complex, intent(in) :: fullmatrix(matrix%imin:matrix%imax,matrix%imin:matrix%imax)
  integer, intent(in) :: root
  type(ptk_comm),intent(in) :: comm
end subroutine pw_parall_complex_m_distr_x

subroutine pw_parall_real_m_distr_x(fullmatrix,matrix,root,comm)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_real_matrix
  type (pw_parall_real_matrix), intent(inout) :: matrix
  real, intent(in) :: fullmatrix(matrix%imin:matrix%imax,matrix%imin:matrix%imax)
  integer, intent(in) :: root
  type(ptk_comm), intent(in) :: comm
end subroutine pw_parall_real_m_distr_x
end interface

interface pw_parall_matrix_collect
subroutine pw_parall_complex_m_collect_x(fullmatrix,matrix,root,comm)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_complex_matrix
  type (pw_parall_complex_matrix), intent(in) :: matrix
  complex, intent(inout) :: fullmatrix(matrix%imin:matrix%imax,matrix%imin:matrix%imax)
  integer, intent(in) :: root
  type(ptk_comm),intent(in) :: comm
end subroutine pw_parall_complex_m_collect_x

subroutine pw_parall_real_m_collect_x(fullmatrix,matrix,root,comm)
  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
  use pw_parall_matrix_type, only : pw_parall_real_matrix
  type (pw_parall_real_matrix), intent(in) :: matrix
  real, intent(inout) :: fullmatrix(matrix%imin:matrix%imax,matrix%imin:matrix%imax)
  integer, intent(in) :: root
  type(ptk_comm), intent(in) :: comm
end subroutine pw_parall_real_m_collect_x
end interface

interface pw_parall_matrix_borrow_line
subroutine pw_parall_complex_m_borrow_line_x(matrix,line,il)
use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm, &
  ptk_allgather, ptk_barrier
use pw_parall_matrix_type, only : pw_parall_complex_matrix
implicit none

type(pw_parall_complex_matrix),intent(in) :: matrix
complex, pointer, optional :: line
integer, optional, intent(in) :: il
end subroutine pw_parall_complex_m_borrow_line_x
end interface

interface pw_parall_matrix_giveback_line
subroutine pw_parall_complex_m_giveback_line_x(matrix,line)
use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm, &
  ptk_allgather, ptk_barrier
use pw_parall_matrix_type, only : pw_parall_complex_matrix
implicit none

type(pw_parall_complex_matrix),intent(in) :: matrix
complex, pointer, optional :: line
end subroutine pw_parall_complex_m_giveback_line_x
end interface

end module pw_parall_matrix_interf
