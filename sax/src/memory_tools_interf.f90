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

#include "tools_error.h"

module memory_tools_interf
implicit none

private

public :: memory_matrix

interface memory_matrix
  subroutine memory_1D_int_x(matrix,dim,mem)
    integer, intent(in) :: matrix
    integer, intent(in) :: dim
    real, intent(out) :: mem
    integer :: a
  end subroutine memory_1D_int_x
  
  subroutine memory_1D_real_x(matrix,dim,mem)
    real, intent(in) :: matrix
    integer, intent(in) :: dim
    real, intent(out) :: mem
    real :: a
  end subroutine memory_1D_real_x

  subroutine memory_1D_complex_x(matrix,dim,mem)
    complex, intent(in) :: matrix
    integer, intent(in) :: dim
    real, intent(out) :: mem
    complex :: a
  end subroutine memory_1D_complex_x

  subroutine memory_2D_int_x(matrix,dim1,dim2,mem)
    integer, intent(in) :: matrix
    integer, intent(in) :: dim1, dim2
    real, intent(out) :: mem
    integer :: a
  end subroutine memory_2D_int_x
                   
  subroutine memory_2D_real_x(matrix,dim1,dim2,mem)
    real, intent(in) :: matrix
    integer, intent(in) :: dim1, dim2
    real, intent(out) :: mem
    real :: a
  end subroutine memory_2D_real_x

  subroutine memory_2D_complex_x(matrix,dim1,dim2,mem)
    complex, intent(in) :: matrix
    integer, intent(in) :: dim1, dim2
    real, intent(out) :: mem
    complex :: a
  end subroutine memory_2D_complex_x

  subroutine memory_3D_int_x(matrix,dim1,dim2,dim3,mem)
     integer, intent(in) :: matrix
     integer, intent(in) :: dim1, dim2, dim3
     real, intent(out) :: mem
     integer :: a
  end subroutine memory_3D_int_x
                   
  subroutine memory_3D_real_x(matrix,dim1,dim2,dim3,mem)
    real, intent(in) :: matrix
    integer, intent(in) :: dim1, dim2, dim3
    real, intent(out) :: mem
    real :: a
  end subroutine memory_3D_real_x

  subroutine memory_3D_complex_x(matrix,dim1,dim2,dim3,mem)
    complex, intent(in) :: matrix
    integer, intent(in) :: dim1, dim2, dim3
    real, intent(out) :: mem
    complex :: a
  end subroutine memory_3D_complex_x

end interface memory_matrix
end module memory_tools_interf            
