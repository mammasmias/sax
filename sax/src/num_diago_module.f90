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

module num_diago_module
use lasi_module, only : lasi_HEEV , lasi_HEGV, lasi_GESV
implicit none
private

public :: num_HEEV,num_HEGV
public :: num_inverse_real_matrix, num_inverse_complex_matrix
contains

subroutine num_HEEV(jobz,N,A,W)
!
integer :: N
integer :: LWORK, INFO
real, dimension(1:N), intent(inout) :: W
real, allocatable :: RWORK(:)
complex, allocatable :: WORK(:)
character*1 :: jobz

complex, dimension(1:N,1:N), intent(inout) :: A


W(:)=0.0
LWORK=2*N-1
allocate(WORK(LWORK))
WORK(:)=(0.0,0.0)
allocate(RWORK(3*N-2))
RWORK(:)=0.0

call lasi_HEEV(jobz,'L',N,A, &
N,W,WORK,LWORK,RWORK,INFO)

if(INFO.ne.0) ERROR("Diagonalization return an error code") 

deallocate(WORK)
deallocate(RWORK)
end subroutine num_HEEV

subroutine num_HEGV(jobz,N,A,B,W)
!
integer :: N
integer :: LWORK, INFO
real, dimension(1:N), intent(inout) :: W
real, allocatable :: RWORK(:)
complex, allocatable :: WORK(:)
character*1 :: jobz

complex, dimension(1:N,1:N), intent(inout) :: A, B


W(:)=0.0
LWORK=2*N-1
allocate(WORK(LWORK))
WORK(:)=(0.0,0.0)
allocate(RWORK(3*N-2))
RWORK(:)=0.0

call lasi_HEGV(1,jobz,'L',N,A,N,B, &
     N,W,WORK,LWORK,RWORK,INFO)

if(INFO/=0) ERROR("diagonalization return an error code") 

deallocate(WORK)
deallocate(RWORK)
end subroutine num_HEGV

subroutine num_inverse_real_matrix(N,A,B)

integer, intent(in) :: N
real, dimension(1:N,1:N), intent(inout) :: A 
real, dimension(1:N,1:N), intent(inout) :: B

integer :: NRHS, LDA, LDB
integer :: INFO
integer, dimension(1:N) :: IPIV

NRHS=N
LDA=N
LDB=N

call lasi_GESV(N,NRHS,A,LDA,IPIV,B,LDB,INFO)

if(INFO/=0) ERROR("inversion return an error code")

end subroutine num_inverse_real_matrix

subroutine num_inverse_complex_matrix(N,A,B)

integer, intent(in) :: N
complex, dimension(1:N,1:N), intent(inout) :: A
complex, dimension(1:N,1:N), intent(inout) :: B

integer :: NRHS, LDA, LDB
integer :: INFO
integer, dimension(1:N) :: IPIV

NRHS=N
LDA=N
LDB=N

call lasi_GESV(N,NRHS,A,LDA,IPIV,B,LDB,INFO)

if(INFO/=0) ERROR("inversion return an error code")

end subroutine num_inverse_complex_matrix


end module num_diago_module 
