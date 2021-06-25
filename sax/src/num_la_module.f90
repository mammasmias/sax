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
!@ MANUAL
module num_la_module
implicit none
! This module contains linear algebra subroutines
! Where it is possible, this are simple wrappers to BLAS or system routines
private
public :: num_simpson, &
          num_nrm2,   &
          num_vemul,  &
          num_dotp, &
          num_vea,    &
          num_gerc,   &
          num_he_inv, &
          num_ge_inv, &
          num_gemv,   &
          num_triple_dot, &
          num_trace,  &
          num_power,  &
          num_c2r, &
          num_r2c, &
          num_matmul, &
          num_determinant, &
          num_inverse, &
          num_matpower, &
          num_he_diag, &
          num_eigenvalues, &
          num_sphaverage, &
          num_spectral_range, &
          num_set_aimag_to_positive
!@ END MANUAL

interface num_trace
  module procedure num_trace_r
  module procedure num_trace_c
end interface

interface num_matmul
  module procedure num_matmul_1_2_r
  module procedure num_matmul_1_2_c
  module procedure num_matmul_1_2_i
  module procedure num_matmul_2_1_r
  module procedure num_matmul_2_1_c
  module procedure num_matmul_2_1_i
  module procedure num_matmul_2_2_r
  module procedure num_matmul_2_2_c
  module procedure num_matmul_2_2_i
end interface

interface num_determinant
  module procedure num_determinant_r
  module procedure num_determinant_i
end interface

contains

function num_sphaverage(a) result(res)
  complex, intent(in) :: a(3,3)
  complex             :: res(3,3)
  complex :: val
  val = (a(1,1)+a(2,2)+a(3,3))/3.0
  res = 0.0
  res(1,1) = val
  res(2,2) = val
  res(3,3) = val
end function num_sphaverage

function num_matpower(a,power) result(matpower)
  use lasi_module, only: lasi_syev
  real, intent(in) :: a(3,3)
  real, intent(in) :: power
  real             :: matpower(3,3)
  real :: ax(3,3),w(3),aw(3,3)
  integer :: info
  real    :: work(8)
  ax = a
  call lasi_syev("V","U",3, ax, 3, W, WORK, 8, INFO )
  if(any(w<0.0)) ERROR("")
  w = exp( power * log(w))
  aw = 0.0
  aw(1,1) = w(1)
  aw(2,2) = w(2)
  aw(3,3) = w(3)
  matpower = num_matmul(ax,num_matmul(aw,transpose(ax)))
end function num_matpower

function num_eigenvalues(a)
  use lasi_module, only : lasi_syev
  real, intent(in) :: a(3,3)
  real             :: num_eigenvalues(3)
  real :: ax(3,3),w(3)
  integer :: info
  real    :: work(8)
  ax = a
  call lasi_syev("V","U",3, ax, 3, W, WORK, 8, INFO )
  num_eigenvalues = w
end function num_eigenvalues


function num_inverse(a) result(inv)
  real              :: inv(0:2,0:2)
  real, intent(in)  :: a(0:2,0:2)
  real :: tmp(0:2,0:2)
  real :: det
  integer i,j
  do i=0,2
    do j=0,2
      tmp(i,j) = a(modulo(i+1,3),modulo(j+1,3)) * a(modulo(i+2,3),modulo(j+2,3)) &
  &            - a(modulo(i+1,3),modulo(j+2,3)) * a(modulo(i+2,3),modulo(j+1,3))
    end do
  end do
  det = num_determinant(a)
  inv = transpose(tmp) / det
  if(sum((num_matmul(inv,a)-reshape((/1,0,0,0,1,0,0,0,1/),(/3,3/)))**2) > 1d-5) then
    write(0,*) "AHIA",sum((num_matmul(inv,a)-reshape((/1,0,0,0,1,0,0,0,1/),(/3,3/))**2))
    write(0,*) "A",a
    write(0,*) "inv",inv
    write(0,*)">>", num_matmul(inv,a)
    ERROR("")
  end if
end function num_inverse

function num_determinant_r(a) result(det)
  real, intent(in) :: a(3,3)
  real             :: det
  det = a(1,1)*a(2,2)*a(3,3) + a(1,2)*a(2,3)*a(3,1) + a(1,3)*a(2,1)*a(3,2) &
    - a(1,1)*a(2,3)*a(3,2) - a(1,2)*a(2,1)*a(3,3) - a(1,3)*a(2,2)*a(3,1)
end function num_determinant_r

function num_determinant_i(a) result(det)
  integer, intent(in) :: a(3,3)
  integer             :: det
  det = a(1,1)*a(2,2)*a(3,3) + a(1,2)*a(2,3)*a(3,1) + a(1,3)*a(2,1)*a(3,2) &
    - a(1,1)*a(2,3)*a(3,2) - a(1,2)*a(2,1)*a(3,3) - a(1,3)*a(2,2)*a(3,1)
end function num_determinant_i

function num_matmul_1_2_r(a,b) result (y)
  use lasi_module, only : lasi_gemv
  real, intent(in) :: b(:,:)
  real, intent(in) :: a(size(b,1))
  real             :: y(size(b,2))
  if(size(a,1) /= size(b,1)) ERROR("")
  call lasi_gemv(trans="T",m=size(b,1),n=size(b,2),alpha=1.0, &
                 a=b,lda=size(b,1),x=a,incx=1,beta=0.0,y=y,incy=1)
end function num_matmul_1_2_r

function num_matmul_1_2_c(a,b) result (y)
  use lasi_module, only : lasi_gemv
  complex, intent(in) :: b(:,:)
  complex, intent(in) :: a(size(b,1))
  complex             :: y(size(b,2))
  if(size(a,1) /= size(b,1)) ERROR("")
  call lasi_gemv(trans="T",m=size(b,1),n=size(b,2),alpha=(1.0,0.0), &
                 a=b,lda=size(b,1),x=a,incx=1,beta=(0.0,0.0),y=y,incy=1)
end function num_matmul_1_2_c

function num_matmul_1_2_i(a,b) result (y)
  integer, intent(in) :: b(:,:)
  integer, intent(in) :: a(size(b,1))
  integer             :: y(size(b,2))
  integer :: i1,i2
  do i1=1,size(y,1)
    y(i1) = 0
    do i2=1,size(a,1)
      y(i1) = y(i1) + a(i2)*b(i2,i1)
    end do
  end do
end function num_matmul_1_2_i

function num_matmul_2_1_r(a,x,sy) result (y)
  use lasi_module, only : lasi_symv , lasi_gemv
  real, intent(in) :: a(:,:)
  real, intent(in) :: x(size(a,2))
  real             :: y(size(a,1))
  logical, optional, intent(in) :: sy !true if symmetric
  logical :: sy_loc
  sy_loc = .false.
  if(present(sy)) sy_loc = sy
  if(size(a,2) /= size(x,1)) ERROR("")
  if(sy_loc) then
    if(size(a,2) /= size(a,1)) ERROR("")
    call lasi_symv(uplo="U",n=size(a,1),alpha=1.0, &
                   a=a,lda=size(a,1),x=x,incx=1,beta=0.0,y=y,incy=1)
  else
    call lasi_gemv(trans="N",m=size(a,1),n=size(x,1),alpha=1.0, &
                   a=a,lda=size(a,1),x=x,incx=1,beta=0.0,y=y,incy=1)
  end if
end function num_matmul_2_1_r

function num_matmul_2_1_c(a,x,he) result (y)
  use lasi_module, only : lasi_hemv,lasi_gemv
  complex, intent(in) :: a(:,:)
  complex, intent(in) :: x(size(a,2))
  logical, optional, intent(in) :: he !true if hermitian
  complex             :: y(size(a,1))
  logical :: he_loc
  he_loc=.false.
  if(present(he)) he_loc =he
  if(size(a,2) /= size(x,1)) ERROR("")
  if(he_loc) then
    if(size(a,2) /= size(a,1)) ERROR("")
    call lasi_hemv(uplo="U",n=size(a,1),alpha=(1.0,0.0), &
                   a=a,lda=size(a,1),x=x,incx=1,beta=(0.0,0.0),y=y,incy=1)
  else
    call lasi_gemv(trans="N",m=size(a,1),n=size(x,1),alpha=(1.0,0.0), &
                   a=a,lda=size(a,1),x=x,incx=1,beta=(0.0,0.0),y=y,incy=1)
  end if
end function num_matmul_2_1_c

function num_matmul_2_1_i(a,x) result (y)
  integer, intent(in) :: a(:,:)
  integer, intent(in) :: x(size(a,2))
  integer             :: y(size(a,1))
  integer :: i1,i2
  do i1=1,size(y,1)
    y(i1) = 0
    do i2=1,size(x,1)
      y(i1) = y(i1) + a(i1,i2)*x(i2)
    end do
  end do
end function num_matmul_2_1_i

function num_matmul_2_2_r(a,x) result (y)
  use lasi_module, only : lasi_gemm
  real, intent(in) :: a(:,:)
  real, intent(in) :: x(:,:)
  real             :: y(size(a,1),size(x,2))                        
  if(size(a,2) /= size(x,1)) ERROR("")
  call lasi_gemm(transa="N",transb="N",m=size(a,1),n=size(a,2),k=size(x,2),alpha=1.0, &
                 a=a,lda=size(a,1),b=x,ldb=size(x,1),beta=0.0,c=y,ldc=size(y,1))
end function num_matmul_2_2_r

function num_matmul_2_2_c(a,x) result (y)
  use lasi_module, only : lasi_gemm
  complex, intent(in) :: a(:,:)
  complex, intent(in) :: x(:,:)
  complex             :: y(size(a,1),size(x,2))
  if(size(a,2) /= size(x,1)) ERROR("")
  call lasi_gemm(transa="N",transb="N",m=size(a,1),n=size(a,2),k=size(x,2),alpha=(1.0,0.0), &
                 a=a,lda=size(a,1),b=x,ldb=size(x,1),beta=(0.0,0.0),c=y,ldc=size(y,1))
end function num_matmul_2_2_c

function num_matmul_2_2_i(a,x) result (y)
  integer, intent(in) :: a(:,:)
  integer, intent(in) :: x(:,:)
  integer             :: y(size(a,1),size(x,2))
  integer :: i1,i2,i3
  if(size(a,2) /= size(x,1)) ERROR("")
  do i1=1,size(y,2)
    do i2=1,size(y,1)
      y(i2,i1)=0
      do i3=1,size(x,2)
        y(i2,i1)=y(i2,i1)+a(i2,i3)*x(i3,i1)
      end do
    end do
  end do
end function num_matmul_2_2_i


!@ MANUAL
function num_triple_dot(x,y,z)
! Returns conjg(x)*conjg(y)*z
  complex              :: num_triple_dot
  complex, intent(in)  :: x(:),y(:),z(:)
!@ END MANUAL
  if(size(y)/=size(x)) ERROR("")
  if(size(z)/=size(x)) ERROR("")
  num_triple_dot = sum(conjg(x)*conjg(y)*z)
end function num_triple_dot

!@ MANUAL
function num_nrm2(x)
  use lasi_module, only : lasi_nrm2
! Returns sqrt(sum(abs(x)**2))
  complex             :: num_nrm2
  complex, intent(in) :: x(:)
!@ END MANUAL
  num_nrm2 = lasi_nrm2(size(x),x,1)
end function num_nrm2

!@ MANUAL
subroutine num_vemul(res,v1,v2)
! Performs res = conjg(v1)*v2
  complex, intent(out) :: res(:)
  complex, intent(in)  :: v1(:),v2(:)
!@ END MANUAL
  if(size(v1)/=size(res)) ERROR("")
  if(size(v2)/=size(res)) ERROR("")
  res = conjg(v1) * v2
end subroutine num_vemul

!@ MANUAL
complex function num_dotp(v1,v2)
  use lasi_module, only : lasi_DOTU
  complex, intent(in)  :: v1(:),v2(:)
!@ END MANUAL
  if(size(v1)/=size(v2)) ERROR("")
  num_dotp = lasi_DOTU( size(v1), v1, 1, v2, 1 )
end function num_dotp

!@ MANUAL
subroutine num_vea(res,v1,v2)
! Performs res = v1 + v2
  complex, intent(out) :: res(:)
  complex, intent(in)  :: v1(:),v2(:)
!@ END MANUAL
  if(size(v1)/=size(res)) ERROR("")
  if(size(v2)/=size(res)) ERROR("")
  res = v1 + v2
end subroutine num_vea

!@ MANUAL
subroutine num_gemv(a,x,y,alpha,beta,trans)
  use lasi_module, only : lasi_gemv
! Performs y = matmul(a,x)
  complex,           intent(in)    :: x(:)
  complex                          :: y(:) ! if(beta==0) intent(out)
                                           ! otherwise   intent(inout)
  complex,           intent(in)    :: a(size(y),size(x))
  complex, optional, intent(in)    :: alpha,beta
  character, optional,intent(in)    :: trans
!@ END MANUAL
  integer :: nx,ny,lda
  complex :: alpha_loc,beta_loc
  character :: trans_loc
  nx = size(x)
  ny = size(y)
  lda = ubound(a,1)
  alpha_loc = (1.0,0.0)
  beta_loc  = (0.0,0.0)
  trans_loc = "N"
  if(present(alpha)) alpha_loc = alpha
  if(present(beta))  beta_loc  = beta
  if(present(trans)) trans_loc = trans
  call lasi_gemv(trans_loc,ny,nx,alpha_loc,a,lda,x,1,beta_loc,y,1)
end subroutine num_gemv

!@ MANUAL
subroutine num_gerc(alpha,x,y,a)
  use lasi_module, only : lasi_gerc
! Performs a(i,j) = a(i,j) + alpha * x(i) * conjg(y(j)))
  complex, intent(in)    :: alpha
  complex, intent(in)    :: x(:),y(:)
  complex, intent(inout) :: a(:,:)
!@ END MANUAL
  integer :: m,n,lda
  m=size(x)
  n=size(y)
  lda=ubound(a,1)
  if(ubound(a,1)/=m .or. ubound(a,2)/=n) ERROR("")
  call lasi_gerc(m,n,alpha,x,1,y,1,a,lda)
end subroutine num_gerc

subroutine num_he_diag(a,ev,evonly)
  use lasi_module, only : lasi_heev
  complex,  target, intent(inout) :: a(:,:)
  real,     intent(out)   :: ev(:)
  logical,  intent(in)    :: evonly
  integer :: n,lda
  complex, allocatable :: work(:)
  complex, pointer     :: a_tmp(:,:)
  integer :: lwork,info,nb
  real,    allocatable :: rwork(:)
  character :: jobz
  n = size(ev)
  lda = ubound(a,1)
  if(evonly) then
    allocate(a_tmp(ubound(a,1),ubound(a,2)))
    a_tmp = a
    jobz = 'N'
  else
    a_tmp => a
    jobz = 'V'
  end if
  nb = 100
  lwork = (nb+1)*n
  allocate(work(lwork),rwork(max(1,3*n-2)))
  call lasi_heev(jobz,'L',n,a_tmp,lda,ev,work,lwork,rwork,info)
  deallocate(work,rwork)
  if(info/=0) ERROR("")
  if(evonly) then
    deallocate(a_tmp)
  end if
end subroutine num_he_diag

!@ MANUAL
subroutine num_he_inv(a)
  use lasi_module, only : lasi_hetrf,lasi_hetri
! Invert an Hermitean matrix
  complex, intent(inout) :: a(:,:)
!@ END MANUAL
! debug
!  real :: a_check(size(a))
! debug
  integer :: n,lda,lwork,nb,info
  integer :: i1,i2
  integer, allocatable :: ipiv(:)
  complex, allocatable :: work(:)
  character(len=10) :: infostr
  n = ubound(a,1)
  lda = n
! debug
!  a_check = a
! debug
!!!!!!!!!!!!!!!!!!!!!!!
!Questo sarebbe da sistemare per benino
  nb = 100 ! A CASO !!!!
!!!!!!!!!!!!!!!!!!!!!!!
  lwork = n*nb
  allocate(ipiv(n),work(lwork))
  call lasi_hetrf('U',n,a,lda,ipiv,work,lwork,info)
  write(infostr,"(i10)") info
  if(info/=0) ERROR(infostr)
  call lasi_hetri('U',n,a,lda,ipiv,work,info)
  write(infostr,"(i10)") info
  if(info/=0) ERROR(infostr)
  deallocate(ipiv,work)
  do i2=1,n
    do i1=i2+1,n
      a(i1,i2) = conjg(a(i2,i1))
    end do
  end do
end subroutine num_he_inv

subroutine num_ge_inv(a)
  use lasi_module, only : lasi_getrf , lasi_getri
  complex, intent(inout) :: a(:,:)
  integer :: n,lda,info,nb,lwork
  integer, allocatable :: ipiv(:)
  complex, allocatable :: work(:)
  character(len=10) :: infostr
  n = ubound(a,1)
  lda = n
  nb = 100
  lwork = n*nb
  allocate(ipiv(n),work(lwork))
  call lasi_getrf(n,n,a,lda,ipiv,info)
  write(infostr,"(i10)") info
  if(info/=0) ERROR(infostr)
  call lasi_getri(n,a,lda,ipiv,work,lwork,info)
  write(infostr,"(i10)") info
  if(info/=0) ERROR(infostr)
  deallocate(ipiv,work)
end subroutine num_ge_inv
  

!@ MANUAL
function num_simpson(f)
! Perform simple simpson integral
  real, intent(in) :: f(:)
  real :: num_simpson
!@ END MANUAL
  integer :: n,i
  real :: sum
  n = size(f)
  sum = 0.0
  do i=2,n-1,2
    sum = sum + f(i-1) + 4.0*f(i) + f(i+1)
  end do
  num_simpson = sum / 3.0
end function num_simpson

function num_trace_c(matrix) result(trace)
  complex             :: trace
  complex, intent(in) :: matrix(:,:)
  complex :: tmp
  integer :: n,i
  n = ubound(matrix,1)
  if(n/=ubound(matrix,2)) ERROR("")
  tmp = 0.0
  do i=1,n
    tmp = tmp + matrix(i,i)
  end do
  trace = tmp
end function num_trace_c

function num_trace_r(matrix) result(trace)
  real             :: trace
  real, intent(in) :: matrix(:,:)
  real :: tmp
  integer :: n,i
  n = ubound(matrix,1)
  if(n/=ubound(matrix,2)) ERROR("")
  tmp = 0.0
  do i=1,n
    tmp = tmp + matrix(i,i)
  end do
  trace = tmp
end function num_trace_r


function num_power(base,exponent)
  real             :: num_power
  real, intent(in) :: base,exponent
  num_power = exp(exponent * log(base))
end function num_power

function num_r2c(r)
  real, intent(in) :: r(:)
  complex :: num_r2c(size(r)/2)
  integer :: i
  do i=1,size(num_r2c)
    num_r2c(i) = cmplx(r(2*i-1),r(2*i))
  end do
end function num_r2c

function num_c2r(c)
  complex, intent(in) :: c(:)
  real :: num_c2r(size(c)*2)
  integer :: i
  do i=1,size(c)
    num_c2r(2*i-1) = real(c(i))
    num_c2r(2*i)   = aimag(c(i))
  end do
end function num_c2r

function num_spectral_range(a,part)
  real :: num_spectral_range(2)
  complex, intent(in) :: a(:,:)
  character, intent(in) :: part
  complex, allocatable :: atmp(:,:)
  real, allocatable :: ev(:)
  allocate(atmp(ubound(a,1),ubound(a,2)),ev(ubound(a,1)))
  select case(part)
  case("H")
    atmp=(a+conjg(transpose(a))) * 0.5
  case("A")
    atmp=(a-conjg(transpose(a))) * (0.5 / (0.0,1.0))
  end select
  call num_he_diag(atmp,ev,.true.)
  deallocate(atmp,ev)
  num_spectral_range=(/maxval(ev),minval(ev)/)
end function num_spectral_range

function num_set_aimag_to_positive(a) result(res)
  complex, intent(in) :: a(:,:)
  complex :: res(ubound(a,1),ubound(a,2))
  complex :: tmp(ubound(a,1),ubound(a,2)),tmpscal
  real    :: ev(ubound(a,1))
  integer :: n,i,j,k
  if(ubound(a,1)/=ubound(a,2)) ERROR("")
  n = ubound(a,1)
  tmp = (a-conjg(transpose(a))) / (0.0,2.0)
  call num_he_diag(tmp,ev,.false.)
  ev = abs(ev)
  do i=1,n
    do j=1,n
      tmpscal = 0.0
      do k=1,n
        tmpscal = tmpscal + tmp(i,k) * ev(k) * conjg(tmp(j,k))
      end do
      res(i,j) = tmpscal
    end do
  end do
  res = (0.0,1.0) * res + (a+conjg(transpose(a))) / 2.0
end function num_set_aimag_to_positive
  
  

end module num_la_module
