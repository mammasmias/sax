# 1 "num_interpolation_module.spp"
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
module num_interpolation_module
implicit none
! This module provides the definition of "num_interpolation" and its methods
private
public :: num_interpolation_init
public :: num_interpolation_destroy
public :: num_interpolation_calc
public :: num_interpolation_calc_der
# 30 "num_interpolation_module.spp"
#if 0
public :: num_interpolation_real_0
public :: num_interpolation_real_0_init
public :: num_interpolation_real_0_destroy
public :: num_interpolation_real_0_calc
#endif
# 30 "num_interpolation_module.spp"
#if 0
public :: num_interpolation_real_1
public :: num_interpolation_real_1_init
public :: num_interpolation_real_1_destroy
public :: num_interpolation_real_1_calc
#endif
# 30 "num_interpolation_module.spp"
#if 0
public :: num_interpolation_real_2
public :: num_interpolation_real_2_init
public :: num_interpolation_real_2_destroy
public :: num_interpolation_real_2_calc
#endif
# 30 "num_interpolation_module.spp"
#if 0
public :: num_interpolation_complex_0
public :: num_interpolation_complex_0_init
public :: num_interpolation_complex_0_destroy
public :: num_interpolation_complex_0_calc
#endif
# 30 "num_interpolation_module.spp"
#if 0
public :: num_interpolation_complex_1
public :: num_interpolation_complex_1_init
public :: num_interpolation_complex_1_destroy
public :: num_interpolation_complex_1_calc
#endif
# 30 "num_interpolation_module.spp"
#if 0
public :: num_interpolation_complex_2
public :: num_interpolation_complex_2_init
public :: num_interpolation_complex_2_destroy
public :: num_interpolation_complex_2_calc
#endif
# 38 "num_interpolation_module.spp"
public :: num_interpolation

!@ END MANUAL

!@ MANUAL
! This is a type for numerical interpolation
! It contains a table of n+1 pairs of reals,
! ordered in decreasing or increasing x.
! if parity is different from 0 it can be used to improve
! the interpolation
type num_interpolation
  integer      :: n
  real         :: xmin,xmax
  real,pointer :: y(:),x(:)
  integer      :: parity
end type num_interpolation
!@ END MANUAL


#if 0
# 63 "num_interpolation_module.spp"
type num_interpolation_real_0
  integer       :: n
  real          :: xmin,xmax
  real,pointer :: x(:),y(:)
  integer       :: parity
# 74 "num_interpolation_module.spp"
end type num_interpolation_real_0
# 63 "num_interpolation_module.spp"
type num_interpolation_real_1
  integer       :: n
  real          :: xmin,xmax
  real,pointer :: x(:),y(:,:)
  integer       :: parity
# 69 "num_interpolation_module.spp"
  integer       :: dim1
# 74 "num_interpolation_module.spp"
end type num_interpolation_real_1
# 63 "num_interpolation_module.spp"
type num_interpolation_real_2
  integer       :: n
  real          :: xmin,xmax
  real,pointer :: x(:),y(:,:,:)
  integer       :: parity
# 69 "num_interpolation_module.spp"
  integer       :: dim1
# 72 "num_interpolation_module.spp"
  integer       :: dim2
# 74 "num_interpolation_module.spp"
end type num_interpolation_real_2
# 63 "num_interpolation_module.spp"
type num_interpolation_complex_0
  integer       :: n
  real          :: xmin,xmax
  complex,pointer :: x(:),y(:)
  integer       :: parity
# 74 "num_interpolation_module.spp"
end type num_interpolation_complex_0
# 63 "num_interpolation_module.spp"
type num_interpolation_complex_1
  integer       :: n
  real          :: xmin,xmax
  complex,pointer :: x(:),y(:,:)
  integer       :: parity
# 69 "num_interpolation_module.spp"
  integer       :: dim1
# 74 "num_interpolation_module.spp"
end type num_interpolation_complex_1
# 63 "num_interpolation_module.spp"
type num_interpolation_complex_2
  integer       :: n
  real          :: xmin,xmax
  complex,pointer :: x(:),y(:,:,:)
  integer       :: parity
# 69 "num_interpolation_module.spp"
  integer       :: dim1
# 72 "num_interpolation_module.spp"
  integer       :: dim2
# 74 "num_interpolation_module.spp"
end type num_interpolation_complex_2
# 77 "num_interpolation_module.spp"
contains

# 81 "num_interpolation_module.spp"
subroutine num_interpolation_real_0_init(interpolation &
# 88 "num_interpolation_module.spp"
                                                                         ,xmin,xmax,delta,parity)
  type (num_interpolation_real_0), intent(out) :: interpolation
  real,                                     intent(in)  :: xmin,xmax,delta
  integer, optional,                        intent(in)  :: parity
# 98 "num_interpolation_module.spp"
  integer :: i
  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1.0,(xmax - xmin))
  allocate(interpolation%x(0:interpolation%n))
# 102 "num_interpolation_module.spp"
  allocate(interpolation%y(0:interpolation%n))
# 113 "num_interpolation_module.spp"
  interpolation%xmin = xmin
  interpolation%xmax = xmax
  do i=0,interpolation%n
    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
  end do
  interpolation%y=0.0
  if(present(parity)) then
    interpolation%parity = parity
  else
    interpolation%parity = 0
  end if
  if(interpolation%parity /= 0 .and. (xmin /= 0.0 .or. xmax<0.0)) then
    WARNING("")
    interpolation%parity = 0
  end if
end subroutine num_interpolation_real_0_init

!@ MANUAL
subroutine num_interpolation_real_0_destroy(interpolation)
! Destroys the table
  type (num_interpolation_real_0), intent(inout) :: interpolation
!@ END MANUAL
  deallocate(interpolation%x)
  deallocate(interpolation%y)
end subroutine num_interpolation_real_0_destroy

subroutine num_interpolation_real_0_little_table(interpolation,x,order,tab_x,tab_y)
! Private routine to build a small table to be used by the low-level
! interpolation routines.
  use numrec_module
  type (num_interpolation_real_0), intent(in)  :: interpolation
  real,                     intent(in)  :: x
  integer,                  intent(in)  :: order
  real,                     intent(out) :: tab_x(0:order-1)
# 148 "num_interpolation_module.spp"
  real,                    intent(out) :: tab_y(0:order-1)
# 156 "num_interpolation_module.spp"
  integer :: j,k
  if(interpolation%parity==0) then
    call numrec_locate(interpolation%x,interpolation%n+1,x,j)
    k = min(max(j-(order-1)/2,1),interpolation%n+1+1-order) - 1
    if(k<0 .or. k+order-1>interpolation%n) ERROR("")
    tab_x(:) = interpolation%x(k:k+order-1)
# 163 "num_interpolation_module.spp"
    tab_y(:) = interpolation%y(k:k+order-1)
# 171 "num_interpolation_module.spp"
  else
    call numrec_locate(interpolation%x,interpolation%n+1,abs(x),j)
    k = min(j-(order-1)/2,interpolation%n+1+1-order) - 1
    if(k+order-1>interpolation%n) ERROR("")
    if(k>=0) then
      tab_x(:) = interpolation%x(k:k+order-1)
# 178 "num_interpolation_module.spp"
      if(interpolation%parity>0) then
        tab_y(:) = interpolation%y(k:k+order-1)
      else
        tab_y(:) = interpolation%y(k:k+order-1) * sign(1.0,x)
      end if
# 198 "num_interpolation_module.spp"
    else
      tab_x(-k:order-1) = interpolation%x(0:k+order-1)
      tab_x(0:-k-1)     = - interpolation%x(-k:1:-1)
# 202 "num_interpolation_module.spp"
      tab_y(-k:order-1) = interpolation%y(0:k+order-1)
      tab_y(0:-k-1)     = interpolation%y(-k:1:-1) * sign(1,interpolation%parity)
# 213 "num_interpolation_module.spp"
    end if
  end if
end subroutine num_interpolation_real_0_little_table


!@ MANUAL
function num_interpolation_real_0_calc(interpolation,x,order)
! Returns the interpolated value in the point x, using a order-1 polynomial
  use numrec_module
  type (num_interpolation_real_0), intent(in) :: interpolation
# 224 "num_interpolation_module.spp"
  real                                :: num_interpolation_real_0_calc
# 232 "num_interpolation_module.spp"
  real,                     intent(in) :: x
  integer,                  intent(in) :: order
  integer :: i1,i2
!@ END MANUAL
  real    :: tab_x(order)
# 238 "num_interpolation_module.spp"
  real   :: tab_y(order)
# 246 "num_interpolation_module.spp"
  real :: y,tab_y_tmp(order)
  real  :: y1,y2,dy
  call num_interpolation_real_0_little_table(interpolation,x,order,tab_x,tab_y)
# 256 "num_interpolation_module.spp"
     tab_y_tmp = tab_y
# 265 "num_interpolation_module.spp"
     call numrec_polint(tab_x,tab_y_tmp,order,abs(x),y,dy)
# 273 "num_interpolation_module.spp"
     num_interpolation_real_0_calc = y
# 281 "num_interpolation_module.spp"

# 288 "num_interpolation_module.spp"

end function num_interpolation_real_0_calc

# 81 "num_interpolation_module.spp"
subroutine num_interpolation_real_1_init(interpolation &
# 83 "num_interpolation_module.spp"
                                                               ,dim &
# 88 "num_interpolation_module.spp"
                                                                         ,xmin,xmax,delta,parity)
  type (num_interpolation_real_1), intent(out) :: interpolation
  real,                                     intent(in)  :: xmin,xmax,delta
  integer, optional,                        intent(in)  :: parity
# 93 "num_interpolation_module.spp"
  integer,                                  intent(in)  :: dim
# 98 "num_interpolation_module.spp"
  integer :: i
  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1.0,(xmax - xmin))
  allocate(interpolation%x(0:interpolation%n))
# 105 "num_interpolation_module.spp"
  interpolation%dim1 = dim
  allocate(interpolation%y(dim,0:interpolation%n))
# 113 "num_interpolation_module.spp"
  interpolation%xmin = xmin
  interpolation%xmax = xmax
  do i=0,interpolation%n
    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
  end do
  interpolation%y=0.0
  if(present(parity)) then
    interpolation%parity = parity
  else
    interpolation%parity = 0
  end if
  if(interpolation%parity /= 0 .and. (xmin /= 0.0 .or. xmax<0.0)) then
    WARNING("")
    interpolation%parity = 0
  end if
end subroutine num_interpolation_real_1_init

!@ MANUAL
subroutine num_interpolation_real_1_destroy(interpolation)
! Destroys the table
  type (num_interpolation_real_1), intent(inout) :: interpolation
!@ END MANUAL
  deallocate(interpolation%x)
  deallocate(interpolation%y)
end subroutine num_interpolation_real_1_destroy

subroutine num_interpolation_real_1_little_table(interpolation,x,order,tab_x,tab_y)
! Private routine to build a small table to be used by the low-level
! interpolation routines.
  use numrec_module
  type (num_interpolation_real_1), intent(in)  :: interpolation
  real,                     intent(in)  :: x
  integer,                  intent(in)  :: order
  real,                     intent(out) :: tab_x(0:order-1)
# 151 "num_interpolation_module.spp"
  real,                    intent(out) :: tab_y(interpolation%dim1,0:order-1)
# 156 "num_interpolation_module.spp"
  integer :: j,k
  if(interpolation%parity==0) then
    call numrec_locate(interpolation%x,interpolation%n+1,x,j)
    k = min(max(j-(order-1)/2,1),interpolation%n+1+1-order) - 1
    if(k<0 .or. k+order-1>interpolation%n) ERROR("")
    tab_x(:) = interpolation%x(k:k+order-1)
# 166 "num_interpolation_module.spp"
    tab_y(:,:) = interpolation%y(:,k:k+order-1)
# 171 "num_interpolation_module.spp"
  else
    call numrec_locate(interpolation%x,interpolation%n+1,abs(x),j)
    k = min(j-(order-1)/2,interpolation%n+1+1-order) - 1
    if(k+order-1>interpolation%n) ERROR("")
    if(k>=0) then
      tab_x(:) = interpolation%x(k:k+order-1)
# 185 "num_interpolation_module.spp"
      if(interpolation%parity>0) then
        tab_y(:,:) = interpolation%y(:,k:k+order-1)
      else
        tab_y(:,:) = interpolation%y(:,k:k+order-1) * sign(1.0,x)
      end if
# 198 "num_interpolation_module.spp"
    else
      tab_x(-k:order-1) = interpolation%x(0:k+order-1)
      tab_x(0:-k-1)     = - interpolation%x(-k:1:-1)
# 206 "num_interpolation_module.spp"
      tab_y(:,-k:order-1) = interpolation%y(:,0:k+order-1)
      tab_y(:,0:-k-1)     = interpolation%y(:,-k:1:-1) * sign(1,interpolation%parity)
# 213 "num_interpolation_module.spp"
    end if
  end if
end subroutine num_interpolation_real_1_little_table


!@ MANUAL
function num_interpolation_real_1_calc(interpolation,x,order)
! Returns the interpolated value in the point x, using a order-1 polynomial
  use numrec_module
  type (num_interpolation_real_1), intent(in) :: interpolation
# 227 "num_interpolation_module.spp"
  real                                :: num_interpolation_real_1_calc (interpolation%dim1)
# 232 "num_interpolation_module.spp"
  real,                     intent(in) :: x
  integer,                  intent(in) :: order
  integer :: i1,i2
!@ END MANUAL
  real    :: tab_x(order)
# 241 "num_interpolation_module.spp"
  real   :: tab_y(interpolation%dim1,order)
# 246 "num_interpolation_module.spp"
  real :: y,tab_y_tmp(order)
  real  :: y1,y2,dy
  call num_interpolation_real_1_little_table(interpolation,x,order,tab_x,tab_y)
# 253 "num_interpolation_module.spp"
  do i1 = 1 , interpolation%dim1
# 259 "num_interpolation_module.spp"
     tab_y_tmp = tab_y(i1,:)
# 265 "num_interpolation_module.spp"
     call numrec_polint(tab_x,tab_y_tmp,order,abs(x),y,dy)
# 276 "num_interpolation_module.spp"
     num_interpolation_real_1_calc(i1) = y
# 281 "num_interpolation_module.spp"

# 283 "num_interpolation_module.spp"
  end do
# 288 "num_interpolation_module.spp"

end function num_interpolation_real_1_calc

# 81 "num_interpolation_module.spp"
subroutine num_interpolation_real_2_init(interpolation &
# 86 "num_interpolation_module.spp"
                                                               ,dim1,dim2 &
# 88 "num_interpolation_module.spp"
                                                                         ,xmin,xmax,delta,parity)
  type (num_interpolation_real_2), intent(out) :: interpolation
  real,                                     intent(in)  :: xmin,xmax,delta
  integer, optional,                        intent(in)  :: parity
# 96 "num_interpolation_module.spp"
  integer,                                  intent(in)  :: dim1,dim2
# 98 "num_interpolation_module.spp"
  integer :: i
  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1.0,(xmax - xmin))
  allocate(interpolation%x(0:interpolation%n))
# 109 "num_interpolation_module.spp"
  interpolation%dim1 = dim1
  interpolation%dim2 = dim2
  allocate(interpolation%y(dim1,dim2,0:interpolation%n))
# 113 "num_interpolation_module.spp"
  interpolation%xmin = xmin
  interpolation%xmax = xmax
  do i=0,interpolation%n
    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
  end do
  interpolation%y=0.0
  if(present(parity)) then
    interpolation%parity = parity
  else
    interpolation%parity = 0
  end if
  if(interpolation%parity /= 0 .and. (xmin /= 0.0 .or. xmax<0.0)) then
    WARNING("")
    interpolation%parity = 0
  end if
end subroutine num_interpolation_real_2_init

!@ MANUAL
subroutine num_interpolation_real_2_destroy(interpolation)
! Destroys the table
  type (num_interpolation_real_2), intent(inout) :: interpolation
!@ END MANUAL
  deallocate(interpolation%x)
  deallocate(interpolation%y)
end subroutine num_interpolation_real_2_destroy

subroutine num_interpolation_real_2_little_table(interpolation,x,order,tab_x,tab_y)
! Private routine to build a small table to be used by the low-level
! interpolation routines.
  use numrec_module
  type (num_interpolation_real_2), intent(in)  :: interpolation
  real,                     intent(in)  :: x
  integer,                  intent(in)  :: order
  real,                     intent(out) :: tab_x(0:order-1)
# 154 "num_interpolation_module.spp"
  real,                    intent(out) :: tab_y(interpolation%dim1,interpolation%dim2,0:order-1)
# 156 "num_interpolation_module.spp"
  integer :: j,k
  if(interpolation%parity==0) then
    call numrec_locate(interpolation%x,interpolation%n+1,x,j)
    k = min(max(j-(order-1)/2,1),interpolation%n+1+1-order) - 1
    if(k<0 .or. k+order-1>interpolation%n) ERROR("")
    tab_x(:) = interpolation%x(k:k+order-1)
# 169 "num_interpolation_module.spp"
    tab_y(:,:,:) = interpolation%y(:,:,k:k+order-1)
# 171 "num_interpolation_module.spp"
  else
    call numrec_locate(interpolation%x,interpolation%n+1,abs(x),j)
    k = min(j-(order-1)/2,interpolation%n+1+1-order) - 1
    if(k+order-1>interpolation%n) ERROR("")
    if(k>=0) then
      tab_x(:) = interpolation%x(k:k+order-1)
# 192 "num_interpolation_module.spp"
      if(interpolation%parity>0) then
        tab_y(:,:,:) = interpolation%y(:,:,k:k+order-1)
      else
        tab_y(:,:,:) = interpolation%y(:,:,k:k+order-1) * sign(1.0,x)
      end if
# 198 "num_interpolation_module.spp"
    else
      tab_x(-k:order-1) = interpolation%x(0:k+order-1)
      tab_x(0:-k-1)     = - interpolation%x(-k:1:-1)
# 210 "num_interpolation_module.spp"
      tab_y(:,:,-k:order-1) = interpolation%y(:,:,0:k+order-1)
      tab_y(:,:,0:-k-1)     = interpolation%y(:,:,-k:1:-1) * sign(1,interpolation%parity)
# 213 "num_interpolation_module.spp"
    end if
  end if
end subroutine num_interpolation_real_2_little_table


!@ MANUAL
function num_interpolation_real_2_calc(interpolation,x,order)
! Returns the interpolated value in the point x, using a order-1 polynomial
  use numrec_module
  type (num_interpolation_real_2), intent(in) :: interpolation
# 230 "num_interpolation_module.spp"
  real                                :: num_interpolation_real_2_calc (interpolation%dim1,interpolation%dim2)
# 232 "num_interpolation_module.spp"
  real,                     intent(in) :: x
  integer,                  intent(in) :: order
  integer :: i1,i2
!@ END MANUAL
  real    :: tab_x(order)
# 244 "num_interpolation_module.spp"
  real   :: tab_y(interpolation%dim1,interpolation%dim2,order)
# 246 "num_interpolation_module.spp"
  real :: y,tab_y_tmp(order)
  real  :: y1,y2,dy
  call num_interpolation_real_2_little_table(interpolation,x,order,tab_x,tab_y)
# 250 "num_interpolation_module.spp"
  do i2 = 1 , interpolation%dim2
# 253 "num_interpolation_module.spp"
  do i1 = 1 , interpolation%dim1
# 262 "num_interpolation_module.spp"
     tab_y_tmp = tab_y(i1,i2,:)
# 265 "num_interpolation_module.spp"
     call numrec_polint(tab_x,tab_y_tmp,order,abs(x),y,dy)
# 279 "num_interpolation_module.spp"
     num_interpolation_real_2_calc(i1,i2) = y
# 281 "num_interpolation_module.spp"

# 283 "num_interpolation_module.spp"
  end do
# 286 "num_interpolation_module.spp"
  end do
# 288 "num_interpolation_module.spp"

end function num_interpolation_real_2_calc

# 81 "num_interpolation_module.spp"
subroutine num_interpolation_complex_0_init(interpolation &
# 88 "num_interpolation_module.spp"
                                                                         ,xmin,xmax,delta,parity)
  type (num_interpolation_complex_0), intent(out) :: interpolation
  real,                                     intent(in)  :: xmin,xmax,delta
  integer, optional,                        intent(in)  :: parity
# 98 "num_interpolation_module.spp"
  integer :: i
  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1.0,(xmax - xmin))
  allocate(interpolation%x(0:interpolation%n))
# 102 "num_interpolation_module.spp"
  allocate(interpolation%y(0:interpolation%n))
# 113 "num_interpolation_module.spp"
  interpolation%xmin = xmin
  interpolation%xmax = xmax
  do i=0,interpolation%n
    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
  end do
  interpolation%y=0.0
  if(present(parity)) then
    interpolation%parity = parity
  else
    interpolation%parity = 0
  end if
  if(interpolation%parity /= 0 .and. (xmin /= 0.0 .or. xmax<0.0)) then
    WARNING("")
    interpolation%parity = 0
  end if
end subroutine num_interpolation_complex_0_init

!@ MANUAL
subroutine num_interpolation_complex_0_destroy(interpolation)
! Destroys the table
  type (num_interpolation_complex_0), intent(inout) :: interpolation
!@ END MANUAL
  deallocate(interpolation%x)
  deallocate(interpolation%y)
end subroutine num_interpolation_complex_0_destroy

subroutine num_interpolation_complex_0_little_table(interpolation,x,order,tab_x,tab_y)
! Private routine to build a small table to be used by the low-level
! interpolation routines.
  use numrec_module
  type (num_interpolation_complex_0), intent(in)  :: interpolation
  real,                     intent(in)  :: x
  integer,                  intent(in)  :: order
  real,                     intent(out) :: tab_x(0:order-1)
# 148 "num_interpolation_module.spp"
  complex,                    intent(out) :: tab_y(0:order-1)
# 156 "num_interpolation_module.spp"
  integer :: j,k
  if(interpolation%parity==0) then
    call numrec_locate(interpolation%x,interpolation%n+1,x,j)
    k = min(max(j-(order-1)/2,1),interpolation%n+1+1-order) - 1
    if(k<0 .or. k+order-1>interpolation%n) ERROR("")
    tab_x(:) = interpolation%x(k:k+order-1)
# 163 "num_interpolation_module.spp"
    tab_y(:) = interpolation%y(k:k+order-1)
# 171 "num_interpolation_module.spp"
  else
    call numrec_locate(interpolation%x,interpolation%n+1,abs(x),j)
    k = min(j-(order-1)/2,interpolation%n+1+1-order) - 1
    if(k+order-1>interpolation%n) ERROR("")
    if(k>=0) then
      tab_x(:) = interpolation%x(k:k+order-1)
# 178 "num_interpolation_module.spp"
      if(interpolation%parity>0) then
        tab_y(:) = interpolation%y(k:k+order-1)
      else
        tab_y(:) = interpolation%y(k:k+order-1) * sign(1.0,x)
      end if
# 198 "num_interpolation_module.spp"
    else
      tab_x(-k:order-1) = interpolation%x(0:k+order-1)
      tab_x(0:-k-1)     = - interpolation%x(-k:1:-1)
# 202 "num_interpolation_module.spp"
      tab_y(-k:order-1) = interpolation%y(0:k+order-1)
      tab_y(0:-k-1)     = interpolation%y(-k:1:-1) * sign(1,interpolation%parity)
# 213 "num_interpolation_module.spp"
    end if
  end if
end subroutine num_interpolation_complex_0_little_table


!@ MANUAL
function num_interpolation_complex_0_calc(interpolation,x,order)
! Returns the interpolated value in the point x, using a order-1 polynomial
  use numrec_module
  type (num_interpolation_complex_0), intent(in) :: interpolation
# 224 "num_interpolation_module.spp"
  complex                                :: num_interpolation_complex_0_calc
# 232 "num_interpolation_module.spp"
  real,                     intent(in) :: x
  integer,                  intent(in) :: order
  integer :: i1,i2
!@ END MANUAL
  real    :: tab_x(order)
# 238 "num_interpolation_module.spp"
  complex   :: tab_y(order)
# 246 "num_interpolation_module.spp"
  complex :: y,tab_y_tmp(order)
  real  :: y1,y2,dy
  call num_interpolation_complex_0_little_table(interpolation,x,order,tab_x,tab_y)
# 256 "num_interpolation_module.spp"
     tab_y_tmp = tab_y
# 268 "num_interpolation_module.spp"
     call numrec_polint(tab_x,real (tab_y_tmp),order,abs(x),y1,dy)
     call numrec_polint(tab_x,aimag(tab_y_tmp),order,abs(x),y2,dy)
     y = cmplx(y1,y2)
# 273 "num_interpolation_module.spp"
     num_interpolation_complex_0_calc = y
# 281 "num_interpolation_module.spp"

# 288 "num_interpolation_module.spp"

end function num_interpolation_complex_0_calc

# 81 "num_interpolation_module.spp"
subroutine num_interpolation_complex_1_init(interpolation &
# 83 "num_interpolation_module.spp"
                                                               ,dim &
# 88 "num_interpolation_module.spp"
                                                                         ,xmin,xmax,delta,parity)
  type (num_interpolation_complex_1), intent(out) :: interpolation
  real,                                     intent(in)  :: xmin,xmax,delta
  integer, optional,                        intent(in)  :: parity
# 93 "num_interpolation_module.spp"
  integer,                                  intent(in)  :: dim
# 98 "num_interpolation_module.spp"
  integer :: i
  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1.0,(xmax - xmin))
  allocate(interpolation%x(0:interpolation%n))
# 105 "num_interpolation_module.spp"
  interpolation%dim1 = dim
  allocate(interpolation%y(dim,0:interpolation%n))
# 113 "num_interpolation_module.spp"
  interpolation%xmin = xmin
  interpolation%xmax = xmax
  do i=0,interpolation%n
    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
  end do
  interpolation%y=0.0
  if(present(parity)) then
    interpolation%parity = parity
  else
    interpolation%parity = 0
  end if
  if(interpolation%parity /= 0 .and. (xmin /= 0.0 .or. xmax<0.0)) then
    WARNING("")
    interpolation%parity = 0
  end if
end subroutine num_interpolation_complex_1_init

!@ MANUAL
subroutine num_interpolation_complex_1_destroy(interpolation)
! Destroys the table
  type (num_interpolation_complex_1), intent(inout) :: interpolation
!@ END MANUAL
  deallocate(interpolation%x)
  deallocate(interpolation%y)
end subroutine num_interpolation_complex_1_destroy

subroutine num_interpolation_complex_1_little_table(interpolation,x,order,tab_x,tab_y)
! Private routine to build a small table to be used by the low-level
! interpolation routines.
  use numrec_module
  type (num_interpolation_complex_1), intent(in)  :: interpolation
  real,                     intent(in)  :: x
  integer,                  intent(in)  :: order
  real,                     intent(out) :: tab_x(0:order-1)
# 151 "num_interpolation_module.spp"
  complex,                    intent(out) :: tab_y(interpolation%dim1,0:order-1)
# 156 "num_interpolation_module.spp"
  integer :: j,k
  if(interpolation%parity==0) then
    call numrec_locate(interpolation%x,interpolation%n+1,x,j)
    k = min(max(j-(order-1)/2,1),interpolation%n+1+1-order) - 1
    if(k<0 .or. k+order-1>interpolation%n) ERROR("")
    tab_x(:) = interpolation%x(k:k+order-1)
# 166 "num_interpolation_module.spp"
    tab_y(:,:) = interpolation%y(:,k:k+order-1)
# 171 "num_interpolation_module.spp"
  else
    call numrec_locate(interpolation%x,interpolation%n+1,abs(x),j)
    k = min(j-(order-1)/2,interpolation%n+1+1-order) - 1
    if(k+order-1>interpolation%n) ERROR("")
    if(k>=0) then
      tab_x(:) = interpolation%x(k:k+order-1)
# 185 "num_interpolation_module.spp"
      if(interpolation%parity>0) then
        tab_y(:,:) = interpolation%y(:,k:k+order-1)
      else
        tab_y(:,:) = interpolation%y(:,k:k+order-1) * sign(1.0,x)
      end if
# 198 "num_interpolation_module.spp"
    else
      tab_x(-k:order-1) = interpolation%x(0:k+order-1)
      tab_x(0:-k-1)     = - interpolation%x(-k:1:-1)
# 206 "num_interpolation_module.spp"
      tab_y(:,-k:order-1) = interpolation%y(:,0:k+order-1)
      tab_y(:,0:-k-1)     = interpolation%y(:,-k:1:-1) * sign(1,interpolation%parity)
# 213 "num_interpolation_module.spp"
    end if
  end if
end subroutine num_interpolation_complex_1_little_table


!@ MANUAL
function num_interpolation_complex_1_calc(interpolation,x,order)
! Returns the interpolated value in the point x, using a order-1 polynomial
  use numrec_module
  type (num_interpolation_complex_1), intent(in) :: interpolation
# 227 "num_interpolation_module.spp"
  complex                                :: num_interpolation_complex_1_calc (interpolation%dim1)
# 232 "num_interpolation_module.spp"
  real,                     intent(in) :: x
  integer,                  intent(in) :: order
  integer :: i1,i2
!@ END MANUAL
  real    :: tab_x(order)
# 241 "num_interpolation_module.spp"
  complex   :: tab_y(interpolation%dim1,order)
# 246 "num_interpolation_module.spp"
  complex :: y,tab_y_tmp(order)
  real  :: y1,y2,dy
  call num_interpolation_complex_1_little_table(interpolation,x,order,tab_x,tab_y)
# 253 "num_interpolation_module.spp"
  do i1 = 1 , interpolation%dim1
# 259 "num_interpolation_module.spp"
     tab_y_tmp = tab_y(i1,:)
# 268 "num_interpolation_module.spp"
     call numrec_polint(tab_x,real (tab_y_tmp),order,abs(x),y1,dy)
     call numrec_polint(tab_x,aimag(tab_y_tmp),order,abs(x),y2,dy)
     y = cmplx(y1,y2)
# 276 "num_interpolation_module.spp"
     num_interpolation_complex_1_calc(i1) = y
# 281 "num_interpolation_module.spp"

# 283 "num_interpolation_module.spp"
  end do
# 288 "num_interpolation_module.spp"

end function num_interpolation_complex_1_calc

# 81 "num_interpolation_module.spp"
subroutine num_interpolation_complex_2_init(interpolation &
# 86 "num_interpolation_module.spp"
                                                               ,dim1,dim2 &
# 88 "num_interpolation_module.spp"
                                                                         ,xmin,xmax,delta,parity)
  type (num_interpolation_complex_2), intent(out) :: interpolation
  real,                                     intent(in)  :: xmin,xmax,delta
  integer, optional,                        intent(in)  :: parity
# 96 "num_interpolation_module.spp"
  integer,                                  intent(in)  :: dim1,dim2
# 98 "num_interpolation_module.spp"
  integer :: i
  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1.0,(xmax - xmin))
  allocate(interpolation%x(0:interpolation%n))
# 109 "num_interpolation_module.spp"
  interpolation%dim1 = dim1
  interpolation%dim2 = dim2
  allocate(interpolation%y(dim1,dim2,0:interpolation%n))
# 113 "num_interpolation_module.spp"
  interpolation%xmin = xmin
  interpolation%xmax = xmax
  do i=0,interpolation%n
    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
  end do
  interpolation%y=0.0
  if(present(parity)) then
    interpolation%parity = parity
  else
    interpolation%parity = 0
  end if
  if(interpolation%parity /= 0 .and. (xmin /= 0.0 .or. xmax<0.0)) then
    WARNING("")
    interpolation%parity = 0
  end if
end subroutine num_interpolation_complex_2_init

!@ MANUAL
subroutine num_interpolation_complex_2_destroy(interpolation)
! Destroys the table
  type (num_interpolation_complex_2), intent(inout) :: interpolation
!@ END MANUAL
  deallocate(interpolation%x)
  deallocate(interpolation%y)
end subroutine num_interpolation_complex_2_destroy

subroutine num_interpolation_complex_2_little_table(interpolation,x,order,tab_x,tab_y)
! Private routine to build a small table to be used by the low-level
! interpolation routines.
  use numrec_module
  type (num_interpolation_complex_2), intent(in)  :: interpolation
  real,                     intent(in)  :: x
  integer,                  intent(in)  :: order
  real,                     intent(out) :: tab_x(0:order-1)
# 154 "num_interpolation_module.spp"
  complex,                    intent(out) :: tab_y(interpolation%dim1,interpolation%dim2,0:order-1)
# 156 "num_interpolation_module.spp"
  integer :: j,k
  if(interpolation%parity==0) then
    call numrec_locate(interpolation%x,interpolation%n+1,x,j)
    k = min(max(j-(order-1)/2,1),interpolation%n+1+1-order) - 1
    if(k<0 .or. k+order-1>interpolation%n) ERROR("")
    tab_x(:) = interpolation%x(k:k+order-1)
# 169 "num_interpolation_module.spp"
    tab_y(:,:,:) = interpolation%y(:,:,k:k+order-1)
# 171 "num_interpolation_module.spp"
  else
    call numrec_locate(interpolation%x,interpolation%n+1,abs(x),j)
    k = min(j-(order-1)/2,interpolation%n+1+1-order) - 1
    if(k+order-1>interpolation%n) ERROR("")
    if(k>=0) then
      tab_x(:) = interpolation%x(k:k+order-1)
# 192 "num_interpolation_module.spp"
      if(interpolation%parity>0) then
        tab_y(:,:,:) = interpolation%y(:,:,k:k+order-1)
      else
        tab_y(:,:,:) = interpolation%y(:,:,k:k+order-1) * sign(1.0,x)
      end if
# 198 "num_interpolation_module.spp"
    else
      tab_x(-k:order-1) = interpolation%x(0:k+order-1)
      tab_x(0:-k-1)     = - interpolation%x(-k:1:-1)
# 210 "num_interpolation_module.spp"
      tab_y(:,:,-k:order-1) = interpolation%y(:,:,0:k+order-1)
      tab_y(:,:,0:-k-1)     = interpolation%y(:,:,-k:1:-1) * sign(1,interpolation%parity)
# 213 "num_interpolation_module.spp"
    end if
  end if
end subroutine num_interpolation_complex_2_little_table


!@ MANUAL
function num_interpolation_complex_2_calc(interpolation,x,order)
! Returns the interpolated value in the point x, using a order-1 polynomial
  use numrec_module
  type (num_interpolation_complex_2), intent(in) :: interpolation
# 230 "num_interpolation_module.spp"
  complex                                :: num_interpolation_complex_2_calc (interpolation%dim1,interpolation%dim2)
# 232 "num_interpolation_module.spp"
  real,                     intent(in) :: x
  integer,                  intent(in) :: order
  integer :: i1,i2
!@ END MANUAL
  real    :: tab_x(order)
# 244 "num_interpolation_module.spp"
  complex   :: tab_y(interpolation%dim1,interpolation%dim2,order)
# 246 "num_interpolation_module.spp"
  complex :: y,tab_y_tmp(order)
  real  :: y1,y2,dy
  call num_interpolation_complex_2_little_table(interpolation,x,order,tab_x,tab_y)
# 250 "num_interpolation_module.spp"
  do i2 = 1 , interpolation%dim2
# 253 "num_interpolation_module.spp"
  do i1 = 1 , interpolation%dim1
# 262 "num_interpolation_module.spp"
     tab_y_tmp = tab_y(i1,i2,:)
# 268 "num_interpolation_module.spp"
     call numrec_polint(tab_x,real (tab_y_tmp),order,abs(x),y1,dy)
     call numrec_polint(tab_x,aimag(tab_y_tmp),order,abs(x),y2,dy)
     y = cmplx(y1,y2)
# 279 "num_interpolation_module.spp"
     num_interpolation_complex_2_calc(i1,i2) = y
# 281 "num_interpolation_module.spp"

# 283 "num_interpolation_module.spp"
  end do
# 286 "num_interpolation_module.spp"
  end do
# 288 "num_interpolation_module.spp"

end function num_interpolation_complex_2_calc

# 293 "num_interpolation_module.spp"
#else
  contains
#endif

!@ MANUAL
subroutine num_interpolation_init(interpolation,xmin,xmax,delta,parity)
! Initialise the table.
! delta is an excess approximation to the interpoint distance
! (the point are equally spaced and span the interval [xmin,xmax]).
! If parity/=0, then xmin should be 0.0 and xmax should be positive.
  type (num_interpolation), intent(out) :: interpolation
  real,                     intent(in)  :: xmin,xmax,delta
  integer, optional,        intent(in)  :: parity
!@ END MANUAL
  integer :: i
  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1.0,(xmax - xmin))
  allocate(interpolation%x(0:interpolation%n))
  allocate(interpolation%y(0:interpolation%n))
  interpolation%xmin = xmin
  interpolation%xmax = xmax
  do i=0,interpolation%n
    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
  end do
  interpolation%y=0.0
  if(present(parity)) then
    interpolation%parity = parity
  else
    interpolation%parity = 0
  end if
  if(interpolation%parity /= 0 .and. (xmin /= 0.0 .or. xmax<0.0)) then
    WARNING("")
    interpolation%parity = 0
  end if
end subroutine num_interpolation_init

!@ MANUAL
subroutine num_interpolation_destroy(interpolation)
! Destroys the table
  type (num_interpolation), intent(inout) :: interpolation
!@ END MANUAL
  deallocate(interpolation%x)
  deallocate(interpolation%y)
end subroutine num_interpolation_destroy

subroutine num_interpolation_little_table(interpolation,x,order,tab_x,tab_y)
! Private routine to build a small table to be used by the low-level
! interpolation routines.
  use numrec_module
  type (num_interpolation), intent(in)  :: interpolation
  real,                     intent(in)  :: x
  integer,                  intent(in)  :: order
  real,                     intent(out) :: tab_x(0:order-1),tab_y(0:order-1)
  integer :: j,k
  if(interpolation%parity==0) then
    call numrec_locate(interpolation%x,interpolation%n+1,x,j)
    k = min(max(j-(order-1)/2,1),interpolation%n+1+1-order) - 1
    if(k<0 .or. k+order-1>interpolation%n) ERROR("")
    tab_x(:) = interpolation%x(k:k+order-1)
    tab_y(:) = interpolation%y(k:k+order-1)
  else
    call numrec_locate(interpolation%x,interpolation%n+1,abs(x),j)
    k = min(j-(order-1)/2,interpolation%n+1+1-order) - 1
    if(k+order-1>interpolation%n) ERROR("")
    if(k>=0) then
      tab_x(:) = interpolation%x(k:k+order-1)
      if(interpolation%parity>0) then
        tab_y(:) = interpolation%y(k:k+order-1)
      else
        tab_y(:) = interpolation%y(k:k+order-1) * sign(1.0,x)
      end if
    else
      tab_x(-k:order-1) = interpolation%x(0:k+order-1)
      tab_y(-k:order-1) = interpolation%y(0:k+order-1)
      tab_x(0:-k-1)     = - interpolation%x(-k:1:-1)
      tab_y(0:-k-1)     = interpolation%y(-k:1:-1) * sign(1,interpolation%parity)
    end if
  end if
end subroutine num_interpolation_little_table

!@ MANUAL
function num_interpolation_calc(interpolation,x,order)
  use numrec_module
! Returns the interpolated value in the point x, using a order-1 polynomial
  real                                 :: num_interpolation_calc
  type (num_interpolation), intent(in) :: interpolation
  real,                     intent(in) :: x
  integer,                  intent(in) :: order
!@ END MANUAL
  real    :: tab_x(order),tab_y(order)
  real :: y,dy
  call num_interpolation_little_table(interpolation,x,order,tab_x,tab_y)
  call numrec_polint(tab_x,tab_y,order,abs(x),y,dy)
  num_interpolation_calc = y
end function num_interpolation_calc

!@ MANUAL
function num_interpolation_calc_der(interpolation,x,order,ider)
  use numrec_module
! Returns the interpolated value of the ider-th derivative in the point x
! Note: ider=0 => the function not derived (in this case prefer num_interpolation_calc)
  real                                 :: num_interpolation_calc_der
  type (num_interpolation), intent(in) :: interpolation
  real,                     intent(in) :: x
  integer,                  intent(in) :: order
  integer,                  intent(in) :: ider
!@ END MANUAL
  real    :: tab_x(order),tab_y(order)
  real    :: cof(order),pd(0:ider)
  call num_interpolation_little_table(interpolation,x,order,tab_x,tab_y)
  call numrec_polcof(tab_x,tab_y,order,cof)
  call numrec_ddpoly(cof,order,x,pd,ider+1)
  num_interpolation_calc_der = pd(ider)
end function num_interpolation_calc_der

end module num_interpolation_module
