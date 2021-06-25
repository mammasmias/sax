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

module pw_fft_module

use pw_field_module
use pw_basis_module
use pw_wfc_module

implicit none
private
public :: pw_wfc2field, pw_field2wfc

contains

subroutine pw_wfc2field(field,wfc)
  use num_module
  use ffti_module, only : ffti_3d
  type(pw_field), intent(inout) :: field
  type(pw_wfc),   intent(in)    :: wfc
  real    :: r(3)
  real    :: g_real(3)
  integer :: g(3)
  complex :: r_factor1(wfc%basis%gmin(1):wfc%basis%gmax(1))
  complex :: r_factor2(wfc%basis%gmin(2):wfc%basis%gmax(2))
  complex :: r_factor3(wfc%basis%gmin(3):wfc%basis%gmax(3))


  if(.not.associated(wfc%basis%struct,field%struct)) ERROR("")
  r      = wfc%basis%r0 - field%r0
  g_real = wfc%basis%k - field%k
  g      = nint(g_real)
  if(any(abs(g_real-g)>1.0d-6)) ERROR("")
  call pw_fft_r_factors(r_factor1,r_factor2,r_factor3,r,g,wfc%basis%gmin,wfc%basis%gmax)
  call pw_wfc2field_tool(field%val,field%dim,field%str2,field%str3, &
                         wfc%val,wfc%basis%g,wfc%basis%npw,wfc%basis%conjg, &
                         r_factor1,r_factor2,r_factor3,wfc%basis%gmin,wfc%basis%gmax,g)
  if(field%str2/=field%dim(1)) ERROR("")
  if(field%str3/=field%dim(1)*field%dim(2)) ERROR("")
  call ffti_3d(field%val,field%dim(1),field%dim(2),field%dim(3),field%dim(1),field%dim(2),field%dim(3),+1)
end subroutine pw_wfc2field

subroutine pw_field2wfc(wfc,field)
  use num_module
  use ffti_module, only : ffti_3d
  type(pw_wfc),   intent(inout)    :: wfc
  type(pw_field), intent(in) :: field
  complex :: field_val(size(field%val))
  real    :: r(3)
  real    :: g_real(3)
  integer :: g(3)
  complex :: r_factor1(wfc%basis%gmin(1):wfc%basis%gmax(1))
  complex :: r_factor2(wfc%basis%gmin(2):wfc%basis%gmax(2))
  complex :: r_factor3(wfc%basis%gmin(3):wfc%basis%gmax(3))
  if(.not.associated(wfc%basis%struct,field%struct)) ERROR("")
  r      = field%r0 - wfc%basis%r0
  g_real = wfc%basis%k - field%k
  g      = nint(g_real)
  if(any(abs(g_real-g)>1.0d-6)) ERROR("")
  field_val=field%val
  if(field%str2/=field%dim(1)) ERROR("")
  if(field%str3/=field%dim(1)*field%dim(2)) ERROR("")
  call ffti_3d(field_val,field%dim(1),field%dim(2),field%dim(3),field%dim(1),field%dim(2),field%dim(3),-1)
  call pw_fft_r_factors(r_factor1,r_factor2,r_factor3,r,g,wfc%basis%gmin,wfc%basis%gmax)
  call pw_field2wfc_tool(field_val,field%dim,field%str2,field%str3, &
                         wfc%val,wfc%basis%g,wfc%basis%npw,wfc%basis%conjg, &
                         r_factor1,r_factor2,r_factor3,wfc%basis%gmin,wfc%basis%gmax,g)
 
end subroutine pw_field2wfc

end module pw_fft_module
