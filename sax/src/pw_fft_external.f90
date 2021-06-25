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

subroutine pw_fft_r_factors(r_factor1,r_factor2,r_factor3,r,g,gmin,gmax)
  use num_module
  implicit none
  integer, intent(in)  :: g(3),gmin(3),gmax(3)
  complex, intent(out) :: r_factor1(gmin(1):gmax(1)), &
                          r_factor2(gmin(2):gmax(2)), &
                          r_factor3(gmin(3):gmax(3))
  real,    intent(in)  :: r(3)
  integer   :: i1,i2,i3
  complex   :: factor(3)
  factor   = exp(-2.0*num_pi*(0.0,1.0)*r)
  do i1 = gmin(1),gmax(1)
    r_factor1(i1) = factor(1)**(i1+g(1))
  end do
  do i2 = gmin(2),gmax(2)
    r_factor2(i2) = factor(2)**(i2+g(2))
  end do
  do i3 = gmin(3),gmax(3)
    r_factor3(i3) = factor(3)**(i3+g(3))
  end do
end subroutine pw_fft_r_factors

subroutine pw_wfc2field_tool(field_val,dim,str2,str3, &
&                            wfc_val,basis_g,npw,lconjg, &
&                            r_factor1,r_factor2,r_factor3,gmin,gmax,g)
  use pw_basis_module
  use pw_common_module
  implicit none
  complex,                    intent(out) :: field_val(*)
  integer,                    intent(in)  :: dim(3),str2,str3
  complex,                    intent(in)  :: wfc_val(*)
  integer,                    intent(in)  :: npw
  integer(pw_basis_int_kind), intent(in)  :: basis_g(3,npw)
  logical,                    intent(in)  :: lconjg
  integer,                    intent(in)  :: gmin(3),gmax(3)
  complex,                    intent(in)  :: r_factor1(gmin(1):gmax(1))
  complex,                    intent(in)  :: r_factor2(gmin(2):gmax(2))
  complex,                    intent(in)  :: r_factor3(gmin(3):gmax(3))
  integer,                    intent(in)  :: g(3)
  integer ipw,pos,vec(3)
  complex :: add,r_factor
  field_val(1:dim(1)-1+(dim(2)-1)*str2+(dim(3)-1)*str3+1)=(0.0,0.0)
  do ipw = 1,npw
    vec(1) = int(basis_g(1,ipw))
    vec(2) = int(basis_g(2,ipw))
    vec(3) = int(basis_g(3,ipw))
    r_factor = r_factor1(vec(1)) * r_factor2(vec(2)) * r_factor3(vec(3))
    vec(1) = modulo(vec(1)+g(1),dim(1))
    vec(2) = modulo(vec(2)+g(2),dim(2))
    vec(3) = modulo(vec(3)+g(3),dim(3))
    pos = (vec(1)+vec(2)*str2+vec(3)*str3)+1
    add = wfc_val(ipw) * r_factor
    if(lconjg) then
      field_val(pos) = field_val(pos) + conjg(add)
    else
      field_val(pos) = field_val(pos) + add
    end if
  end do
end subroutine pw_wfc2field_tool

subroutine pw_field2wfc_tool(field_val,dim,str2,str3, &
&                            wfc_val,basis_g,npw,lconjg, &
&                            r_factor1,r_factor2,r_factor3,gmin,gmax,g)
  use pw_basis_module
  use pw_common_module
  implicit none
  complex,                    intent(in)  :: field_val(*)
  integer,                    intent(in)  :: dim(3),str2,str3
  complex,                    intent(out) :: wfc_val(*)
  integer,                    intent(in)  :: npw
  integer(pw_basis_int_kind), intent(in)  :: basis_g(3,npw)
  logical,                    intent(in)  :: lconjg
  integer,                    intent(in)  :: gmin(3),gmax(3)
  complex,                    intent(in)  :: r_factor1(gmin(1):gmax(1))
  complex,                    intent(in)  :: r_factor2(gmin(2):gmax(2))
  complex,                    intent(in)  :: r_factor3(gmin(3):gmax(3))
  integer,                    intent(in)  :: g(3)
  integer ipw,pos,vec(3)
  complex add,r_factor
  wfc_val(1:npw) = (0.0,0.0)
  do ipw = 1,npw
    vec(1) = int(basis_g(1,ipw))
    vec(2) = int(basis_g(2,ipw))
    vec(3) = int(basis_g(3,ipw))
    r_factor = r_factor1(vec(1)) * r_factor2(vec(2)) * r_factor3(vec(3))
    vec(1) = modulo(vec(1)+g(1),dim(1))
    vec(2) = modulo(vec(2)+g(2),dim(2))
    vec(3) = modulo(vec(3)+g(3),dim(3))
    pos = (vec(1)+vec(2)*str2+vec(3)*str3)+1
    add = field_val(pos) * r_factor
    if(lconjg) then
      wfc_val(ipw) = wfc_val(ipw) + conjg(add)
    else
      wfc_val(ipw) = wfc_val(ipw) + add
    end if
  end do
end subroutine pw_field2wfc_tool

