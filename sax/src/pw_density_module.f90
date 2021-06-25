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
module pw_density_module
use pw_basis_module
use pw_struct_module
use pw_wfc_module
use pw_common_module
implicit none
! This module contains the object pw_density and its methods
private
public :: pw_density,         &
          pw_density_init,    &
          pw_density_destroy, &
          pw_density_write,   &
          pw_density_read,    &
          pw_density_from_wfc
!@ END MANUAL

!@ MANUAL
type pw_density
  type (pw_basis) :: basis
  type (pw_wfc)   :: wfc
end type pw_density
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_density_init(density,struct,cutoff)
  type (pw_density), intent(out)        :: density
  type (pw_struct),  intent(in)         :: struct
  real,              intent(in)         :: cutoff
!@ END MANUAL
  call pw_basis_init(density%basis,struct)
  call pw_basis_create(density%basis,(/0.0,0.0,0.0/),cutoff)
  call pw_wfc_init(density%wfc,density%basis)
end subroutine pw_density_init

!@ MANUAL
subroutine pw_density_destroy(density)
  type (pw_density),   intent(inout) :: density
!@ END MANUAL
  call pw_wfc_destroy(density%wfc)
  call pw_basis_destroy(density%basis)
end subroutine pw_density_destroy

subroutine pw_density_write(density,unit,name)
  use iotk_module
  type (pw_density), intent(in) :: density
  integer,        intent(in) :: unit
  character(*),   intent(in) :: name

  character(len=iotk_attlenx) :: attr

  call iotk_write_attr (attr,"type","pw_density",first=.true.)
  call iotk_write_begin(unit,trim(name),attr)
  call pw_basis_write(density%basis,unit,"iotk",name="basis")
  call pw_wfc_write(density%wfc,unit,fmt="iotk",name="wfc")
  call iotk_write_end  (unit,trim(name))
end subroutine pw_density_write

subroutine pw_density_read(density,unit,name)
  use iotk_module
  type (pw_density), intent(inout) :: density
  integer,        intent(in) :: unit
  character(*),   intent(in) :: name

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype

  call iotk_scan_begin(unit,trim(name),attr)
  call iotk_scan_attr (attr,"type",rtype,default="pw_density")
  if(rtype/="pw_density") ERROR("")
  call pw_basis_read(density%basis,unit,"iotk",name="basis")
  call pw_wfc_set_basis(density%wfc,density%basis)
  call pw_wfc_read(density%wfc,unit,"iotk",name="wfc")
  call iotk_scan_end  (unit,trim(name))
end subroutine pw_density_read

!@ MANUAL
subroutine pw_density_from_wfc(density,wfc,weights)
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  type (pw_density), intent(inout) :: density
  type (pw_wfc),     intent(in)    :: wfc(:,:)
  real,              intent(in)    :: weights(:,:)
!@ END MANUAL
  type (pw_field) :: rdensity,rdensity_sum
  integer :: n1,n2,i1,i2
  real :: weight
  if(any(shape(wfc)/=shape(weights))) ERROR("")
  n1 = ubound(wfc,1)
  n2 = ubound(wfc,2)
  call pw_field_init(rdensity,density%basis%struct)
  call pw_field_init(rdensity_sum,density%basis%struct)
  call pw_field_set_dim(rdensity_sum,pw_field_dim_from_basis(density%basis), &
                        k=(/0.0,0.0,0.0/),r0=(/0.0,0.0,0.0/))
  do i2=1,n2
    do i1=1,n1
      weight = sqrt(weights(i1,i2) / density%basis%struct%a_omega)
      if(weight == 0.0) cycle
      call pw_field_set_dim(rdensity,pw_field_dim_from_basis(density%basis), &
                        k=wfc(i1,i2)%basis%k,r0=(/0.0,0.0,0.0/))
      call pw_wfc2field(rdensity,wfc(i1,i2))
      call pw_field_scale(rdensity,weight)
      call pw_field_add_density(rdensity_sum,rdensity)
    end do
  end do
  call pw_field2wfc(density%wfc,rdensity_sum)
  call pw_field_destroy(rdensity)
  call pw_field_destroy(rdensity_sum)

end subroutine pw_density_from_wfc

end module pw_density_module

