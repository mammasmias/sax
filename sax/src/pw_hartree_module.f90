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
module pw_hartree_module
use pw_basis_module
use pw_struct_module
use pw_wfc_module
use pw_common_module
implicit none
! This module contains the object pw_hartree and its methods
private
public :: pw_hartree,         &
          pw_hartree_init,    &
          pw_hartree_destroy, &
          pw_hartree_write,   &
          pw_hartree_read,    &
          pw_hartree_from_density, &
          pw_hartree_corr
!@ END MANUAL

!@ MANUAL
type pw_hartree
  type (pw_basis) :: basis
  type (pw_wfc)   :: wfc
end type pw_hartree
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_hartree_init(hartree,struct,cutoff)
  type (pw_hartree), intent(out)        :: hartree
  type (pw_struct),  intent(in)         :: struct
  real,              intent(in)         :: cutoff
!@ END MANUAL
  call pw_basis_init(hartree%basis,struct)
  call pw_basis_create(hartree%basis,(/0.0,0.0,0.0/),cutoff)
  call pw_wfc_init(hartree%wfc,hartree%basis)
end subroutine pw_hartree_init

!@ MANUAL
subroutine pw_hartree_destroy(hartree)
  type (pw_hartree),   intent(inout) :: hartree
!@ END MANUAL
  call pw_wfc_destroy(hartree%wfc)
  call pw_basis_destroy(hartree%basis)
end subroutine pw_hartree_destroy

subroutine pw_hartree_write(hartree,unit,name)
  use iotk_module
  type (pw_hartree), intent(in) :: hartree
  integer,        intent(in) :: unit
  character(*),   intent(in) :: name

  character(len=iotk_attlenx) :: attr

  call iotk_write_attr (attr,"type","pw_hartree",first=.true.)
  call iotk_write_begin(unit,trim(name),attr)
  call pw_basis_write(hartree%basis,unit,"iotk",name="basis")
  call pw_wfc_write(hartree%wfc,unit,fmt="iotk",name="wfc")
  call iotk_write_end  (unit,trim(name))
end subroutine pw_hartree_write

subroutine pw_hartree_read(hartree,unit,name)
  use iotk_module
  type (pw_hartree), intent(inout) :: hartree
  integer,        intent(in) :: unit
  character(*),   intent(in) :: name

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype

  call iotk_scan_begin(unit,trim(name),attr)
  call iotk_scan_attr (attr,"type",rtype,default="pw_hartree")
  if(rtype/="pw_hartree") ERROR("")
  call pw_basis_read(hartree%basis,unit,"iotk",name="basis")
  call pw_wfc_set_basis(hartree%wfc,hartree%basis)
  call pw_wfc_read(hartree%wfc,unit,"iotk",name="wfc")
  call iotk_scan_end  (unit,trim(name))
end subroutine pw_hartree_read

!@ MANUAL
subroutine pw_hartree_from_density(hartree,density,coulomb)
  use num_module
  use pw_density_module
  use pw_coulomb_module
! Calcola il potenziale di Hartree dalla densita'
  type(pw_hartree),           intent(inout) :: hartree
  type(pw_density),           intent(in)    :: density
  type(pw_coulomb), optional, intent(in)    :: coulomb
!@ END MANUAL
  type (pw_coulomb) :: coulomb_loc
  type (pw_wfc) :: coulomb_wfc
  if(hartree%basis%npw /= density%basis%npw) ERROR("")
  if(.not. hartree%basis == density%basis) ERROR("")
  call pw_wfc_init(coulomb_wfc,density%basis)
  if(present(coulomb)) then
    call pw_coulomb_get(3,coulomb,coulomb_wfc)
    coulomb_wfc%val(density%basis%index_Gzero) = 0.0
  else
    call pw_coulomb_init(coulomb_loc,density%basis%struct%b)
    call pw_coulomb_get(3,coulomb_loc,coulomb_wfc)
    coulomb_wfc%val(density%basis%index_Gzero) = 0.0
    call pw_coulomb_destroy(coulomb_loc)
  end if
!  call pw_wfc_mul(hartree%wfc,density%wfc,coulomb_wfc)
!  write(0,*) "before hartree density * coulomb" 
  hartree%wfc%val = density%wfc%val * coulomb_wfc%val
!  write(0,*) "after hartree density * coulomb"
  call pw_wfc_destroy(coulomb_wfc)
end subroutine pw_hartree_from_density

!@ MANUAL
subroutine pw_hartree_corr(hartree,density,vcut)
  use num_module
  use pw_density_module
  use coulomb_vcut_module, only : vcut_type, vcut_get
! Calcola il potenziale di Hartree dalla densita'
  type(pw_hartree),           intent(inout) :: hartree
  type(pw_density),           intent(in)    :: density
  type(vcut_type), intent(in)    :: vcut
!@ END MANUAL
  real :: b(3,3), kg(3)
  integer :: ipw
  if(hartree%basis%npw /= density%basis%npw) ERROR("")
  if(.not. hartree%basis == density%basis) ERROR("")
  b = hartree%wfc%basis%struct%b
  do ipw=1, hartree%basis%npw
    kg = num_matmul(b,hartree%wfc%basis%g(:,ipw)+hartree%wfc%basis%k)
    hartree%wfc%val(ipw) = &
      density%wfc%val(ipw) * vcut_get(vcut,kg)
  enddo
end subroutine pw_hartree_corr

end module pw_hartree_module

