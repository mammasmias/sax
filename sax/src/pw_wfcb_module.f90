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
module pw_wfcb_module
use pw_basis_module
use pw_struct_module
use pw_wfc_module
use pw_common_module
implicit none
private
public :: pw_wfcb, &
          pw_wfcb_init, &
          pw_wfcb_destroy, &
          pw_wfcb_write, &
          pw_wfcb_read

type pw_wfcb
  type (pw_basis) :: basis
  type (pw_wfc)   :: wfc
end type pw_wfcb

contains

subroutine pw_wfcb_init(wfcb,struct,k,cutoff)
  type (pw_wfcb),   intent(out)        :: wfcb
  type (pw_struct), intent(in)         :: struct
  real,             intent(in)         :: k(3),cutoff

  call pw_basis_init(wfcb%basis,struct)
  call pw_basis_create(wfcb%basis,k,cutoff)
  call pw_wfc_init(wfcb%wfc,wfcb%basis)
end subroutine pw_wfcb_init

subroutine pw_wfcb_destroy(wfcb)
  type (pw_wfcb),   intent(inout) :: wfcb
  call pw_wfc_destroy(wfcb%wfc)
  call pw_basis_destroy(wfcb%basis)
end subroutine pw_wfcb_destroy

subroutine pw_wfcb_write(wfcb,unit,name)
  use iotk_module
  type (pw_wfcb), intent(in) :: wfcb
  integer,        intent(in) :: unit
  character(*),   intent(in) :: name
  
  character(len=iotk_attlenx) :: attr
  
  call iotk_write_attr (attr,"type","pw_wfcb",first=.true.)
  call iotk_write_begin(unit,trim(name),attr)
  call pw_basis_write(wfcb%basis,unit,"iotk",name="basis")
  call pw_wfc_write(wfcb%wfc,unit,fmt="iotk",name="wfc")
  call iotk_write_end  (unit,trim(name))
end subroutine pw_wfcb_write

subroutine pw_wfcb_read(wfcb,unit,name)
  use iotk_module
  type (pw_wfcb), intent(inout) :: wfcb
  integer,        intent(in) :: unit
  character(*),   intent(in) :: name

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype

  call iotk_scan_begin(unit,trim(name),attr)
  call iotk_scan_attr (attr,"type",rtype,default="pw_wfcb")
  if(rtype/="pw_wfcb") ERROR("")
  call pw_basis_read(wfcb%basis,unit,"iotk",name="basis")
  call pw_wfc_set_basis(wfcb%wfc,wfcb%basis)
  call pw_wfc_read(wfcb%wfc,unit,"iotk",name="wfc")
  call iotk_scan_end  (unit,trim(name))
end subroutine pw_wfcb_read



end module pw_wfcb_module

