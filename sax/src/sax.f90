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
#include "sax_version.h"
program sax
!
use ptk_module
use iotk_module
use tools_module
use num_module
use sax_arguments_module
use mp_global, only : npool

implicit none

character(LEN=10) :: code
logical :: ionode
integer :: rank,ierr,npe

integer :: iostat

call ptk_init(verbosity=1)
call ptk_comm_rank(ptk_comm_world,rank=rank)
call ptk_comm_size(ptk_comm_world,size=npe)

ionode = .false.
if(rank==0) ionode = .true.

if(ionode) then
   call sax_arguments(code)
   write(0,*) "code= ", trim(code)
   write(0,*) "npool= ", npool
endif  

call ptk_bcast(code,0,ptk_comm_world)
call ptk_bcast(npool,0,ptk_comm_world)

if(trim(code)=='gw'.or.trim(code)=='GW') then
  call gw_program()
  elseif(trim(code)=='bse'.or.trim(code)=='BSE') then
  call bse_program()
elseif(trim(code)=='spectra'.or.trim(code)=='SPECTRA') then
  call spectra_program(npool)
elseif(trim(code)=='pptools'.or.trim(code)=='PPTOOLS') then
  call pptools_program(npool)
elseif(trim(code)=='memory'.or.trim(code)=='MEMORY') then
  call memory_program(npool)
endif  

call ptk_finalize

end program sax
