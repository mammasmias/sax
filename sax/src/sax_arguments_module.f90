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

#if defined(__ABSOFT)
#  define getarg getarg_
#  define iargc  iargc_
#endif

module sax_arguments_module

private

public :: sax_arguments      

contains
      
subroutine sax_arguments(code)
!
use ptk_module, only : ptk_comm
use iotk_module
use tools_module
use num_module
use mp_global, only : npool

! Part of this routine comes from startup.f90 of the quantum-espresso
! distribution Copyright (C) 2001-2004 PWSCF group.

 IMPLICIT NONE
 !
 character (LEN=10), intent(out) :: code
!      
 INTEGER            :: ierr = 0, ilen, nargs, iiarg
 character (LEN=10) :: np     
 INTEGER :: iargc

    ! ... code ?
    !
    nargs  = iargc() 
    !
    DO iiarg = 1, ( nargs - 1 )
       !
       CALL getarg( iiarg, np )
       !
!       IF ( TRIM( np ) == '-gw' .OR. TRIM( np ) == '--gw'.or. &
!              TRIM( np ) == '--GW'.OR. TRIM( np ) == '-GW' ) THEN
!         code = 'gw'
!       ELSEIF  ( TRIM( np ) == '-spectra' .OR. TRIM( np ) == '--spectra'.or. &
!              TRIM( np ) == '--SPECTRA'.OR. TRIM( np ) == '-SPECTRA' ) THEN
!         code = 'spectra'
!       ELSEIF  ( TRIM( np ) == '-bse' .OR. TRIM( np ) == '--bse'.or. &
!              TRIM( np ) == '--BSE'.OR. TRIM( np ) == '-BSE' ) THEN
!         code = 'bse'
!       ELSEIF  ( TRIM( np ) == '-memory' .OR. TRIM( np ) == '--memory'.or. &
!              TRIM( np ) == '--MEMORY'.OR. TRIM( np ) == '-MEMORY' ) THEN
!         code = 'memory'
!       ELSEIF  ( TRIM( np ) == '-pptools' .OR. TRIM( np ) == '--pptools'.or. &
!              TRIM( np ) == '--PPTOOLS'.OR. TRIM( np ) == '-PPTOOLS' ) THEN
!         code = 'pptools'
!       ENDIF  

       IF ( TRIM( np ) == '-code' .OR. TRIM( np ) == '--code'.or. &
              TRIM( np ) == '--CODE'.OR. TRIM( np ) == '-CODE' ) THEN
         !
         CALL getarg( ( iiarg + 1 ), np )  
         READ( np, * ) code 
         !
       END IF
       !
    END DO
    !
    !
    ! ... How many pools ?
    !
    npool = 1
    nargs = iargc() 
    !
    DO iiarg = 1, ( nargs - 1 )
       !
       CALL getarg( iiarg, np )
       !
       IF ( TRIM( np ) == '-npool' .OR. TRIM( np ) == '-npools'.OR. &
           TRIM( np ) == '--npool'.OR. TRIM( np ) == '--npools') THEN
         !
         CALL getarg( ( iiarg + 1 ), np )  
         READ( np, * ) npool  
         !
       END IF
       !
    END DO
    !
    !          
 RETURN
 !
end subroutine sax_arguments

end module sax_arguments_module    
