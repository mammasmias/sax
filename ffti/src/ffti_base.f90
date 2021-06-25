! Copyright (C) 2005 Giovanni Bussi
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .

#include "ffti_fdefs.h"
module ffti_base
  implicit none
  public
  integer, parameter :: dbl = selected_real_kind(14,200)

! CONFIGURATION
        INTEGER, PARAMETER :: ndims = 3
        !   ndims   Number of different FFT tables that the module
        !           could keep into memory without reinitialization
        INTEGER, parameter :: library_fftw = 0
        INTEGER, parameter :: library_essl = 1
        INTEGER, parameter :: library_scsl = 2
        INTEGER, parameter :: library_complib = 3
        INTEGER, parameter :: library_singleton = 4

        INTEGER, save :: library_id = -1

! ESSL
        INTEGER, PARAMETER :: essl_nfftx = 2049
        INTEGER, PARAMETER :: essl_lwork = 20000 + ( 2 * essl_nfftx + 256 ) * 64 + 3 * essl_nfftx
        INTEGER, PARAMETER :: essl_ltabl = 20000 + 3 * essl_nfftx
        REAL (dbl) :: essl_fw_table( essl_ltabl, 3, ndims )
        REAL (dbl) :: essl_bw_table( essl_ltabl, 3, ndims )
        REAL (dbl) :: essl_work( essl_lwork )
        !   see the ESSL manual ( DCFT ) for the workspace and table lenght formulas

! SCSL
        INTEGER, PARAMETER :: scsl_nfftx = 1025
        INTEGER, PARAMETER :: scsl_lwork = 2 * scsl_nfftx
        INTEGER, PARAMETER :: scsl_ltabl = 2 * scsl_nfftx + 256
        COMPLEX (dbl) :: scsl_work(scsl_lwork)
        REAL    (dbl) :: scsl_tablez(scsl_ltabl,ndims)
        REAL    (dbl) :: scsl_tablex(scsl_ltabl,ndims)
        REAL    (dbl) :: scsl_tabley(scsl_ltabl,ndims)
        REAL (dbl)    :: scsl_DUMMY
        INTEGER       :: scsl_isys(0:1)

! COMPLIB
        INTEGER, PARAMETER :: complib_nfftx = 1025
        INTEGER, PARAMETER :: complib_lwork = 20 * complib_nfftx
        INTEGER, PARAMETER :: complib_ltabl = 4 * complib_nfftx
        REAL (dbl) :: complib_work(complib_lwork)
        REAL (dbl) :: complib_tablez(complib_ltabl,ndims)
        REAL (dbl) :: complib_tablex(complib_ltabl,ndims)
        REAL (dbl) :: complib_tabley(complib_ltabl,ndims)

! FFTW
        INTEGER, PARAMETER :: fftw_nfftx = 1025
        INTEGER, PARAMETER :: fftw_lwork = 20 * fftw_nfftx
        INTEGER, PARAMETER :: fftw_ltabl = 4 * fftw_nfftx
        !   C_POINTER is defined in include/ffti_fdefs.h
        !   for 32bit executables, C_POINTER is integer(4)
        !   for 64bit executables, C_POINTER is integer(8)
!        C_POINTER :: fftw_fw_plan( 3, ndims ) = 0
!        C_POINTER :: fftw_bw_plan( 3, ndims ) = 0
        !   Pointers to the "C" structures containing FFT factors ( PLAN )

! SINGLETON
        INTEGER, PARAMETER :: singleton_nfftx = 1025


end module ffti_base
