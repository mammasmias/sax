!
! Copyright (C) 2004 Andrea Ferretti
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

PROGRAM test1
!
! performs simple tests on kind definitions for real numbers
!
   USE lasi_module
   USE lasi_kinds, ONLY : sgl_mod => sgl, dbl_mod => dbl
   IMPLICIT NONE

   INTEGER, PARAMETER     :: stdout = 6

   INTEGER, PARAMETER     :: frt = SELECTED_REAL_KIND(18,300)
   INTEGER, PARAMETER     :: dbl = SELECTED_REAL_KIND(14,200)
   INTEGER, PARAMETER     :: sgl = SELECTED_REAL_KIND(6,30)

   REAL                   :: rtmp = 0.0
   DOUBLE PRECISION       :: dtmp = 0.0
   COMPLEX                :: ctmp = 0.0
   COMPLEX*16             :: ztmp = 0.0

!---------------------------------------

   WRITE(stdout,"('Simple test on KINDS',/)")
   WRITE(stdout,"('defined kinds:')")
   WRITE(stdout,"(2x,'QUADRUPLE prec. KIND = ',i3)") frt
   WRITE(stdout,"(2x,'   DOUBLE prec. KIND = ',i3)") dbl
   WRITE(stdout,"(2x,'   SINGLE prec. KIND = ',i3)") sgl
   WRITE(stdout,"()")

   IF ( sgl_mod /= sgl ) &
      WRITE(stdout,"(2x,'ERROR: sgl in LASI different from expectation: ',i3)") sgl_mod
   IF ( dbl_mod /= dbl ) &
      WRITE(stdout,"(2x,'ERROR: dbl in LASI different from expectation: ',i3)") dbl_mod
   IF ( KIND(rtmp) /= KIND(ctmp) ) &
      WRITE(stdout,"(2x,'ERROR: RTMP and CTMP have diff kinds: respectively ',2i3)") &
            KIND(rtmp) /= KIND(ctmp)
   IF ( KIND(dtmp) /= KIND(ztmp) ) &
      WRITE(stdout,"(2x,'ERROR: DTMP and ZTMP have diff kinds: respectively ',2i3)") &
            KIND(dtmp) /= KIND(ztmp)
      
END PROGRAM


