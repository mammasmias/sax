! Copyright (C) 2005 Giovanni Bussi
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .

#ifndef __FFTW_SKIP
#ifndef __FFTW
#define __FFTW
#endif
#endif
#include "ffti_fdefs.h"

#if defined __ESSL
#elif defined __SCSL
#elif defined __COMPLIB
#elif defined __FFTW
#else
# error
#endif


!*****************************************
SUBROUTINE ffti_check_installed_x
  !*****************************************
  use ffti_base
  use ffti_interface
  implicit none
  if(library_id<0) then
#if defined __ESSL
    library_id = library_essl
#elif defined __SCSL
    library_id = library_scsl
#elif defined __COMPLIB
    library_id = library_complib
#elif defined __FFTW
    library_id = library_fftw
#else
    library_id = library_singleton
#endif
  end if
END SUBROUTINE ffti_check_installed_x


!*****************************************
SUBROUTINE ffti_set_x(library)
  !*****************************************
  use ffti_base
  use ffti_interface
  implicit none
  character(len=*), intent(in) :: library
  call ffti_check_installed
  select case(trim(library))
#ifdef __ESSL
  case("ESSL","essl")
    library_id = library_essl
#endif
#ifdef __SCSL
  case("SCSL","scsl")
    library_id = library_scsl
#endif
#ifdef __COMPLIB
  case("COMPLIB","complib")
    library_id = library_complib
#endif
#ifdef __FFTW
  case("FFTW","fftw")
    library_id = library_fftw
#endif
  case("SINGLETON","singleton")
    library_id = library_singleton
  case default
    continue
  end select
END SUBROUTINE ffti_set_x


!*****************************************
SUBROUTINE ffti_which_x(library)
  !*****************************************
  use ffti_base
  use ffti_interface
  implicit none
  character(len=*), intent(out) :: library
  call ffti_check_installed
  select case(library_id)
  case(library_fftw)
    library = "FFTW"
  case(library_essl)
    library = "ESSL"
  case(library_scsl)
    library = "SCSL"
  case(library_complib)
    library = "COMPLIB"
  case(library_singleton)
    library = "SINGLETON"
  case default
    library = ""
  end select
END SUBROUTINE ffti_which_x


!*****************************************
function ffti_good_dimension_x (n)
  !*****************************************
  use ffti_base
  !
  ! Determines the optimal maximum dimensions of fft arrays
  ! Useful on some machines to avoid memory conflicts
  !
  implicit none
  integer :: ffti_good_dimension_x
  integer, intent(in) :: n
  integer :: nx
  ! this is the default: max dimension = fft dimension
  nx = n
#if defined(__ESSL)
if(library_id == library_essl) then
  if ( n==8 .or. n==16 .or. n==32 .or. n==64 .or. n==128 .or. n==256) &
       nx = n + 1
end if
#endif
  ffti_good_dimension_x = nx
end function ffti_good_dimension_x


!*****************************************
function ffti_allowed_x (nr)
  !*****************************************
  ! find if the fft dimension is a good one
  ! a "bad one" is either not implemented (as on IBM with ESSL)
  ! or implemented but with awful performances (most other cases)
  use ffti_base
  use ffti_interface
  implicit none
  integer :: nr
  logical :: ffti_allowed_x
  integer :: pwr (5)
  integer :: mr, i, fac, p, maxpwr
  integer :: factors( 5 ) = (/ 2, 3, 5, 7, 11 /)


  call ffti_check_installed

  ! find the factors of the fft dimension

  mr  = nr
  pwr = 0
  factors_loop: do i = 1, 5
     fac = factors (i)
     maxpwr = NINT ( LOG( REAL (mr) ) / LOG( REAL (fac) ) ) + 1
     do p = 1, maxpwr
        if ( mr == 1 ) EXIT factors_loop
        if ( MOD (mr, fac) == 0 ) then
           mr = mr / fac
           pwr (i) = pwr (i) + 1
        endif
     enddo
  end do factors_loop

  IF ( nr /= ( mr * 2**pwr (1) * 3**pwr (2) * 5**pwr (3) * 7**pwr (4) * 11**pwr (5) ) ) &
     CALL errore ('ffti_allowed', 'what ?!?', 1 )

  if ( mr /= 1 ) then

     ! fft dimension contains factors > 11 : no good in any case

     ffti_allowed_x = .false.

  else

if(library_id == library_essl) then
     ! IBM machines with essl libraries

     ffti_allowed_x =  ( pwr(1) >= 1 ) .and. ( pwr(2) <= 2 ) .and. ( pwr(3) <= 1 ) .and. &
                ( pwr(4) <= 1 ) .and. ( pwr(5) <= 1 ) .and. &
                ( ( (pwr(2) == 0 ) .and. ( pwr(3) + pwr(4) + pwr(5) ) <= 2 ) .or. &
                  ( (pwr(2) /= 0 ) .and. ( pwr(3) + pwr(4) + pwr(5) ) <= 1 ) )
     ! fftw and all other cases: no factors 7 and 11
else
     ffti_allowed_x = ( ( pwr(4) == 0 ) .and. ( pwr(5) == 0 ) )
end if
!    ffti_allowed_x = ((pwr(2)==0).and.(pwr(3)==0).and.(pwr(4)==0).and.(pwr(5)==0))
  endif

end function ffti_allowed_x


!
!=----------------------------------------------------------------------=!
!
!
!
!         FFT along "z" 
!
!
!
!=----------------------------------------------------------------------=!
!

!*****************************************
   SUBROUTINE ffti_1dz_x(c, nsl, nz, ldc, sgn, cout)
   !*****************************************
   USE ffti_base
   USE ffti_interface
   USE ffti_singleton_module
   IMPLICIT NONE
   !
   !     driver routine for m 1d complex fft's 
   !     nx=n+1 is allowed (in order to avoid memory conflicts)
   !     A separate initialization is stored each combination of input sizes
   !     NOTA BENE: the output is in cout !

     INTEGER, INTENT(IN) :: sgn
     INTEGER, INTENT(IN) :: nsl, nz, ldc
     COMPLEX (dbl) :: c(:), cout(:) 
     REAL(dbl)  :: tscale
     INTEGER    :: i, j, err, idir, ip, isign

#ifdef __FFTW
     C_POINTER, save :: fftw_fw_plan(3,ndims) = 0
     C_POINTER, save :: fftw_bw_plan(3,ndims) = 0
     INTEGER, SAVE :: fftw_icurrent = 1
     INTEGER, SAVE :: fftw_zdims(3,ndims) = -1
#endif

#ifdef __COMPLIB
     real(kind=8), save :: complib_table( 3 * complib_ltabl,  ndims )
     INTEGER, SAVE :: complib_icurrent = 1
     INTEGER, SAVE :: complib_zdims(3,ndims) = -1
#endif

#ifdef __SCSL
     real(kind=8), save :: scsl_table( 3 * scsl_ltabl,  ndims )
     INTEGER, SAVE :: scsl_icurrent = 1
     INTEGER, SAVE :: scsl_zdims(3,ndims) = -1
#endif


     IF( nsl < 0 ) CALL errore("ffti_1dz"," nsl out of range ", -nsl)

#if defined __SCSL
      scsl_isys(0) = 1
#endif

     isign = -sgn

     SELECT CASE(library_id)

#if defined __FFTW
     CASE(library_fftw)

        !
        !   Here initialize table only if necessary
        ip = -1
        DO i = 1, ndims
           !
           !   first check if there is already a table initialized
           !   for this combination of parameters
           IF( ALL( (/nz,nsl,ldc/) == fftw_zdims(:,i) ) ) THEN
              ip = i
              EXIT
           ENDIF
        ENDDO

        IF( ip == -1 ) THEN
           !
           !   initialize a new table
           IF( fftw_fw_plan( 3, fftw_icurrent) /= 0 ) &
              CALL FFTI_DESTROY_PLAN_1D( fftw_fw_plan( 3, fftw_icurrent) )
           IF( fftw_bw_plan( 3, fftw_icurrent) /= 0 ) &
              CALL FFTI_DESTROY_PLAN_1D( fftw_bw_plan( 3, fftw_icurrent) )
           idir = -1; CALL FFTI_CREATE_PLAN_1D( fftw_fw_plan( 3, fftw_icurrent), nz, idir) 
           idir =  1; CALL FFTI_CREATE_PLAN_1D( fftw_bw_plan( 3, fftw_icurrent), nz, idir) 
           !
           fftw_zdims(:,fftw_icurrent) = (/nz,nsl,ldc/)
           ip = fftw_icurrent
           fftw_icurrent = MOD( fftw_icurrent, ndims ) + 1
           !
        ENDIF

#elif defined __COMPLIB
     CASE(library_complib)

        !
        !   Here initialize table only if necessary
        ip = -1
        DO i = 1, ndims
           !
           !   first check if there is already a table initialized
           !   for this combination of parameters
           IF( ALL( (/nz,nsl,ldc/) == complib_zdims(:,i) ) ) THEN
              ip = i
              EXIT
           ENDIF
        ENDDO

        IF( ip == -1 ) THEN
           !
           !   initialize a new table
           CALL ZFFT1DI( nz, complib_tablez(1,complib_icurrent) )
           !
           complib_zdims(:,complib_icurrent) = (/nz,nsl,ldc/)
           ip = complib_icurrent
           complib_icurrent = MOD( complib_icurrent, ndims ) + 1
           !
        ENDIF

#elif defined __SCSL
     CASE(library_scsl)

        !
        !   Here initialize table only if necessary
        ip = -1
        DO i = 1, ndims
           !
           !   first check if there is already a table initialized
           !   for this combination of parameters
           IF( ALL( (/nz,nsl,ldc/) == scsl_zdims(:,i) ) ) THEN
              ip = i
              EXIT
           ENDIF
        ENDDO

        IF( ip == -1 ) THEN
           !
           !   initialize a new table
           CALL ZZFFTM (0, nz, 0, 0.0D0, scsl_DUMMY, 1, scsl_DUMMY, 1, &
                        scsl_tablez(1,scsl_icurrent), scsl_DUMMY, scsl_isys)
           !
           scsl_zdims(:,scsl_icurrent) = (/nz,nsl,ldc/)
           ip = scsl_icurrent
           scsl_icurrent = MOD( scsl_icurrent, ndims ) + 1
           !
        ENDIF


#elif defined __ESSL
     CASE(library_ESSL)
        !
        !   Here initialize table only if necessary
        ip = -1
        DO i = 1, ndims
           !
           !   first check if there is already a table initialized
           !   for this combination of parameters
           IF( ALL( (/nz,nsl,ldc/) == essl_zdims(:,i) ) ) THEN
              ip = i
              EXIT
           ENDIF
        ENDDO

        IF( ip == -1 ) THEN
           !
           !   initialize a new table
           tscale = 1.0d0 / nz
           CALL DCFT ( 1, c(1), 1, ldc, cout(1), 1, ldc, nz, nsl,  1, &
                tscale, essl_fw_table(1, 3, essl_icurrent), essl_ltabl, &
                essl_work(1), essl_lwork)
           CALL DCFT ( 1, c(1), 1, ldc, cout(1), 1, ldc, nz, nsl, -1, &
                1.0d0, essel_bw_table(1, 3, essl_icurrent), essl_ltabl, &
                essl_work(1), essl_lwork)
           !
           essl_zdims(:,essl_icurrent) = (/nz,nsl,ldc/)
           ip = essl_icurrent
           essl_icurrent = MOD( essl_icurrent, ndims ) + 1
           !
        ENDIF

#else 

# error
! no scalar driver specified

#endif
     END SELECT


     !
     !   Now perform the FFTs using machine specific drivers
     !
     SELECT CASE (library_id)

#if defined __FFTW
     CASE(library_fftw)

        IF (isign > 0) THEN
          tscale = 1.0d0 / nz
          CALL FFTI_Z_STICK(fftw_fw_plan( 3, ip), c(1), ldc, nsl)
          CALL ZDSCAL( ldc * nsl, tscale, c(1), 1)
        ELSE IF (isign < 0) THEN
          CALL FFTI_Z_STICK(fftw_bw_plan( 3, ip), c(1), ldc, nsl)
        ENDIF

#elif defined __COMPLIB
     CASE(library_complib)

        IF (isign /= 0) THEN
          IF( isign < 0 ) idir = +1
          IF( isign > 0 ) idir = -1
          CALL zfftm1d( idir, nz, nsl, c(1), 1, ldc, complib_tablez(1,ip) )
          IF (isign > 0) THEN
            tscale = 1.0d0 / nz
            CALL ZDSCAL( ldc * nsl, tscale, c(1), 1)
          ENDIF
        ENDIF

#elif defined __SCSL
     CASE(library_scsl)

        IF ( isign /= 0 ) THEN    
           IF ( isign > 0 ) THEN
              idir   = -1
              tscale = 1.0d0 / nz
           ELSE IF ( isign < 0 ) THEN
              idir   = 1
              tscale = 1.0d0
           END IF
           CALL ZZFFTM (idir, nz, nsl, tscale, c(1), ldc, cout(1), ldc,    &
                       scsl_tablez(1,ip), scsl_work, scsl_isys)
        END IF

#elif defined __ESSL
     CASE(library_essl)

        IF( isign > 0 ) THEN
          tscale = 1.0d0 / nz
          idir   = 1
          CALL DCFT (0, c(1), 1, ldc, cout(1), 1, ldc, nz, nsl, idir, &
             tscale, essl_fw_table(1, 3, ip), essl_ltabl, essl_work, essl_lwork)
        ELSE IF( isign < 0 ) THEN
          idir   = -1
          tscale = 1.0d0
          CALL DCFT (0, c(1), 1, ldc, cout(1), 1, ldc, nz, nsl, idir, &
             tscale, essl_bw_table(1, 3, ip), essl_ltabl, essl_work, essl_lwork)
        END IF

#else 

# error

#endif
     END SELECT


#if defined __FFTW || defined __COMPLIB
     cout( 1 : ldc * nsl ) = c( 1 : ldc * nsl )
#endif

     RETURN
END SUBROUTINE ffti_1dz_x


!
!=----------------------------------------------------------------------=!
!
!
!
!         3D scalar FFTs
!
!
!
!=----------------------------------------------------------------------=!
!

!*****************************************
   SUBROUTINE ffti_3d_x( f, nr1, nr2, nr3, nr1x, nr2x, nr3x, sgn )
   !*****************************************

     USE ffti_base
     USE ffti_interface
     USE ffti_singleton_module
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: nr1, nr2, nr3, nr1x, nr2x, nr3x, sgn 
     COMPLEX (dbl) :: f(:)
     INTEGER :: i, k, j, err, idir, ip, isign
     REAL(dbl) :: tscale

#ifdef __FFTW
     C_POINTER, save :: fftw_fw_plan(ndims) = 0
     C_POINTER, save :: fftw_bw_plan(ndims) = 0
     INTEGER, SAVE :: fftw_icurrent = 1
     INTEGER, SAVE :: fftw_dims(3,ndims) = -1
#endif

#ifdef __COMPLIB
     real(kind=8), save :: complib_table( 3 * complib_ltabl,  ndims )
     INTEGER, SAVE :: complib_icurrent = 1
     INTEGER, SAVE :: complib_dims(3,ndims) = -1
#endif

#ifdef __SCSL
     real(kind=8), save :: scsl_table( 3 * scsl_ltabl,  ndims )
     INTEGER, SAVE :: scsl_icurrent = 1
     INTEGER, SAVE :: scsl_dims(3,ndims) = -1
#endif

     call ffti_check_installed

     isign = -sgn

     select case(library_id)
#if defined __FFTW
     case(library_fftw)

     ip = -1
     DO i = 1, ndims
       IF ( all ( (/nr1,nr2,nr3/) == fftw_dims(:,i) ) ) THEN
         ip = i
         EXIT
       ENDIF
     END DO
     IF (ip == -1) THEN
       IF ( nr1 /= nr1x .or. nr2 /= nr2x .or. nr3 /= nr3x ) &
         call errore('ffti_3d','not implemented',1)

       IF( fftw_fw_plan(fftw_icurrent) /= 0 ) &
           CALL FFTI_DESTROY_PLAN_3D( fftw_fw_plan(fftw_icurrent) )
       IF( fftw_bw_plan(fftw_icurrent) /= 0 ) &
           CALL FFTI_DESTROY_PLAN_3D( fftw_bw_plan(fftw_icurrent) )
       idir = -1; CALL FFTI_CREATE_PLAN_3D( fftw_fw_plan(fftw_icurrent), nr1, nr2, nr3, idir)
       idir =  1; CALL FFTI_CREATE_PLAN_3D( fftw_bw_plan(fftw_icurrent), nr1, nr2, nr3, idir)
       fftw_dims(:,fftw_icurrent) = (/nr1,nr2,nr3/)
       ip = fftw_icurrent
       fftw_icurrent = MOD( fftw_icurrent, ndims ) + 1
     END IF
     IF( isign > 0 ) THEN
       call FFTI_INPLACE_DRV_3D( fftw_fw_plan(ip), 1, f(1), 1, 1 )
       tscale = 1.0d0 / DBLE( nr1 * nr2 * nr3 )
       call ZDSCAL( nr1 * nr2 * nr3, tscale, f(1), 1)
     ELSE IF( isign < 0 ) THEN
       call FFTI_INPLACE_DRV_3D( fftw_bw_plan(ip), 1, f(1), 1, 1 )
     END IF
#endif

#if defined __COMPLIB
     case(library_complib)

     ip = -1
     DO i = 1, ndims
       IF ( all ( (/nr1,nr2,nr3/) == complib_dims(:,i) ) ) THEN
         ip = i
         EXIT
       ENDIF
     END DO
     IF (ip == -1) THEN
       CALL zfft3di( nr1, nr2, nr3, complib_table(1,complib_icurrent) )
       complib_dims(:,complib_icurrent) = (/nr1,nr2,nr3/)
       ip = complib_icurrent
       complib_icurrent = MOD( complib_icurrent, ndims ) + 1
     END IF
     IF( isign > 0 ) idir = -1
     IF( isign < 0 ) idir = +1
     IF( isign /= 0 ) &
       CALL zfft3d( idir, nr1, nr2, nr3, f(1), nr1x, nr2x, complib_table(1,ip) )
     IF( isign > 0 ) THEN
       tscale = 1.0d0 / DBLE( nr1 * nr2 * nr3 )
       call ZDSCAL( nr1x * nr2x * nr3x, tscale, f(1), 1)
     END IF
end if
#endif

#if defined __SCSL
     case(library_scsl)

      scsl_isys(0) = 1
     ip = -1
     DO i = 1, ndims
       IF ( all ( (/nr1,nr2,nr3/) == scsl_dims(:,i) ) ) THEN
         ip = i
         EXIT
       ENDIF
     END DO
     IF (ip == -1) THEN
       CALL zzfft3d (0, nr1, nr2, nr3, 0.0D0, scsl_DUMMY, 1, 1, scsl_DUMMY, 1, 1, &
                     scsl_table(1, scsl_icurrent), scsl_work(1), scsl_isys)
       scsl_dims(:,scsl_icurrent) = (/nr1,nr2,nr3/)
       ip = scsl_icurrent
       scsl_icurrent = MOD( scsl_icurrent, ndims ) + 1
     END IF
     IF ( isign /= 0 ) THEN
        IF ( isign > 0 ) THEN
           idir = -1
           tscale = 1.0D0 / DBLE( nr1 * nr2 * nr3 )
        ELSE IF ( isign < 0 ) THEN
           idir = 1
           tscale = 1.0D0
        END IF
        CALL ZZFFT3D ( idir, nr1, nr2, nr3, tscale, f(1), nr1x, nr2x,   &
                       f(1), nr1x, nr2x, scsl_table(1, ip), scsl_work(1), scsl_isys )
     END IF
#endif

#if defined __ESSL
     case(library_essl)

     if ( isign > 0 ) then
       tscale = 1.0d0 / ( nr1 * nr2 * nr3 )
     else
       tscale = 1.0d0
     end if
 
     call dcft3( f(1), nr1x, nr1x*nr2x, f(1), nr1x, nr1x*nr2x, nr1, nr2, nr3,  &
       isign, tscale, essl_work(1), essl_lwork)

#endif

      case(library_singleton)
  if(isign>0) then
    idir = -1
  else
    idir = +1
  end if
  call ffti_singleton3d(f,nr1,nr2,nr3,idir)
  IF( isign > 0 ) THEN
    tscale = 1.0d0 / DBLE( nr1 * nr2 * nr3 )
    call ZDSCAL( nr1x * nr2x * nr3x, tscale, f(1), 1)
  END IF
      end select

   END SUBROUTINE


!*****************************************
   INTEGER FUNCTION ffti_good_order_x ( nr, np )
   !*****************************************
   !
   !    This function find a "good" fft order value grather or equal to "nr"
   !
   !    nr  (input) tentative order n of a fft
   !
   !    np  (optional input) if present restrict the search of the order
   !        in the ensamble of multiples of np
   !
   !    Output: the same if n is a good number
   !         the closest higher number that is good
   !         an fft order is not good if not implemented (as on IBM with ESSL)
   !         or implemented but with awful performances (most other cases)
   !
     use ffti_base
     use ffti_interface

     IMPLICIT NONE
     INTEGER, INTENT(IN) :: nr
     INTEGER, OPTIONAL, INTENT(IN) :: np
     INTEGER :: new
     INTEGER :: local_nfftx

     call ffti_check_installed

     select case(library_id)
#ifdef __SCSL
     case(library_scsl)
       local_nfftx = scsl_nfftx
#endif
#ifdef __ESSL
     case(library_essl)
       local_nfftx = essl_nfftx
#endif
#ifdef __COMPLIB
     case(library_complib)
       local_nfftx = complib_nfftx
#endif
#ifdef __FFTW
     case(library_fftw)
       local_nfftx = fftw_nfftx
#endif
     case(library_singleton)
       local_nfftx = singleton_nfftx
     case default
       local_nfftx = 0
     end select

     new = nr
     IF( PRESENT( np ) ) THEN
       DO WHILE( ( ( .NOT. ffti_allowed( new ) ) .OR. ( MOD( new, np ) /= 0 ) ) &
                  .AND. ( new <= local_nfftx ) )
         new = new + 1
       END DO
     ELSE
       DO WHILE( ( .NOT. ffti_allowed( new ) ) .AND. ( new <= local_nfftx ) )
         new = new + 1
       END DO
     END IF

     IF( new > local_nfftx ) &
       CALL errore('ffti_good_order','fft order too large',new)

     ffti_good_order_x = new

   END FUNCTION ffti_good_order_x


