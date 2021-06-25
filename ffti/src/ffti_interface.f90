!
! Copyright (C) 2002 FPMD group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Copyright (C) 2005 Giovanni Bussi
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!


!----------------------------------------------------------------------!
! FFT scalar drivers Module.
! Written by Carlo Cavazzoni 
! Last update January 2004
!----------------------------------------------------------------------!


#ifndef __FFTW_SKIP
#ifndef __FFTW
#define __FFTW
#endif
#endif

#define __SKIP
! Presently, only ffti_3d and ffti_1dz are correctly implemented


#include "ffti_fdefs.h"


!=----------------------------------------------------------------------=!
   MODULE ffti_interface
!=----------------------------------------------------------------------=!

        USE ffti_base
        IMPLICIT NONE
        SAVE
        PRIVATE
        PUBLIC :: ffti_check_installed
        PUBLIC :: ffti_set
        PUBLIC :: ffti_which
        PUBLIC :: ffti_1dz
        PUBLIC :: ffti_3d
        PUBLIC :: ffti_good_dimension
        PUBLIC :: ffti_allowed
        PUBLIC :: ffti_good_order
        PUBLIC :: errore

INTERFACE ffti_check_installed
  SUBROUTINE ffti_check_installed_x
  END SUBROUTINE ffti_check_installed_x
END INTERFACE

INTERFACE ffti_set
  SUBROUTINE ffti_set_x(library)
    implicit none
    character(len=*), intent(in) :: library
  END SUBROUTINE ffti_set_x
END INTERFACE

INTERFACE ffti_which
  SUBROUTINE ffti_which_x(library)
    implicit none
    character(len=*), intent(out) :: library
  END SUBROUTINE ffti_which_x
END INTERFACE

INTERFACE ffti_1dz
  SUBROUTINE ffti_1dz_x(c, nsl, nz, ldc, sgn, cout)
    use ffti_base
    implicit none
    complex (dbl) :: c(:), cout(:)
    integer, intent(in) :: nsl, nz, ldc, sgn
  END SUBROUTINE ffti_1dz_x
END INTERFACE

INTERFACE ffti_3d
  SUBROUTINE ffti_3d_x( f, nr1, nr2, nr3, nr1x, nr2x, nr3x, sgn )
    use ffti_base
    implicit none
    complex (dbl) :: f(:)
    integer, intent(in) :: nr1, nr2, nr3, nr1x, nr2x, nr3x, sgn
  END SUBROUTINE ffti_3d_x
END INTERFACE

INTERFACE ffti_good_dimension
FUNCTION ffti_good_dimension_x (n)
  implicit none
  integer, intent(in) :: n
  integer :: ffti_good_dimension_x
END FUNCTION
END INTERFACE

INTERFACE ffti_allowed
FUNCTION ffti_allowed_x(n)
  implicit none
  integer, intent(in) :: n
  logical :: ffti_allowed_x
END FUNCTION ffti_allowed_x
END INTERFACE

INTERFACE ffti_good_order
FUNCTION ffti_good_order_x ( nr, np )
  IMPLICIT NONE
  INTEGER :: ffti_good_order_x
  INTEGER, INTENT(IN) :: nr
  INTEGER, OPTIONAL, INTENT(IN) :: np
END FUNCTION ffti_good_order_x
END INTERFACE





!=----------------------------------------------------------------------=!
   CONTAINS
!=----------------------------------------------------------------------=!

#ifndef __SKIP


!
!=----------------------------------------------------------------------=!
!
!
!
!         FFT along "x" and "y" direction
!
!
!
!=----------------------------------------------------------------------=!
!

   SUBROUTINE ffti_2dxy(r, nzl, nx, ny, ldx, ldy, sgn, pl2ix)

!     driver routine for nzl 2d complex fft's of lengths nx and ny
!     (sparse grid, both charge and wavefunctions) 
!     on input, sgn=+/-1 for charge density, sgn=+/-2 for wavefunctions
!     ldx is the actual dimension of f (may differ from n)
!     for compatibility: ldy is not used
!     A separate initialization is stored for each combination of input parameters

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: sgn, ldx, ldy, nx, ny, nzl
     INTEGER, OPTIONAL, INTENT(IN) :: pl2ix(:)
     COMPLEX (dbl) :: r( : )
     INTEGER :: i, k, j, err, idir, ip, isign, kk
     REAL(dbl) :: tscale
     INTEGER, SAVE :: icurrent = 1
     INTEGER, SAVE :: dims( 4, ndims) = -1
     LOGICAL :: dofft( nfftx )
     INTEGER, PARAMETER  :: stdout = 6
#if defined __SCSL
     INTEGER                :: index(nx), ndoX
     COMPLEX(dbl)           :: XY(nx+nx*ny)
#endif

#if defined __SCSL
     scsl_isys(0) = 1
#endif

     isign = - sgn

     dofft( 1 : nx ) = .TRUE.
     IF( PRESENT( pl2ix ) ) THEN
       IF( SIZE( pl2ix ) < nx ) &
         CALL errore( ' ffti_2dxy ', ' wrong dimension for arg no. 8 ', 1 )
       DO i = 1, nx
         IF( pl2ix(i) < 1 ) dofft( i ) = .FALSE.
       END DO
     END IF

     ! WRITE( stdout,*) 'DEBUG: ', COUNT( dofft )

     !
     !   Here initialize table only if necessary
     !

     ip = -1
     DO i = 1, ndims
            
       !   first check if there is already a table initialized
       !   for this combination of parameters

       IF( ( ny == dims(1,i) ) .and. ( ldx == dims(2,i) ) .and. &
           ( nx == dims(3,i) ) .and. ( nzl == dims(4,i) ) ) THEN
         ip = i
         EXIT
       END IF

     END DO

     IF( ip == -1 ) THEN

       !   no table exist for these parameters
       !   initialize a new one 

       ! WRITE( stdout, fmt="('DEBUG ffti_2dxy, reinitializing tables ', I3)" ) icurrent

#if defined __FFTW

       IF( fw_plan( 2, icurrent) /= 0 )   CALL FFTI_DESTROY_PLAN_1D( fw_plan( 2, icurrent) )
       IF( bw_plan( 2, icurrent) /= 0 )   CALL FFTI_DESTROY_PLAN_1D( bw_plan( 2, icurrent) )
       idir = -1; CALL FFTI_CREATE_PLAN_1D( fw_plan( 2, icurrent), ny, idir)
       idir =  1; CALL FFTI_CREATE_PLAN_1D( bw_plan( 2, icurrent), ny, idir)

       IF( fw_plan( 1, icurrent) /= 0 ) CALL FFTI_DESTROY_PLAN_1D( fw_plan( 1, icurrent) )
       IF( bw_plan( 1, icurrent) /= 0 ) CALL FFTI_DESTROY_PLAN_1D( bw_plan( 1, icurrent) )
       idir = -1; CALL FFTI_CREATE_PLAN_1D( fw_plan( 1, icurrent), nx, idir) 
       idir =  1; CALL FFTI_CREATE_PLAN_1D( bw_plan( 1, icurrent), nx, idir) 

#elif defined __ESSL

       tscale = 1.0d0 / ( nx * ny )
       CALL DCFT ( 1, r(1), ldx, 1, r(1), ldx, 1, ny, 1,  1, 1.0d0, &
          fw_table( 1, 2, icurrent), ltabl, work(1), lwork )
       CALL DCFT ( 1, r(1), ldx, 1, r(1), ldx, 1, ny, 1, -1, 1.0d0, &
          bw_table(1, 2, icurrent), ltabl, work(1), lwork )
       CALL DCFT ( 1, r(1), 1, ldx, r(1), 1, ldx, nx, ny,  1, &
          tscale, fw_table( 1, 1, icurrent), ltabl, work(1), lwork)
       CALL DCFT ( 1, r(1), 1, ldx, r(1), 1, ldx, nx, ny, -1, &
          1.0d0, bw_table(1, 1, icurrent), ltabl, work(1), lwork)

#elif defined __COMPLIB

       CALL ZFFT1DI( ny, tabley(1, icurrent) )
       CALL ZFFT1DI( nx, tablex(1, icurrent) )

#elif defined __SCSL

       CALL ZZFFTMR (0, ny, 0, 0.0D0, DUMMY, 1, DUMMY, 1,               &
                     tabley(1, icurrent), DUMMY, scsl_isys)
       CALL ZZFFTM  (0, nx, 0, 0.0D0, DUMMY, 1, DUMMY, 1,               &
                     tablex(1, icurrent), DUMMY, scsl_isys)

#else

#error

#endif

       dims(1,icurrent) = ny; dims(2,icurrent) = ldx; 
       dims(3,icurrent) = nx; dims(4,icurrent) = nzl;
       ip = icurrent
       icurrent = MOD( icurrent, ndims ) + 1

     END IF

     !
     !   Now perform the FFTs using machine specific drivers
     !

#if defined __FFTW

     IF( isign > 0 ) THEN

       CALL FFTI_X_STICK( fw_plan( 1, ip), r(1), nx, ny, nzl, ldx, ldy ) 
       do i = 1, nx
         do k = 1, nzl
           IF( dofft( i ) ) THEN
             j = i + ldx*ldy * ( k - 1 )
             call FFTI_Y_STICK(fw_plan( 2, ip), r(j), ny, ldx) 
           END IF
         end do
       end do
       tscale = 1.0d0 / ( nx * ny )
       CALL ZDSCAL( ldx * ldy * nzl, tscale, r(1), 1)

     ELSE IF( isign < 0 ) THEN

       do i = 1, nx
         do k = 1, nzl
           IF( dofft( i ) ) THEN
             j = i + ldx*ldy * ( k - 1 )
             call FFTI_Y_STICK( bw_plan( 2, ip), r(j), ny, ldx) 
           END IF
         end do
       end do
       CALL FFTI_X_STICK( bw_plan( 1, ip), r(1), nx, ny, nzl, ldx, ldy ) 

     END IF

#elif defined __ESSL

     IF( isign > 0 ) THEN

       idir = 1
       tscale = 1.0d0 / ( nx * ny )
       do k = 1, nzl
         kk = 1 + ( k - 1 ) * ldx * ldy
         CALL DCFT ( 0, r(kk), 1, ldx, r(kk), 1, ldx, nx, ny, idir, &
           tscale, fw_table( 1, 1, ip ), ltabl, work( 1 ), lwork)
         do i = 1, nx
           IF( dofft( i ) ) THEN
             kk = i + ( k - 1 ) * ldx * ldy
             call DCFT ( 0, r( kk ), ldx, 1, r( kk ), ldx, 1, ny, 1, &
               idir, 1.0d0, fw_table(1, 2, ip), ltabl, work( 1 ), lwork)
           END IF
         end do
       end do

     ELSE IF( isign < 0 ) THEN

       idir = -1
       DO k = 1, nzl
         do i = 1, nx
           IF( dofft( i ) ) THEN
             kk = i + ( k - 1 ) * ldx * ldy
             call DCFT ( 0, r( kk ), ldx, 1, r( kk ), ldx, 1, ny, 1, &
               idir, 1.0d0, bw_table(1, 2, ip), ltabl, work( 1 ), lwork)
           END IF
         end do
         kk = 1 + ( k - 1 ) * ldx * ldy
         CALL DCFT ( 0, r( kk ), 1, ldx, r( kk ), 1, ldx, nx, ny, idir, &
           1.0d0, bw_table(1, 1,  ip), ltabl, work( 1 ), lwork)
       END DO
         
     END IF

#elif defined __COMPLIB

     IF( isign > 0 ) THEN
       idir =  -1
       DO i = 1, nzl
         k = 1 + ( i - 1 ) * ldx * ldy
         call zfftm1d( idir, nx, ny, r(k), 1, ldx, tablex(1,ip) )
       END DO
       do i = 1, nx
         IF( dofft( i ) ) THEN
           call zfftm1d( idir, ny, nzl, r(i), ldx, ldx*ldy, tabley(1, ip) )
         END IF
       end do
       tscale = 1.0d0 / ( nx * ny )
       CALL ZDSCAL( ldx * ldy * nzl, tscale, r(1), 1)
     ELSE IF( isign < 0 ) THEN
       idir = 1
       do i = 1, nx
         IF( dofft( i ) ) THEN
           call zfftm1d( idir, ny, nzl, r(i), ldx, ldx*ldy, tabley(1, ip) )
         END IF
       end do
       DO i = 1, nzl
         k = 1 + ( i - 1 ) * ldx * ldy
         call zfftm1d( idir, nx, ny, r(k), 1, ldx, tablex(1,ip) )
       END DO
     END IF

#elif defined __SCSL

      ndoX = 0
      DO i = 1, nx
         IF ( dofft(i) ) THEN
            ndoX = ndoX + 1
            index(ndoX) = i
         END IF
      END DO
!
      IF( isign > 0 ) THEN
!
       idir = -1
       tscale = 1.0d0 / ( nx * ny )
       DO k = 1, nzl
          kk = 1 + ( k - 1 ) * ldx * ldy
! FORWARD: first the X direction
          CALL ZZFFTM ( idir, nx, ny, tscale, r(kk), ldx, r(kk), ldx,   &
                        tablex(1, ip), work(1), scsl_isys )
! FORWARD: then the Y direction
! Gather R -> XY
          DO j = 1, ny
             DO i = 1, ndoX
                XY( i + ( j - 1 ) * nx ) = r( index( i ) + ( j - 1 ) *  &
                   ldx + ( k - 1 ) * ldx * ldy )
             END DO
          END DO
! ndoX FFTs in the Y direction
          CALL ZZFFTMR ( idir, ny, ndoX, 1.0D0, XY, ldx, XY, ldx,       &
                         tabley(1, ip), work(1), scsl_isys )
! Scatter back XY -> R
          DO j = 1, ny
             DO i = 1, ndoX
                r( index( i ) + ( j - 1 ) * ldx + ( k - 1 ) * ldx *     &
                  ldy ) = XY( i + ( j - 1 ) * nx )
             END DO
          END DO
       END DO
!
     END IF
!
     IF ( isign < 0 ) THEN
!
       idir = 1
       tscale = 1.0d0
       DO k = 1, nzl
          kk = 1 + ( k - 1 ) * ldx * ldy
! BACKWARD: first the Y direction
! Gather R -> XY
          DO j = 1, ny
             DO i = 1, ndoX
                XY( i + ( j - 1 ) * nx ) = r( index( i ) + ( j - 1 ) *  &
                   ldx + ( k - 1 ) * ldx * ldy )
             END DO
          END DO
!
! ndoX FFTs in the Y direction
          CALL ZZFFTMR ( idir, ny, ndoX, 1.0D0, XY, ldx, XY, ldx,       &
                         tabley(1, ip), work(1), scsl_isys )
! Scatter back XY -> R
          DO j = 1, ny
             DO i = 1, ndoX
                r( index( i ) + ( j - 1 ) * ldx + ( k - 1 ) * ldx *     &
                  ldy ) = XY( i + ( j - 1 ) * nx )
             END DO
          END DO
! BACKWARD: then the X direction
          CALL ZZFFTM ( idir, nx, ny, tscale, r(kk), ldx, r(kk), ldx,   &
                        tablex(1, ip), work(1), scsl_isys )
       END DO
!
     END IF

#else

#error

#endif

     return
   end subroutine ffti_2dxy

!
!=----------------------------------------------------------------------=!
!
!
!
!         3D scalar FFTs,  but using sticks!
!
!
!
!=----------------------------------------------------------------------=!
!


!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------

subroutine ffti_3ds (f, nr1, nr2, nr3, nrx1, nrx2, nrx3, sign, do_fft_x, do_fft_y)
  !
  !     driver routine for 3d complex "reduced" fft
  !     sign > 0 : f(G) => f(R)   ; sign < 0 : f(R) => f(G)
  !
  !     The 3D fft are computed only on lines and planes which have
  !     non zero elements. These lines and planes are defined by
  !     the two vectors do_fft_x and do_fft_y 
  !
  !     The routine is implemented for:
  !
  !       IBM      : essl library
  !
  !----------------------------------------------------------------------
  !
  implicit none

  integer :: nr1, nr2, nr3, nrx1, nrx2, nrx3, sign
  !
  !   logical dimensions of the fft
  !   physical dimensions of the f array
  !   sign of the transformation

  complex(dbl) :: f ( nrx1 * nrx2 * nrx3 )
  integer :: do_fft_x(:), do_fft_y(:)
  !
  ! the fft array
  !
  ! ESSL fft's require a different initialization for sign=-1 and sign=1
  ! aux1 contains the initialized quantities
  ! aux2 is work space
  !
  integer :: m, incx1, incx2
  INTEGER :: i, k, j, err, idir, ip, isign, ii, jj
  REAL(dbl) :: tscale
  INTEGER, SAVE :: icurrent = 1
  INTEGER, SAVE :: dims(3,ndims) = -1


  tscale = 1.d0
  isign = - sign   !  here we follow ESSL convention

  !
  ! ESSL sign convention for fft's is the opposite of the "usual" one
  !

  ! WRITE( stdout, fmt="('DEBUG ffti_3ds :',6I6)") nr1, nr2, nr3, nrx1, nrx2, nrx3
  ! WRITE( stdout, fmt="('DEBUG ffti_3ds :',24I2)") do_fft_x
  ! WRITE( stdout, fmt="('DEBUG ffti_3ds :',24I2)") do_fft_y

  IF( nr2 /= nrx2 ) &
    CALL errore(' ffti_3ds ', ' wrong dimensions: nr2 /= nrx2 ', 1 )

     ip = -1
     DO i = 1, ndims

       !   first check if there is already a table initialized
       !   for this combination of parameters

       IF( ( nr1 == dims(1,i) ) .and. ( nr2 == dims(2,i) ) .and. &
           ( nr3 == dims(3,i) ) ) THEN
         ip = i
         EXIT
       END IF

     END DO

     IF( ip == -1 ) THEN

       !   no table exist for these parameters
       !   initialize a new one

#if defined __FFTW

       IF( fw_plan( 1, icurrent) /= 0 ) CALL FFTI_DESTROY_PLAN_1D( fw_plan( 1, icurrent) )
       IF( bw_plan( 1, icurrent) /= 0 ) CALL FFTI_DESTROY_PLAN_1D( bw_plan( 1, icurrent) )
       IF( fw_plan( 2, icurrent) /= 0 ) CALL FFTI_DESTROY_PLAN_1D( fw_plan( 2, icurrent) )
       IF( bw_plan( 2, icurrent) /= 0 ) CALL FFTI_DESTROY_PLAN_1D( bw_plan( 2, icurrent) )
       IF( fw_plan( 3, icurrent) /= 0 ) CALL FFTI_DESTROY_PLAN_1D( fw_plan( 3, icurrent) )
       IF( bw_plan( 3, icurrent) /= 0 ) CALL FFTI_DESTROY_PLAN_1D( bw_plan( 3, icurrent) )
       idir = -1; CALL FFTI_CREATE_PLAN_1D( fw_plan( 1, icurrent), nr1, idir) 
       idir =  1; CALL FFTI_CREATE_PLAN_1D( bw_plan( 1, icurrent), nr1, idir) 
       idir = -1; CALL FFTI_CREATE_PLAN_1D( fw_plan( 2, icurrent), nr2, idir) 
       idir =  1; CALL FFTI_CREATE_PLAN_1D( bw_plan( 2, icurrent), nr2, idir) 
       idir = -1; CALL FFTI_CREATE_PLAN_1D( fw_plan( 3, icurrent), nr3, idir) 
       idir =  1; CALL FFTI_CREATE_PLAN_1D( bw_plan( 3, icurrent), nr3, idir) 

#elif defined __ESSL

       tscale = 1.0d0 
       !  x - direction
       incx1 = 1; incx2 = nrx1; m = 1
       CALL DCFT ( 1, f(1), incx1, incx2, f(1), incx1, incx2, nr1, m,  1, 1.0d0, &
          fw_table( 1, 1, icurrent), ltabl, work(1), lwork )
       CALL DCFT ( 1, f(1), incx1, incx2, f(1), incx1, incx2, nr1, m, -1, 1.0d0, &
          bw_table(1, 1, icurrent), ltabl, work(1), lwork )
       !  y - direction
       incx1 = nrx1; incx2 = 1; m = nr1;
       CALL DCFT ( 1, f(1), incx1, incx2, f(1), incx1, incx2, nr2, m,  1, 1.0d0, &
          fw_table( 1, 2, icurrent), ltabl, work(1), lwork )
       CALL DCFT ( 1, f(1), incx1, incx2, f(1), incx1, incx2, nr2, m, -1, 1.0d0, &
          bw_table(1, 2, icurrent), ltabl, work(1), lwork )
       !  z - direction
       incx1 = nrx1 * nrx2; incx2 = 1; m = nrx1 * nr2
       CALL DCFT ( 1, f(1), incx1, incx2, f(1), incx1, incx2, nr3, m,  1, 1.0d0, &
          fw_table(1, 3, icurrent), ltabl, work(1), lwork )
       CALL DCFT ( 1, f(1), incx1, incx2, f(1), incx1, incx2, nr3, m, -1, 1.0d0, &
          bw_table(1, 3, icurrent), ltabl, work(1), lwork )

#else

       CALL errore(' ffti_3ds ',' no scalar fft driver specified ', 1)

#endif

       dims(1,icurrent) = nr1; dims(2,icurrent) = nr2; dims(3,icurrent) = nr3
       ip = icurrent
       icurrent = MOD( icurrent, ndims ) + 1

     END IF


     IF ( isign < 0 ) THEN
   
        !
        !  i - direction ...
        !

        incx1 = 1;  incx2 = nrx1;  m = 1

        do k = 1, nr3
           do j = 1, nr2
              jj = j + ( k - 1 ) * nrx2 
              ii = 1 + nrx1 * ( jj - 1 ) 
              if ( do_fft_x( jj ) == 1 ) THEN
#if defined __FFTW
                call FFTI_INPLACE_DRV_1D( bw_plan( 1, ip), m, f( ii ), incx1, incx2 )
#elif defined __ESSL
                call dcft (0, f ( ii ), incx1, incx2, f ( ii ), incx1, incx2, nr1, m, &
                  isign, 1.0d0, bw_table ( 1, 1,  ip ), ltabl, work( 1 ), lwork)
#else
                call errore(' ffti_3ds ',' no scalar fft driver specified ', 1)
#endif
              endif
           enddo
        enddo

        !
        !  ... j-direction ...
        !

        incx1 = nrx1;  incx2 = 1;  m = nr1

        do k = 1, nr3
           ii = 1 + nrx1 * nrx2 * ( k - 1 ) 
           if ( do_fft_y( k ) == 1 ) then
#if defined __FFTW
             call FFTI_INPLACE_DRV_1D( bw_plan( 2, ip), m, f( ii ), incx1, incx2 )
#elif defined __ESSL
             call dcft (0, f ( ii ), incx1, incx2, f ( ii ), incx1, incx2, nr2, m, &
               isign, 1.0d0, bw_table ( 1, 2,  ip ), ltabl, work( 1 ), lwork)
#else
             call errore(' ffti_3ds ',' no scalar fft driver specified ', 1)
#endif
           endif
        enddo

        !
        !     ... k-direction
        !

        incx1 = nrx1 * nrx2;  incx2 = 1;  m = nrx1 * nr2

#if defined __FFTW
        call FFTI_INPLACE_DRV_1D( bw_plan( 3, ip), m, f( 1 ), incx1, incx2 )
#elif defined __ESSL
        call dcft (0, f( 1 ), incx1, incx2, f( 1 ), incx1, incx2, nr3, m, &
          isign, 1.0d0, bw_table ( 1, 3, ip ), ltabl, work( 1 ), lwork)
#endif

     ELSE

        !
        !     ... k-direction
        !

        incx1 = nrx1 * nr2;  incx2 = 1;  m = nrx1 * nr2

#if defined __FFTW
        call FFTI_INPLACE_DRV_1D( fw_plan( 3, ip), m, f( 1 ), incx1, incx2 )
#elif defined __ESSL
        call dcft (0, f( 1 ), incx1, incx2, f( 1 ), incx1, incx2, nr3, m, &
          isign, 1.0d0, fw_table ( 1, 3, ip ), ltabl, work( 1 ), lwork)
#endif

        !
        !     ... j-direction ...
        !

        incx1 = nrx1;  incx2 = 1;  m = nr1

        do k = 1, nr3
           ii = 1 + nrx1 * nrx2 * ( k - 1 ) 
           if ( do_fft_y ( k ) == 1 ) then
#if defined __FFTW
             call FFTI_INPLACE_DRV_1D( fw_plan( 2, ip), m, f( ii ), incx1, incx2 )
#elif defined __ESSL
             call dcft (0, f ( ii ), incx1, incx2, f ( ii ), incx1, incx2, nr2, m, &
               isign, 1.0d0, fw_table ( 1, 2, ip ), ltabl, work( 1 ), lwork)
#else
             call errore(' ffti_3ds ',' no scalar fft driver specified ', 1)
#endif
           endif
        enddo

        !
        !     i - direction ...
        !

        incx1 = 1;  incx2 = nrx1;  m = 1

        do k = 1, nr3
           do j = 1, nr2
              jj = j + ( k - 1 ) * nrx2 
              ii = 1 + nrx1 * ( jj - 1 ) 
              if ( do_fft_x( jj ) == 1 ) then
#if defined __FFTW
                call FFTI_INPLACE_DRV_1D( fw_plan( 1, ip), m, f( ii ), incx1, incx2 )
#elif defined __ESSL
                call dcft (0, f ( ii ), incx1, incx2, f ( ii ), incx1, incx2, nr1, m, &
                   isign, 1.0d0, fw_table ( 1, 1, ip ), ltabl, work( 1 ), lwork)
#else
                call errore(' ffti_3ds ',' no scalar fft driver specified ', 1)
#endif
              endif
           enddo
        enddo

#if defined __ESSL || defined __FFTW
        call DSCAL (2 * nrx1 * nrx2 * nr3, 1.0d0 / (nr1 * nr2 * nr3), f( 1 ), 1)
#endif

     END IF

     RETURN
   END SUBROUTINE 

!
!=----------------------------------------------------------------------=!
!
!
!
!         3D parallel FFT on sub-grids
!
!
!
!=----------------------------------------------------------------------=!
!
   SUBROUTINE ffti_3db ( f, n1, n2, n3, n1x, n2x, n3x, imin3, imax3, sgn )

!     driver routine for 3d complex fft's on box grid - ibm essl
!     fft along xy is done only on planes that correspond to
!     dense grid planes on the current processor, i.e. planes
!     with imin3 .le. n3 .le. imax3
!
      implicit none
      integer n1,n2,n3,n1x,n2x,n3x,imin3,imax3,sgn
      complex(kind=8) :: f(:)

      integer isign, naux, ibid, nplanes, nstart, k
      real(dbl) :: tscale

      integer :: ip, i
      integer, save :: icurrent = 1
      integer, save :: dims( 4, ndims ) = -1

#if defined __FFTW

      C_POINTER, save :: bw_planz(  ndims ) = 0
      C_POINTER, save :: bw_planxy( ndims ) = 0
#endif

#if defined __ESSL

      real(kind=8), save :: aux3( ltabl, ndims )
      real(kind=8), save :: aux2( ltabl, ndims )
      real(kind=8), save :: aux1( ltabl, ndims )

#elif defined __COMPLIB

      real(kind=8), save :: bw_coeffz( ltabl,  ndims )
      real(kind=8), save :: bw_coeffy( ltabl,  ndims )
      real(kind=8), save :: bw_coeffx( ltabl,  ndims )

#elif defined __SCSL

      real(kind=8), save :: bw_coeffz( ltabl,  ndims )
      real(kind=8), save :: bw_coeffy( ltabl,  ndims )
      real(kind=8), save :: bw_coeffx( ltabl,  ndims )
      complex(kind=8)    :: fy(n2 + n1x * n2), fz(n3 + n1x * n2x * n3)
      INTEGER            :: j

#endif


      isign = -sgn
      tscale = 1.d0

      if ( isign > 0 ) then
         call errore('ffti_3db','not implemented',isign)
      end if
!
! 2d fft on xy planes - only needed planes are transformed
! note that all others are left in an unusable state
!
      nplanes = imax3 - imin3 + 1
      nstart  = ( imin3 - 1 ) * n1x * n2x + 1

      !
      !   Here initialize table only if necessary
      !

      ip = -1
      DO i = 1, ndims

        !   first check if there is already a table initialized
        !   for this combination of parameters

        IF ( ( n1 == dims(1,i) ) .and. ( n2 == dims(2,i) ) .and. &
             ( n3 == dims(3,i) ) .and. ( nplanes == dims(4,i) ) ) THEN
           ip = i
           EXIT
        END IF

      END DO

      IF( ip == -1 ) THEN

        !   no table exist for these parameters
        !   initialize a new one

#if defined __FFTW

        if ( bw_planz(icurrent) /= 0 ) call FFTI_DESTROY_PLAN_1D( bw_planz(icurrent) )
        call FFTI_CREATE_PLAN_1D( bw_planz(icurrent), n3, 1 )

        if ( bw_planxy(icurrent) /= 0 ) call FFTI_DESTROY_PLAN_2D( bw_planxy(icurrent) )
        call FFTI_CREATE_PLAN_2D( bw_planxy(icurrent), n1, n2, 1 )
!
#elif defined __ESSL

         if( n3 /= dims(3,icurrent) ) then
           call dcft( 1, f(1), n1x*n2x, 1, f(1), n1x*n2x, 1, n3, n1x*n2x, isign,          &
     &        tscale, aux3(1,icurrent), ltabl, work(1), lwork)
         end if
         call dcft( 1, f(1), 1, n1x, f(1), 1, n1x, n1, n2x*nplanes, isign,              &
     &        tscale, aux1(1,icurrent), ltabl, work(1), lwork)
         if( n2 /= dims(2,icurrent) ) then
           call dcft( 1, f(1), n1x, 1, f(1), n1x, 1, n2, n1x, isign,                      &
     &        tscale, aux2(1,icurrent), ltabl, work(1), lwork)
         end if

#elif defined __COMPLIB

         call zfft1di( n3, bw_coeffz( 1, icurrent ) )
         call zfft1di( n2, bw_coeffy( 1, icurrent ) )
         call zfft1di( n1, bw_coeffx( 1, icurrent ) )

#elif defined __SCSL

         CALL ZZFFT (0, n3, 0.0D0, DUMMY, 1, bw_coeffz(1, icurrent),    &
                     work(1), scsl_isys)
         CALL ZZFFT (0, n2, 0.0D0, DUMMY, 1, bw_coeffy(1, icurrent),    &
                     work(1), scsl_isys)
         CALL ZZFFT (0, n1, 0.0D0, DUMMY, 1, bw_coeffx(1, icurrent),    &
                     work(1), scsl_isys)

#else

#error

#endif

        dims(1,icurrent) = n1; dims(2,icurrent) = n2
        dims(3,icurrent) = n3; dims(4,icurrent) = nplanes
        ip = icurrent
        icurrent = MOD( icurrent, ndims ) + 1

      END IF


#if defined __FFTW

      call FFTI_INPLACE_DRV_1D( bw_planz(ip), n1x*n2x, f(1), n1x*n2x, 1 )
      call FFTI_INPLACE_DRV_2D( bw_planxy(ip), nplanes, f(nstart), 1, n1x*n2x )

#elif defined __ESSL


      !   fft in the z-direction...

      call dcft( 0, f(1), n1x*n2x, 1, f(1), n1x*n2x, 1, n3, n1x*n2x, isign,             &
     &        tscale, aux3(1,ip), ltabl, work(1), lwork)

      !   x-direction

      call dcft( 0, f(nstart), 1, n1x, f(nstart), 1, n1x, n1, n2x*nplanes, isign,  &
     &        tscale, aux1(1,ip), ltabl, work(1), lwork)
     
      !   y-direction
     
      DO K = imin3, imax3
        nstart = ( k - 1 ) * n1x * n2x + 1
        call dcft( 0, f(nstart), n1x, 1, f(nstart), n1x, 1, n2, n1x, isign,        &
     &        tscale, aux2(1,ip), ltabl, work(1), lwork)
      END DO

#elif defined __COMPLIB

      call zfftm1d( 1, n3, n1x*n2x, f(1), n1x*n2x, 1, bw_coeffz(1, ip) )
      call zfftm1d( 1, n1, n2x*nplanes, f(nstart), 1, n1x, bw_coeffx(1, ip) )
      DO K = imin3, imax3
        nstart = ( k - 1 ) * n1x * n2x + 1
        call zfftm1d( 1, n2, n1x, f(nstart), n1x, 1, bw_coeffy(1, ip) )
      END DO

#elif defined __SCSL

! Gather f -> fz
      DO j = 1, n1x*n2x
         DO i = 1, n3
            fz( i + ( j - 1 ) * n3 ) = f( (( i - 1 ) * ( n1x *n2x ))    &
               + j )
         END DO
      END DO
! z-direction
      CALL ZZFFTM (1, n3, n1x*n2x, tscale, fz(1), n3, fz(1), n3,        &
                   bw_coeffz(1, ip), work(1), scsl_isys)
! Scatter back fz -> f
      DO j = 1, n1x*n2x
         DO i = 1, n3
            f( (( i - 1 ) * ( n1x *n2x )) + j ) = fz( i + ( j - 1 ) *   &
              n3 )
         END DO
      END DO
! x-direction
      CALL ZZFFTM (1, n1, n2x*nplanes, tscale, f(nstart), n1x,          &
                   f(nstart), n1x, bw_coeffx(1, ip), work(1), scsl_isys)
! y-direction
      DO k = imin3, imax3
        nstart = ( k - 1 ) * n1x * n2x + 1
! Gather f -> fy
         DO j = 1, n1x
            DO i = 1, n2
                fy( i + ( j - 1 ) * n2 ) = f( (( i - 1 ) * n1x ) + j +  &
                   ( k - 1 ) * n1x * n2x )
            END DO
         END DO
         CALL ZZFFTM (1, n2, n1x, tscale, fy, n2, fy,                   &
                      n2, bw_coeffy(1, ip), work(1), scsl_isys)
! Scatter back fy -> f
         DO j = 1, n1x
            DO i = 1, n2
               f( (( i - 1 ) * n1x ) + j + ( k - 1 ) * n1x * n2x ) =    &
                 fy( i + ( j - 1 ) * n2 )
            END DO
         END DO
      END DO

#endif

     RETURN
   END SUBROUTINE
#endif

!
!=----------------------------------------------------------------------=!
!
!
!
!         FFT support Functions/Subroutines
!
!
!
!=----------------------------------------------------------------------=!
!

!
!=----------------------------------------------------------------------=!

!=----------------------------------------------------------------------=!


   SUBROUTINE errore(a,b,n)

!  this routine prints an error and warning message and 
!  if necessary terminates the program.
!  INPUT: a, b, n
!    a   (character)   subroutine name
!    b   (character)   error message
!    n   (integer)     error code
!                      if n > 0 write the error message and terminate the execution
!                      if n = 0 do nothing
!                      if n < 0 print the error message and return
!  OUTPUT: none
!  ----------------------------------------------
!  END manual

    IMPLICIT NONE

#if defined __MPI
      include 'mpif.h'
#endif

! ... declare subroutine arguments
      CHARACTER(LEN=*)    :: a, b
      INTEGER, INTENT(IN) :: n

      INTEGER :: ip, nproc, mpime, ierr

! ... declare function

!  end of declarations
!  ----------------------------------------------

#if defined __MPI
      CALL mpi_comm_size(mpi_comm_world,nproc,ierr)
      CALL mpi_comm_rank(mpi_comm_world,mpime,ierr)
#else
      MPIME = 0
      NPROC = 1
#endif

! ... print the error message
!
      DO ip = 0, nproc-1
        IF( n > 0 ) THEN
          WRITE (6,100) mpime, a, b, n
          OPEN(UNIT=15, FILE='CRASH', POSITION='append', STATUS='unknown')
          WRITE (15,100) mpime, a, b, n
          CLOSE(UNIT=15)
        ELSE IF ( n < 0 ) THEN
          IF( mpime == 0 ) WRITE (6,200) a, b
        END IF
#if defined __MPI
        CALL mpi_barrier(mpi_comm_world,ierr)
#endif
      END DO

! ... terminate the program
!
      ! CALL cpflush  ! flush output streams

      IF( n > 0 ) THEN
#if defined __MPI
        CALL mpi_finalize(ierr)
        IF ( ierr/=0 ) THEN
          CALL mpi_abort(mpi_comm_world, ierr)
        END IF
#endif
      END IF

100   FORMAT (/,' *** from PE    : ',I5, &
              /,' *** in routine : ',A, &
              /,' *** error msg. : ',A, &
              /,' *** error code : ',I5, &
              /,' *** aborting ***', /)

200   FORMAT ('   Warning (', A, ') : ', A)

      IF( n > 0 ) THEN
        STOP 'CRASH'
      ELSE IF( n == 0 ) THEN
        WRITE(6,*) ' ERROR DEBUG ', a, b
      END IF
 
    RETURN

  END SUBROUTINE

!=----------------------------------------------------------------------=!
   END MODULE ffti_interface
!=----------------------------------------------------------------------=!


