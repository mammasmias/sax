
! Copyright (C) 2005 Giovanni Bussi
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .

!**************************
PROGRAM test_1d
   !**************************
   USE ffti_module
   IMPLICIT NONE
   !
   ! This program tests the FFT 1D interface
   ! The task performed is the Fourier transform of a
   ! pole which has a known analytical form
   !
   DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979323846D0
   COMPLEX*16, PARAMETER   :: CI=(0.0,1.0)
   !
   INTEGER :: nz
   DOUBLE PRECISION                :: zmin, zmax, D, dz, dk
   DOUBLE PRECISION, ALLOCATABLE   :: grid(:)
   COMPLEX*16, ALLOCATABLE         :: array1(:),array2(:)
   COMPLEX*16, ALLOCATABLE         :: array1_out(:),array2_out(:)
   COMPLEX*16, ALLOCATABLE         :: array_analyt(:)
   !
   INTEGER :: i
   CHARACTER(10) :: library

!
!--------------------
! main body
!--------------------
!

   !
   ! parameters for the construction of the trial function
   !
   D = 0.05
   zmin = -10.0
   zmax =  10.0
  

   !
   ! loop over different grid dimensions
   !
   DO nz= 20000, 20000
        !
        dz = (zmax-zmin)/REAL(nz)
        dk = 2 * PI / (zmax-zmin)
        !
        ALLOCATE( grid(nz) )
        ALLOCATE( array1(nz), array2(nz) )
        ALLOCATE( array1_out(nz), array2_out(nz) )
        ALLOCATE( array_analyt(nz) )

        array_analyt = 0.0 

        DO i = 1 , nz
               !
               ! g(z) = 1 / ( z + iD )
               !
               grid(i) = zmin + REAL(i-1) * dz
               !
               array1(i) = 1. / ( grid(i) + CI * D )
               array2(i) = array1(i)
               !
               ! analytic FFT (continous)
               ! g(t) =  0                         t < 0
               !         -i SQRT(pi/2)             t = 0
               !         -i SQRT(pi*2) e^{-Dk}     t > 0
               IF ( i < nz / 2)  &
                  array_analyt(i) =  -CI * (zmax-zmin) / SQRT(REAL(nz)) * SQRT( PI * 2) * &
                                      EXP( -D * REAL(i) * dk ) 
        ENDDO
        DO i=1,nz
           WRITE(200,"(3f25.18)") grid(i), array1(i)
        ENDDO
  
        !
        ! FFT computation
        !

        !
        ! 1st FFT
        CALL ffti_set("")
        CALL ffti_which(library)
        WRITE(*,*) "FFT dimension = ", nz
        WRITE(*,*) "1st FFT: lib = ", library
        CALL ffti_1dz(array1,1,nz,nz, -1, array1_out)
        !
        ! impose a suitable phase
        !
        DO i=1, nz
           array1_out(i) = array1_out(i) * (-1)**(i+1)
        ENDDO
        WRITE(*,*) "1st FFT: done"

        !
        ! 2st FFT
        CALL ffti_set("FFTW")
        CALL ffti_which(library)
        WRITE(*,*) "2st FFT: lib = ", library
        CALL ffti_1dz(array2,1,nz,nz, -1, array2_out)
        !
        ! impose a suitable phase
        !
        DO i=1, nz
           array2_out(i) = array2_out(i) * (-1)**(i+1)
        ENDDO
        WRITE(*,*) "2st FFT: done"

        !
        ! check the difference
        DO i=1,nz
           WRITE(301,"(4f25.18)") grid(i), AIMAG(array1_out(i)), ABS(array1_out(i))
           WRITE(302,"(4f25.18)") grid(i), AIMAG(array2_out(i)), ABS(array2_out(i))
           WRITE(400,"(4f25.18)") grid(i), ABS( array1_out(i)-array2_out(i) ), &
                                           array1_out(i)-array2_out(i)
           WRITE(500,"(5f25.18)") grid(i), AIMAG( array_analyt(i) )
        ENDDO

        DEALLOCATE(array1, array2, array1_out, array2_out, grid)
        DEALLOCATE(array_analyt)
   ENDDO

END PROGRAM test_1d

