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

! -------------------------------------------------------------------------
! Standard linear algebra for complex (square) parallel matrix distributed
! on proc with the line index.
! -------------------------------------------------------------------------

#include "tools_error.h"
!@ MANUAL
module num_la_parall_module
implicit none
private
public :: num_parall_he_diag, num_parall_c_transpose
!@ END MANUAL


contains

subroutine num_parall_he_diag(matrix,root,comm)
use ptk_module, only : ptk_comm_rank, ptk_comm, ptk_comm_size, &
  ptk_barrier
use pw_parall_matrix_type
use num_la_module
use num_diago_module

implicit none

type(pw_parall_complex_matrix), intent(inout) :: matrix
integer, intent(in) :: root
type(ptk_comm), intent(in) :: comm

integer :: myrank
integer :: m_dim,loc_dim
complex, allocatable :: matrix_tmp(:,:)
complex, allocatable :: cwork(:)
real, allocatable :: rwork(:)

integer :: ierr
integer :: i,k

!call ptk_comm_rank(comm,myrank) 

!if(myrank==matrix%rank) then

m_dim=matrix%m_dim
loc_dim=matrix%m_dim_loc

!if((matrix%npe==1)) then  ! DEBUG (matrix%npe==1)
!   call num_HEEV( 'V', loc_dim, matrix%val, matrix%eigenval )
!else

   allocate( matrix_tmp( loc_dim, m_dim ) )
   matrix_tmp(:,:)=0.0
  
   CALL pzhpev_drv( 'V', matrix%val, loc_dim, matrix%eigenval, matrix_tmp, loc_dim, &
                          loc_dim, m_dim, matrix%npe, matrix%rank, comm%comm  )

   matrix%val(:,:) = matrix_tmp(:,:)

   deallocate(matrix_tmp)
!endif
!endif
end subroutine num_parall_he_diag


subroutine num_parall_c_transpose(matrix,root,comm)
use ptk_module, only : ptk_comm_rank, ptk_comm, ptk_comm_size, &
  ptk_barrier
use pw_parall_matrix_type
use num_la_module
use num_diago_module

implicit none

type(pw_parall_complex_matrix), intent(inout) :: matrix
integer, intent(in) :: root
type(ptk_comm), intent(in) :: comm

integer :: myrank
integer :: m_dim,loc_dim
complex, allocatable :: matrix_tmp(:,:)

integer :: ierr
integer :: i,k

m_dim=matrix%m_dim
loc_dim=matrix%m_dim_loc


allocate( matrix_tmp( loc_dim, m_dim ) )
matrix_tmp(:,:)=0.0


call mytrasp_dist(matrix%val,loc_dim,'R',matrix_tmp,loc_dim,'C',m_dim,matrix%rank,matrix%npe)

matrix%val(:,:) = matrix_tmp(:,:)

deallocate(matrix_tmp)

end subroutine num_parall_c_transpose

   !
   !-------------------------------------------------------------------------
   SUBROUTINE pzhptrd( n, nrl, ap, lda, d, e, tau, nproc, me, comm )
     !-------------------------------------------------------------------------
      !
      !  Parallel MPI version of the LAPACK routine ZHPTRD
      !
      !     Carlo Cavazzoni (carlo.cavazzoni@cineca.it) -- CINECA
      !     Dicember 12, 1999
      !
      !  REFERENCES :
      !
      !     NUMERICAL RECIPES, THE ART OF SCIENTIFIC COMPUTING.
      !     W.H. PRESS, B.P. FLANNERY, S.A. TEUKOLSKY, AND W.T. VETTERLING,
      !     CAMBRIDGE UNIVERSITY PRESS, CAMBRIDGE.
      !
      !     PARALLEL NUMERICAL ALGORITHMS,
      !     T.L. FREEMAN AND C.PHILLIPS,
      !     PRENTICE HALL INTERNATIONAL (1992).
      !
      !     LAPACK routine (version 2.0) --
      !     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      !     Courant Institute, Argonne National Lab, and Rice University
      !

      IMPLICIT NONE

!     .. __SCALAR Arguments ..
      INTEGER            LDA, N, NRL, NPROC, ME, comm
!     ..
!     .. Array Arguments ..
      REAL             D( * ), E( * )
      COMPLEX         AP(LDA, * ), TAU( * )
!     ..
!
!  Purpose
!  =======
!
!  PZHPTRD reduces a complex Hermitian distributed matrix AP  to
!  real symmetric tridiagonal form T by a unitary similarity
!  transformation: Q**H * A * Q = T.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the mglobal atrix AP.  N >= 0.
!
!  NRL     (input) INTEGER
!          The number of local rows of the matrix AP. NRL >= 0.
!
!  AP      (input/output) COMPLEX array, dimension (LDA,N)
!          On entry, the Hermitian matrix AP.
!          The rows of the matrix are distributed among processors
!          with blocking factor 1.
!              Example for NPROC = 4 :
!              ROW | PE
!              1   | 0
!              2   | 1
!              3   | 2
!              4   | 3
!              5   | 0
!              6   | 1
!              ..  | ..

!          On exit, the diagonal and first subdiagonal
!          of A are overwritten by the corresponding elements of the
!          tridiagonal matrix T, and the elements below the first
!          subdiagonal, with the array TAU, represent the unitary
!          matrix Q as a product of elementary reflectors; 
!
!  LDA     (input) INTEGER
!          Leading dimension of the local matrix AP, LDA > NRL
!
!  D       (output) DOUBLE PRECISION array, dimension (N)
!          The diagonal elements of the tridiagonal matrix T:
!          D(i) = AP(i,i).
!
!  E       (output) DOUBLE PRECISION array, dimension (N-1)
!          The off-diagonal elements of the tridiagonal matrix T:
!          E(i) = A(i+1,i) 
!
!  TAU     (output) COMPLEX array, dimension (N-1)
!          The __SCALAR factors of the elementary reflectors (see Further
!          Details).
!
!  NPROC   (input) INTEGER
!          Number of processors
!
!  ME      (input) INTEGER
!          Index of the local processor  ( 0, 1, 2, ..., NPROC-1 )

!
!  Further Details
!  ===============
!
!  the matrix Q is represented as a product of elementary
!  reflectors
!
!     Q = H(1) H(2) . . . H(n-1).
!
!  Each H(i) has the form
!
!     H(i) = I - tau * v * v'
!
!  where tau is a complex __SCALAR, and v is a complex vector with
!  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in AP,
!  overwriting A(i+2:n,i), and tau is stored in TAU(i).
!
!  =====================================================================
!
!     .. Parameters ..
# if defined (__PARA) && defined (__MPI)
      include 'mpif.h'
# endif      

      COMPLEX  ONE, ZERO, HALF
      PARAMETER   ( ONE = ( 1.0D+0, 0.0D+0 ),ZERO = ( 0.0D+0, 0.0D+0 ),  &
     &             HALF = ( 0.5D+0, 0.0D+0 ) )
      REAL      RONE, RZERO
      PARAMETER   ( RONE = 1.0D+0, RZERO = 0.0D+0 ) 

      INTEGER QI
      INTEGER IL(N+1)
      INTEGER OW(N+1)  
      COMPLEX CTMP
      COMPLEX CTMPV(N+1)
      COMPLEX TAUL(N+1)
      COMPLEX APKI(N+1)
      REAL     TMP
      REAL     TMPV(N+1)

!     ..
!     .. Local __SCALARs ..
      INTEGER            J, I, I1, K, I2, NI1, JL
      INTEGER            KL, J1
      COMPLEX         ALPHA, TAUI
      INTEGER            KNT, IERR
      REAL             ALPHI, ALPHR, BETA, RSAFMN, SAFMIN, XNORM
!     ..
!     .. External Subroutines ..
      EXTERNAL           ZAXPY
      EXTERNAL           ZDSCAL, ZSCAL                                          
!     ..
!     .. External Functions ..
      COMPLEX         ZDOTC
      EXTERNAL           ZDOTC
      REAL             DLAMCH, DLAPY3, DZNRM2
      COMPLEX         ZLADIV
      EXTERNAL           DLAMCH, DLAPY3, DZNRM2, ZLADIV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, AIMAG, SIGN
!     cmplx removed because preprocessed
!
!     .. Executable Statements ..
!
!     Quick return if possible
!
      IF(N.LE.0) THEN
        RETURN
      END IF

      DO I = 1,N+1
        QI     = (I-1)/NPROC
        OW(I)  = MOD((I-1),NPROC) 
        IF(ME .le. OW(I) ) then
          IL(I) = QI + 1
        ELSE
          IL(I) = QI
        END IF
      END DO                                                                    
!
!        Reduce the lower triangle of A. 
!
         IF (OW(1).EQ.ME) THEN
           AP( IL(1), 1 ) = DBLE( AP( IL(1), 1 ) )
         END IF                                                             

         DO I = 1, N - 1
!
!           Generate elementary reflector H(i) = I - tau * v * v'
!           to annihilate A(i+2:n,i)
!
            IF (OW(I+1).EQ.ME) THEN
              ALPHA = AP( IL(I+1), I ) 
            END IF                                                             

#if defined (__PARA) && defined (__MPI)
            CALL MPI_BCAST(ALPHA,1,MPI_DOUBLE_COMPLEX,OW(I+1),comm,IERR)
#endif

            IF( (N-I).LE.0 ) THEN
              TAUI = RZERO
            ELSE
              IF(OW(I+2).EQ.ME) THEN
                I2 = IL(I+2)
              ELSE
                I2 = IL(I+2) + 1          ! I+2
              ENDIF
              NI1 = NRL - I2 + 1          ! N-I-1

              IF((N-I-1).GT.0) THEN
                XNORM = DZNRM2( NI1, AP( I2, I ), 1 )
#if defined __PARA
                XNORM = XNORM ** 2 
#  if defined __MPI
                CALL MPI_ALLREDUCE(XNORM,TMP,1,MPI_DOUBLE_PRECISION, &
     &                             MPI_SUM, comm,IERR)
#  endif
                XNORM = SQRT(TMP)
#endif
              ELSE
                XNORM = 0.0D0
              ENDIF

              ALPHR = DBLE( ALPHA )
              ALPHI = AIMAG( ALPHA )
              IF( XNORM.EQ.RZERO .AND. ALPHI.EQ.RZERO ) THEN
                TAUI = RZERO
              ELSE  
                BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
                SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
                RSAFMN = RONE / SAFMIN   
                IF( ABS( BETA ).LT.SAFMIN ) THEN
                  KNT = 0
   10             CONTINUE
                  KNT = KNT + 1

                  IF(NI1.GT.0) THEN
                    CALL ZDSCAL( NI1, RSAFMN, AP( I2, I ), 1 )
                  ENDIF

                  BETA = BETA*RSAFMN
                  ALPHI = ALPHI*RSAFMN
                  ALPHR = ALPHR*RSAFMN
                  IF( ABS( BETA ).LT.SAFMIN ) GO TO 10 

                  IF((N-I-1).GT.0) THEN
                    XNORM = DZNRM2( NI1, AP( I2, I ), 1 )
#if defined __PARA
                    XNORM = XNORM ** 2 
#  if defined __MPI
                    CALL MPI_ALLREDUCE(XNORM,TMP,1,MPI_DOUBLE_PRECISION, &
     &                                 MPI_SUM, comm,IERR)
#  endif
                    XNORM = SQRT(TMP)
#endif
                  ELSE
                    XNORM = 0.0D0
                  ENDIF

                  ALPHA = CMPLX( ALPHR, ALPHI )
                  BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
                  TAUI = CMPLX( (BETA-ALPHR)/BETA, -ALPHI/BETA )
                  ALPHA = ZLADIV( ONE, ALPHA-BETA )

                  IF(NI1.GT.0) THEN
                    CALL ZSCAL( NI1, ALPHA, AP( I2, I ), 1 )
                  ENDIF

                  ALPHA = BETA
                  DO J = 1, KNT
                    ALPHA = ALPHA*SAFMIN
                  END DO   

                ELSE

                  TAUI = CMPLX( (BETA-ALPHR)/BETA, -ALPHI/BETA )
                  ALPHA = ZLADIV( ONE, ALPHA-BETA )

                  IF(NI1.GT.0) THEN
                    CALL ZSCAL( NI1, ALPHA, AP( I2, I ), 1 )
                  ENDIF

                  ALPHA = BETA
                END IF
              END IF    
            ENDIF
!
            E( I ) = ALPHA
!
            IF( TAUI.NE.ZERO ) THEN
!
!              Apply H(i) from both sides to A(i+1:n,i+1:n)
!
               ! ... AP( I+1, I ) = ONE
               IF (OW(I+1).EQ.ME) THEN
                 AP( IL(I+1), I ) = ONE
               END IF
!
!              Compute  y := tau * A * v  storing y in TAU(i:n-1)
!

               ! ... broadcast A(K,I)
               IF(OW(I+1).EQ.ME) THEN
                 I1 = IL(I+1)
               ELSE
                 I1 = IL(I+1) + 1          ! I+2
               ENDIF

#if defined __PARA
               DO J = I+1, N
                 CTMPV(J) = ZERO
               END DO
               DO JL = I1, NRL
                 J = ME + (JL-1)*NPROC + 1
                 CTMPV(J) = AP(JL,I)
               END DO
#  if defined __MPI
               CALL MPI_ALLREDUCE(CTMPV(I+1),APKI(I+1),N-I, &
     &         MPI_DOUBLE_COMPLEX, MPI_SUM, comm,IERR)
#  endif
#else
               DO J = I+1,N
                 APKI(J) = AP(J,I)
               ENDDO
#endif
               DO J = I+1, N+1 
                 TAU(J-1) = ZERO
               END DO
               DO JL = I1, NRL 
                 J = ME + (JL-1)*NPROC + 1
                 TAU(J-1) = ZERO
                 DO K = I+1, J
                   TAU(J-1) = TAU(J-1) + TAUI * AP(JL,K) * APKI(K)
                 END DO
               END DO
               DO J = I+1, N 
                 IF(OW(J+1).EQ.ME) THEN
                   J1 = IL(J+1)
                 ELSE
                   J1 = IL(J+1) + 1          ! I+2
                 ENDIF
                 DO KL = J1, NRL
                   K = ME + (KL-1)*NPROC + 1
                   TAU(J-1) = TAU(J-1) + TAUI * CONJG(AP(KL,J)) * APKI(K)
                 END DO
               END DO


#if defined __PARA
               ! ... parallel sum TAU
#  if defined __MPI
               CALL MPI_ALLREDUCE(TAU(I),CTMPV(I),N-I+1, &
     &              MPI_DOUBLE_COMPLEX,MPI_SUM,comm,IERR)
#  endif
               DO J = I, N
                 TAU(J) = CTMPV(J)
               END DO
#endif
!
!              Compute  w := y - 1/2 * tau * (y'*v) * v
!
               ! ... ALPHA = -HALF*TAUI*ZDOTC(N-I,TAU(I),1,AP(I+1,I),1)

               JL = 1
               DO J = I, N
                 IF(OW(J+1).EQ.ME) THEN
                   TAUL(JL) = TAU(J)
                   JL = JL + 1
                 END IF
               END DO
               IF(OW(I+1).EQ.ME) THEN
                 I1 = IL(I+1)
               ELSE
                 I1 = IL(I+1) + 1          ! I+1
               ENDIF
               NI1 = NRL - I1 + 1          ! N-I
               ALPHA = -HALF*TAUI*ZDOTC(NI1,TAUL(1),1,AP(I1,I),1)

#if defined __PARA
#  if defined __MPI
               CALL MPI_ALLREDUCE(ALPHA,CTMP,1, &
     &         MPI_DOUBLE_COMPLEX, MPI_SUM, comm,IERR)
#  endif
               ALPHA = CTMP
#endif


#if defined __PARA
               CALL ZAXPY(NI1,ALPHA,AP(I1,I),1,TAUL(1),1)
               JL = 1
               DO J = I, N
                 CTMPV(J) = ZERO
                 IF(OW(J+1).EQ.ME) THEN
                   CTMPV(J) = TAUL(JL)
                   JL = JL + 1
                 END IF
               END DO
#  if defined __MPI
               CALL MPI_ALLREDUCE(CTMPV(I),TAU(I),N-I+1, &
     &         MPI_DOUBLE_COMPLEX, MPI_SUM, comm,IERR)
#  endif
#else
               CALL ZAXPY(N-I,ALPHA,AP(I+1,I),1,TAU(I),1)
#endif

!
!              Apply the transformation as a rank-2 update:
!                 A := A - v * w' - w * v'
!
               ! ... broadcast A(K,I)
               IF(OW(I+1).EQ.ME) THEN
                 I1 = IL(I+1)
               ELSE
                 I1 = IL(I+1) + 1          ! I+2
               ENDIF

#if defined __PARA
               DO J = I+1, N
                 CTMPV(J) = ZERO
               END DO
               DO JL = I1, NRL
                 J = ME + (JL-1)*NPROC + 1
                 CTMPV(J) = AP(JL,I)
               END DO
#  if defined __MPI
               CALL MPI_ALLREDUCE(CTMPV(I+1),APKI(I+1),N-I, &
     &         MPI_DOUBLE_COMPLEX, MPI_SUM, comm,IERR)
#  endif
#else
               DO J = I+1, N
                 APKI(J) = AP(J,I)
               END DO
#endif

               DO K = I+1,N
                 DO JL = I1,NRL
                   J = ME + (JL-1)*NPROC + 1
                   AP(JL,K) = AP(JL,K) - ONE * AP(JL,I) * CONJG(TAU(K-1)) - &
     &                CONJG(ONE) * TAU(J-1) * CONJG(APKI(K))
                 END DO
               END DO
!
            END IF
            IF(OW(I+1).EQ.ME) THEN
              AP(IL(I+1),I) = E( I )
            END IF
            IF(OW(I).EQ.ME) THEN
              D( I ) = DBLE(AP( IL(I),I ))
            END IF
#if defined __PARA && defined __MPI
            CALL MPI_BCAST(D(I),1,MPI_DOUBLE_PRECISION,OW(I), comm,IERR)
#endif
            TAU( I ) = TAUI
         END DO
         IF(OW(I).EQ.ME) THEN
            D( N ) = DBLE(AP( IL(I),I ))
         END IF
#if defined __PARA && defined __MPI
         CALL MPI_BCAST(D(N),1,MPI_DOUBLE_PRECISION,OW(I), comm,IERR)
#endif
!
      RETURN

!
!     End of ZHPTRD
!
      END SUBROUTINE pzhptrd

!==----------------------------------------------==!

   SUBROUTINE pzupgtr( n, nrl, ap, lda, tau, q, ldq, nproc, me, comm)

!
!  Parallel MPI version of the LAPACK routine ZUPGTR
!
!     Carlo Cavazzoni (carlo.cavazzoni@cineca.it) -- CINECA
!     Dicember 12, 1999
!
!  REFERENCES :
!
!     NUMERICAL RECIPES, THE ART OF SCIENTIFIC COMPUTING.
!     W.H. PRESS, B.P. FLANNERY, S.A. TEUKOLSKY, AND W.T. VETTERLING,
!     CAMBRIDGE UNIVERSITY PRESS, CAMBRIDGE.
!
!     PARALLEL NUMERICAL ALGORITHMS,
!     T.L. FREEMAN AND C.PHILLIPS,
!     PRENTICE HALL INTERNATIONAL (1992).
!
!     LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University

      IMPLICIT NONE
# if defined (__PARA) && defined (__MPI)
      include 'mpif.h'
# endif      

!
!     .. __SCALAR Arguments ..

      INTEGER            INFO, LDQ, N, LDA, NRL, NPROC, ME, comm
!     ..
!     .. Array Arguments ..
      COMPLEX         AP(LDA, * ), Q( LDQ, * ), TAU( * )
!     ..
!
!  Purpose
!  =======
!
!  PZUPGTR generates a complex unitary matrix Q which is defined as the
!  product of n-1 elementary reflectors H(i) of order n, as returned by
!  PZHPTRD :
!
!  Q = H(1) H(2) . . . H(n-1).
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the mglobal atrix AP.  N >= 0.
!
!  NRL     (input) INTEGER
!          The number of local rows of the matrix AP. NRL >= 0.
!
!  AP      (input) COMPLEX array, dimension (LDA,N)
!          The vectors which define the elementary reflectors, as
!          returned by PZHPTRD.
!          The rows of the matrix are distributed among processors
!          with blocking factor 1.
!              Example for NPROC = 4 :
!              ROW | PE
!              1   | 0
!              2   | 1
!              3   | 2
!              4   | 3
!              5   | 0
!              6   | 1
!              ..  | ..
!
!  LDA     (input) INTEGER
!          Leading dimension of the local matrix AP, LDA > NRL
!
!  TAU     (input) COMPLEX array, dimension (N-1)
!          TAU(i) must contain the __SCALAR factor of the elementary
!          reflector H(i), as returned by PZHPTRD.
!
!  Q       (output) COMPLEX array, dimension (LDQ,N)
!          The N-by-N unitary matrix Q.
!          The rows of the matrix are distributed among processors
!          in the same way of the matrix AP
!
!  LDQ     (input) INTEGER
!          The leading dimension of the array Q. LDQ >= max(1,NRL).
!
!  NPROC   (input) INTEGER
!          Number of processors
!
!  ME      (input) INTEGER
!          Index of the local processor  ( 0, 1, 2, ..., NPROC-1 )
!
!  =====================================================================
!
!     .. Parameters ..

      COMPLEX         ONE, ZERO
      PARAMETER          ( ONE = (1.0D+0,0.0D+0), ZERO = (0.0D+0,0.0D+0) ) 

      INTEGER QI
      INTEGER IL(N+1)
      INTEGER OW(N+1)
      COMPLEX CTMP
      COMPLEX CTMPV(N+1)
      COMPLEX WORK(N+1)

!     ..
!     .. Local __SCALARs ..
      INTEGER            I, IINFO, J, K, JL, KL, J1, I1, I2, NI1, L, IERR
!     ..

!     .. Executable Statements ..
!
!     Test the input arguments
!
!     Quick return if possible
!
      IF( N.EQ.0 ) THEN
        RETURN
      END IF

!
      DO I = 1,N+1
        QI     = (I-1)/NPROC
        OW(I)  = MOD((I-1),NPROC)
        IF(ME .le. OW(I) ) then
          IL(I) = QI + 1
        ELSE
          IL(I) = QI
        END IF
      END DO
!
!        Unpack the vectors which define the elementary reflectors and
!        set the first row and column of Q equal to those of the unit
!        matrix
!
      IF(OW(1).EQ.ME) THEN
        Q( IL(1), 1 ) = ONE
        DO KL = 2, NRL
          Q( KL, 1 ) = ZERO
        END DO
        DO J = 2, N
          Q( IL(1), J ) = ZERO
        END DO
      ELSE
        DO KL = 1, NRL
          Q( KL, 1 ) = ZERO
        END DO
      ENDIF

      DO J = 2, N
        IF(OW(J+1).EQ.ME) THEN
          J1 = IL(J+1)
        ELSE
          J1 = IL(J+1) + 1 
        ENDIF
        DO KL = J1, NRL
          Q( KL, J ) = AP( KL, J-1 )
        END DO
      END DO

      IF( N.GT.1 ) THEN
!
!           Generate Q(2:n,2:n)
!
        DO I = N-1, 1, -1
!
!         Apply H(i) to A(i:m,i:n) from the left
!
          IF( I.LT.(N-1) ) THEN

            IF(OW(I+1).EQ.ME) THEN
              Q( IL(I+1), I+1 ) = ONE
            END IF
!
!           Form  H * C
!
            IF( TAU(I).NE.ZERO ) THEN
!
!             w := C' * v
!
              IF(OW(I+1).EQ.ME) THEN
                I1 = IL(I+1)
              ELSE
                I1 = IL(I+1) + 1 
              ENDIF
              DO J = 1, N-1-I
                CTMP = ZERO
                DO KL = I1, NRL
                  CTMP = CTMP + CONJG( Q( KL, J+I+1 ) ) * Q( KL,I+1 )  
                END DO
                WORK(J) = CTMP
              END DO 

#if defined __PARA
#  if defined __MPI
              CALL MPI_ALLREDUCE(WORK,CTMPV,N-1-I,MPI_DOUBLE_COMPLEX, &
     &             MPI_SUM, comm,IERR)
#  endif
              DO J = 1,N-1-I
                WORK(J) = CTMPV(J)
              END DO
#endif
!
!             C := C - v * w'
!
              DO J = 1, N-1-I
                CTMP = -TAU(I) * CONJG( WORK( J ) ) 
                DO KL = I1, NRL
                  Q( KL, J+I+1 ) = Q( KL, J+I+1 ) + CTMP * Q(KL, I+1)    
                END DO
              END DO 
            END IF
          END IF

          IF( I.LT.(N-1) ) THEN
            IF(OW(I+2).EQ.ME) THEN
              I2 = IL(I+2)              ! I+2
            ELSE
              I2 = IL(I+2) + 1          ! local ind. of the first element > I+2 
            ENDIF
            NI1 = NRL - I2 + 1          ! N-I-1
            CALL ZSCAL( NI1, -TAU( I ), Q( I2, I+1 ), 1 )
          END IF

          IF(OW(I+1).EQ.ME) THEN
            Q( IL(I+1), I+1 ) = ONE - TAU( I )
          END IF
!
!             Set A(1:i-1,i) to zero
!
          DO L = 1, I - 1
            IF(OW(L+1).EQ.ME) THEN
              Q( IL(L+1), I+1 ) = ZERO
            END IF
          END DO
        END DO   
      END IF


      RETURN

!
!     End of ZUPGTR
!
      END SUBROUTINE pzupgtr

!==----------------------------------------------==!

      SUBROUTINE pzsteqr( compz, n, nrl, d, e, z, ldz, nproc, me, comm )
!
!  Parallel MPI version of the LAPACK routine ZHPTRD
!
!     Carlo Cavazzoni (carlo.cavazzoni@cineca.it) -- CINECA
!     Dicember 12, 1999
!
!  REFERENCES :
!
!     NUMERICAL RECIPES, THE ART OF SCIENTIFIC COMPUTING.
!     W.H. PRESS, B.P. FLANNERY, S.A. TEUKOLSKY, AND W.T. VETTERLING,
!     CAMBRIDGE UNIVERSITY PRESS, CAMBRIDGE.
!
!     PARALLEL NUMERICAL ALGORITHMS,
!     T.L. FREEMAN AND C.PHILLIPS,
!     PRENTICE HALL INTERNATIONAL (1992).
!
!     LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!

      IMPLICIT NONE

# if defined (__PARA) && defined (__MPI)
      include 'mpif.h'
# endif      

!     .. __SCALAR Arguments ..
      CHARACTER          COMPZ
      INTEGER            LDZ, N, NRL, NPROC, ME, comm
!     ..
!     .. Array Arguments ..
      REAL   D( * ), E( * )
      COMPLEX         Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  PZSTEQR computes all eigenvalues and, optionally, eigenvectors of a
!  symmetric tridiagonal matrix using the implicit QL or QR method.
!  The eigenvectors of a full or band complex Hermitian matrix can also
!  be found if PZHPTRD has been used to reduce this
!  matrix to tridiagonal form.
!
!  Arguments
!  =========
!
!  COMPZ   (input) CHARACTER*1
!          = 'N':  Compute eigenvalues only.
!          = 'V':  Compute eigenvalues and eigenvectors of the original
!                  Hermitian matrix.  On entry, Z must contain the
!                  unitary matrix used to reduce the original matrix
!                  to tridiagonal form.
!          = 'I':  Compute eigenvalues and eigenvectors of the
!                  tridiagonal matrix.  Z is initialized to the identity
!                  matrix.
!
!  N       (input) INTEGER
!          The order of the mglobal atrix AP.  N >= 0.
!
!  NRL     (input) INTEGER
!          The number of local rows of the matrix AP. NRL >= 0.
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the diagonal elements of the tridiagonal matrix.
!          On exit, if INFO = 0, the eigenvalues in ascending order.
!
!  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, the (n-1) subdiagonal elements of the tridiagonal
!          matrix.
!          On exit, E has been destroyed.
!
!  Z       (input/output) COMPLEX array, dimension (LDZ, N)
!          On entry, if  COMPZ = 'V', then Z contains the unitary
!          matrix used in the reduction to tridiagonal form.
!          On exit if COMPZ = 'V', Z contains the
!          orthonormal eigenvectors of the original Hermitian matrix,
!          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
!          of the symmetric tridiagonal matrix.
!          If COMPZ = 'N', then Z is not referenced.
!          The rows of the matrix are distributed among processors
!          with blocking factor 1, i.e. for NPROC = 4 :
!              ROW Index | Processor index owning the row 
!                    1   |    0
!                    2   |    1
!                    3   |    2
!                    4   |    3
!                    5   |    0
!                    6   |    1
!                    ..  |    ..
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDZ >= 1, and if
!          eigenvectors are desired, then  LDZ >= max(1,NRL).
!
!  NPROC   (input) INTEGER
!          Number of processors
!
!  ME      (input) INTEGER
!          Index of the local processor  ( 0, 1, 2, ..., NPROC-1 )
!
!  =====================================================================
!
!     .. Parameters ..
      REAL   ZERO, ONE, TWO, THREE, CTEMP, STEMP
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0, &
     &                   THREE = 3.0D0 )
      COMPLEX         CZERO, CONE,ZTEMP
      PARAMETER          ( CZERO = ( 0.0D0, 0.0D0 ), CONE = ( 1.0D0, 0.0D0 ) )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 30 )
!     ..

      INTEGER QI, KL, INFO
      INTEGER IL(N+1)
      INTEGER OW(N+1)
      REAL  WORK(2*N)

      REAL  t2

!     .. Local __SCALARs ..
      INTEGER            I, ICOMPZ, II, ISCALE, J, JTOT, K, L, L1, LEND, &
     &                   LENDM1, LENDP1, LENDSV, LM1, LSV, M, MM, MM1,   &
     &                   NM1, NMAXIT
      REAL   ANORM, B, C, EPS, EPS2, F, G, P, R, RT1, RT2,   &
     &                   S, SAFMAX, SAFMIN, SSFMAX, SSFMIN, TST
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      REAL   DLAMCH, DLANST, DLAPY2
      EXTERNAL           LSAME, DLAMCH, DLANST, DLAPY2
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLAE2, DLAEV2, DLARTG, DLASCL, DLASRT, XERBLA, &
     &                   ZLASET, ZLASR, ZSWAP
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN, SQRT
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0

!
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ICOMPZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      END IF
      IF( ICOMPZ.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( (LDZ.LT.1) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX(1,NRL) ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZSTEQR', -INFO )
         RETURN
      END IF
!
!     Quick return if possible
!
      IF(N.LE.0) THEN
        RETURN
      END IF
!
      DO I = 1,N+1
        QI     = (I-1)/NPROC
        OW(I)  = MOD((I-1),NPROC)
        IF(ME .le. OW(I) ) then
          IL(I) = QI + 1
        ELSE
          IL(I) = QI
        END IF
      END DO

      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.EQ.2 .AND. OW(1).EQ.ME ) Z( IL(1), 1 ) = CONE
         RETURN
      END IF
!
!     Determine the unit roundoff and over/underflow thresholds.
!
      EPS = DLAMCH( 'E' )
      EPS2 = EPS**2
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      SSFMAX = SQRT( SAFMAX ) / THREE
      SSFMIN = SQRT( SAFMIN ) / EPS2
!
!     Compute the eigenvalues and eigenvectors of the tridiagonal
!     matrix.
!
      IF( ICOMPZ.EQ.2 ) THEN
        CALL ZLASET( 'Full', NRL, N, CZERO, CZERO, Z, LDZ )
        DO J = 1, N
          IF(OW(J).EQ.ME) THEN
            Z( IL(J), J ) = CONE
          END IF
        END DO
      END IF
!
      NMAXIT = N*MAXIT
      JTOT = 0
!
!     Determine where the matrix splits and choose QL or QR iteration
!     for each block, according to whether top or bottom diagonal
!     element is smaller.
!
      L1 = 1
      NM1 = N - 1
!
   10 CONTINUE

      IF( L1.GT.N )   GO TO 160
      IF( L1.GT.1 )   E( L1-1 ) = ZERO
      IF( L1.LE.NM1 ) THEN
         DO 20 M = L1, NM1
            TST = ABS( E( M ) )
            IF( TST.EQ.ZERO )        GO TO 30
            IF( TST.LE.( SQRT(ABS(D(M)))*SQRT(ABS(D(M+1))))*EPS ) THEN
               E( M ) = ZERO
               GO TO 30
            END IF
   20    CONTINUE
      END IF
      M = N
!
   30 CONTINUE
      L = L1
      LSV = L
      LEND = M
      LENDSV = LEND
      L1 = M + 1
      IF( LEND.EQ.L )  GO TO 10
!
!     Scale submatrix in rows and columns L to LEND
!
      ANORM = DLANST( 'I', LEND-L+1, D( L ), E( L ) )
      ISCALE = 0
      IF( ANORM.EQ.ZERO )   GO TO 10
      IF( ANORM.GT.SSFMAX ) THEN
         ISCALE = 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L+1, 1, D( L ), N, INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L, 1, E( L ), N, INFO )
      ELSE IF( ANORM.LT.SSFMIN ) THEN
         ISCALE = 2
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L+1, 1, D( L ), N, INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L, 1, E( L ), N, INFO )
      END IF
!
!     Choose between QL and QR iteration
!
      IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN
         LEND = LSV
         L = LENDSV
      END IF
!
      IF( LEND.GT.L ) THEN
!
!        QL Iteration
!
!        Look for small subdiagonal element.
!
   40    CONTINUE
         IF( L.NE.LEND ) THEN
            LENDM1 = LEND - 1
            DO 50 M = L, LENDM1
               TST = ABS( E( M ) )**2
               IF( TST.LE.( EPS2*ABS(D(M)) )*ABS(D(M+1))+ SAFMIN )GO TO 60
   50       CONTINUE
         END IF
!
         M = LEND
!
   60    CONTINUE
         IF( M.LT.LEND )  E( M ) = ZERO
         P = D( L )
         IF( M.EQ.L )     GO TO 80
!
!        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
!        to compute its eigensystem.
!
         IF( M.EQ.L+1 ) THEN
            IF( ICOMPZ.GT.0 ) THEN
               CALL DLAEV2( D( L ), E( L ), D( L+1 ), RT1, RT2, C, S )
               WORK( L ) = C
               WORK( N-1+L ) = S
               CTEMP = WORK( L )
               STEMP = WORK( N-1+L )
               IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                  DO KL = 1, NRL
                     ZTEMP = Z( KL, 1+L )
                     Z( KL, 1+L ) = CTEMP*ZTEMP - STEMP*Z( KL, L )
                     Z( KL, L )   = STEMP*ZTEMP + CTEMP*Z( KL, L )
                  END DO
               END IF
            ELSE
               CALL DLAE2( D( L ), E( L ), D( L+1 ), RT1, RT2 )
            END IF
            D( L ) = RT1
            D( L+1 ) = RT2
            E( L ) = ZERO
            L = L + 2
            IF( L.LE.LEND )     GO TO 40
            GO TO 140
         END IF
!
         IF( JTOT.EQ.NMAXIT )   GO TO 140
         JTOT = JTOT + 1
!
!        Form shift.
!
         G = ( D( L+1 )-P ) / ( TWO*E( L ) )
         R = DLAPY2( G, ONE )
         G = D( M ) - P + ( E( L ) / ( G+SIGN( R, G ) ) )
!
         S = ONE
         C = ONE
         P = ZERO
!
!        Inner loop
!
         MM1 = M - 1
         DO 70 I = MM1, L, -1
            F = S*E( I )
            B = C*E( I )
            CALL DLARTG( G, F, C, S, R )
            IF( I.NE.M-1 )  E( I+1 ) = R
            G = D( I+1 ) - P
            R = ( D( I )-G )*S + TWO*C*B
            P = S*R
            D( I+1 ) = G + P
            G = C*R - B
!
!           If eigenvectors are desired, then save rotations.
!
            IF( ICOMPZ.GT.0 ) THEN
               WORK( I ) = C
               WORK( N-1+I ) = -S
            END IF
!
   70    CONTINUE
!
!        If eigenvectors are desired, then apply saved rotations.
!
         IF( ICOMPZ.GT.0 ) THEN
           DO J = M - L + 1 - 1, 1, -1
             CTEMP =  WORK( L + J -1)
             STEMP =  WORK( N-1+L + J-1)
             IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
               DO KL = 1, NRL
                 ZTEMP = Z( KL, J+1+L-1 )
                 Z( KL, J+1+L-1 ) = CTEMP*ZTEMP - STEMP*Z( KL, J+L-1 )
                 Z( KL, J+L-1 ) = STEMP*ZTEMP + CTEMP*Z( KL, J+L-1 )
               END DO
             END IF
           END DO                                                         
         END IF
!
         D( L ) = D( L ) - P
         E( L ) = G
         GO TO 40
!
!        Eigenvalue found.
!
   80    CONTINUE
         D( L ) = P
!
         L = L + 1
         IF( L.LE.LEND )   GO TO 40
         GO TO 140
!
      ELSE
!
!        QR Iteration
!
!        Look for small superdiagonal element.
!
   90    CONTINUE
         IF( L.NE.LEND ) THEN
            LENDP1 = LEND + 1
            DO 100 M = L, LENDP1, -1
               TST = ABS( E( M-1 ) )**2
               IF( TST.LE.(EPS2*ABS(D(M)))*ABS(D(M-1))+ SAFMIN )GO TO 110
  100       CONTINUE
         END IF
!
         M = LEND
!
  110    CONTINUE
         IF( M.GT.LEND )   E( M-1 ) = ZERO
         P = D( L )
         IF( M.EQ.L )      GO TO 130
!
!        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
!        to compute its eigensystem.
!
         IF( M.EQ.L-1 ) THEN
            IF( ICOMPZ.GT.0 ) THEN
               CALL DLAEV2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2, C, S )
               WORK( M ) = C
               WORK( N-1+M ) = S
               CTEMP = WORK( M )
               STEMP = WORK( N-1+M )
               IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                  DO KL = 1, NRL
                     ZTEMP = Z( KL, L)
                     Z( KL, L )   = CTEMP*ZTEMP - STEMP*Z( KL, L-1 )
                     Z( KL, L-1 ) = STEMP*ZTEMP + CTEMP*Z( KL, L-1 )
                  END DO
               END IF
            ELSE
               CALL DLAE2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2 )
            END IF
            D( L-1 ) = RT1
            D( L ) = RT2
            E( L-1 ) = ZERO
            L = L - 2
            IF( L.GE.LEND )    GO TO 90
            GO TO 140
         END IF
!
         IF( JTOT.EQ.NMAXIT )  GO TO 140
         JTOT = JTOT + 1
!
!        Form shift.
!
         G = ( D( L-1 )-P ) / ( TWO*E( L-1 ) )
         R = DLAPY2( G, ONE )
         G = D( M ) - P + ( E( L-1 ) / ( G+SIGN( R, G ) ) )
!
         S = ONE
         C = ONE
         P = ZERO
!
!        Inner loop
!
         LM1 = L - 1
         DO 120 I = M, LM1
            F = S*E( I )
            B = C*E( I )
            CALL DLARTG( G, F, C, S, R )
            IF( I.NE.M )     E( I-1 ) = R
            G = D( I ) - P
            R = ( D( I+1 )-G )*S + TWO*C*B
            P = S*R
            D( I ) = G + P
            G = C*R - B
!
!           If eigenvectors are desired, then save rotations.
!
            IF( ICOMPZ.GT.0 ) THEN
               WORK( I ) = C
               WORK( N-1+I ) = S
            END IF
!
  120    CONTINUE
!
!        If eigenvectors are desired, then apply saved rotations.
!
         IF( ICOMPZ.GT.0 ) THEN
            DO J = 1, L - M
               CTEMP = WORK( M+J-1 )
               STEMP = WORK( N-1+M+J-1 )
               IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                  DO KL = 1, NRL
                     ZTEMP = Z( KL, J+M )
                     Z( KL, J+M )   = CTEMP*ZTEMP - STEMP*Z(KL, J+M-1)
                     Z( KL, J+M-1 ) = STEMP*ZTEMP + CTEMP*Z(KL, J+M-1)
                  END DO
               END IF
            END DO                                                         
         END IF
!
         D( L ) = D( L ) - P
         E( LM1 ) = G
         GO TO 90
!
!        Eigenvalue found.
!
  130    CONTINUE
         D( L ) = P
!
         L = L - 1
         IF( L.GE.LEND )   GO TO 90
         GO TO 140
!
      END IF
!
!     Undo scaling if necessary
!
  140 CONTINUE

      IF( ISCALE.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV+1, 1, &
     &                D( LSV ), N, INFO )
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV, 1, E( LSV ), &
     &                N, INFO )
      ELSE IF( ISCALE.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV+1, 1, &
     &                D( LSV ), N, INFO )
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV, 1, E( LSV ), &
     &                N, INFO )
      END IF
!
!     Check for no convergence to an eigenvalue after a total
!     of N*MAXIT iterations.
!
      IF( JTOT .EQ. NMAXIT ) THEN
         DO 150 I = 1, N - 1
            IF( E( I ) .NE. ZERO )  INFO = INFO + 1
  150    CONTINUE
         WRITE(6,*) 'WARNING pzsteqr, convergence not achieved INFO = ', INFO
         RETURN
      END IF
      GO TO 10
!
!     Order eigenvalues and eigenvectors.
!
  160 CONTINUE

      IF( ICOMPZ.EQ.0 ) THEN
!
!        Use Quick Sort
!
         CALL DLASRT( 'I', N, D, INFO )
!
      ELSE
!
!        Use Selection Sort to minimize swaps of eigenvectors
!
         DO 180 II = 2, N
            I = II - 1
            K = I
            P = D( I )
            DO 170 J = II, N
               IF( D( J ).LT.P ) THEN
                  K = J
                  P = D( J )
               END IF
  170       CONTINUE
            IF( K.NE.I ) THEN
               D( K ) = D( I )
               D( I ) = P
               CALL ZSWAP( NRL, Z( 1, I ), 1, Z( 1, K ), 1 )
            END IF
  180    CONTINUE
      END IF

      RETURN
!
!     End of ZSTEQR
!
      END SUBROUTINE pzsteqr

!==----------------------------------------------==!

   SUBROUTINE zhpev_drv( JOBZ, UPLO, N, AP, W, Z, LDZ )

        IMPLICIT NONE

        CHARACTER ::       JOBZ, UPLO
        INTEGER   ::       IOPT, INFO, LDZ, N
        COMPLEX ::  AP( * ), Z( LDZ, * )
        REAL ::  W( * )
        REAL, ALLOCATABLE :: RWORK(:)
        COMPLEX, ALLOCATABLE :: ZWORK(:)

#if defined __AIX
        IOPT = 0
        IF((JOBZ .EQ. 'V') .OR. (JOBZ .EQ. 'v') ) iopt = iopt + 1
        IF((UPLO .EQ. 'U') .OR. (UPLO .EQ. 'u') ) iopt = iopt + 20
        ALLOCATE( rwork( 4*n ) )
        CALL ZHPEV(IOPT, ap, w, z, ldz, n, rwork, 4*n)
        DEALLOCATE( rwork )
#else
        ALLOCATE( rwork( MAX(1, 3*n-2) ), zwork( MAX(1, 2*n-1)) )
        CALL ZHPEV(jobz, uplo, n, ap, w, z, ldz, zwork, rwork, INFO)
        DEALLOCATE( rwork, zwork )
        IF( info .NE. 0 ) THEN
          ERROR("diagonalization failed")
        END IF
#endif
        RETURN
   END SUBROUTINE zhpev_drv

!==----------------------------------------------==!

   SUBROUTINE pzhpev_drv( jobz, ap, lda, w, z, ldz, &
                          nrl, n, nproc, mpime, comm_in )

     IMPLICIT NONE
# if defined (__PARA) && defined (__MPI)
      include 'mpif.h'
# endif      
     CHARACTER :: JOBZ
     INTEGER   :: lda, ldz, nrl, n, nproc, mpime
     INTEGER, OPTIONAL, INTENT(IN) :: comm_in
     COMPLEX :: ap( lda, * ), z( ldz, * )
     REAL :: w( * )
     REAL :: rwork( n )
     COMPLEX :: cwork( n )
     INTEGER   :: comm
     !
#if defined (__PARA) && defined (__MPI)
     IF ( PRESENT( comm_in ) ) THEN
        comm = comm_in
     ELSE
        comm = MPI_COMM_WORLD
     END IF
#endif
     CALL pzhptrd( n, nrl, ap, lda, w, rwork, cwork, nproc, mpime, comm)
     IF( jobz == 'V' .OR. jobz == 'v' ) THEN
        CALL pzupgtr( n, nrl, ap, lda, cwork, z, ldz, nproc, mpime, comm)
     END IF
     CALL pzsteqr( jobz, n, nrl, w, rwork, z, ldz, nproc, mpime, comm)
     RETURN
   END SUBROUTINE pzhpev_drv

SUBROUTINE mytrasp_dist(ax, lda, dista, bx, ldb, distb, n, mpime, nproc)
  IMPLICIT NONE
#if defined __MPI
  INCLUDE 'mpif.h'
  INTEGER ISTATUS(MPI_STATUS_SIZE), ierr
#endif
  INTEGER :: lda, ldb, n, mpime, nproc
  CHARACTER(LEN=1) :: dista, distb
  COMPLEX :: ax(lda,n), bx(ldb,n)
  COMPLEX, ALLOCATABLE :: bs(:,:), br(:,:)
  COMPLEX, ALLOCATABLE :: as(:,:), ar(:,:)
  COMPLEX, ALLOCATABLE :: cs(:,:), cr(:,:), cc(:,:)
  INTEGER :: ip, i, ii, j, jj, k, kk, nloc_ip, nloc
  INTEGER :: nloc_src, nloc_dst, idest, isour
  CHARACTER(LEN=1) :: da, db

  INTEGER :: imina, iminb, jmina, jminb, imaxa, imaxb, jmaxa, jmaxb

  imina=lbound(ax,1)
  imaxa=ubound(ax,1)
  jmina=lbound(ax,2)
  jmaxa=ubound(ax,2)

  iminb=lbound(bx,1)
  imaxb=ubound(bx,1)
  jminb=lbound(bx,2)
  jmaxb=ubound(bx,2)

  da = 'R'
  db = 'R'
  IF( dista == 'c' .OR. dista == 'C' ) THEN
    da = 'C'
  END IF
  IF( distb == 'c' .OR. distb == 'C' ) THEN
    db = 'C'
  END IF

  nloc = ldim_cyclic(n, nproc, mpime)

    IF( da == 'R' .AND. db == 'R' ) THEN

        DO j = 1, n
          DO i = 1, nloc
            bx(i,(jminb-1+j)) = ax(i,(jmina-1+j))
          END DO
        END DO

    ELSE IF( da == 'R' .AND. db == 'C' ) THEN

      DO ip = 0, nproc - 1
        isour = MOD(mpime-ip+nproc, nproc)
        idest = MOD(mpime+ip      , nproc)
        nloc_src = ldim_cyclic(n, nproc, isour)
        nloc_dst = ldim_cyclic(n, nproc, idest)
        ALLOCATE( cs( nloc, nloc_dst ) )
        ALLOCATE( cr( nloc_src, nloc ) )
        jj = idest+jminb
        DO j = 1, nloc_dst
          DO i = 1, nloc
            cs(i,j) = ax(i,jj)
          END DO
          jj = jj + nproc
        END DO
#if defined __MPI
        CALL MPI_SENDRECV(cs, SIZE(cs), MPI_DOUBLE_COMPLEX,  &
           IDEST, ip,  cr, SIZE(cr), MPI_DOUBLE_COMPLEX, &
           ISOUR, ip, MPI_COMM_WORLD, ISTATUS, ierr)
        IF(ierr .NE. 0) THEN
           ERROR("ERROR in sendrecv")
           STOP
        END IF
#else
        cr = cs
#endif
        DO j = 1, nloc
          ii = isour+jmina
          DO i = 1, nloc_src
            bx(j,ii) = cr(i,j)
            ii = ii + nproc
          END DO
        END DO
        DEALLOCATE( cs )
        DEALLOCATE( cr )
      END DO

    ELSE IF( da == 'C' .AND. db == 'R' ) THEN

      DO ip = 0, nproc - 1
        isour = MOD(mpime-ip+nproc, nproc)
        idest = MOD(mpime+ip      , nproc)
        nloc_src = ldim_cyclic(n, nproc, isour)
        nloc_dst = ldim_cyclic(n, nproc, idest)
        ALLOCATE( cs( nloc_dst, nloc ) )
        ALLOCATE( cr( nloc, nloc_src ) )
        DO i = 1, nloc
          jj = idest+iminb
          DO j = 1, nloc_dst
            cs(i, j) = ax(jj, i)
            jj = jj + nproc
          END DO
        END DO
#if defined __MPI
        CALL MPI_SENDRECV(cs, SIZE(cs), MPI_DOUBLE_COMPLEX,  &
           IDEST, ip,  cr, SIZE(cr), MPI_DOUBLE_COMPLEX, &
           ISOUR, ip, MPI_COMM_WORLD, ISTATUS, ierr)
        IF(ierr .NE. 0) THEN
           ERROR("ERROR in sendrecv")
        END IF
#else
        cr = cs
#endif
        ii = isour+imina
        DO i = 1, nloc_src
          DO j = 1, nloc
            bx(j,ii) = cr(j,i)
          END DO
          ii = ii + nproc
        END DO
        DEALLOCATE( cs )
        DEALLOCATE( cr )
      END DO

    ELSE IF( da == 'C' .AND. db == 'C' ) THEN

        DO j = 1, nloc
          DO i = 1, n
            bx(i,(jminb-1+j)) = ax(i,(jmina-1+j))
          END DO
        END DO

    ELSE
       ERROR("Wrong transpose kind") 
    END IF


  RETURN
END SUBROUTINE mytrasp_dist

function ldim_cyclic(n,nproc,myrank)
integer :: ldim_cyclic
integer, intent(in) :: n, nproc, myrank

integer :: i, tmp(1:n)

do i=1,n
  tmp(i)=modulo((i-1),nproc)
enddo

ldim_cyclic = count(tmp(:)==myrank)

end function ldim_cyclic

end module num_la_parall_module
