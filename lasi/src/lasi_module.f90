!
! Linear Algebra Subroutine Interfaces (LASI)
! Copyright (C) 2004 Andrea Ferretti
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!-------------------------------------------------------
! Linear Algebra Subroutines Interfaces (LASI)
! Last Revision  28.12.2004  ANDREA
!-------------------------------------------------------
!

! <INFO>
!*********************************************
   MODULE lasi_kinds
!*********************************************
#  include "lasi_config.h"
   IMPLICIT NONE
      PRIVATE
      REAL                 :: rtmp = 0.0
      DOUBLE PRECISION     :: dtmp = 0.0d0

      INTEGER, PARAMETER   :: sgl = KIND(rtmp)
      INTEGER, PARAMETER   :: dbl = KIND(dtmp)
!
! ... alternative definition, less standard wrt BLAS and LAPACK implem. 
!      INTEGER, PARAMETER   :: dbl = __LASI_KIND_DOUBLE
!      INTEGER, PARAMETER   :: sgl = __LASI_KIND_SINGLE

      PUBLIC :: sgl, dbl
END MODULE lasi_kinds


!*********************************************
   MODULE lasi_module
!*********************************************
   USE lasi_kinds, ONLY : sgl, dbl
   IMPLICIT NONE
   PUBLIC

! This module contains INTERFACES to some linear algreba routines
! (BLAS, LAPACK). Lapack interfaces (apart slight changes) are freely 
! taken from:
! 
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
! which is aknowledged.
!
! All definitions about kinds (machine dependent) and required
! interfaces (interface switches) should be modified in 
! lasi_config.h ; the code present in this file should usually NOT
! be modified.
!
! All preprocessor macros starting with __LASI_ are supposed to be 
! used only by this library. Other preproc variables are not used. 
!
! The convention about names of interfaced routine is:
! lasi_<name>, 
! where <name> is usually equal to the name of the called std routine
! without the first character specifing the type and kind.
! Here below the full list of names is reported.
!
!----------------------------------------
! BLAS interfaced in this module:
!
! **** BLAS 1 ****
! FUNCTION idamax isamax icamax izamax  -> lasi_IAMAX(n,dx,incx)
! FUNCTION  snrm2  dnrm2 scnrm2 dznmr2  -> lasi_NRM2(n,x,incx)
! FUNCTION  sasum  dasum scasum dzasum  -> lasi_ASUM(n,x,incx)
! FUNCTION               cdotu  zdotu   -> lasi_DOTU(n,x,incx,y,incy)
! FUNCTION               cdotc  zdotc   -> lasi_DOTC(n,x,incx,y,incy)
! FUNCTION  sdot   ddot                 -> lasi_DOT(n,x,incx,y,incy)
! FUNCTION  sdsdot sdot                 -> lasi_DSDOT(n,x,incx,y,incy)
! SUBROUTINE saxpy daxpy caxpy  zaxpy   -> lasi_AXPY(n,ca,cx,incx,cy,incy)
! SUBROUTINE scopy dcopy ccopy  zcopy   -> lasi_COPY(n,cx,incx,cy,incy)
! SUBROUTINE srotg drotg crotg  zrotg   -> lasi_ROTG(a,b,c,s)
! SUBROUTINE srot  drot  csrot  zdrot   -> lasi_ROT(n,x,incx,y,incy,c,s)
! SUBROUTINE sscal dscal cscal  zscal  
!                        csscal zdscal  -> lasi_SCAL(n,a,x,incx)
! SUBROUTINE sswap dswap cswap  zswap   -> lasi_SWAP(n,x,incx,y,incy)
!
! **** BLAS 2 ****
! SUBROUTINE sgbmv dgbmv cgbmv zgbmv    -> lasi_GBMV(trans,m,kl,ku,n,alpha,A,lda,
!                                                    X,incx,beta,Y,incy)
! SUBROUTINE sgemv dgemv cgemv zgemv    -> lasi_GEMV(trans,m,n,alpha,A,lda,
!                                                    X,incx,beta,Y,incy)
! SUBROUTINE             cgerc zgerc    -> lasi_GERC(m,n,alpha,x,incx,y,incy,A,lda)
! SUBROUTINE             cgeru zgeru    -> lasi_GERU(m,n,alpha,x,incx,y,incy,A,lda)
! SUBROUTINE             chbmv zhbmv    -> lasi_HBMV(uplo,n,k,alpha,A,lda,X,incx,beta,Y,incy)
! SUBROUTINE             chemv zhemv    -> lasi_HEMV(uplo,n,alpha,A,lda,X,incx,beta,Y,incy)
! SUBROUTINE             cher  zher     -> lasi_HER(uplo,n,alpha,x,incx,A,lda)
! SUBROUTINE             cher2 zher2    -> lasi_HER2(uplo,n,alpha,x,incx,y,incy,A,lda)
! SUBROUTINE             chpr  zhpr     -> lasi_HPR(uplo,n,alpha,x,incx,AP)
! SUBROUTINE             chpr2 zhpr2    -> lasi_HPR2(uplo,n,alpha,x,incx,y,incy,AP)
! SUBROUTINE             chpmv zhpmv    -> lasi_HPMV(uplo,n,alpha,AP,X,incx,beta,Y,incy)
! SUBROUTINE stbmv dtbmv ctbmv ztbmv    -> lasi_TBMV(uplo,trans,diag,n,k,A,lda,X,incx)
! SUBROUTINE stpmv dtpmv ctpmv ztpmv    -> lasi_TPMV(uplo,trans,diag,n,AP,X,incx)
! SUBROUTINE strmv dtrmv ctrmv ztrmv    -> lasi_TRMV(uplo,trans,diag,n,A,lda,X,incx)
! SUBROUTINE ssymv dsymv                -> lasi_SYMV(uplo,n,alpha,A,lda,X,incx,beta,Y,incy)
! SUBROUTINE ssbmv dsbmv                -> lasi_SBMV(uplo,n,k,alpha,A,lda,X,incx,beta,Y,incy)
! SUBROUTINE sspmv dspmv                -> lasi_SPMV(uplo,n,alpha,AP,X,incx,beta,Y,incy)
! SUBROUTINE stbsv dtbsv ctbsv ztbsv    -> lasi_TBSV(uplo,trans,diag,n,k,A,lda,X,incx)
! SUBROUTINE stpsv dtpsv ctpsv ztpsv    -> lasi_TPSV(uplo,trans,diag,n,AP,X,incx)
! SUBROUTINE strsv dtrsv ctrsv ztrsv    -> lasi_TRSV(uplo,trans,diag,n,A,lda,X,incx)
! SUBROUTINE  sger  dger                -> lasi_GER(m,n,alpha,x,incx,y,incy,A,lda)
! SUBROUTINE  sspr  dspr                -> lasi_SPR(uplo,n,alpha,x,incx,AP)
! SUBROUTINE sspr2 dspr2                -> lasi_SPR2(uplo,n,alpha,x,incx,y,incy,AP)
! SUBROUTINE  ssyr  dsyr                -> lasi_SYR(uplo,n,alpha,x,incx,A,lda)
! SUBROUTINE ssyr2 dsyr2                -> lasi_SYR2(uplo,n,alpha,x,incx,y,incy,A,lda)
!
! **** BLAS 3 ****
! SUBROUTINE sgemm dgemm cgemm zgemm    -> lasi_GEMM(transA,transB,m,n,k,alpha,A,lda,
!                                                    B,ldb,beta,C,ldc)
! SUBROUTINE             chemm zhemm    -> lasi_HEMM(side,uplo,m,n,alpha,A,lda,
!                                                    B,ldb,beta,C,ldc)
! SUBROUTINE            cher2k zher2k   -> lasi_HER2K(uplo,trans,n,k,alpha,A,lda,B,ldb,
!                                                    beta,C,ldc)
! SUBROUTINE             cherk zherk    -> lasi_HERK(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
! SUBROUTINE ssymm dsymm csymm zsymm    -> lasi_SYMM(side,uplo,m,n,alpha,A,lda,B,ldb,beta,
!                                                    C,ldc)
! SUBROUTINE ssyr2k dsyr2k csyr2k zsyr2k-> lasi_SYR2K(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,
!                                                    C,ldc)
! SUBROUTINE ssyrk dsyrk csyrk zsyrk    -> lasi_SYRK(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
! SUBROUTINE strmm dtrmm ctrmm ztrmm    -> lasi_TRMM(side,uplo,transa,diag,m,n,alpha,A,lda,
!                                                    B,ldb)
! SUBROUTINE strsm dtrsm ctrsm ztrsm    -> lasi_TRSM(side,uplo,transa,diag,m,n,alpha,A,lda,
!                                                    B,ldb)
!
!----------------------------------------
! LAPACK interfaced in this module:
!
!       lasi_LANGB  lasi_TGSEN  lasi_TGSNA  lasi_TGSYL  lasi_TGEXC
!       lasi_BDSDC  lasi_STEGR  lasi_ORMRZ  lasi_UNMRZ  lasi_TZRZF
!       lasi_GEQP3  lasi_GESDD  lasi_GGRQF  lasi_GGQRF  lasi_DISNA
!       lasi_TGSJA  lasi_GGSVP  lasi_TGEVC  lasi_HGEQZ  lasi_GGBAK
!       lasi_GGBAL  lasi_GGHRD  lasi_PBSTF  lasi_SBGST  lasi_HBGST
!       lasi_SPGST  lasi_HPGST  lasi_BDSQR  lasi_ORMBR  lasi_UNMBR
!       lasi_ORGBR  lasi_UNGBR  lasi_GBBRD  lasi_GEBRD  lasi_TRSEN
!       lasi_TRSNA  lasi_TRSYL  lasi_TREXC  lasi_TREVC  lasi_HSEIN
!       lasi_HSEQR  lasi_ORMHR  lasi_UNMHR  lasi_ORGHR  lasi_UNGHR
!       lasi_GEBAK  lasi_GEBAL  lasi_GEHRD  lasi_PTEQR  lasi_STEIN
!       lasi_STEBZ  lasi_STEDC  lasi_STERF  lasi_STEQR  lasi_OPMTR
!       lasi_UPMTR  lasi_OPGTR  lasi_UPGTR  lasi_ORMTR  lasi_UNMTR
!       lasi_SBTRD  lasi_HBTRD  lasi_SPTRD  lasi_HPTRD  lasi_TZRQF
!       lasi_ORMRQ  lasi_UNMRQ  lasi_ORGRQ  lasi_UNGRQ  lasi_GERQF
!       lasi_ORMQL  lasi_UNMQL  lasi_ORGQL  lasi_UNGQL  lasi_GEQLF
!       lasi_ORMLQ  lasi_UNMLQ  lasi_ORGLQ  lasi_UNGLQ  lasi_GELQF
!       lasi_ORMQR  lasi_UNMQR  lasi_ORGQR  lasi_UNGQR  lasi_GEQRF
!       lasi_GEQPF  lasi_TBRFS  lasi_TBCON  lasi_TBTRS  lasi_TPTRI
!       lasi_TPRFS  lasi_TPCON  lasi_TPTRS  lasi_TRTRI  lasi_TRRFS
!       lasi_TRCON  lasi_TRTRS  lasi_SPTRI  lasi_HPTRI  lasi_SPRFS
!       lasi_HPRFS  lasi_HPCON  lasi_SPCON  lasi_SPTRS  lasi_HPTRS
!       lasi_HPTRF  lasi_SPTRF  lasi_SYTRI  lasi_HETRI  lasi_SYRFS
!       lasi_HERFS  lasi_SYCON  lasi_HECON  lasi_HETRS  lasi_SYTRS
!       lasi_HETRF  lasi_SYTRF  lasi_PTRFS  lasi_PTCON  lasi_PTTRS
!       lasi_PTTRF  lasi_PBEQU  lasi_PBRFS  lasi_PBCON  lasi_PBTRS
!       lasi_PBTRF  lasi_PPEQU  lasi_PPTRI  lasi_PPRFS  lasi_PPCON
!       lasi_PPTRS  lasi_PPTRF  lasi_POEQU  lasi_POTRI  lasi_PORFS
!       lasi_POTRS  lasi_GTRFS  lasi_GTCON  lasi_GTTRS  lasi_GTTRF
!       lasi_GBEQU  lasi_GBRFS  lasi_GBCON  lasi_GBTRS  lasi_GBTRF
!       lasi_GGSVD  lasi_GEGV   lasi_GEGS   lasi_SBGVX  lasi_HBGVX
!       lasi_SBGVD  lasi_HBGVD  lasi_SBGV   lasi_HBGV   lasi_SPGVX
!       lasi_HPGVX  lasi_SPGVD  lasi_HPGVD  lasi_SPGV   lasi_HPGV
!       lasi_GESVD  lasi_GEEVX  lasi_GGEVX  lasi_GGEV   lasi_GEEV
!       lasi_GEESX  lasi_GGESX  lasi_GGES   lasi_GEES   lasi_STEVR
!       lasi_STEVX  lasi_STEVD  lasi_STEV   lasi_SBEVX  lasi_HBEVX
!       lasi_SBEVD  lasi_HBEVD  lasi_SBEV   lasi_HBEV   lasi_SPEVX
!       lasi_HPEVX  lasi_SPEVD  lasi_HPEVD  lasi_SPEV   lasi_HPEV
!       lasi_GGGLM  lasi_GGLSE  lasi_GELSY  lasi_GELSD  lasi_GELSX
!       lasi_GELSS  lasi_GELS   lasi_SPSV   lasi_HPSV   lasi_SYSV
!       lasi_HESV   lasi_PTSV   lasi_PBSV   lasi_PPSV   lasi_POSV
!       lasi_GTSV   lasi_GBSV   lasi_GESV   lasi_SPSVX  lasi_HPSVX
!       lasi_SYSVX  lasi_HESVX  lasi_PTSVX  lasi_PBSVX  lasi_PPSVX
!       lasi_POSVX  lasi_GTSVX  lasi_GBSVX  lasi_GESVX  lasi_GETRF
!       lasi_GETRS  lasi_GETRI  lasi_GERFS  lasi_GEEQU  lasi_LANGE
!       lasi_GECON  lasi_SYEV   lasi_HEEV   lasi_SYEVD  lasi_HEEVD
!       lasi_SYEVR  lasi_HEEVR  lasi_SYEVX  lasi_HEEVX  lasi_SYGST
!       lasi_HEGST  lasi_SYGV   lasi_HEGV   lasi_SYGVX  lasi_HEGVX
!       lasi_SYGVD  lasi_HEGVD  lasi_SYTRD  lasi_HETRD  lasi_ORGTR
!       lasi_UNGTR  lasi_LANSY  lasi_POTRF  lasi_POCON  lasi_ILAENV
!       lasi_LAGGE  lasi_LAMCH
!
! </INFO>
!

!
! from lasi_kinds module
PRIVATE :: dbl, sgl


!------------------------
! BLAS LEVEL 1
!------------------------
   !
   ! lasi_IAMAX
   !
#ifdef __LASI_IAMAX
   INTERFACE lasi_IAMAX
       INTEGER FUNCTION isamax(n,dx,incx) 
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP), INTENT(in)  :: dx(*)
          INTEGER, INTENT(in)    :: n,incx
       END FUNCTION isamax
       INTEGER FUNCTION idamax(n,dx,incx) 
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP), INTENT(in)  :: dx(*)
          INTEGER, INTENT(in)    :: n,incx
       END FUNCTION idamax
       INTEGER FUNCTION icamax(n,dx,incx) 
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          COMPLEX(WP), INTENT(in)  :: dx(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION icamax
       INTEGER FUNCTION izamax(n,dx,incx) 
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          COMPLEX(WP), INTENT(in)  :: dx(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION izamax
   END INTERFACE
#endif

   !
   ! lasi_NRM2
   !
#ifdef __LASI_NRM2
   INTERFACE lasi_NRM2
       FUNCTION snrm2(n,x,incx) 
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP)                  :: snrm2
          REAL(WP), INTENT(in)      :: x(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION snrm2
       FUNCTION dnrm2(n,x,incx) 
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP)                  :: dnrm2
          REAL(WP), INTENT(in)      :: x(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION dnrm2
       FUNCTION scnrm2(n,x,incx) 
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP)                  :: scnrm2
          COMPLEX(WP), INTENT(in)   :: x(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION scnrm2
       FUNCTION dznrm2(n,x,incx) 
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP)                  :: dznrm2
          COMPLEX(WP), INTENT(in)   :: x(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION dznrm2
   END INTERFACE 
#endif

   !
   ! lasi_ASUM
   !
#ifdef __LASI_ASUM
   INTERFACE lasi_ASUM
       FUNCTION sasum(n,x,incx) 
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP)                  :: sasum
          REAL(WP), INTENT(in)      :: x(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION sasum
       FUNCTION dasum(n,x,incx) 
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP)                  :: dasum
          REAL(WP), INTENT(in)      :: x(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION dasum
       FUNCTION scasum(n,x,incx) 
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP)                  :: scasum
          COMPLEX(WP), INTENT(in)   :: x(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION scasum
       FUNCTION dzasum(n,x,incx) 
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP)                  :: dzasum
          COMPLEX(WP), INTENT(in)   :: x(*)
          INTEGER, INTENT(in)       :: n,incx
       END FUNCTION dzasum
   END INTERFACE 
#endif

   !
   ! lasi_DOTU
   !
#ifdef __LASI_DOTU
   INTERFACE lasi_DOTU
       FUNCTION cdotu(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          COMPLEX(WP)               :: cdotu
          COMPLEX(WP), INTENT(in)   :: x(*),y(*)
          INTEGER, INTENT(in)       :: n,incx,incy
       END FUNCTION cdotu
       FUNCTION zdotu(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          COMPLEX(WP)               :: zdotu
          COMPLEX(WP), INTENT(in)   :: x(*),y(*)
          INTEGER, INTENT(in)       :: n,incx,incy
       END FUNCTION zdotu
   END INTERFACE
#endif

   !
   ! lasi_DOTC
   !
#ifdef __LASI_DOTC
   INTERFACE lasi_DOTC
       FUNCTION cdotc(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          COMPLEX(WP)               :: cdotc
          COMPLEX(WP), INTENT(in)   :: x(*),y(*)
          INTEGER, INTENT(in)       :: n,incx,incy
       END FUNCTION cdotc
       FUNCTION zdotc(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          COMPLEX(WP)               :: zdotc
          COMPLEX(WP), INTENT(in)   :: x(*),y(*)
          INTEGER, INTENT(in)       :: n,incx,incy
       END FUNCTION zdotc
   END INTERFACE
#endif

   !
   ! lasi_DOT
   !
#ifdef __LASI_DOT
   INTERFACE lasi_DOT
       FUNCTION sdot(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP)               :: sdot
          REAL(WP), INTENT(in)   :: x(*),y(*)
          INTEGER, INTENT(in)    :: n,incx,incy
       END FUNCTION sdot
       FUNCTION ddot(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP)               :: ddot
          REAL(WP), INTENT(in)   :: x(*),y(*)
          INTEGER, INTENT(in)    :: n,incx,incy
       END FUNCTION ddot
   END INTERFACE
#endif

   !
   ! lasi_DSDOT 
   !
#ifdef __LASI_DSDOT
   INTERFACE lasi_DSDOT
       FUNCTION sdsdot(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP)               :: sdsdot
          REAL(WP), INTENT(in)   :: x(*),y(*)
          INTEGER, INTENT(in)    :: n,incx,incy
       END FUNCTION sdsdot
       FUNCTION dsdot(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP)               :: dsdot
          REAL(WP), INTENT(in)   :: x(*),y(*)
          INTEGER, INTENT(in)    :: n,incx,incy
       END FUNCTION dsdot
   END INTERFACE
#endif

   !
   ! lasi_AXPY
   !
#ifdef __LASI_AXPY
   INTERFACE lasi_AXPY
       SUBROUTINE saxpy(n,ca,cx,incx,cy,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: n, incx, incy
          REAL(WP), INTENT(in)    :: cx(*), ca
          REAL(WP), INTENT(inout) :: cy(*)
       END SUBROUTINE saxpy
       SUBROUTINE daxpy(n,ca,cx,incx,cy,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: n, incx, incy
          REAL(WP), INTENT(in)    :: cx(*), ca
          REAL(WP), INTENT(inout) :: cy(*)
       END SUBROUTINE daxpy
       SUBROUTINE caxpy(n,ca,cx,incx,cy,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: n, incx, incy
          COMPLEX(WP), INTENT(in)    :: cx(*), ca
          COMPLEX(WP), INTENT(inout) :: cy(*)
       END SUBROUTINE caxpy
       SUBROUTINE zaxpy(n,ca,cx,incx,cy,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: n, incx, incy
          COMPLEX(WP), INTENT(in)    :: cx(*), ca
          COMPLEX(WP), INTENT(inout) :: cy(*)
       END SUBROUTINE zaxpy
   END INTERFACE 
#endif

   !
   ! lasi_COPY
   !
#ifdef __LASI_COPY
   INTERFACE lasi_COPY
       SUBROUTINE scopy(n,cx,incx,cy,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: n, incx, incy
          REAL(WP), INTENT(in)    :: cx(*)
          REAL(WP), INTENT(inout) :: cy(*)
       END SUBROUTINE scopy
       SUBROUTINE dcopy(n,cx,incx,cy,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: n, incx, incy
          REAL(WP), INTENT(in)    :: cx(*)
          REAL(WP), INTENT(inout) :: cy(*)
       END SUBROUTINE dcopy
       SUBROUTINE ccopy(n,cx,incx,cy,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: n, incx, incy
          COMPLEX(WP), INTENT(in)    :: cx(*)
          COMPLEX(WP), INTENT(inout) :: cy(*)
       END SUBROUTINE ccopy
       SUBROUTINE zcopy(n,cx,incx,cy,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: n, incx, incy
          COMPLEX(WP), INTENT(in)    :: cx(*)
          COMPLEX(WP), INTENT(inout) :: cy(*)
       END SUBROUTINE zcopy
   END INTERFACE 
#endif

   !
   ! lasi_ROTG
   !
#ifdef __LASI_ROTG
   INTERFACE lasi_ROTG
       SUBROUTINE srotg(a,b,c,s)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP), INTENT(inout) :: a,b,c,s
       END SUBROUTINE srotg
       SUBROUTINE drotg(a,b,c,s)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP), INTENT(inout) :: a,b,c,s
       END SUBROUTINE drotg
       SUBROUTINE crotg(a,b,c,s)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          COMPLEX(WP), INTENT(inout) :: a,b,s
          REAL(WP), INTENT(inout) :: c
       END SUBROUTINE crotg
       SUBROUTINE zrotg(a,b,c,s)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          COMPLEX(WP), INTENT(inout) :: a,b,s
          REAL(WP), INTENT(inout) :: c
       END SUBROUTINE zrotg
   END INTERFACE 
#endif

   !
   ! lasi_ROT
   !
#ifdef __LASI_ROT
   INTERFACE lasi_ROT
       SUBROUTINE srot(n,x,incx,y,incy,c,s)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx,incy
          REAL(WP), INTENT(in) :: c,s
          REAL(WP), INTENT(inout) :: x(*),y(*)
       END SUBROUTINE srot
       SUBROUTINE drot(n,x,incx,y,incy,c,s)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx,incy
          REAL(WP), INTENT(in) :: c,s
          REAL(WP), INTENT(inout) :: x(*),y(*)
       END SUBROUTINE drot
       SUBROUTINE csrot(n,x,incx,y,incy,c,s)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx,incy
          REAL(WP), INTENT(in) :: c,s
          COMPLEX(WP), INTENT(inout) :: x(*),y(*)
       END SUBROUTINE csrot
       SUBROUTINE zdrot(n,x,incx,y,incy,c,s)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx,incy
          REAL(WP), INTENT(in) :: c,s
          COMPLEX(WP), INTENT(inout) :: x(*),y(*)
       END SUBROUTINE zdrot
   END INTERFACE 
#endif

   !
   ! lasi_SCAL
   !
#ifdef __LASI_SCAL
   INTERFACE lasi_SCAL
       SUBROUTINE sscal(n,a,x,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx
          REAL(WP), INTENT(in) :: a
          REAL(WP), INTENT(inout) :: x(*)
       END SUBROUTINE sscal
       SUBROUTINE dscal(n,a,x,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx
          REAL(WP), INTENT(in) :: a
          REAL(WP), INTENT(inout) :: x(*)
       END SUBROUTINE dscal
       SUBROUTINE cscal(n,a,x,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx
          COMPLEX(WP), INTENT(in) :: a
          COMPLEX(WP), INTENT(inout) :: x(*)
       END SUBROUTINE cscal
       SUBROUTINE zscal(n,a,x,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx
          COMPLEX(WP), INTENT(in) :: a
          COMPLEX(WP), INTENT(inout) :: x(*)
       END SUBROUTINE zscal
       SUBROUTINE csscal(n,a,x,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx
          REAL(WP), INTENT(in) :: a
          COMPLEX(WP), INTENT(inout) :: x(*)
       END SUBROUTINE csscal
       SUBROUTINE zdscal(n,a,x,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx
          REAL(WP), INTENT(in) :: a
          COMPLEX(WP), INTENT(inout) :: x(*)
       END SUBROUTINE zdscal
   END INTERFACE 
#endif

   !
   ! lasi_SWAP
   !
#ifdef __LASI_SWAP
   INTERFACE lasi_SWAP
       SUBROUTINE sswap(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx,incy
          REAL(WP), INTENT(inout) :: x(*),y(*)
       END SUBROUTINE sswap
       SUBROUTINE dswap(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx,incy
          REAL(WP), INTENT(inout) :: x(*),y(*)
       END SUBROUTINE dswap
       SUBROUTINE cswap(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx,incy
          COMPLEX(WP), INTENT(inout) :: x(*),y(*)
       END SUBROUTINE cswap
       SUBROUTINE zswap(n,x,incx,y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in) :: n,incx,incy
          COMPLEX(WP), INTENT(inout) :: x(*),y(*)
       END SUBROUTINE zswap
   END INTERFACE 
#endif


!------------------------
! BLAS LEVEL 2
!------------------------
   !
   ! lasi_GBMV
   !
#ifdef __LASI_GBMV
   INTERFACE lasi_GBMV
       SUBROUTINE sgbmv(trans,m,n,kl,ku,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: trans
          INTEGER, INTENT(in)      :: m,n,kl,ku,lda,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE sgbmv
       SUBROUTINE dgbmv(trans,m,n,kl,ku,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: trans
          INTEGER, INTENT(in)      :: m,n,kl,ku,lda,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE dgbmv
       SUBROUTINE cgbmv(trans,m,n,kl,ku,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: trans
          INTEGER, INTENT(in)      :: m,n,kl,ku,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE cgbmv
       SUBROUTINE zgbmv(trans,m,n,kl,ku,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: trans
          INTEGER, INTENT(in)      :: m,n,kl,ku,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE zgbmv
   END INTERFACE 
#endif

   !
   ! lasi_GEMV
   !
#ifdef __LASI_GEMV
   INTERFACE lasi_GEMV
       SUBROUTINE sgemv(trans,m,n,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: trans
          INTEGER, INTENT(in)      :: m,n,lda,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE sgemv
       SUBROUTINE dgemv(trans,m,n,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: trans
          INTEGER, INTENT(in)      :: m,n,lda,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE dgemv
       SUBROUTINE cgemv(trans,m,n,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: trans
          INTEGER, INTENT(in)      :: m,n,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE cgemv
       SUBROUTINE zgemv(trans,m,n,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: trans
          INTEGER, INTENT(in)      :: m,n,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE zgemv
   END INTERFACE 
#endif

   !
   ! lasi_GERC
   !
#ifdef __LASI_GERC
   INTERFACE lasi_GERC
       SUBROUTINE cgerc(m,n,alpha,X,incx,Y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: m,n,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha
          COMPLEX(WP), INTENT(in)    :: Y(*), X(*)
          COMPLEX(WP), INTENT(inout) :: A(lda,*)
       END SUBROUTINE cgerc
       SUBROUTINE zgerc(m,n,alpha,X,incx,Y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: m,n,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha
          COMPLEX(WP), INTENT(in)    :: Y(*), X(*)
          COMPLEX(WP), INTENT(inout) :: A(lda,*)
       END SUBROUTINE zgerc
   END INTERFACE 
#endif

   !
   ! lasi_GERU
   !
#ifdef __LASI_GERU
   INTERFACE lasi_GERU
       SUBROUTINE cgeru(m,n,alpha,X,incx,Y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: m,n,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha
          COMPLEX(WP), INTENT(in)    :: Y(*), X(*)
          COMPLEX(WP), INTENT(inout) :: A(lda,*)
       END SUBROUTINE cgeru
       SUBROUTINE zgeru(m,n,alpha,X,incx,Y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in)      :: m,n,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha
          COMPLEX(WP), INTENT(in)    :: Y(*), X(*)
          COMPLEX(WP), INTENT(inout) :: A(lda,*)
       END SUBROUTINE zgeru
   END INTERFACE 
#endif

   !
   ! lasi_HBMV
   !
#ifdef __LASI_HBMV
   INTERFACE lasi_HBMV
       SUBROUTINE chbmv(uplo,n,k,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,k,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE chbmv
       SUBROUTINE zhbmv(uplo,n,k,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,k,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE zhbmv
   END INTERFACE 
#endif

   !
   ! lasi_HEMV
   !
#ifdef __LASI_HEMV
   INTERFACE lasi_HEMV
       SUBROUTINE chemv(uplo,n,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE chemv
       SUBROUTINE zhemv(uplo,n,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,lda,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE zhemv
   END INTERFACE 
#endif

   !
   ! lasi_HER
   !
#ifdef __LASI_HER
   INTERFACE lasi_HER
       SUBROUTINE cher(uplo,n,alpha,x,incx,A,lda)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,lda
          REAL(WP), INTENT(in)    :: alpha
          COMPLEX(WP), INTENT(in)    :: x(*)
          COMPLEX(WP), INTENT(inout) :: A(lda,*)
       END SUBROUTINE cher
       SUBROUTINE zher(uplo,n,alpha,x,incx,A,lda)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,lda
          REAL(WP), INTENT(in)    :: alpha
          COMPLEX(WP), INTENT(in)    :: x(*)
          COMPLEX(WP), INTENT(inout) :: A(lda,*)
       END SUBROUTINE zher
   END INTERFACE
#endif

   !
   ! lasi_HER2
   !
#ifdef __LASI_HER2
   INTERFACE lasi_HER2
       SUBROUTINE cher2(uplo,n,alpha,x,incx,y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,incy,lda
          COMPLEX(WP), INTENT(in)    :: alpha, x(*), y(*)
          COMPLEX(WP), INTENT(inout) :: A(lda,*)
       END SUBROUTINE cher2
       SUBROUTINE zher2(uplo,n,alpha,x,incx,y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,incy,lda
          COMPLEX(WP), INTENT(in)    :: alpha, x(*), y(*)
          COMPLEX(WP), INTENT(inout) :: A(lda,*)
       END SUBROUTINE zher2
   END INTERFACE
#endif

   !
   ! lasi_HPR
   !
#ifdef __LASI_HPR
   INTERFACE lasi_HPR
       SUBROUTINE chpr(uplo,n,alpha,x,incx,AP)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx
          REAL(WP), INTENT(in)    :: alpha
          COMPLEX(WP), INTENT(in)    :: x(*)
          COMPLEX(WP), INTENT(inout) :: AP(*)
       END SUBROUTINE chpr
       SUBROUTINE zhpr(uplo,n,alpha,x,incx,AP)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx
          REAL(WP), INTENT(in)    :: alpha
          COMPLEX(WP), INTENT(in)    :: x(*)
          COMPLEX(WP), INTENT(inout) :: AP(*)
       END SUBROUTINE zhpr
   END INTERFACE
#endif

   !
   ! lasi_HPR2
   !
#ifdef __LASI_HPR2
   INTERFACE lasi_HPR2
       SUBROUTINE chpr2(uplo,n,alpha,x,incx,y,incy,AP)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, x(*), y(*)
          COMPLEX(WP), INTENT(inout) :: AP(*)
       END SUBROUTINE chpr2
       SUBROUTINE zhpr2(uplo,n,alpha,x,incx,y,incy,AP)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, x(*), y(*)
          COMPLEX(WP), INTENT(inout) :: AP(*)
       END SUBROUTINE zhpr2
   END INTERFACE
#endif

   !
   ! lasi_HPMV
   !
#ifdef __LASI_HPMV
   INTERFACE lasi_HPMV
       SUBROUTINE chpmv(uplo,n,alpha,AP,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: AP(*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE chpmv
       SUBROUTINE zhpmv(uplo,n,alpha,AP,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,incy
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: AP(*), X(*)
          COMPLEX(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE zhpmv
   END INTERFACE 
#endif

   !
   ! lasi_TBMV
   !
#ifdef __LASI_TBMV
   INTERFACE lasi_TBMV
       SUBROUTINE stbmv(uplo,trans,diag,n,k,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,k,lda,n
          REAL(WP), INTENT(in)    :: A(lda,*)
          REAL(WP), INTENT(inout) :: X(*)
       END SUBROUTINE stbmv
       SUBROUTINE dtbmv(uplo,trans,diag,n,k,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,k,lda,n
          REAL(WP), INTENT(in)    :: A(lda,*)
          REAL(WP), INTENT(inout) :: X(*)
       END SUBROUTINE dtbmv
       SUBROUTINE ctbmv(uplo,trans,diag,n,k,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,k,lda,n
          COMPLEX(WP), INTENT(in)    :: A(lda,*)
          COMPLEX(WP), INTENT(inout) :: X(*)
       END SUBROUTINE ctbmv
       SUBROUTINE ztbmv(uplo,trans,diag,n,k,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,k,lda,n
          COMPLEX(WP), INTENT(in)    :: A(lda,*)
          COMPLEX(WP), INTENT(inout) :: X(*)
       END SUBROUTINE ztbmv
   END INTERFACE 
#endif

   !
   ! lasi_TPMV
   !
#ifdef __LASI_TPMV
   INTERFACE lasi_TPMV
       SUBROUTINE stpmv(uplo,trans,diag,n,AP,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,n
          REAL(WP), INTENT(in)    :: AP(*)
          REAL(WP), INTENT(inout) :: X(*)
       END SUBROUTINE stpmv
       SUBROUTINE dtpmv(uplo,trans,diag,n,AP,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,n
          REAL(WP), INTENT(in)    :: AP(*)
          REAL(WP), INTENT(inout) :: X(*)
       END SUBROUTINE dtpmv
       SUBROUTINE ctpmv(uplo,trans,diag,n,AP,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,n
          COMPLEX(WP), INTENT(in)    :: AP(*)
          COMPLEX(WP), INTENT(inout) :: X(*)
       END SUBROUTINE ctpmv
       SUBROUTINE ztpmv(uplo,trans,diag,n,AP,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,n
          COMPLEX(WP), INTENT(in)    :: AP(*)
          COMPLEX(WP), INTENT(inout) :: X(*)
       END SUBROUTINE ztpmv
   END INTERFACE 
#endif

   !
   ! lasi_TRMV
   !
#ifdef __LASI_TRMV
   INTERFACE lasi_TRMV
       SUBROUTINE strmv(uplo,trans,diag,n,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,lda,n
          REAL(WP), INTENT(in)    :: A(lda,*)
          REAL(WP), INTENT(inout) :: X(*)
       END SUBROUTINE strmv
       SUBROUTINE dtrmv(uplo,trans,diag,n,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,lda,n
          REAL(WP), INTENT(in)    :: A(lda,*)
          REAL(WP), INTENT(inout) :: X(*)
       END SUBROUTINE dtrmv
       SUBROUTINE ctrmv(uplo,trans,diag,n,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,lda,n
          COMPLEX(WP), INTENT(in)    :: A(lda,*)
          COMPLEX(WP), INTENT(inout) :: X(*)
       END SUBROUTINE ctrmv
       SUBROUTINE ztrmv(uplo,trans,diag,n,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: diag,trans,uplo
          INTEGER, INTENT(in)      :: incx,lda,n
          COMPLEX(WP), INTENT(in)    :: A(lda,*)
          COMPLEX(WP), INTENT(inout) :: X(*)
       END SUBROUTINE ztrmv
   END INTERFACE 
#endif

   !
   ! lasi_SYMV
   !
#ifdef __LASI_SYMV
   INTERFACE lasi_SYMV
       SUBROUTINE ssymv(uplo,n,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,lda,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE ssymv
       SUBROUTINE dsymv(uplo,n,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,lda,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE dsymv
   END INTERFACE 
#endif

   !
   ! lasi_SBMV
   !
#ifdef __LASI_SBMV
   INTERFACE lasi_SBMV
       SUBROUTINE ssbmv(uplo,n,k,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,k,lda,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE ssbmv
       SUBROUTINE dsbmv(uplo,n,k,alpha,A,lda,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,k,lda,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE dsbmv
   END INTERFACE 
#endif

   !
   ! lasi_SPMV
   !
#ifdef __LASI_SPMV
   INTERFACE lasi_SPMV
       SUBROUTINE sspmv(uplo,n,alpha,AP,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: AP(*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE sspmv
       SUBROUTINE dspmv(uplo,n,alpha,AP,X,incx,beta,Y,incy)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: uplo
          INTEGER, INTENT(in)      :: n,incx,incy
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: AP(*), X(*)
          REAL(WP), INTENT(inout) :: Y(*)
       END SUBROUTINE dspmv
   END INTERFACE 
#endif

   !
   ! lasi_TBSV
   !
#ifdef __LASI_TBSV
   INTERFACE lasi_TBSV
       SUBROUTINE stbsv(uplo,trans,diag,n,k,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,k,lda,incx
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE stbsv
       SUBROUTINE dtbsv(uplo,trans,diag,n,k,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,k,lda,incx
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE dtbsv
       SUBROUTINE ctbsv(uplo,trans,diag,n,k,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,k,lda,incx
          COMPLEX(WP), INTENT(in)      :: A(lda,*)
          COMPLEX(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE ctbsv
       SUBROUTINE ztbsv(uplo,trans,diag,n,k,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,k,lda,incx
          COMPLEX(WP), INTENT(in)      :: A(lda,*)
          COMPLEX(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE ztbsv
   END INTERFACE 
#endif

   !
   ! lasi_TPSV
   !
#ifdef __LASI_TPSV
   INTERFACE lasi_TPSV
       SUBROUTINE stpsv(uplo,trans,diag,n,AP,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,incx
          REAL(WP), INTENT(in)      :: AP(*)
          REAL(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE stpsv
       SUBROUTINE dtpsv(uplo,trans,diag,n,AP,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,incx
          REAL(WP), INTENT(in)      :: AP(*)
          REAL(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE dtpsv
       SUBROUTINE ctpsv(uplo,trans,diag,n,AP,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,incx
          COMPLEX(WP), INTENT(in)      :: AP(*)
          COMPLEX(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE ctpsv
       SUBROUTINE ztpsv(uplo,trans,diag,n,AP,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,incx
          COMPLEX(WP), INTENT(in)      :: AP(*)
          COMPLEX(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE ztpsv
   END INTERFACE 
#endif

   !
   ! lasi_TRSV
   !
#ifdef __LASI_TRSV
   INTERFACE lasi_TRSV
       SUBROUTINE strsv(uplo,trans,diag,n,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,lda,incx
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE strsv
       SUBROUTINE dtrsv(uplo,trans,diag,n,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,lda,incx
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE dtrsv
       SUBROUTINE ctrsv(uplo,trans,diag,n,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,lda,incx
          COMPLEX(WP), INTENT(in)      :: A(lda,*)
          COMPLEX(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE ctrsv
       SUBROUTINE ztrsv(uplo,trans,diag,n,A,lda,X,incx)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans, diag
          INTEGER, INTENT(in)        :: n,lda,incx
          COMPLEX(WP), INTENT(in)      :: A(lda,*)
          COMPLEX(WP), INTENT(inout)   :: X(*)
       END SUBROUTINE ztrsv
   END INTERFACE 
#endif

   !
   ! lasi_GER
   !
#ifdef __LASI_GER
   INTERFACE lasi_GER
       SUBROUTINE sger(m,n,alpha,X,incx,Y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(in)        :: m,n,lda,incx,incy
          REAL(WP), INTENT(in)      :: X(*), Y(*), alpha
          REAL(WP), INTENT(inout)   :: A(lda,*)
       END SUBROUTINE sger
       SUBROUTINE dger(m,n,alpha,X,incx,Y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(in)        :: m,n,lda,incx,incy
          REAL(WP), INTENT(in)      :: X(*), Y(*), alpha
          REAL(WP), INTENT(inout)   :: A(lda,*)
       END SUBROUTINE dger
   END INTERFACE 
#endif

   !
   ! lasi_SPR
   !
#ifdef __LASI_SPR
   INTERFACE lasi_SPR
       SUBROUTINE sspr(uplo,n,alpha,X,incx,AP)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo
          INTEGER, INTENT(in)        :: n,incx
          REAL(WP), INTENT(in)      :: X(*), alpha
          REAL(WP), INTENT(inout)   :: AP(*)
       END SUBROUTINE sspr
       SUBROUTINE dspr(uplo,n,alpha,X,incx,AP)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo
          INTEGER, INTENT(in)        :: n,incx
          REAL(WP), INTENT(in)      :: X(*), alpha
          REAL(WP), INTENT(inout)   :: AP(*)
       END SUBROUTINE dspr
   END INTERFACE 
#endif

   !
   ! lasi_SPR2
   !
#ifdef __LASI_SPR2
   INTERFACE lasi_SPR2
       SUBROUTINE sspr2(uplo,n,alpha,X,incx,Y,incy,AP)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo
          INTEGER, INTENT(in)        :: n,incx,incy
          REAL(WP), INTENT(in)      :: X(*), Y(*), alpha
          REAL(WP), INTENT(inout)   :: AP(*)
       END SUBROUTINE sspr2
       SUBROUTINE dspr2(uplo,n,alpha,X,incx,Y,incy,AP)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo
          INTEGER, INTENT(in)        :: n,incx,incy
          REAL(WP), INTENT(in)      :: X(*), Y(*), alpha
          REAL(WP), INTENT(inout)   :: AP(*)
       END SUBROUTINE dspr2
   END INTERFACE 
#endif

   !
   ! lasi_SYR
   !
#ifdef __LASI_SYR
   INTERFACE lasi_SYR
       SUBROUTINE ssyr(uplo,n,alpha,X,incx,A,lda)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo
          INTEGER, INTENT(in)        :: n,incx,lda
          REAL(WP), INTENT(in)      :: X(*), alpha
          REAL(WP), INTENT(inout)   :: A(lda,*)
       END SUBROUTINE ssyr
       SUBROUTINE dsyr(uplo,n,alpha,X,incx,A,lda)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo
          INTEGER, INTENT(in)        :: n,incx,lda
          REAL(WP), INTENT(in)      :: X(*), alpha
          REAL(WP), INTENT(inout)   :: A(lda,*)
       END SUBROUTINE dsyr
   END INTERFACE 
#endif

   !
   ! lasi_SYR2
   !
#ifdef __LASI_SYR2
   INTERFACE lasi_SYR2
       SUBROUTINE ssyr2(uplo,n,alpha,X,incx,Y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo
          INTEGER, INTENT(in)        :: n,incx,incy,lda
          REAL(WP), INTENT(in)      :: X(*), Y(*), alpha
          REAL(WP), INTENT(inout)   :: A(lda,*)
       END SUBROUTINE ssyr2
       SUBROUTINE dsyr2(uplo,n,alpha,X,incx,Y,incy,A,lda)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo
          INTEGER, INTENT(in)        :: n,incx,incy,lda
          REAL(WP), INTENT(in)      :: X(*), Y(*), alpha
          REAL(WP), INTENT(inout)   :: A(lda,*)
       END SUBROUTINE dsyr2
   END INTERFACE 
#endif

!------------------------
! BLAS LEVEL 3
!------------------------
   !
   ! lasi_GEMM
   !
#ifdef __LASI_GEMM
   INTERFACE lasi_GEMM
       SUBROUTINE sgemm(transA,transB,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: transA,transB
          INTEGER, INTENT(in)      :: m,n,k,lda,ldb,ldc
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), B(ldb,*)
          REAL(WP), INTENT(inout) :: C(ldc,*)
       END SUBROUTINE sgemm
       SUBROUTINE dgemm(transA,transB,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)  :: transA,transB
          INTEGER, INTENT(in)       :: m,n,k,lda,ldb,ldc
          REAL(WP), INTENT(in)     :: alpha, beta
          REAL(WP), INTENT(in)     :: A(lda,*), B(ldb,*)
          REAL(WP), INTENT(inout)  :: C(ldc,*)
       END SUBROUTINE dgemm
       SUBROUTINE cgemm(transA,transB,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: transA,transB
          INTEGER, INTENT(in)        :: m,n,k,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)   :: alpha, beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE cgemm
       SUBROUTINE zgemm(transA,transB,m,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: transA,transB
          INTEGER, INTENT(in)        :: m,n,k,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)   :: alpha, beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE zgemm
   END INTERFACE 
#endif

   !
   ! lasi_HEMM
   !
#ifdef __LASI_HEMM
   INTERFACE lasi_HEMM
       SUBROUTINE chemm(side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side,uplo
          INTEGER, INTENT(in)        :: m,n,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)   :: alpha, beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE chemm
       SUBROUTINE zhemm(side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side,uplo
          INTEGER, INTENT(in)        :: m,n,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)   :: alpha, beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE zhemm
   END INTERFACE 
#endif

   !
   ! lasi_HER2K
   !
#ifdef __LASI_HER2K
   INTERFACE lasi_HER2K
       SUBROUTINE cher2k(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)   :: alpha
          REAL(WP), INTENT(in)      :: beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE cher2k
       SUBROUTINE zher2k(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)   :: alpha
          REAL(WP), INTENT(in)      :: beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE zher2k
   END INTERFACE 
#endif

   !
   ! lasi_HERK
   !
#ifdef __LASI_HERK
   INTERFACE lasi_HERK
       SUBROUTINE cherk(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldc
          REAL(WP), INTENT(in)      :: alpha, beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE cherk
       SUBROUTINE zherk(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldc
          REAL(WP), INTENT(in)      :: alpha, beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE zherk
   END INTERFACE 
#endif

   !
   ! lasi_SYMM
   !
#ifdef __LASI_SYMM
   INTERFACE lasi_SYMM
       SUBROUTINE ssymm(side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: side,uplo
          INTEGER, INTENT(in)      :: m,n,lda,ldb,ldc
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), B(ldb,*)
          REAL(WP), INTENT(inout) :: C(ldc,*)
       END SUBROUTINE ssymm
       SUBROUTINE dsymm(side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: side,uplo
          INTEGER, INTENT(in)      :: m,n,lda,ldb,ldc
          REAL(WP), INTENT(in)    :: alpha, beta
          REAL(WP), INTENT(in)    :: A(lda,*), B(ldb,*)
          REAL(WP), INTENT(inout) :: C(ldc,*)
       END SUBROUTINE dsymm
       SUBROUTINE csymm(side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: side,uplo
          INTEGER, INTENT(in)      :: m,n,lda,ldb,ldc
          COMPLEX(WP), INTENT(in) :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout) :: C(ldc,*)
       END SUBROUTINE csymm
       SUBROUTINE zsymm(side,uplo,m,n,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in) :: side,uplo
          INTEGER, INTENT(in)      :: m,n,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)    :: alpha, beta
          COMPLEX(WP), INTENT(in)    :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout) :: C(ldc,*)
       END SUBROUTINE zsymm
   END INTERFACE 
#endif

   !
   ! lasi_SYR2K
   !
#ifdef __LASI_SYR2K
   INTERFACE lasi_SYR2K
       SUBROUTINE ssyr2k(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldb,ldc
          REAL(WP), INTENT(in)      :: alpha
          REAL(WP), INTENT(in)      :: beta
          REAL(WP), INTENT(in)      :: A(lda,*), B(ldb,*)
          REAL(WP), INTENT(inout)   :: C(ldc,*)
       END SUBROUTINE ssyr2k
       SUBROUTINE dsyr2k(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldb,ldc
          REAL(WP), INTENT(in)      :: alpha
          REAL(WP), INTENT(in)      :: beta
          REAL(WP), INTENT(in)      :: A(lda,*), B(ldb,*)
          REAL(WP), INTENT(inout)   :: C(ldc,*)
       END SUBROUTINE dsyr2k
       SUBROUTINE csyr2k(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)   :: alpha
          COMPLEX(WP), INTENT(in)   :: beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE csyr2k
       SUBROUTINE zsyr2k(uplo,trans,n,k,alpha,A,lda,B,ldb,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldb,ldc
          COMPLEX(WP), INTENT(in)   :: alpha
          COMPLEX(WP), INTENT(in)   :: beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*), B(ldb,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE zsyr2k
   END INTERFACE 
#endif

   !
   ! lasi_SYRK
   !
#ifdef __LASI_SYRK
   INTERFACE lasi_SYRK
       SUBROUTINE ssyrk(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldc
          REAL(WP), INTENT(in)      :: alpha
          REAL(WP), INTENT(in)      :: beta
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: C(ldc,*)
       END SUBROUTINE ssyrk
       SUBROUTINE dsyrk(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldc
          REAL(WP), INTENT(in)      :: alpha
          REAL(WP), INTENT(in)      :: beta
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: C(ldc,*)
       END SUBROUTINE dsyrk
       SUBROUTINE csyrk(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldc
          COMPLEX(WP), INTENT(in)   :: alpha
          COMPLEX(WP), INTENT(in)   :: beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE csyrk
       SUBROUTINE zsyrk(uplo,trans,n,k,alpha,A,lda,beta,C,ldc)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: uplo, trans
          INTEGER, INTENT(in)        :: n,k,lda,ldc
          COMPLEX(WP), INTENT(in)   :: alpha
          COMPLEX(WP), INTENT(in)   :: beta
          COMPLEX(WP), INTENT(in)   :: A(lda,*)
          COMPLEX(WP), INTENT(inout):: C(ldc,*)
       END SUBROUTINE zsyrk
   END INTERFACE 
#endif

   !
   ! lasi_TRMM
   !
#ifdef __LASI_TRMM
   INTERFACE lasi_TRMM
       SUBROUTINE strmm(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side, uplo, transa, diag
          INTEGER, INTENT(in)        :: m, n, lda, ldb
          REAL(WP), INTENT(in)      :: alpha
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: B(ldb,*)
       END SUBROUTINE strmm
       SUBROUTINE dtrmm(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side, uplo, transa, diag
          INTEGER, INTENT(in)        :: m, n, lda, ldb
          REAL(WP), INTENT(in)      :: alpha
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: B(ldb,*)
       END SUBROUTINE dtrmm
       SUBROUTINE ctrmm(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side, uplo, transa, diag
          INTEGER, INTENT(in)        :: m, n, lda, ldb
          COMPLEX(WP), INTENT(in)      :: alpha
          COMPLEX(WP), INTENT(in)      :: A(lda,*)
          COMPLEX(WP), INTENT(inout)   :: B(ldb,*)
       END SUBROUTINE ctrmm
       SUBROUTINE ztrmm(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side, uplo, transa, diag
          INTEGER, INTENT(in)        :: m, n, lda, ldb
          COMPLEX(WP), INTENT(in)      :: alpha
          COMPLEX(WP), INTENT(in)      :: A(lda,*)
          COMPLEX(WP), INTENT(inout)   :: B(ldb,*)
       END SUBROUTINE ztrmm
   END INTERFACE 
#endif

   !
   ! lasi_TRSM
   !
#ifdef __LASI_TRSM
   INTERFACE lasi_TRSM
       SUBROUTINE strsm(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side, uplo, transa, diag
          INTEGER, INTENT(in)        :: m, n, lda, ldb
          REAL(WP), INTENT(in)      :: alpha
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: B(ldb,*)
       END SUBROUTINE strsm
       SUBROUTINE dtrsm(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side, uplo, transa, diag
          INTEGER, INTENT(in)        :: m, n, lda, ldb
          REAL(WP), INTENT(in)      :: alpha
          REAL(WP), INTENT(in)      :: A(lda,*)
          REAL(WP), INTENT(inout)   :: B(ldb,*)
       END SUBROUTINE dtrsm
       SUBROUTINE ctrsm(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side, uplo, transa, diag
          INTEGER, INTENT(in)        :: m, n, lda, ldb
          COMPLEX(WP), INTENT(in)      :: alpha
          COMPLEX(WP), INTENT(in)      :: A(lda,*)
          COMPLEX(WP), INTENT(inout)   :: B(ldb,*)
       END SUBROUTINE ctrsm
       SUBROUTINE ztrsm(side,uplo,transa,diag,m,n,alpha,A,lda,B,ldb)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(1), INTENT(in)   :: side, uplo, transa, diag
          INTEGER, INTENT(in)        :: m, n, lda, ldb
          COMPLEX(WP), INTENT(in)      :: alpha
          COMPLEX(WP), INTENT(in)      :: A(lda,*)
          COMPLEX(WP), INTENT(inout)   :: B(ldb,*)
       END SUBROUTINE ztrsm
   END INTERFACE 
#endif

!------------------------
! LAPACK
!------------------------
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
 
! 
! lasi_LANGB
! 
#ifdef __LASI_LANGB 
      INTERFACE lasi_LANGB

       FUNCTION SLANGB( NORM, N, KL, KU, AB, LDAB, WORK )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         REAL(WP) :: SLANGB
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDAB, N, KL, KU
         REAL(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION SLANGB

       FUNCTION DLANGB( NORM, N, KL, KU, AB, LDAB, WORK )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         REAL(WP) :: DLANGB
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDAB, N, KL, KU
         REAL(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION DLANGB

       FUNCTION CLANGB( NORM, N, KL, KU, AB, LDAB, WORK )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         REAL(WP) :: CLANGB
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDAB, N, KL, KU
         COMPLEX(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION CLANGB

       FUNCTION ZLANGB( NORM, N, KL, KU, AB, LDAB, WORK )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         REAL(WP) :: ZLANGB
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDAB, N, KL, KU
         COMPLEX(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION ZLANGB

       END INTERFACE
#endif
       
! 
! lasi_TGSEN
! 
#ifdef __LASI_TGSEN 
       INTERFACE lasi_TGSEN

      SUBROUTINE STGSEN( IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
     &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, M, PL,   &
     &                   PR, DIF, WORK, LWORK, IWORK, LIWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
      REAL(WP), INTENT(OUT) :: PL, PR
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*), Z(LDZ,*)
      REAL(WP), INTENT(OUT) :: ALPHAI(*), ALPHAR(*), BETA(*), DIF(2),   &
     &                         WORK(LWORK)
      END SUBROUTINE STGSEN

      SUBROUTINE DTGSEN( IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
     &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, M, PL,   &
     &                   PR, DIF, WORK, LWORK, IWORK, LIWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
      REAL(WP), INTENT(OUT) :: PL, PR
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*), Z(LDZ,*)
      REAL(WP), INTENT(OUT) :: ALPHAI(*), ALPHAR(*), BETA(*), DIF(2),   &
     &                         WORK(LWORK)
      END SUBROUTINE DTGSEN


      SUBROUTINE CTGSEN( IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
     &                   ALPHA, BETA, Q, LDQ, Z, LDZ, M, PL, PR, DIF,   &
     &                   WORK, LWORK, IWORK, LIWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
      REAL(WP), INTENT(OUT) :: PL, PR
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(OUT) :: DIF(2)
      COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
      COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), WORK(LWORK)
      END SUBROUTINE CTGSEN

      SUBROUTINE ZTGSEN( IJOB, WANTQ, WANTZ, SELECT, N, A, LDA, B, LDB, &
     &                   ALPHA, BETA, Q, LDQ, Z, LDZ, M, PL, PR, DIF,   &
     &                   WORK, LWORK, IWORK, LIWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDQ, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
      REAL(WP), INTENT(OUT) :: PL, PR
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(OUT) :: DIF(2)
      COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
      COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), WORK(LWORK)
      END SUBROUTINE ZTGSEN

       END INTERFACE
#endif

       
! 
! lasi_TGSNA
! 
#ifdef __LASI_TGSNA 
       INTERFACE lasi_TGSNA

      SUBROUTINE STGSNA( JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
     &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
     &                   IWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
      INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(OUT) :: DIF(*), S(*)
      REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), VL(LDVL,*),           &
     &                        VR(LDVR,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE STGSNA

      SUBROUTINE DTGSNA( JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
     &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
     &                   IWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
      INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(OUT) :: DIF(*), S(*)
      REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), VL(LDVL,*),           &
     &                        VR(LDVR,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DTGSNA

      SUBROUTINE CTGSNA( JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
     &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
     &                   IWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
      INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(OUT) :: DIF(*), S(*)
      COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), VL(LDVL,*),        &
     &                           VR(LDVR,*)
      COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CTGSNA

      SUBROUTINE ZTGSNA( JOB, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,    &
     &                   LDVL, VR, LDVR, S, DIF, MM, M, WORK, LWORK,    &
     &                   IWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
      INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, LWORK, MM, N
      INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
      LOGICAL, INTENT(IN) :: SELECT(*)
      REAL(WP), INTENT(OUT) :: DIF(*), S(*)
      COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), VL(LDVL,*),        &
     &                           VR(LDVR,*)
      COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZTGSNA

       END INTERFACE
#endif
       
! 
! lasi_TGSYL
! 
#ifdef __LASI_TGSYL 
       INTERFACE lasi_TGSYL

      SUBROUTINE STGSYL( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
     &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
     &                   IWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
     &                       M, N
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: DIF, SCALE
      REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), D(LDD,*), E(LDF,*)
      REAL(WP), INTENT(INOUT) :: C(LDC,*), F(LDF,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE STGSYL

      SUBROUTINE DTGSYL( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
     &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
     &                   IWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
     &                       M, N
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: DIF, SCALE
      REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), D(LDD,*), E(LDF,*)
      REAL(WP), INTENT(INOUT) :: C(LDC,*), F(LDF,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DTGSYL

      SUBROUTINE CTGSYL( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
     &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
     &                   IWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
     &                       M, N
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: DIF, SCALE
      COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), D(LDD,*), E(LDF,*)
      COMPLEX(WP), INTENT(INOUT) :: C(LDC,*), F(LDF,*)
      COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CTGSYL

      SUBROUTINE ZTGSYL( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,  &
     &                   LDD, E, LDE, F, LDF, SCALE, DIF, WORK, LWORK,  &
     &                   IWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: TRANS
      INTEGER, INTENT(IN) :: IJOB, LDA, LDB, LDC, LDD, LDE, LDF, LWORK, &
     &                       M, N
      INTEGER, INTENT(OUT) :: INFO, IWORK(*)
      REAL(WP), INTENT(OUT) :: DIF, SCALE
      COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*), D(LDD,*), E(LDF,*)
      COMPLEX(WP), INTENT(INOUT) :: C(LDC,*), F(LDF,*)
      COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZTGSYL

       END INTERFACE
#endif
       
! 
! lasi_TGEXC
! 
#ifdef __LASI_TGEXC 
       INTERFACE lasi_TGEXC

         SUBROUTINE STGEXC( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
     &                      LDZ, IFST, ILST, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDZ, LWORK, N
      INTEGER, INTENT(INOUT) :: IFST, ILST
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*), Z(LDZ,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE STGEXC

         SUBROUTINE DTGEXC( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
     &                      LDZ, IFST, ILST, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDZ, LWORK, N
      INTEGER, INTENT(INOUT) :: IFST, ILST
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*), Z(LDZ,*)
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DTGEXC


         SUBROUTINE CTGEXC( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
     &                      LDZ, IFST, ILST, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) ::  LDA, LDB, LDQ, LDZ, N
      INTEGER, INTENT(INOUT) :: IFST, ILST
      INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),    &
     &                                 Z(LDZ,*)
      END SUBROUTINE CTGEXC

         SUBROUTINE ZTGEXC( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z, &
     &                      LDZ, IFST, ILST, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      LOGICAL, INTENT(IN) :: WANTQ, WANTZ
      INTEGER, INTENT(IN) ::  LDA, LDB, LDQ, LDZ, N
      INTEGER, INTENT(INOUT) :: IFST, ILST
      INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),    &
     &                                 Z(LDZ,*)
      END SUBROUTINE ZTGEXC

       END INTERFACE
#endif

! 
! lasi_BDSDC
! 
#ifdef __LASI_BDSDC 
       INTERFACE lasi_BDSDC

         SUBROUTINE SBDSDC( UPLO, COMPQ, N, D, E, U, LDU, VT, LDVT, Q,  &
     &                      IQ, WORK, IWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: COMPQ, UPLO
      INTEGER, INTENT(IN) :: LDU, LDVT, N
      INTEGER, INTENT(OUT) :: INFO, IQ( * ), IWORK( * )
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(OUT) :: Q(*), U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE SBDSDC

         SUBROUTINE DBDSDC( UPLO, COMPQ, N, D, E, U, LDU, VT, LDVT, Q,  &
     &                      IQ, WORK, IWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: COMPQ, UPLO
      INTEGER, INTENT(IN) :: LDU, LDVT, N
      INTEGER, INTENT(OUT) :: INFO, IQ( * ), IWORK( * )
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(OUT) :: Q(*), U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE DBDSDC

       END INTERFACE
#endif

! 
! lasi_STEGR
! 
#ifdef __LASI_STEGR 
       INTERFACE lasi_STEGR

         SUBROUTINE SSTEGR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
     &                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
     &                      IWORK, LIWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl  
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
      INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M
      INTEGER, INTENT(OUT) :: ISUPPZ( * ), IWORK(LIWORK)
      REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(IN) :: W( * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      REAL(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE SSTEGR
        
         SUBROUTINE DSTEGR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
     &                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
     &                      IWORK, LIWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl  
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
      INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M
      INTEGER, INTENT(OUT) :: ISUPPZ( * ), IWORK(LIWORK)
      REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(IN) :: W( * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      REAL(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE DSTEGR
        
         SUBROUTINE CSTEGR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
     &                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
     &                      IWORK, LIWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl  
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
      INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M
      INTEGER, INTENT(OUT) :: ISUPPZ( * ), IWORK(LIWORK)
      REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(IN) :: W( * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      COMPLEX(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE CSTEGR
        
         SUBROUTINE ZSTEGR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU,       &
     &                      ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,  &
     &                      IWORK, LIWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl  
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
      INTEGER, INTENT(IN) :: IL, IU, LDZ, LIWORK, LWORK, N
      INTEGER, INTENT(OUT) :: INFO, M
      INTEGER, INTENT(OUT) :: ISUPPZ( * ), IWORK(LIWORK)
      REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
      REAL(WP), INTENT(INOUT) :: D( * ), E( * )
      REAL(WP), INTENT(IN) :: W( * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      COMPLEX(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE ZSTEGR
        
       END INTERFACE
#endif

! 
! lasi_ORMRZ
! 
#ifdef __LASI_ORMRZ 
       INTERFACE lasi_ORMRZ

         SUBROUTINE SORMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,    &
     &                      LDC, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
      INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(IN) :: A( LDA, * ), TAU( * )
      REAL(WP), INTENT(INOUT) :: C( LDC, * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMRZ

         SUBROUTINE DORMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,    &
     &                      LDC, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
      INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(IN) :: A( LDA, * ), TAU( * )
      REAL(WP), INTENT(INOUT) :: C( LDC, * )
      REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMRZ

       END INTERFACE
#endif


! 
! lasi_UNMRZ
! 
#ifdef __LASI_UNMRZ 
       INTERFACE lasi_UNMRZ

         SUBROUTINE CUNMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,    &
     &                      LDC, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
      INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(IN) :: A( LDA, * ), TAU( * )
      COMPLEX(WP), INTENT(INOUT) :: C( LDC, * )
      COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNMRZ

         SUBROUTINE ZUNMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,    &
     &                      LDC, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
      INTEGER, INTENT(IN) :: K, L, LDA, LDC, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(IN) :: A( LDA, * ), TAU( * )
      COMPLEX(WP), INTENT(INOUT) :: C( LDC, * )
      COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNMRZ

       END INTERFACE
#endif

! 
! lasi_TZRZF
! 
#ifdef __LASI_TZRZF 
       INTERFACE lasi_TZRZF

         SUBROUTINE STZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A( LDA, * )
      REAL(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE STZRZF

         SUBROUTINE DTZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(INOUT) :: A( LDA, * )
      REAL(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE DTZRZF

         SUBROUTINE CTZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(INOUT) :: A( LDA, * )
      COMPLEX(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE CTZRZF

         SUBROUTINE ZTZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      COMPLEX(WP), INTENT(INOUT) :: A( LDA, * )
      COMPLEX(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE ZTZRZF

       END INTERFACE
#endif

! 
! lasi_GEQP3
! 
#ifdef __LASI_GEQP3 
       INTERFACE lasi_GEQP3

         SUBROUTINE SGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK,       &
     &                      INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: JPVT( * )
      REAL(WP), INTENT(INOUT) :: A( LDA, * )
      REAL(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE SGEQP3

         SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK,       &
     &                      INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: JPVT( * )
      REAL(WP), INTENT(INOUT) :: A( LDA, * )
      REAL(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      END SUBROUTINE DGEQP3


         SUBROUTINE CGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, RWORK,&
     &                      INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: JPVT( * )
      COMPLEX(WP), INTENT(INOUT) :: A( LDA, * )
      COMPLEX(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      REAL(WP), INTENT(OUT) ::  RWORK( * )
      END SUBROUTINE CGEQP3

         SUBROUTINE ZGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, RWORK,&
     &                      INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      INTEGER, INTENT(IN) :: LDA, LWORK, M, N
      INTEGER, INTENT(OUT) :: INFO
      INTEGER, INTENT(INOUT) :: JPVT( * )
      COMPLEX(WP), INTENT(INOUT) :: A( LDA, * )
      COMPLEX(WP), INTENT(OUT) :: TAU( * ), WORK(LWORK)
      REAL(WP), INTENT(OUT) ::  RWORK( * )
      END SUBROUTINE ZGEQP3

       END INTERFACE
#endif

! 
! lasi_GESDD
! 
#ifdef __LASI_GESDD 
       INTERFACE lasi_GESDD


         SUBROUTINE SGESDD( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT,    &
     &                      WORK, LWORK, IWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ
      INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: S(*)
      REAL(WP), INTENT(INOUT) :: A(LDA,*)
      REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      INTEGER :: IWORK(*)
      END SUBROUTINE SGESDD


         SUBROUTINE DGESDD( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT,    &
     &                      WORK, LWORK, IWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ
      INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: S(*)
      REAL(WP), INTENT(INOUT) :: A(LDA,*)
      REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      INTEGER :: IWORK(*)
      END SUBROUTINE DGESDD


        SUBROUTINE CGESDD(  JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT,    &
     &                      WORK, LWORK, RWORK, IWORK, INFO )
      USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ
      INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: S(*)
      REAL(WP) :: RWORK(*)
      COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      COMPLEX(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      INTEGER :: IWORK(*)
      END SUBROUTINE  CGESDD
        SUBROUTINE ZGESDD(  JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT,    &
     &                      WORK, LWORK, RWORK, IWORK, INFO )
      USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ
      INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
      INTEGER, INTENT(OUT) :: INFO
      REAL(WP), INTENT(OUT) :: S(*)
      REAL(WP) :: RWORK(*)
      COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      COMPLEX(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      INTEGER :: IWORK(*)
      END SUBROUTINE  ZGESDD
      END INTERFACE       
#endif


! 
! lasi_GGRQF
! 
#ifdef __LASI_GGRQF 
      INTERFACE lasi_GGRQF

      SUBROUTINE SGGRQF( M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE SGGRQF

      SUBROUTINE DGGRQF( M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE DGGRQF

      SUBROUTINE CGGRQF( M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE CGGRQF

      SUBROUTINE ZGGRQF( M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE ZGGRQF

      END INTERFACE
#endif

! 
! lasi_GGQRF
! 
#ifdef __LASI_GGQRF 
      INTERFACE lasi_GGQRF

      SUBROUTINE SGGQRF( N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE SGGQRF

      SUBROUTINE DGGQRF( N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE DGGQRF

      SUBROUTINE CGGQRF( N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE CGGQRF

      SUBROUTINE ZGGQRF( N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, LWORK, M, N, P
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: TAUA(*), TAUB(*), WORK(*)
      END SUBROUTINE ZGGQRF

      END INTERFACE
#endif

! 
! lasi_DISNA
! 
#ifdef __LASI_DISNA 
      INTERFACE lasi_DISNA

      SUBROUTINE SDISNA( JOB, M, N, D, SEP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(OUT) :: SEP(*)
      END SUBROUTINE SDISNA

      SUBROUTINE DDISNA( JOB, M, N, D, SEP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(OUT) :: SEP(*)
      END SUBROUTINE DDISNA

      END INTERFACE
#endif

! 
! lasi_TGSJA
! 
#ifdef __LASI_TGSJA 
      INTERFACE lasi_TGSJA

      SUBROUTINE STGSJA( JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
     &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
     &                   Q, LDQ, WORK, NCYCLE, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
     &                          NCYCLE, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              U(LDU,*), V(LDV,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STGSJA

      SUBROUTINE DTGSJA( JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
     &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
     &                   Q, LDQ, WORK, NCYCLE, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
     &                          NCYCLE, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              U(LDU,*), V(LDV,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTGSJA

      SUBROUTINE CTGSJA( JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
     &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
     &                   Q, LDQ, WORK, NCYCLE, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
     &                          NCYCLE, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),    &
     &                                 U(LDU,*), V(LDV,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTGSJA

      SUBROUTINE ZTGSJA( JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B,    &
     &                   LDB, TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV,  &
     &                   Q, LDQ, WORK, NCYCLE, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: K, L, LDA, LDB, LDQ, LDU, LDV, M, N,    &
     &                          NCYCLE, P
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),    &
     &                                 U(LDU,*), V(LDV,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTGSJA

      END INTERFACE
#endif

! 
! lasi_GGSVP
! 
#ifdef __LASI_GGSVP 
      INTERFACE lasi_GGSVP

      SUBROUTINE SGGSVP( JOBU, JOBV, JOBQ, M, P, N, A, LDA, B, LDB,     &
     &                   TOLA, TOLB, K, L, U, LDU, V, LDV, Q, LDQ,      &
     &                   IWORK, TAU, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDU, LDV, M, N, P
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), TAU(*), U(LDU,*), V(LDV,*), &
     &                            WORK(*)
      END SUBROUTINE SGGSVP

      SUBROUTINE DGGSVP( JOBU, JOBV, JOBQ, M, P, N, A, LDA, B, LDB,     &
     &                   TOLA, TOLB, K, L, U, LDU, V, LDV, Q, LDQ,      &
     &                   IWORK, TAU, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDU, LDV, M, N, P
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), TAU(*), U(LDU,*), V(LDV,*), &
     &                            WORK(*)
      END SUBROUTINE DGGSVP

      SUBROUTINE CGGSVP( JOBU, JOBV, JOBQ, M, P, N, A, LDA, B, LDB,     &
     &                   TOLA, TOLB, K, L, U, LDU, V, LDV, Q, LDQ,      &
     &                   IWORK, RWORK, TAU, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDU, LDV, M, N, P
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(IN) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: Q(LDQ,*), TAU(*), U(LDU,*),        &
     &                               V(LDV,*), WORK(*)
      END SUBROUTINE CGGSVP

      SUBROUTINE ZGGSVP( JOBU, JOBV, JOBQ, M, P, N, A, LDA, B, LDB,     &
     &                   TOLA, TOLB, K, L, U, LDU, V, LDV, Q, LDQ,      &
     &                   IWORK, RWORK, TAU, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBQ, JOBU, JOBV
         INTEGER, INTENT(IN) :: LDA, LDB, LDQ, LDU, LDV, M, N, P
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(IN) :: TOLA, TOLB
         REAL(WP), INTENT(IN) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: Q(LDQ,*), TAU(*), U(LDU,*),        &
     &                               V(LDV,*), WORK(*)
      END SUBROUTINE ZGGSVP

      END INTERFACE
#endif

! 
! lasi_TGEVC
! 
#ifdef __LASI_TGEVC 
      INTERFACE lasi_TGEVC

      SUBROUTINE STGEVC( SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
     &                   LDVL, VR, LDVR, MM, M, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STGEVC

      SUBROUTINE DTGEVC( SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
     &                   LDVL, VR, LDVR, MM, M, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTGEVC

      SUBROUTINE CTGEVC( SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
     &                   LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTGEVC

      SUBROUTINE ZTGEVC( SIDE, HOWMNY, SELECT, N, A, LDA, B, LDB, VL,   &
     &                   LDVL, VR, LDVR, MM, M, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDA, LDB, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTGEVC

      END INTERFACE
#endif

! 
! lasi_HGEQZ
! 
#ifdef __LASI_HGEQZ 
      INTERFACE lasi_HGEQZ

      SUBROUTINE SHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
     &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,    &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*), WORK(LWORK)
      END SUBROUTINE SHGEQZ

      SUBROUTINE DHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
     &                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, WORK,    &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*), WORK(LWORK)
      END SUBROUTINE DHGEQZ

      SUBROUTINE CHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
     &                   ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,      &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK( * )
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),    &
     &                                 Z(LDZ,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), WORK(LWORK)
      END SUBROUTINE CHGEQZ

      SUBROUTINE ZHGEQZ( JOB, COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB,&
     &                   ALPHA, BETA, Q, LDQ, Z, LDZ, WORK, LWORK,      &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK( * )
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),    &
     &                                 Z(LDZ,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), WORK(LWORK)
      END SUBROUTINE ZHGEQZ

      END INTERFACE
#endif

! 
! lasi_GGBAK
! 
#ifdef __LASI_GGBAK 
      INTERFACE lasi_GGBAK

      SUBROUTINE SGGBAK( JOB, SIDE, N, ILO, IHI, LSCALE, RSCALE, M, V,  &
     &                   LDV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: LSCALE(*), RSCALE(*)
         REAL(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE SGGBAK

      SUBROUTINE DGGBAK( JOB, SIDE, N, ILO, IHI, LSCALE, RSCALE, M, V,  &
     &                   LDV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: LSCALE(*), RSCALE(*)
         REAL(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE DGGBAK

      SUBROUTINE CGGBAK( JOB, SIDE, N, ILO, IHI, LSCALE, RSCALE, M, V,  &
     &                   LDV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: LSCALE(*), RSCALE(*)
         COMPLEX(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE CGGBAK

      SUBROUTINE ZGGBAK( JOB, SIDE, N, ILO, IHI, LSCALE, RSCALE, M, V,  &
     &                   LDV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: LSCALE(*), RSCALE(*)
         COMPLEX(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE ZGGBAK

      END INTERFACE
#endif

! 
! lasi_GGBAL
! 
#ifdef __LASI_GGBAL 
      INTERFACE lasi_GGBAL

      SUBROUTINE SGGBAL( JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,      &
     &                   RSCALE, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, LDB, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: LSCALE(*), RSCALE(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE SGGBAL

      SUBROUTINE DGGBAL( JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,      &
     &                   RSCALE, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, LDB, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: LSCALE(*), RSCALE(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE DGGBAL

      SUBROUTINE CGGBAL( JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,      &
     &                   RSCALE, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, LDB, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: LSCALE(*), RSCALE(*), WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE CGGBAL

      SUBROUTINE ZGGBAL( JOB, N, A, LDA, B, LDB, ILO, IHI, LSCALE,      &
     &                   RSCALE, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, LDB, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: LSCALE(*), RSCALE(*), WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE ZGGBAL

      END INTERFACE
#endif

! 
! lasi_GGHRD
! 
#ifdef __LASI_GGHRD 
      INTERFACE lasi_GGHRD

      SUBROUTINE SGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,  &
     &                   LDQ, Z, LDZ, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
      END SUBROUTINE SGGHRD

      SUBROUTINE DGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,  &
     &                   LDQ, Z, LDZ, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),       &
     &                              Z(LDZ,*)
      END SUBROUTINE DGGHRD

      SUBROUTINE CGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,  &
     &                   LDQ, Z, LDZ, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),    &
     &                                 Z(LDZ,*)
      END SUBROUTINE CGGHRD

      SUBROUTINE ZGGHRD( COMPQ, COMPZ, N, ILO, IHI, A, LDA, B, LDB, Q,  &
     &                   LDQ, Z, LDZ, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, COMPZ
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDB, LDQ, LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), Q(LDQ,*),    &
     &                                 Z(LDZ,*)
      END SUBROUTINE ZGGHRD

      END INTERFACE
#endif

! 
! lasi_PBSTF
! 
#ifdef __LASI_PBSTF 
      INTERFACE lasi_PBSTF

      SUBROUTINE SPBSTF( UPLO, N, KD, AB, LDAB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) ::UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB( LDAB, * )
      END SUBROUTINE SPBSTF

      SUBROUTINE DPBSTF( UPLO, N, KD, AB, LDAB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
         IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) ::UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB( LDAB, * )
      END SUBROUTINE DPBSTF

      SUBROUTINE CPBSTF( UPLO, N, KD, AB, LDAB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
         IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) ::UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB( LDAB, * )
      END SUBROUTINE CPBSTF

      SUBROUTINE ZPBSTF( UPLO, N, KD, AB, LDAB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) ::UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB( LDAB, * )
      END SUBROUTINE ZPBSTF

      END INTERFACE
#endif

! 
! lasi_SBGST
! 
#ifdef __LASI_SBGST 
      INTERFACE lasi_SBGST

      SUBROUTINE SSBGST( VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,  &
     &                   LDX, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: BB(LDBB,*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(LDX,*)
      END SUBROUTINE SSBGST

      SUBROUTINE DSBGST( VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,  &
     &                   LDX, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: BB(LDBB,*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(LDX,*)
      END SUBROUTINE DSBGST

      END INTERFACE
#endif

! 
! lasi_HBGST
! 
#ifdef __LASI_HBGST 
      INTERFACE lasi_HBGST

      SUBROUTINE CHBGST( VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,  &
     &                   LDX, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(IN) :: BB(LDBB,*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), X(LDX,*)
      END SUBROUTINE CHBGST

      SUBROUTINE ZHBGST( VECT, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, X,  &
     &                   LDX, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, LDX, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(IN) :: BB(LDBB,*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), X(LDX,*)
      END SUBROUTINE ZHBGST

      END INTERFACE
#endif

! 
! lasi_SPGST
! 
#ifdef __LASI_SPGST 
      INTERFACE lasi_SPGST

      SUBROUTINE SSPGST( ITYPE, UPLO, N, AP, BP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: BP(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE SSPGST

      SUBROUTINE DSPGST( ITYPE, UPLO, N, AP, BP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: BP(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE DSPGST

      END INTERFACE
#endif

! 
! lasi_HPGST
! 
#ifdef __LASI_HPGST 
      INTERFACE lasi_HPGST

      SUBROUTINE CHPGST( ITYPE, UPLO, N, AP, BP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: BP(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE CHPGST

      SUBROUTINE ZHPGST( ITYPE, UPLO, N, AP, BP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: BP(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE ZHPGST

      END INTERFACE
#endif

! 
! lasi_BDSQR
! 
#ifdef __LASI_BDSQR 
      INTERFACE lasi_BDSQR

      SUBROUTINE SBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
     &                   LDU, C, LDC, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(INOUT) :: D(*), E(*)
         REAL, INTENT(OUT) :: RWORK(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*), U(LDU,*), VT(LDVT,*)
      END SUBROUTINE SBDSQR

      SUBROUTINE DBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
     &                   LDU, C, LDC, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(INOUT) :: D(*), E(*)
         REAL, INTENT(OUT) :: RWORK(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*), U(LDU,*), VT(LDVT,*)
      END SUBROUTINE DBDSQR

      SUBROUTINE CBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
     &                   LDU, C, LDC, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(INOUT) :: D(*), E(*)
         REAL, INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*), U(LDU,*), VT(LDVT,*)
      END SUBROUTINE CBDSQR

      SUBROUTINE ZBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,    &
     &                   LDU, C, LDC, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDC, LDU, LDVT, N, NCC, NCVT, NRU
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(INOUT) :: D(*), E(*)
         REAL, INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*), U(LDU,*), VT(LDVT,*)
      END SUBROUTINE ZBDSQR

      END INTERFACE
#endif

! 
! lasi_ORMBR
! 
#ifdef __LASI_ORMBR 
      INTERFACE lasi_ORMBR

      SUBROUTINE SORMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,    &
     &                   LDC, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMBR

      SUBROUTINE DORMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,    &
     &                   LDC, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMBR

      END INTERFACE
#endif

! 
! lasi_UNMBR
! 
#ifdef __LASI_UNMBR 
      INTERFACE lasi_UNMBR

      SUBROUTINE CUNMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,    &
     &                   LDC, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNMBR

      SUBROUTINE ZUNMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,    &
     &                   LDC, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, VECT
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNMBR

      END INTERFACE
#endif

! 
! lasi_ORGBR
! 
#ifdef __LASI_ORGBR 
      INTERFACE lasi_ORGBR

      SUBROUTINE SORGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK,       &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGBR

      SUBROUTINE DORGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK,       &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGBR

      END INTERFACE
#endif

! 
! lasi_UNGBR
! 
#ifdef __LASI_UNGBR 
      INTERFACE lasi_UNGBR

      SUBROUTINE CUNGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK,       &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNGBR

      SUBROUTINE ZUNGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK,       &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNGBR

      END INTERFACE
#endif

! 
! lasi_GBBRD
! 
#ifdef __LASI_GBBRD 
      INTERFACE lasi_GBBRD

      SUBROUTINE SGBBRD( VECT, M, N, NCC, KL, KU, AB, LDAB, D, E, Q,    &
     &                   LDQ, PT, LDPT, C, LDC, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDC, LDPT, LDQ, M, N, NCC
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), C(LDC,*)
         REAL(WP), INTENT(OUT) :: PT(LDPT,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE SGBBRD

      SUBROUTINE DGBBRD( VECT, M, N, NCC, KL, KU, AB, LDAB, D, E, Q,    &
     &                   LDQ, PT, LDPT, C, LDC, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDC, LDPT, LDQ, M, N, NCC
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), C(LDC,*)
         REAL(WP), INTENT(OUT) :: PT(LDPT,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE DGBBRD

      SUBROUTINE CGBBRD( VECT, M, N, NCC, KL, KU, AB, LDAB, D, E, Q,    &
     &                   LDQ, PT, LDPT, C, LDC, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDC, LDPT, LDQ, M, N, NCC
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(OUT) :: D(*), E(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: PT(LDPT,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE CGBBRD

      SUBROUTINE ZGBBRD( VECT, M, N, NCC, KL, KU, AB, LDAB, D, E, Q,    &
     &                   LDQ, PT, LDPT, C, LDC, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: VECT
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDC, LDPT, LDQ, M, N, NCC
         INTEGER, INTENT(OUT) :: INFO
         REAL, INTENT(OUT) :: D(*), E(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: PT(LDPT,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE ZGBBRD

      END INTERFACE
#endif

! 
! lasi_GEBRD
! 
#ifdef __LASI_GEBRD 
      INTERFACE lasi_GEBRD

      SUBROUTINE SGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,   &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAUP(*), TAUQ(*), WORK(LWORK)
      END SUBROUTINE SGEBRD

      SUBROUTINE DGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,   &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAUP(*), TAUQ(*), WORK(LWORK)
      END SUBROUTINE DGEBRD

      SUBROUTINE CGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,   &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAUP(*), TAUQ(*), WORK(LWORK)
      END SUBROUTINE CGEBRD

      SUBROUTINE ZGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,   &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAUP(*), TAUQ(*), WORK(LWORK)
      END SUBROUTINE ZGEBRD

      END INTERFACE
#endif

! 
! lasi_TRSEN
! 
#ifdef __LASI_TRSEN 
      INTERFACE lasi_TRSEN

      SUBROUTINE STRSEN( JOB, COMPQ, SELECT, N, T, LDT, Q, LDQ, WR, WI, &
     &                   M, S, SEP, WORK, LWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, JOB
         INTEGER, INTENT(IN) :: LDQ, LDT, LWORK, N, LIWORK
         INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
         REAL(WP), INTENT(OUT) :: S, SEP
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*)
         REAL(WP), INTENT(IN) :: WR(*), WI(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE STRSEN

      SUBROUTINE DTRSEN( JOB, COMPQ, SELECT, N, T, LDT, Q, LDQ, WR, WI, &
     &                   M, S, SEP, WORK, LWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, JOB
         INTEGER, INTENT(IN) :: LDQ, LDT, LWORK, N, LIWORK
         INTEGER, INTENT(OUT) :: INFO, M, IWORK(LIWORK)
         REAL(WP), INTENT(OUT) :: S, SEP
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*)
         REAL(WP), INTENT(IN) :: WR(*), WI(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DTRSEN

      SUBROUTINE CTRSEN( JOB, COMPQ, SELECT, N, T, LDT, Q, LDQ, W, M, S,&
     &                   SEP, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, JOB
         INTEGER, INTENT(IN) :: LDQ, LDT, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, M
         REAL(WP), INTENT(OUT) :: S, SEP
         LOGICAL, INTENT(IN) :: SELECT(*)
         COMPLEX(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*)
         COMPLEX(WP), INTENT(IN) :: W(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CTRSEN

      SUBROUTINE ZTRSEN( JOB, COMPQ, SELECT, N, T, LDT, Q, LDQ, W, M, S,&
     &                   SEP, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ, JOB
         INTEGER, INTENT(IN) :: LDQ, LDT, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, M
         REAL(WP), INTENT(OUT) :: S, SEP
         LOGICAL, INTENT(IN) :: SELECT(*)
         COMPLEX(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*)
         COMPLEX(WP), INTENT(IN) :: W(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZTRSEN

      END INTERFACE
#endif

! 
! lasi_TRSNA
! 
#ifdef __LASI_TRSNA 
      INTERFACE lasi_TRSNA

      SUBROUTINE STRSNA( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,  &
     &                   LDVR, S, SEP, MM, M, WORK, LDWORK, IWORK,      &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, LDWORK, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: S(*), SEP(*)
         REAL(WP), INTENT(IN) :: T(LDT,*), VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(LDWORK,*)
      END SUBROUTINE STRSNA

      SUBROUTINE DTRSNA( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,  &
     &                   LDVR, S, SEP, MM, M, WORK, LDWORK, IWORK,      &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, LDWORK, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IWORK(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: S(*), SEP(*)
         REAL(WP), INTENT(IN) :: T(LDT,*), VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(LDWORK,*)
      END SUBROUTINE DTRSNA

      SUBROUTINE CTRSNA( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,  &
     &                   LDVR, S, SEP, MM, M, WORK, LDWORK, RWORK,      &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, LDWORK, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: RWORK(*), S(*), SEP(*)
         COMPLEX(WP), INTENT(IN) :: T(LDT,*), VL(LDVL,*), VR(LDVR,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LDWORK,*)
      END SUBROUTINE CTRSNA

      SUBROUTINE ZTRSNA( JOB, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,  &
     &                   LDVR, S, SEP, MM, M, WORK, LDWORK, RWORK,      &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, JOB
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, LDWORK, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: RWORK(*), S(*), SEP(*)
         COMPLEX(WP), INTENT(IN) :: T(LDT,*), VL(LDVL,*), VR(LDVR,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LDWORK,*)
      END SUBROUTINE ZTRSNA

      END INTERFACE
#endif

! 
! lasi_TRSYL
! 
#ifdef __LASI_TRSYL 
      INTERFACE lasi_TRSYL

      SUBROUTINE STRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,   &
     &                   LDC, SCALE, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANA, TRANB
         INTEGER, INTENT(IN) :: ISGN, LDA, LDB, LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: SCALE
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE STRSYL

      SUBROUTINE DTRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,   &
     &                   LDC, SCALE, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANA, TRANB
         INTEGER, INTENT(IN) :: ISGN, LDA, LDB, LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: SCALE
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE DTRSYL

      SUBROUTINE CTRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,   &
     &                   LDC, SCALE, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANA, TRANB
         INTEGER, INTENT(IN) :: ISGN, LDA, LDB, LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: SCALE
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE CTRSYL

      SUBROUTINE ZTRSYL( TRANA, TRANB, ISGN, M, N, A, LDA, B, LDB, C,   &
     &                   LDC, SCALE, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANA, TRANB
         INTEGER, INTENT(IN) :: ISGN, LDA, LDB, LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: SCALE
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE ZTRSYL

      END INTERFACE
#endif

! 
! lasi_TREXC
! 
#ifdef __LASI_TREXC 
      INTERFACE lasi_TREXC

      SUBROUTINE STREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, WORK,    &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ
         INTEGER, INTENT(IN) :: IFST, ILST, LDQ, LDT, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*), WORK(*)
      END SUBROUTINE STREXC

      SUBROUTINE DTREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, WORK,    &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ
         INTEGER, INTENT(IN) :: IFST, ILST, LDQ, LDT, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*), WORK(*)
      END SUBROUTINE DTREXC

      SUBROUTINE CTREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ
         INTEGER, INTENT(IN) :: IFST, ILST, LDQ, LDT, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*)
      END SUBROUTINE CTREXC

      SUBROUTINE ZTREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPQ
         INTEGER, INTENT(IN) :: IFST, ILST, LDQ, LDT, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: Q(LDQ,*), T(LDT,*)
      END SUBROUTINE ZTREXC

      END INTERFACE
#endif

! 
! lasi_TREVC
! 
#ifdef __LASI_TREVC 
      INTERFACE lasi_TREVC

      SUBROUTINE STREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, &
     &                   LDVR, MM, M, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(INOUT) :: SELECT(*)
         REAL(WP), INTENT(IN) :: T(LDT,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STREVC

      SUBROUTINE DTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, &
     &                   LDVR, MM, M, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(INOUT) :: SELECT(*)
         REAL(WP), INTENT(IN) :: T(LDT,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTREVC

      SUBROUTINE CTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, &
     &                   LDVR, MM, M, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(INOUT) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: T(LDT,*), VL(LDVL,*), VR(LDVR,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTREVC

      SUBROUTINE ZTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR, &
     &                   LDVR, MM, M, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: HOWMNY, SIDE
         INTEGER, INTENT(IN) :: LDT, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M
         LOGICAL, INTENT(INOUT) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: T(LDT,*), VL(LDVL,*), VR(LDVR,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTREVC

      END INTERFACE
#endif

! 
! lasi_HSEIN
! 
#ifdef __LASI_HSEIN 
      INTERFACE lasi_HSEIN

      SUBROUTINE SHSEIN( SIDE, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,&
     &                   VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL,       &
     &                   IFAILR, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
         INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(INOUT) :: WR(*), WI(*)
         REAL(WP), INTENT(IN) :: H(LDH,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SHSEIN

      SUBROUTINE DHSEIN( SIDE, EIGSRC, INITV, SELECT, N, H, LDH, WR, WI,&
     &                   VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL,       &
     &                   IFAILR, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
         INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(INOUT) :: WR(*), WI(*)
         REAL(WP), INTENT(IN) :: H(LDH,*)
         REAL(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DHSEIN

      SUBROUTINE CHSEIN( SIDE, EIGSRC, INITV, SELECT, N, H, LDH, W, VL, &
     &                   LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL,    &
     &                   IFAILR, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
         INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: RWORK( * )
         COMPLEX(WP), INTENT(IN) :: H(LDH,*)
         COMPLEX(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*), W(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHSEIN

      SUBROUTINE ZHSEIN( SIDE, EIGSRC, INITV, SELECT, N, H, LDH, W, VL, &
     &                   LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL,    &
     &                   IFAILR, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: EIGSRC, INITV, SIDE
         INTEGER, INTENT(IN) :: LDH, LDVL, LDVR, MM, N
         INTEGER, INTENT(OUT) :: INFO, M, IFAILL(*), IFAILR(*)
         LOGICAL, INTENT(IN) :: SELECT(*)
         REAL(WP), INTENT(OUT) :: RWORK( * )
         COMPLEX(WP), INTENT(IN) :: H(LDH,*)
         COMPLEX(WP), INTENT(INOUT) :: VL(LDVL,*), VR(LDVR,*), W(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHSEIN

      END INTERFACE
#endif

! 
! lasi_HSEQR
! 
#ifdef __LASI_HSEQR 
      INTERFACE lasi_HSEQR

      SUBROUTINE SHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,    &
     &                   LDZ, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: WR(*), WI(*)
         REAL(WP), INTENT(INOUT) :: H(LDH,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SHSEQR

      SUBROUTINE DHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, WR, WI, Z,    &
     &                   LDZ, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: WR(*), WI(*)
         REAL(WP), INTENT(INOUT) :: H(LDH,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DHSEQR

      SUBROUTINE CHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,    &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: H(LDH,*), Z(LDZ,*)
         COMPLEX(WP), INTENT(OUT) :: W(*), WORK(LWORK)
      END SUBROUTINE CHSEQR

      SUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,    &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ, JOB
         INTEGER, INTENT(IN) :: IHI, ILO, LDH, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: H(LDH,*), Z(LDZ,*)
         COMPLEX(WP), INTENT(OUT) :: W(*), WORK(LWORK)
      END SUBROUTINE ZHSEQR

      END INTERFACE
#endif

! 
! lasi_ORMHR
! 
#ifdef __LASI_ORMHR 
      INTERFACE lasi_ORMHR

      SUBROUTINE SORMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,   &
     &                   LDC, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1),  INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMHR

      SUBROUTINE DORMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,   &
     &                   LDC, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1),  INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMHR

      END INTERFACE
#endif

! 
! lasi_UNMHR
! 
#ifdef __LASI_UNMHR 
      INTERFACE lasi_UNMHR

      SUBROUTINE CUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,   &
     &                   LDC, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1),  INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNMHR

      SUBROUTINE ZUNMHR( SIDE, TRANS, M, N, ILO, IHI, A, LDA, TAU, C,   &
     &                   LDC, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1),  INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNMHR

      END INTERFACE
#endif

! 
! lasi_ORGHR
! 
#ifdef __LASI_ORGHR 
      INTERFACE lasi_ORGHR

      SUBROUTINE SORGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGHR

      SUBROUTINE DORGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGHR

      END INTERFACE
#endif

! 
! lasi_UNGHR
! 
#ifdef __LASI_UNGHR 
      INTERFACE lasi_UNGHR

      SUBROUTINE CUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNGHR

      SUBROUTINE ZUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNGHR

      END INTERFACE
#endif

! 
! lasi_GEBAK
! 
#ifdef __LASI_GEBAK 
      INTERFACE lasi_GEBAK

      SUBROUTINE SGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,      &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: SCALE(*)
         REAL(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE SGEBAK

      SUBROUTINE DGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,      &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: SCALE(*)
         REAL(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE DGEBAK

      SUBROUTINE CGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,      &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: SCALE(*)
         COMPLEX(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE CGEBAK

      SUBROUTINE ZGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,      &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB, SIDE
         INTEGER, INTENT(IN) :: IHI, ILO, LDV, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: SCALE(*)
         COMPLEX(WP), INTENT(INOUT) :: V(LDV,*)
      END SUBROUTINE ZGEBAK

      END INTERFACE
#endif

! 
! lasi_GEBAL
! 
#ifdef __LASI_GEBAL 
      INTERFACE lasi_GEBAL

      SUBROUTINE SGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: SCALE(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE SGEBAL

      SUBROUTINE DGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: SCALE(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE DGEBAL

      SUBROUTINE CGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: SCALE(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE CGEBAL

      SUBROUTINE ZGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOB
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: IHI, ILO, INFO
         REAL(WP), INTENT(OUT) :: SCALE(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE ZGEBAL

      END INTERFACE
#endif

! 
! lasi_GEHRD
! 
#ifdef __LASI_GEHRD 
      INTERFACE lasi_GEHRD

      SUBROUTINE SGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGEHRD

      SUBROUTINE DGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGEHRD

      SUBROUTINE CGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE CGEHRD

      SUBROUTINE ZGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: IHI, ILO, LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE ZGEHRD

      END INTERFACE
#endif

! 
! lasi_PTEQR
! 
#ifdef __LASI_PTEQR 
      INTERFACE lasi_PTEQR

      SUBROUTINE SPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: INFO, LDZ, N
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE SPTEQR

      SUBROUTINE DPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: INFO, LDZ, N
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE DPTEQR

      SUBROUTINE CPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: INFO, LDZ, N
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE CPTEQR

      SUBROUTINE ZPTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: INFO, LDZ, N
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE ZPTEQR

      END INTERFACE
#endif

! 
! lasi_STEIN
! 
#ifdef __LASI_STEIN 
      INTERFACE lasi_STEIN

      SUBROUTINE SSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
     &                   IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
         INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE SSTEIN

      SUBROUTINE DSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
     &                   IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
         INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE DSTEIN

      SUBROUTINE CSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
     &                   IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
         INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE CSTEIN

      SUBROUTINE ZSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,   &
     &                   IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDZ, M, N, IBLOCK(*), ISPLIT(*)
         INTEGER, INTENT(OUT) :: INFO, IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: D(*), E(*), W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z( LDZ, * )
      END SUBROUTINE ZSTEIN

      END INTERFACE
#endif

! 
! lasi_STEBZ
! 
#ifdef __LASI_STEBZ 
      INTERFACE lasi_STEBZ

      SUBROUTINE SSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, &
     &                   M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK,     &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: ORDER, RANGE
         INTEGER, INTENT(IN) :: IL, IU, M, N
         INTEGER, INTENT(OUT) :: INFO, NSPLIT, IBLOCK(*), ISPLIT(*),    &
     &                           IWORK(*) 
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU, D(*), E(*)
         REAL(WP), INTENT(OUT) :: W(*), WORK(*)
      END SUBROUTINE SSTEBZ

      SUBROUTINE DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, &
     &                   M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK,     &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: ORDER, RANGE
         INTEGER, INTENT(IN) :: IL, IU, M, N
         INTEGER, INTENT(OUT) :: INFO, NSPLIT, IBLOCK(*), ISPLIT(*),    &
     &                           IWORK(*) 
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU, D(*), E(*)
         REAL(WP), INTENT(OUT) :: W(*), WORK(*)
      END SUBROUTINE DSTEBZ

      END INTERFACE
#endif

! 
! lasi_STEDC
! 
#ifdef __LASI_STEDC 
      INTERFACE lasi_STEDC

      SUBROUTINE SSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
     &                   LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, LIWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SSTEDC

      SUBROUTINE DSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
     &                   LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, LIWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DSTEDC

      SUBROUTINE CSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, RWORK,    &
     &                   LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, LIWORK, LRWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: RWORK(LRWORK)
         COMPLEX(WP), INTENT(INOUT) :: Z(LDZ,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CSTEDC

      SUBROUTINE ZSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, RWORK,    &
     &                   LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, LIWORK, LRWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(LIWORK)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: RWORK(LRWORK)
         COMPLEX(WP), INTENT(INOUT) :: Z(LDZ,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZSTEDC

      END INTERFACE
#endif

! 
! lasi_STERF
! 
#ifdef __LASI_STERF 
      INTERFACE lasi_STERF

      SUBROUTINE SSTERF( N, D, E, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
      END SUBROUTINE SSTERF

      SUBROUTINE DSTERF( N, D, E, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
      END SUBROUTINE DSTERF

      END INTERFACE
#endif

! 
! lasi_STEQR
! 
#ifdef __LASI_STEQR 
      INTERFACE lasi_STEQR

      SUBROUTINE SSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE SSTEQR

      SUBROUTINE DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE DSTEQR

      SUBROUTINE CSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE CSTEQR

      SUBROUTINE ZSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: COMPZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: Z(LDZ,*)
      END SUBROUTINE ZSTEQR

      END INTERFACE
#endif

! 
! lasi_OPMTR
! 
#ifdef __LASI_OPMTR 
      INTERFACE lasi_OPMTR

      SUBROUTINE SOPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SOPMTR

      SUBROUTINE DOPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DOPMTR

      END INTERFACE
#endif

! 
! lasi_UPMTR
! 
#ifdef __LASI_UPMTR 
      INTERFACE lasi_UPMTR

      SUBROUTINE CUPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CUPMTR

      SUBROUTINE ZUPMTR( SIDE, UPLO, TRANS, M, N, AP, TAU, C, LDC, WORK,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDC, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZUPMTR

      END INTERFACE
#endif

! 
! lasi_OPGTR
! 
#ifdef __LASI_OPGTR 
      INTERFACE lasi_OPGTR

      SUBROUTINE SOPGTR( UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*), TAU(*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), WORK(*)
      END SUBROUTINE SOPGTR

      SUBROUTINE DOPGTR( UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*), TAU(*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), WORK(*)
      END SUBROUTINE DOPGTR

      END INTERFACE
#endif

! 
! lasi_UPGTR
! 
#ifdef __LASI_UPGTR 
      INTERFACE lasi_UPGTR

      SUBROUTINE CUPGTR( UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*), TAU(*)
         COMPLEX(WP), INTENT(OUT) :: Q(LDQ,*), WORK(*)
      END SUBROUTINE CUPGTR

      SUBROUTINE ZUPGTR( UPLO, N, AP, TAU, Q, LDQ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*), TAU(*)
         COMPLEX(WP), INTENT(OUT) :: Q(LDQ,*), WORK(*)
      END SUBROUTINE ZUPGTR

      END INTERFACE
#endif

! 
! lasi_ORMTR
! 
#ifdef __LASI_ORMTR 
      INTERFACE lasi_ORMTR

      SUBROUTINE SORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,  &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE SORMTR

      SUBROUTINE DORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,  &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE DORMTR

      END INTERFACE
#endif

! 
! lasi_UNMTR
! 
#ifdef __LASI_UNMTR 
      INTERFACE lasi_UNMTR

      SUBROUTINE CUNMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,  &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE CUNMTR

      SUBROUTINE ZUNMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC,  &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
      END SUBROUTINE ZUNMTR

      END INTERFACE
#endif

! 
! lasi_SBTRD
! 
#ifdef __LASI_SBTRD 
      INTERFACE lasi_SBTRD

      SUBROUTINE SSBTRD( VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,     &
     &                   WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), Q(LDQ,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSBTRD

      SUBROUTINE DSBTRD( VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,     &
     &                   WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), Q(LDQ,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSBTRD

      END INTERFACE
#endif

! 
! lasi_HBTRD
! 
#ifdef __LASI_HBTRD 
      INTERFACE lasi_HBTRD

      SUBROUTINE CHBTRD( VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,     &
     &                   WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), Q(LDQ,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHBTRD

      SUBROUTINE ZHBTRD( VECT, UPLO, N, KD, AB, LDAB, D, E, Q, LDQ,     &
     &                   WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, VECT
         INTEGER, INTENT(IN) :: KD, LDAB, LDQ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), Q(LDQ,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHBTRD

      END INTERFACE
#endif

! 
! lasi_SPTRD
! 
#ifdef __LASI_SPTRD 
      INTERFACE lasi_SPTRD

      SUBROUTINE SSPTRD( UPLO, N, AP, D, E, TAU, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: TAU(*)
      END SUBROUTINE SSPTRD

      SUBROUTINE DSPTRD( UPLO, N, AP, D, E, TAU, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: TAU(*)
      END SUBROUTINE DSPTRD

      END INTERFACE
#endif

! 
! lasi_HPTRD
! 
#ifdef __LASI_HPTRD 
      INTERFACE lasi_HPTRD

      SUBROUTINE CHPTRD( UPLO, N, AP, D, E, TAU, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*)
      END SUBROUTINE CHPTRD

      SUBROUTINE ZHPTRD( UPLO, N, AP, D, E, TAU, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*)
      END SUBROUTINE ZHPTRD

      END INTERFACE
#endif

! 
! lasi_TZRQF
! 
#ifdef __LASI_TZRQF 
      INTERFACE lasi_TZRQF

      SUBROUTINE STZRQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE STZRQF

      SUBROUTINE DTZRQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DTZRQF

      SUBROUTINE CTZRQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE CTZRQF

      SUBROUTINE ZTZRQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE ZTZRQF

      END INTERFACE
#endif

! 
! lasi_ORMRQ
! 
#ifdef __LASI_ORMRQ 
      INTERFACE lasi_ORMRQ

      SUBROUTINE SORMRQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMRQ

      SUBROUTINE DORMRQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMRQ

      END INTERFACE
#endif

! 
! lasi_UNMRQ
! 
#ifdef __LASI_UNMRQ 
      INTERFACE lasi_UNMRQ

      SUBROUTINE CUNMRQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNMRQ

      SUBROUTINE ZUNMRQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNMRQ

      END INTERFACE
#endif

! 
! lasi_ORGRQ
! 
#ifdef __LASI_ORGRQ 
      INTERFACE lasi_ORGRQ

      SUBROUTINE SORGRQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGRQ

      SUBROUTINE DORGRQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGRQ

      END INTERFACE
#endif

! 
! lasi_UNGRQ
! 
#ifdef __LASI_UNGRQ 
      INTERFACE lasi_UNGRQ

      SUBROUTINE CUNGRQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNGRQ

      SUBROUTINE ZUNGRQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNGRQ

      END INTERFACE
#endif

! 
! lasi_GERQF
! 
#ifdef __LASI_GERQF 
      INTERFACE lasi_GERQF

      SUBROUTINE SGERQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGERQF

      SUBROUTINE DGERQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGERQF

      SUBROUTINE CGERQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE CGERQF

      SUBROUTINE ZGERQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE ZGERQF

      END INTERFACE
#endif

! 
! lasi_ORMQL
! 
#ifdef __LASI_ORMQL 
      INTERFACE lasi_ORMQL

      SUBROUTINE SORMQL( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMQL

      SUBROUTINE DORMQL( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMQL

      END INTERFACE
#endif

! 
! lasi_UNMQL
! 
#ifdef __LASI_UNMQL 
      INTERFACE lasi_UNMQL

      SUBROUTINE CUNMQL( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNMQL

      SUBROUTINE ZUNMQL( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNMQL

      END INTERFACE
#endif

! 
! lasi_ORGQL
! 
#ifdef __LASI_ORGQL 
      INTERFACE lasi_ORGQL

      SUBROUTINE SORGQL( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGQL

      SUBROUTINE DORGQL( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGQL

      END INTERFACE
#endif

! 
! lasi_UNGQL
! 
#ifdef __LASI_UNGQL 
      INTERFACE lasi_UNGQL

      SUBROUTINE CUNGQL( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNGQL

      SUBROUTINE ZUNGQL( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNGQL

      END INTERFACE
#endif

! 
! lasi_GEQLF
! 
#ifdef __LASI_GEQLF 
      INTERFACE lasi_GEQLF

      SUBROUTINE SGEQLF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGEQLF

      SUBROUTINE DGEQLF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGEQLF

      SUBROUTINE CGEQLF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE CGEQLF

      SUBROUTINE ZGEQLF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE ZGEQLF

      END INTERFACE
#endif

! 
! lasi_ORMLQ
! 
#ifdef __LASI_ORMLQ 
      INTERFACE lasi_ORMLQ

      SUBROUTINE SORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMLQ

      SUBROUTINE DORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMLQ

      END INTERFACE
#endif

! 
! lasi_UNMLQ
! 
#ifdef __LASI_UNMLQ 
      INTERFACE lasi_UNMLQ

      SUBROUTINE CUNMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNMLQ

      SUBROUTINE ZUNMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNMLQ

      END INTERFACE
#endif

! 
! lasi_ORGLQ
! 
#ifdef __LASI_ORGLQ 
      INTERFACE lasi_ORGLQ

      SUBROUTINE SORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGLQ

      SUBROUTINE DORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGLQ

      END INTERFACE
#endif

! 
! lasi_UNGLQ
! 
#ifdef __LASI_UNGLQ 
      INTERFACE lasi_UNGLQ

      SUBROUTINE CUNGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNGLQ

      SUBROUTINE ZUNGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNGLQ

      END INTERFACE
#endif

! 
! lasi_GELQF
! 
#ifdef __LASI_GELQF 
      INTERFACE lasi_GELQF

      SUBROUTINE SGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGELQF

      SUBROUTINE DGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGELQF

      SUBROUTINE CGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE CGELQF

      SUBROUTINE ZGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE ZGELQF

      END INTERFACE
#endif

! 
! lasi_ORMQR
! 
#ifdef __LASI_ORMQR 
      INTERFACE lasi_ORMQR

      SUBROUTINE SORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORMQR

      SUBROUTINE DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         REAL(WP), INTENT(INOUT) :: C(LDC,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORMQR

      END INTERFACE
#endif

! 
! lasi_UNMQR
! 
#ifdef __LASI_UNMQR 
      INTERFACE lasi_UNMQR

      SUBROUTINE CUNMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNMQR

      SUBROUTINE ZUNMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,     &
     &                   WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: SIDE, TRANS
         INTEGER, INTENT(IN) :: K, LDA, LDC, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: C(LDC,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNMQR

      END INTERFACE
#endif

! 
! lasi_ORGQR
! 
#ifdef __LASI_ORGQR 
      INTERFACE lasi_ORGQR

      SUBROUTINE SORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGQR

      SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGQR

      END INTERFACE
#endif

! 
! lasi_UNGQR
! 
#ifdef __LASI_UNGQR 
      INTERFACE lasi_UNGQR

      SUBROUTINE CUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNGQR

      SUBROUTINE ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: K, LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNGQR

      END INTERFACE
#endif

! 
! lasi_GEQRF
! 
#ifdef __LASI_GEQRF 
      INTERFACE lasi_GEQRF

      SUBROUTINE SGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SGEQRF

      SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DGEQRF

      SUBROUTINE CGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE CGEQRF

      SUBROUTINE ZGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, M, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE ZGEQRF

      END INTERFACE
#endif

! 
! lasi_GEQPF
! 
#ifdef __LASI_GEQPF 
      INTERFACE lasi_GEQPF

      SUBROUTINE SGEQPF( M, N, A, LDA, JPVT, TAU, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(*)
      END SUBROUTINE SGEQPF

      SUBROUTINE DGEQPF( M, N, A, LDA, JPVT, TAU, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(*)
      END SUBROUTINE DGEQPF

      SUBROUTINE CGEQPF( M, N, A, LDA, JPVT, TAU, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(*)
      END SUBROUTINE CGEQPF

      SUBROUTINE ZGEQPF( M, N, A, LDA, JPVT, TAU, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(*)
      END SUBROUTINE ZGEQPF

      END INTERFACE
#endif

! 
! lasi_TBRFS
! 
#ifdef __LASI_TBRFS 
      INTERFACE lasi_TBRFS

      SUBROUTINE STBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STBRFS

      SUBROUTINE DTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTBRFS

      SUBROUTINE CTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTBRFS

      SUBROUTINE ZTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTBRFS

      MODULE PROCEDURE STBRFS1
      MODULE PROCEDURE DTBRFS1
      MODULE PROCEDURE CTBRFS1
      MODULE PROCEDURE ZTBRFS1

      END INTERFACE
#endif

! 
! lasi_TBCON
! 
#ifdef __LASI_TBCON 
      INTERFACE lasi_TBCON

      SUBROUTINE STBCON( NORM, UPLO, DIAG, N, KD, AB, LDAB, RCOND, WORK,&
     &                   IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STBCON

      SUBROUTINE DTBCON( NORM, UPLO, DIAG, N, KD, AB, LDAB, RCOND, WORK,&
     &                   IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTBCON

      SUBROUTINE CTBCON( NORM, UPLO, DIAG, N, KD, AB, LDAB, RCOND, WORK,&
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTBCON

      SUBROUTINE ZTBCON( NORM, UPLO, DIAG, N, KD, AB, LDAB, RCOND, WORK,&
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTBCON

      END INTERFACE
#endif

! 
! lasi_TBTRS
! 
#ifdef __LASI_TBTRS 
      INTERFACE lasi_TBTRS

      SUBROUTINE STBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE STBTRS

      SUBROUTINE DTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DTBTRS

      SUBROUTINE CTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE CTBTRS

      SUBROUTINE ZTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,   &
     &                   LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE ZTBTRS

      MODULE PROCEDURE STBTRS1
      MODULE PROCEDURE DTBTRS1
      MODULE PROCEDURE CTBTRS1
      MODULE PROCEDURE ZTBTRS1

      END INTERFACE
#endif

! 
! lasi_TPTRI
! 
#ifdef __LASI_TPTRI 
      INTERFACE lasi_TPTRI

      SUBROUTINE STPTRI( UPLO, DIAG, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP( * )
      END SUBROUTINE STPTRI

      SUBROUTINE DTPTRI( UPLO, DIAG, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP( * )
      END SUBROUTINE DTPTRI

      SUBROUTINE CTPTRI( UPLO, DIAG, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP( * )
      END SUBROUTINE CTPTRI

      SUBROUTINE ZTPTRI( UPLO, DIAG, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP( * )
      END SUBROUTINE ZTPTRI

      END INTERFACE
#endif

! 
! lasi_TPRFS
! 
#ifdef __LASI_TPRFS 
      INTERFACE lasi_TPRFS

      SUBROUTINE STPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,&
     &                   FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STPRFS

      SUBROUTINE DTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,&
     &                   FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTPRFS

      SUBROUTINE CTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,&
     &                   FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTPRFS

      SUBROUTINE ZTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,&
     &                   FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTPRFS

      MODULE PROCEDURE STPRFS1
      MODULE PROCEDURE DTPRFS1
      MODULE PROCEDURE CTPRFS1
      MODULE PROCEDURE ZTPRFS1

      END INTERFACE
#endif

! 
! lasi_TPCON
! 
#ifdef __LASI_TPCON 
      INTERFACE lasi_TPCON

      SUBROUTINE STPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK, IWORK,   &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STPCON

      SUBROUTINE DTPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK, IWORK,   &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTPCON

      SUBROUTINE CTPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK, RWORK,   &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTPCON

      SUBROUTINE ZTPCON( NORM, UPLO, DIAG, N, AP, RCOND, WORK, RWORK,   &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTPCON

      END INTERFACE
#endif

! 
! lasi_TPTRS
! 
#ifdef __LASI_TPTRS 
      INTERFACE lasi_TPTRS

      SUBROUTINE STPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE STPTRS

      SUBROUTINE DTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DTPTRS

      SUBROUTINE CTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE CTPTRS

      SUBROUTINE ZTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE ZTPTRS

      MODULE PROCEDURE STPTRS1
      MODULE PROCEDURE DTPTRS1
      MODULE PROCEDURE CTPTRS1
      MODULE PROCEDURE ZTPTRS1

      END INTERFACE
#endif

! 
! lasi_TRTRI
! 
#ifdef __LASI_TRTRI 
      INTERFACE lasi_TRTRI

      SUBROUTINE STRTRI( UPLO, DIAG, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A( LDA, * )
      END SUBROUTINE STRTRI

      SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A( LDA, * )
      END SUBROUTINE DTRTRI

      SUBROUTINE CTRTRI( UPLO, DIAG, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A( LDA, * )
      END SUBROUTINE CTRTRI

      SUBROUTINE ZTRTRI( UPLO, DIAG, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A( LDA, * )
      END SUBROUTINE ZTRTRI

      END INTERFACE
#endif

! 
! lasi_TRRFS
! 
#ifdef __LASI_TRRFS 
      INTERFACE lasi_TRRFS

      SUBROUTINE STRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X, &
     &                   LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(IN) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STRRFS

      SUBROUTINE DTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X, &
     &                   LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(IN) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTRRFS

      SUBROUTINE CTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X, &
     &                   LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTRRFS

      SUBROUTINE ZTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X, &
     &                   LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTRRFS

      MODULE PROCEDURE STRRFS1
      MODULE PROCEDURE DTRRFS1
      MODULE PROCEDURE CTRRFS1
      MODULE PROCEDURE ZTRRFS1

      END INTERFACE
#endif

! 
! lasi_TRCON
! 
#ifdef __LASI_TRCON 
      INTERFACE lasi_TRCON

      SUBROUTINE STRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,      &
     &                   IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE STRCON

      SUBROUTINE DTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,      &
     &                   IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DTRCON

      SUBROUTINE CTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,      &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CTRCON

      SUBROUTINE ZTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,      &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZTRCON

      END INTERFACE
#endif

! 
! lasi_TRTRS
! 
#ifdef __LASI_TRTRS 
      INTERFACE lasi_TRTRS

      SUBROUTINE STRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,    &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE STRTRS

      SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,    &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DTRTRS

      SUBROUTINE CTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,    &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE CTRTRS

      SUBROUTINE ZTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,    &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE ZTRTRS

      MODULE PROCEDURE STRTRS1
      MODULE PROCEDURE DTRTRS1
      MODULE PROCEDURE CTRTRS1
      MODULE PROCEDURE ZTRTRS1

      END INTERFACE
#endif

! 
! lasi_SPTRI
! 
#ifdef __LASI_SPTRI 
      INTERFACE lasi_SPTRI

      SUBROUTINE SSPTRI( UPLO, N, AP, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSPTRI

      SUBROUTINE DSPTRI( UPLO, N, AP, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSPTRI

      SUBROUTINE CSPTRI( UPLO, N, AP, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CSPTRI

      SUBROUTINE ZSPTRI( UPLO, N, AP, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZSPTRI

      END INTERFACE
#endif

! 
! lasi_HPTRI
! 
#ifdef __LASI_HPTRI 
      INTERFACE lasi_HPTRI

      SUBROUTINE CHPTRI( UPLO, N, AP, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHPTRI

      SUBROUTINE ZHPTRI( UPLO, N, AP, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHPTRI

      END INTERFACE
#endif

! 
! lasi_SPRFS
! 
#ifdef __LASI_SPRFS 
      INTERFACE lasi_SPRFS

      SUBROUTINE SSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,  &
     &                   FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSPRFS

      SUBROUTINE DSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,  &
     &                   FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSPRFS

      SUBROUTINE CSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,  &
     &                   FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CSPRFS

      SUBROUTINE ZSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,  &
     &                   FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZSPRFS

      MODULE PROCEDURE SSPRFS1
      MODULE PROCEDURE DSPRFS1
      MODULE PROCEDURE CSPRFS1
      MODULE PROCEDURE ZSPRFS1

      END INTERFACE
#endif

! 
! lasi_HPRFS
! 
#ifdef __LASI_HPRFS 
      INTERFACE lasi_HPRFS

      SUBROUTINE CHPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,  &
     &                   FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHPRFS

      SUBROUTINE ZHPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,  &
     &                   FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHPRFS

      MODULE PROCEDURE CHPRFS1
      MODULE PROCEDURE ZHPRFS1

      END INTERFACE
#endif

! 
! lasi_HPCON
! 
#ifdef __LASI_HPCON 
      INTERFACE lasi_HPCON

      SUBROUTINE CHPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHPCON

      SUBROUTINE ZHPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHPCON

      END INTERFACE
#endif

! 
! lasi_SPCON
! 
#ifdef __LASI_SPCON 
      INTERFACE lasi_SPCON

      SUBROUTINE SSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSPCON

      SUBROUTINE DSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, IWORK,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSPCON

      SUBROUTINE CSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CSPCON

      SUBROUTINE ZSPCON( UPLO, N, AP, IPIV, ANORM, RCOND, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZSPCON

      END INTERFACE
#endif

! 
! lasi_SPTRS
! 
#ifdef __LASI_SPTRS 
      INTERFACE lasi_SPTRS

      SUBROUTINE SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE SSPTRS

      SUBROUTINE DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DSPTRS

      SUBROUTINE CSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE CSPTRS

      SUBROUTINE ZSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE ZSPTRS

      MODULE PROCEDURE SSPTRS1
      MODULE PROCEDURE DSPTRS1
      MODULE PROCEDURE CSPTRS1
      MODULE PROCEDURE ZSPTRS1

      END INTERFACE
#endif

! 
! lasi_HPTRS
! 
#ifdef __LASI_HPTRS 
      INTERFACE lasi_HPTRS

      SUBROUTINE CHPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE CHPTRS

      SUBROUTINE ZHPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE ZHPTRS

      MODULE PROCEDURE CHPTRS1
      MODULE PROCEDURE ZHPTRS1

      END INTERFACE
#endif

! 
! lasi_HPTRF
! 
#ifdef __LASI_HPTRF 
      INTERFACE lasi_HPTRF

      SUBROUTINE CHPTRF( UPLO, N, AP, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE CHPTRF

      SUBROUTINE ZHPTRF( UPLO, N, AP, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE ZHPTRF

      END INTERFACE
#endif

! 
! lasi_SPTRF
! 
#ifdef __LASI_SPTRF 
      INTERFACE lasi_SPTRF

      SUBROUTINE SSPTRF( UPLO, N, AP, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE SSPTRF

      SUBROUTINE DSPTRF( UPLO, N, AP, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE DSPTRF

      SUBROUTINE CSPTRF( UPLO, N, AP, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE CSPTRF

      SUBROUTINE ZSPTRF( UPLO, N, AP, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE ZSPTRF

      END INTERFACE
#endif

! 
! lasi_SYTRI
! 
#ifdef __LASI_SYTRI 
      INTERFACE lasi_SYTRI

      SUBROUTINE SSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYTRI

      SUBROUTINE DSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYTRI

      SUBROUTINE CSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CSYTRI

      SUBROUTINE ZSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZSYTRI

      END INTERFACE
#endif

! 
! lasi_HETRI
! 
#ifdef __LASI_HETRI 
      INTERFACE lasi_HETRI

      SUBROUTINE CHETRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHETRI

      SUBROUTINE ZHETRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHETRI

      END INTERFACE
#endif

! 
! lasi_SYRFS
! 
#ifdef __LASI_SYRFS 
      INTERFACE lasi_SYRFS

      SUBROUTINE SSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
     &                   X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*) 
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYRFS

      SUBROUTINE DSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
     &                   X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*) 
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYRFS

      SUBROUTINE CSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
     &                   X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CSYRFS

      SUBROUTINE ZSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
     &                   X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZSYRFS

      MODULE PROCEDURE SSYRFS1
      MODULE PROCEDURE DSYRFS1
      MODULE PROCEDURE CSYRFS1
      MODULE PROCEDURE ZSYRFS1

      END INTERFACE
#endif

! 
! lasi_HERFS
! 
#ifdef __LASI_HERFS 
      INTERFACE lasi_HERFS

      SUBROUTINE CHERFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
     &                   X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHERFS

      SUBROUTINE ZHERFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
     &                   X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHERFS

      MODULE PROCEDURE CHERFS1
      MODULE PROCEDURE ZHERFS1

      END INTERFACE
#endif

! 
! lasi_SYCON
! 
#ifdef __LASI_SYCON 
      INTERFACE lasi_SYCON

      SUBROUTINE SSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,     &
     &                   IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYCON

      SUBROUTINE DSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,     &
     &                   IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYCON

      SUBROUTINE CSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,     &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CSYCON

      SUBROUTINE ZSYCON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,     &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZSYCON

      END INTERFACE
#endif

! 
! lasi_HECON
! 
#ifdef __LASI_HECON 
      INTERFACE lasi_HECON

      SUBROUTINE CHECON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,     &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHECON

      SUBROUTINE ZHECON( UPLO, N, A, LDA, IPIV, ANORM, RCOND, WORK,     &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHECON

      END INTERFACE
#endif

! 
! lasi_HETRS
! 
#ifdef __LASI_HETRS 
      INTERFACE lasi_HETRS

      SUBROUTINE CHETRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE CHETRS

      SUBROUTINE ZHETRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE ZHETRS

      MODULE PROCEDURE CHETRS1
      MODULE PROCEDURE ZHETRS1

      END INTERFACE
#endif

! 
! lasi_SYTRS
! 
#ifdef __LASI_SYTRS 
      INTERFACE lasi_SYTRS

      SUBROUTINE SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SSYTRS

      SUBROUTINE DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DSYTRS

      SUBROUTINE CSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE CSYTRS

      SUBROUTINE ZSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE ZSYTRS

      MODULE PROCEDURE SSYTRS1
      MODULE PROCEDURE DSYTRS1
      MODULE PROCEDURE CSYTRS1
      MODULE PROCEDURE ZSYTRS1

      END INTERFACE
#endif

! 
! lasi_HETRF
! 
#ifdef __LASI_HETRF 
      INTERFACE lasi_HETRF

      SUBROUTINE CHETRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK( LWORK )
      END SUBROUTINE CHETRF

      SUBROUTINE ZHETRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK( LWORK )
      END SUBROUTINE ZHETRF

      END INTERFACE
#endif

! 
! lasi_SYTRF
! 
#ifdef __LASI_SYTRF 
      INTERFACE lasi_SYTRF

      SUBROUTINE SSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK( LWORK )
      END SUBROUTINE SSYTRF

      SUBROUTINE DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
         REAL(WP), INTENT(OUT) :: WORK( LWORK )
      END SUBROUTINE DSYTRF

      SUBROUTINE CSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK( LWORK )
      END SUBROUTINE CSYTRF

      SUBROUTINE ZSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK( LWORK )
      END SUBROUTINE ZSYTRF

      END INTERFACE
#endif

! 
! lasi_PTRFS
! 
#ifdef __LASI_PTRFS 
      INTERFACE lasi_PTRFS

      SUBROUTINE SPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR,   &
     &                   BERR, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPTRFS

      SUBROUTINE DPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR,   &
     &                   BERR, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPTRFS

      SUBROUTINE CPTRFS( UPLO, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,   &
     &                   FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CPTRFS

      SUBROUTINE ZPTRFS( UPLO, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,   &
     &                   FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZPTRFS

      MODULE PROCEDURE SPTRFS1
      MODULE PROCEDURE DPTRFS1
      MODULE PROCEDURE CPTRFS1
      MODULE PROCEDURE ZPTRFS1

      END INTERFACE
#endif

! 
! lasi_PTCON
! 
#ifdef __LASI_PTCON 
      INTERFACE lasi_PTCON

      SUBROUTINE SPTCON( N, D, E, ANORM, RCOND, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM, D(*)
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         REAL(WP), INTENT(IN) :: E(*)
      END SUBROUTINE SPTCON

      SUBROUTINE DPTCON( N, D, E, ANORM, RCOND, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM, D(*)
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         REAL(WP), INTENT(IN) :: E(*)
      END SUBROUTINE DPTCON

      SUBROUTINE CPTCON( N, D, E, ANORM, RCOND, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM, D(*)
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: E(*)
      END SUBROUTINE CPTCON

      SUBROUTINE ZPTCON( N, D, E, ANORM, RCOND, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM, D(*)
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: E(*)
      END SUBROUTINE ZPTCON

      END INTERFACE
#endif

! 
! lasi_PTTRS
! 
#ifdef __LASI_PTTRS 
      INTERFACE lasi_PTTRS

      SUBROUTINE SPTTRS( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SPTTRS

      SUBROUTINE DPTTRS( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DPTTRS

      SUBROUTINE CPTTRS( UPLO, N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(IN) :: E(*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE CPTTRS

      SUBROUTINE ZPTTRS( UPLO, N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(IN) :: E(*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE ZPTTRS

      MODULE PROCEDURE SPTTRS1
      MODULE PROCEDURE DPTTRS1
      MODULE PROCEDURE CPTTRS1
      MODULE PROCEDURE ZPTTRS1

      END INTERFACE
#endif

! 
! lasi_PTTRF
! 
#ifdef __LASI_PTTRF 
      INTERFACE lasi_PTTRF

      SUBROUTINE SPTTRF( N, D, E, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D( * )
         REAL(WP), INTENT(INOUT) :: E( * )
      END SUBROUTINE SPTTRF

      SUBROUTINE DPTTRF( N, D, E, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D( * )
         REAL(WP), INTENT(INOUT) :: E( * )
      END SUBROUTINE DPTTRF

      SUBROUTINE CPTTRF( N, D, E, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D( * )
         COMPLEX(WP), INTENT(INOUT) :: E( * )
      END SUBROUTINE CPTTRF

      SUBROUTINE ZPTTRF( N, D, E, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D( * )
         COMPLEX(WP), INTENT(INOUT) :: E( * )
      END SUBROUTINE ZPTTRF

      END INTERFACE
#endif

! 
! lasi_PBEQU
! 
#ifdef __LASI_PBEQU 
      INTERFACE lasi_PBEQU

      SUBROUTINE SPBEQU( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE SPBEQU

      SUBROUTINE DPBEQU( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE DPBEQU

      SUBROUTINE CPBEQU( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE CPBEQU

      SUBROUTINE ZPBEQU( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE ZPBEQU

      END INTERFACE
#endif

! 
! lasi_PBRFS
! 
#ifdef __LASI_PBRFS 
      INTERFACE lasi_PBRFS

      SUBROUTINE SPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,    &
     &                   LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPBRFS

      SUBROUTINE DPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,    &
     &                   LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPBRFS

      SUBROUTINE CPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,    &
     &                   LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*),        &
     &                               B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CPBRFS

      SUBROUTINE ZPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,    &
     &                   LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*),        &
     &                               B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZPBRFS

      MODULE PROCEDURE SPBRFS1
      MODULE PROCEDURE DPBRFS1
      MODULE PROCEDURE CPBRFS1
      MODULE PROCEDURE ZPBRFS1

      END INTERFACE
#endif

! 
! lasi_PBCON
! 
#ifdef __LASI_PBCON 
      INTERFACE lasi_PBCON

      SUBROUTINE SPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,     &
     &                   IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPBCON

      SUBROUTINE DPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,     &
     &                   IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPBCON

      SUBROUTINE CPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,     &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CPBCON

      SUBROUTINE ZPBCON( UPLO, N, KD, AB, LDAB, ANORM, RCOND, WORK,     &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZPBCON

      END INTERFACE
#endif

! 
! lasi_PBTRS
! 
#ifdef __LASI_PBTRS 
      INTERFACE lasi_PBTRS

      SUBROUTINE SPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SPBTRS

      SUBROUTINE DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DPBTRS

      SUBROUTINE CPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE CPBTRS

      SUBROUTINE ZPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE ZPBTRS

      MODULE PROCEDURE SPBTRS1
      MODULE PROCEDURE DPBTRS1
      MODULE PROCEDURE CPBTRS1
      MODULE PROCEDURE ZPBTRS1

      END INTERFACE
#endif

! 
! lasi_PBTRF
! 
#ifdef __LASI_PBTRF 
      INTERFACE lasi_PBTRF

      SUBROUTINE SPBTRF( UPLO, N, KD, AB, LDAB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB( LDAB,*)
      END SUBROUTINE SPBTRF

      SUBROUTINE DPBTRF( UPLO, N, KD, AB, LDAB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB( LDAB,*)
      END SUBROUTINE DPBTRF

      SUBROUTINE CPBTRF( UPLO, N, KD, AB, LDAB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB( LDAB,*)
      END SUBROUTINE CPBTRF

      SUBROUTINE ZPBTRF( UPLO, N, KD, AB, LDAB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB( LDAB,*)
      END SUBROUTINE ZPBTRF

      END INTERFACE
#endif

! 
! lasi_PPEQU
! 
#ifdef __LASI_PPEQU 
      INTERFACE lasi_PPEQU

      SUBROUTINE SPPEQU( UPLO, N, AP, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: AP(*)
      END SUBROUTINE SPPEQU

      SUBROUTINE DPPEQU( UPLO, N, AP, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: AP(*)
      END SUBROUTINE DPPEQU

      SUBROUTINE CPPEQU( UPLO, N, AP, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
      END SUBROUTINE CPPEQU

      SUBROUTINE ZPPEQU( UPLO, N, AP, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
      END SUBROUTINE ZPPEQU

      END INTERFACE
#endif

! 
! lasi_PPTRI
! 
#ifdef __LASI_PPTRI 
      INTERFACE lasi_PPTRI

      SUBROUTINE SPPTRI( UPLO, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE SPPTRI

      SUBROUTINE DPPTRI( UPLO, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE DPPTRI

      SUBROUTINE CPPTRI( UPLO, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE CPPTRI

      SUBROUTINE ZPPTRI( UPLO, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE ZPPTRI

      END INTERFACE
#endif

! 
! lasi_PPRFS
! 
#ifdef __LASI_PPRFS 
      INTERFACE lasi_PPRFS

      SUBROUTINE SPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR,  &
     &                   BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPPRFS

      SUBROUTINE DPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR,  &
     &                   BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPPRFS

      SUBROUTINE CPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR,  &
     &                   BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CPPRFS

      SUBROUTINE ZPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR,  &
     &                   BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZPPRFS

      MODULE PROCEDURE SPPRFS1
      MODULE PROCEDURE DPPRFS1
      MODULE PROCEDURE CPPRFS1
      MODULE PROCEDURE ZPPRFS1

      END INTERFACE
#endif

! 
! lasi_PPCON
! 
#ifdef __LASI_PPCON 
      INTERFACE lasi_PPCON

      SUBROUTINE SPPCON( UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPPCON

      SUBROUTINE DPPCON( UPLO, N, AP, ANORM, RCOND, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPPCON

      SUBROUTINE CPPCON( UPLO, N, AP, ANORM, RCOND, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CPPCON

      SUBROUTINE ZPPCON( UPLO, N, AP, ANORM, RCOND, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZPPCON

      END INTERFACE
#endif

! 
! lasi_PPTRS
! 
#ifdef __LASI_PPTRS 
      INTERFACE lasi_PPTRS

      SUBROUTINE SPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SPPTRS

      SUBROUTINE DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DPPTRS

      SUBROUTINE CPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE CPPTRS

      SUBROUTINE ZPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE ZPPTRS

      MODULE PROCEDURE SPPTRS1
      MODULE PROCEDURE DPPTRS1
      MODULE PROCEDURE CPPTRS1
      MODULE PROCEDURE ZPPTRS1

      END INTERFACE
#endif

! 
! lasi_PPTRF
! 
#ifdef __LASI_PPTRF 
      INTERFACE lasi_PPTRF

      SUBROUTINE SPPTRF( UPLO, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE SPPTRF

      SUBROUTINE DPPTRF( UPLO, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE DPPTRF

      SUBROUTINE CPPTRF( UPLO, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE CPPTRF

      SUBROUTINE ZPPTRF( UPLO, N, AP, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
      END SUBROUTINE ZPPTRF

      END INTERFACE
#endif

! 
! lasi_POEQU
! 
#ifdef __LASI_POEQU 
      INTERFACE lasi_POEQU

      SUBROUTINE SPOEQU( N, A, LDA, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
      END SUBROUTINE SPOEQU

      SUBROUTINE DPOEQU( N, A, LDA, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
      END SUBROUTINE DPOEQU

      SUBROUTINE CPOEQU( N, A, LDA, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
      END SUBROUTINE CPOEQU

      SUBROUTINE ZPOEQU( N, A, LDA, S, SCOND, AMAX, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, SCOND, S(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
      END SUBROUTINE ZPOEQU

      END INTERFACE
#endif

! 
! lasi_POTRI
! 
#ifdef __LASI_POTRI 
      INTERFACE lasi_POTRI

      SUBROUTINE SPOTRI( UPLO, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
      END SUBROUTINE SPOTRI

      SUBROUTINE DPOTRI( UPLO, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A( LDA,*)
      END SUBROUTINE DPOTRI

      SUBROUTINE CPOTRI( UPLO, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
      END SUBROUTINE CPOTRI

      SUBROUTINE ZPOTRI( UPLO, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A( LDA,*)
      END SUBROUTINE ZPOTRI

      END INTERFACE
#endif

! 
! lasi_PORFS
! 
#ifdef __LASI_PORFS 
      INTERFACE lasi_PORFS

      SUBROUTINE SPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
     &                   LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SPORFS

      SUBROUTINE DPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
     &                   LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DPORFS

      SUBROUTINE CPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
     &                   LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CPORFS

      SUBROUTINE ZPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,    &
     &                   LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZPORFS

      MODULE PROCEDURE SPORFS1
      MODULE PROCEDURE DPORFS1
      MODULE PROCEDURE CPORFS1
      MODULE PROCEDURE ZPORFS1

      END INTERFACE
#endif

! 
! lasi_POTRS
! 
#ifdef __LASI_POTRS 
      INTERFACE lasi_POTRS

      SUBROUTINE SPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SPOTRS

      SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DPOTRS

      SUBROUTINE CPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE CPOTRS

      SUBROUTINE ZPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE ZPOTRS

      MODULE PROCEDURE SPOTRS1
      MODULE PROCEDURE DPOTRS1
      MODULE PROCEDURE CPOTRS1
      MODULE PROCEDURE ZPOTRS1

      END INTERFACE
#endif

! 
! lasi_GTRFS
! 
#ifdef __LASI_GTRFS 
      INTERFACE lasi_GTRFS

      SUBROUTINE SGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*), DLF(*), &
     &                           DU(*), DU2(*), DUF(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGTRFS

      SUBROUTINE DGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*), DLF(*), &
     &                           DU(*), DU2(*), DUF(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGTRFS

      SUBROUTINE CGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*),      &
     &                              DLF(*), DU(*), DU2(*), DUF(*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CGTRFS

      SUBROUTINE ZGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*),      &
     &                              DLF(*), DU(*), DU2(*), DUF(*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZGTRFS

      MODULE PROCEDURE SGTRFS1
      MODULE PROCEDURE DGTRFS1
      MODULE PROCEDURE CGTRFS1
      MODULE PROCEDURE ZGTRFS1

      END INTERFACE
#endif

! 
! lasi_GTCON
! 
#ifdef __LASI_GTCON 
      INTERFACE lasi_GTCON

      SUBROUTINE SGTCON( NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
     &                   WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGTCON

      SUBROUTINE DGTCON( NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
     &                   WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGTCON

      SUBROUTINE CGTCON( NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
     &                   WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CGTCON

      SUBROUTINE ZGTCON( NORM, N, DL, D, DU, DU2, IPIV, ANORM, RCOND,   &
     &                   WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZGTCON

      END INTERFACE
#endif

! 
! lasi_GTTRS
! 
#ifdef __LASI_GTTRS 
      INTERFACE lasi_GTTRS

      SUBROUTINE SGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE SGTTRS

      SUBROUTINE DGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE DGTTRS

      SUBROUTINE CGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE CGTTRS

      SUBROUTINE ZGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
      END SUBROUTINE ZGTTRS

      MODULE PROCEDURE SGTTRS1
      MODULE PROCEDURE DGTTRS1
      MODULE PROCEDURE CGTTRS1
      MODULE PROCEDURE ZGTTRS1

      END INTERFACE
#endif

! 
! lasi_GTTRF
! 
#ifdef __LASI_GTTRF 
      INTERFACE lasi_GTTRF

      SUBROUTINE SGTTRF( N, DL, D, DU, DU2, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
         REAL(WP), INTENT(OUT) :: DU2(*)
      END SUBROUTINE SGTTRF

      SUBROUTINE DGTTRF( N, DL, D, DU, DU2, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
         REAL(WP), INTENT(OUT) :: DU2(*)
      END SUBROUTINE DGTTRF

      SUBROUTINE CGTTRF( N, DL, D, DU, DU2, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
         COMPLEX(WP), INTENT(OUT) :: DU2(*)
      END SUBROUTINE CGTTRF

      SUBROUTINE ZGTTRF( N, DL, D, DU, DU2, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: D(*), DL(*), DU(*)
         COMPLEX(WP), INTENT(OUT) :: DU2(*)
      END SUBROUTINE ZGTTRF

      END INTERFACE
#endif

! 
! lasi_GBEQU
! 
#ifdef __LASI_GBEQU 
      INTERFACE lasi_GBEQU

      SUBROUTINE SGBEQU( M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,  &
     &                   AMAX, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(OUT) :: C(*), R(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE SGBEQU

      SUBROUTINE DGBEQU( M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,  &
     &                   AMAX, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(OUT) :: C(*), R(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE DGBEQU

      SUBROUTINE CGBEQU( M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,  &
     &                   AMAX, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(OUT) :: C(*), R(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE CGBEQU

      SUBROUTINE ZGBEQU( M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,  &
     &                   AMAX, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(OUT) :: C(*), R(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
      END SUBROUTINE ZGBEQU

      END INTERFACE
#endif

! 
! lasi_GBRFS
! 
#ifdef __LASI_GBRFS 
      INTERFACE lasi_GBRFS

      SUBROUTINE SGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B( LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
      END SUBROUTINE SGBRFS

      SUBROUTINE DGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B( LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: X( LDX,*)
      END SUBROUTINE DGBRFS

      SUBROUTINE CGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*),         &
     &                              B( LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
      END SUBROUTINE CGBRFS

      SUBROUTINE ZGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,  &
     &                   IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*),         &
     &                              B( LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
      END SUBROUTINE ZGBRFS

      MODULE PROCEDURE SGBRFS1
      MODULE PROCEDURE DGBRFS1
      MODULE PROCEDURE CGBRFS1
      MODULE PROCEDURE ZGBRFS1
      END INTERFACE
#endif

! 
! lasi_GBCON
! 
#ifdef __LASI_GBCON 
      INTERFACE lasi_GBCON

      SUBROUTINE SGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, &
     &                   WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: KL, KU, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE SGBCON

      SUBROUTINE DGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, &
     &                   WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: KL, KU, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: AB( LDAB, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE DGBCON

      SUBROUTINE CGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, &
     &                   WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: KL, KU, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         REAL(WP), INTENT(OUT) :: RWORK( * )
         COMPLEX(WP), INTENT(IN) :: AB( LDAB, * )
         COMPLEX(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE CGBCON

      SUBROUTINE ZGBCON( NORM, N, KL, KU, AB, LDAB, IPIV, ANORM, RCOND, &
     &                   WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: KL, KU, LDAB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(IN) :: IPIV( * )
         REAL(WP), INTENT(OUT) :: RWORK( * )
         COMPLEX(WP), INTENT(IN) :: AB( LDAB, * )
         COMPLEX(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE ZGBCON

      END INTERFACE
#endif

! 
! lasi_GBTRS
! 
#ifdef __LASI_GBTRS 
      INTERFACE lasi_GBTRS

      SUBROUTINE SGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE SGBTRS

      SUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DGBTRS

      SUBROUTINE CGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE CGBTRS

      SUBROUTINE ZGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE ZGBTRS

      MODULE PROCEDURE SGBTRS1
      MODULE PROCEDURE DGBTRS1
      MODULE PROCEDURE CGBTRS1
      MODULE PROCEDURE ZGBTRS1
      END INTERFACE
#endif

! 
! lasi_GBTRF
! 
#ifdef __LASI_GBTRF 
      INTERFACE lasi_GBTRF

      SUBROUTINE SGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
      END SUBROUTINE SGBTRF

      SUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
      END SUBROUTINE DGBTRF

      SUBROUTINE CGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
      END SUBROUTINE CGBTRF

      SUBROUTINE ZGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
      END SUBROUTINE ZGBTRF

      END INTERFACE
#endif

!! 
!! lasi_LAMCH
!! 
!#ifdef __LASI_LAMCH
!      INTERFACE lasi_LAMCH
!
!      FUNCTION SLAMCH( CMACH )
!         USE lasi_kinds, ONLY: WP => sgl
!         IMPLICIT NONE
!         REAL(WP) :: SLAMCH
!         CHARACTER(LEN=1), INTENT(IN) :: CMACH
!      END FUNCTION SLAMCH
!
!      FUNCTION DLAMCH( CMACH )
!         USE lasi_kinds, ONLY: WP => dbl
!         IMPLICIT NONE
!         REAL(WP) :: DLAMCH
!         CHARACTER(LEN=1), INTENT(IN) :: CMACH
!      END FUNCTION DLAMCH
!
!      END INTERFACE
!#endif

! 
! lasi_GGSVD
! 
#ifdef __LASI_GGSVD 
      INTERFACE lasi_GGSVD

       SUBROUTINE SGGSVD( JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,   &
     &                    LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ,     &
     &                    WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBV, JOBQ
         INTEGER, INTENT(IN) :: M, N, P, LDA, LDB, LDU, LDV, LDQ
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: U(LDU,*), V(LDV,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE SGGSVD

       SUBROUTINE DGGSVD( JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,   &
     &                    LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ,     &
     &                    WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBV, JOBQ
         INTEGER, INTENT(IN) :: M, N, P, LDA, LDB, LDU, LDV, LDQ
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: U(LDU,*), V(LDV,*), Q(LDQ,*), WORK(*)
      END SUBROUTINE DGGSVD

       SUBROUTINE CGGSVD( JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,   &
     &                    LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ,     &
     &                    WORK, RWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBV, JOBQ
         INTEGER, INTENT(IN) :: M, N, P, LDA, LDB, LDU, LDV, LDQ
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: U(LDU,*), V(LDV,*), Q(LDQ,*),      &
     &                               WORK(*)
      END SUBROUTINE CGGSVD

       SUBROUTINE ZGGSVD( JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,   &
     &                    LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ,     &
     &                    WORK, RWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBV, JOBQ
         INTEGER, INTENT(IN) :: M, N, P, LDA, LDB, LDU, LDV, LDQ
         INTEGER, INTENT(OUT) :: INFO, K, L, IWORK(*)
         REAL(WP), INTENT(OUT) :: ALPHA(*), BETA(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: U(LDU,*), V(LDV,*), Q(LDQ,*),      &
     &                               WORK(*)
      END SUBROUTINE ZGGSVD

       END INTERFACE
#endif

! 
! lasi_GEGV
! 
#ifdef __LASI_GEGV 
      INTERFACE lasi_GEGV

       SUBROUTINE SGEGV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR,       &
     &                   ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*)
      END SUBROUTINE SGEGV

       SUBROUTINE DGEGV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR,       &
     &                   ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*)
      END SUBROUTINE DGEGV

       SUBROUTINE CGEGV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA,  &
     &                   VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VL(LDVL,*),     &
     &                               VR(LDVR,*), WORK(*)
      END SUBROUTINE CGEGV

       SUBROUTINE ZGEGV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA,  &
     &                   VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VL(LDVL,*),     &
     &                               VR(LDVR,*), WORK(*)
      END SUBROUTINE ZGEGV

       END INTERFACE
#endif

! 
! lasi_GEGS
! 
#ifdef __LASI_GEGS 
      INTERFACE lasi_GEGS

       SUBROUTINE SGEGS( JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHAR,     &
     &                   ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR, WORK,    &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
      END SUBROUTINE SGEGS

       SUBROUTINE DGEGS( JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHAR,     &
     &                   ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR, WORK,    &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
      END SUBROUTINE DGEGS

       SUBROUTINE CGEGS( JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHA, BETA,&
     &                   VSL, LDVSL, VSR, LDVSR, WORK, LWORK, RWORK,    &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VSL(LDVSL,*),   &
     &                               VSR(LDVSR,*), WORK(*)
      END SUBROUTINE CGEGS

       SUBROUTINE ZGEGS( JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHA, BETA,&
     &                   VSL, LDVSL, VSR, LDVSR, WORK, LWORK, RWORK,    &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VSL(LDVSL,*),   &
     &                               VSR(LDVSR,*), WORK(*)
      END SUBROUTINE ZGEGS

       END INTERFACE
#endif

! 
! lasi_SBGVX
! 
#ifdef __LASI_SBGVX 
        INTERFACE lasi_SBGVX


       SUBROUTINE SSBGVX( JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
     &                    LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
     &                    LDZ, WORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         REAL(WP), INTENT(OUT) :: WORK(*), Q(LDQ,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE SSBGVX


       SUBROUTINE DSBGVX( JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
     &                    LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
     &                    LDZ, WORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         REAL(WP), INTENT(OUT) :: WORK(*), Q(LDQ,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE DSBGVX

        END INTERFACE
#endif
    
! 
! lasi_HBGVX
! 
#ifdef __LASI_HBGVX 
       INTERFACE lasi_HBGVX
    
       SUBROUTINE CHBGVX( JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
     &                    LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
     &                    LDZ, WORK, RWORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Q(LDQ,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: RWORK (*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE CHBGVX

       SUBROUTINE ZHBGVX( JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB, &
     &                    LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,&
     &                    LDZ, WORK, RWORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, LDQ, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Q(LDQ,*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: RWORK (*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE ZHBGVX

        END INTERFACE
#endif

! 
! lasi_SBGVD
! 
#ifdef __LASI_SBGVD 
        INTERFACE lasi_SBGVD


        SUBROUTINE SSBGVD( JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB, &
     &                     W, Z, LDZ, WORK, LWORK, IWORK, LIWORK,       &
     &                     INFO )
           USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
           CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO 
           INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
           INTEGER, INTENT(IN) :: LWORK, LIWORK
           INTEGER, INTENT(OUT) :: INFO, IWORK(*)
           REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
           REAL(WP), INTENT(OUT) :: W(*)
           REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
        END SUBROUTINE SSBGVD
 

        SUBROUTINE DSBGVD( JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB, &
     &                     W, Z, LDZ, WORK, LWORK, IWORK, LIWORK,       &
     &                     INFO )
           USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
           CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO 
           INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
           INTEGER, INTENT(IN) :: LWORK, LIWORK
           INTEGER, INTENT(OUT) :: INFO, IWORK(*)
           REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
           REAL(WP), INTENT(OUT) :: W(*)
           REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
        END SUBROUTINE DSBGVD
 
         END INTERFACE
#endif

! 
! lasi_HBGVD
! 
#ifdef __LASI_HBGVD 
         INTERFACE lasi_HBGVD
 
       SUBROUTINE CHBGVD( JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB,  &
     &                    W, Z, LDZ, WORK, LWORK, RWORK, LRWORK, IWORK, &
     &                    LIWORK, INFO )
           USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
           CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
           INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
           INTEGER, INTENT(IN) :: LWORK, LRWORK, LIWORK
           INTEGER, INTENT(OUT) :: INFO, IWORK(*)
           COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
           REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
           COMPLEX(WP),INTENT(OUT) :: Z(LDZ,*), WORK(*)
          END SUBROUTINE CHBGVD
  
       SUBROUTINE ZHBGVD( JOBZ, UPLO, N, KAB, KBB, AB, LDAB, BB, LDBB,  &
     &                    W, Z, LDZ, WORK, LWORK, RWORK, LRWORK, IWORK, &
     &                    LIWORK, INFO )
           USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
           CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
           INTEGER, INTENT(IN) :: N, KAB, KBB, LDAB, LDBB, LDZ
           INTEGER, INTENT(IN) :: LWORK, LRWORK, LIWORK
           INTEGER, INTENT(OUT) :: INFO, IWORK(*)
           COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
           REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
           COMPLEX(WP),INTENT(OUT) :: Z(LDZ,*), WORK(*)
          END SUBROUTINE ZHBGVD
  
        
         END INTERFACE
#endif

! 
! lasi_SBGV
! 
#ifdef __LASI_SBGV 
      INTERFACE lasi_SBGV

       SUBROUTINE SSBGV( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,  &
     &                   Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSBGV

       SUBROUTINE DSBGV( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,  &
     &                   Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSBGV

       END INTERFACE
#endif

! 
! lasi_HBGV
! 
#ifdef __LASI_HBGV 
      INTERFACE lasi_HBGV

       SUBROUTINE CHBGV( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,  &
     &                   Z, LDZ, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE CHBGV

       SUBROUTINE ZHBGV( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,  &
     &                   Z, LDZ, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: KA, KB, LDAB, LDBB, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), BB(LDBB,*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE ZHBGV

       END INTERFACE
#endif

! 
! lasi_SPGVX
! 
#ifdef __LASI_SPGVX 
       INTERFACE lasi_SPGVX


       SUBROUTINE SSPGVX( ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,  &
     &                    IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,    &
     &                    IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE SSPGVX


       SUBROUTINE DSPGVX( ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,  &
     &                    IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,    &
     &                    IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE DSPGVX

        END INTERFACE
#endif

! 
! lasi_HPGVX
! 
#ifdef __LASI_HPGVX 
        INTERFACE lasi_HPGVX

       SUBROUTINE CHPGVX( ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,  &
     &                    IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK,    &
     &                    IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE CHPGVX


       SUBROUTINE ZHPGVX( ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,  &
     &                    IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK,    &
     &                    IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
        END SUBROUTINE ZHPGVX


        END INTERFACE
#endif

! 
! lasi_SPGVD
! 
#ifdef __LASI_SPGVD 
       INTERFACE lasi_SPGVD
       

        SUBROUTINE SSPGVD( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ,     &
     &                     WORK, LWORK, IWORK, LIWORK, INFO )
        USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
        CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
        INTEGER, INTENT(IN) :: ITYPE, N, LDZ
        INTEGER, INTENT(IN) :: LWORK, LIWORK
        INTEGER, INTENT(OUT) :: INFO, IWORK(*)
        REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
        REAL(WP), INTENT(OUT) :: W(*)
        REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
       END SUBROUTINE SSPGVD


        SUBROUTINE DSPGVD( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ,     &
     &                     WORK, LWORK, IWORK, LIWORK, INFO )
        USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
        CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
        INTEGER, INTENT(IN) :: ITYPE, N, LDZ
        INTEGER, INTENT(IN) :: LWORK, LIWORK
        INTEGER, INTENT(OUT) :: INFO, IWORK(*)
        REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
        REAL(WP), INTENT(OUT) :: W(*)
        REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
       END SUBROUTINE DSPGVD

        END INTERFACE
#endif

! 
! lasi_HPGVD 
! 
#ifdef __LASI_HPGVD  
        INTERFACE lasi_HPGVD 

       SUBROUTINE CHPGVD( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,&
     &                    LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
        USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
        CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
        INTEGER, INTENT(IN) :: ITYPE, N, LDZ
        INTEGER, INTENT(IN) :: LWORK, LRWORK, LIWORK
        INTEGER, INTENT(OUT) :: INFO, IWORK(*)
        COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
        REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
        COMPLEX(WP), INTENT(OUT):: Z(LDZ,*), WORK(*)
       END SUBROUTINE CHPGVD

       SUBROUTINE ZHPGVD( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,&
     &                    LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
        USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
        CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
        INTEGER, INTENT(IN) :: ITYPE, N, LDZ
        INTEGER, INTENT(IN) :: LWORK, LRWORK, LIWORK
        INTEGER, INTENT(OUT) :: INFO, IWORK(*)
        COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
        REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
        COMPLEX(WP), INTENT(OUT):: Z(LDZ,*), WORK(*)
       END SUBROUTINE ZHPGVD

        END INTERFACE
#endif

! 
! lasi_SPGV
! 
#ifdef __LASI_SPGV 
      INTERFACE lasi_SPGV

       SUBROUTINE SSPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSPGV

       SUBROUTINE DSPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: AP(*), BP(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSPGV

       END INTERFACE
#endif

! 
! lasi_HPGV
! 
#ifdef __LASI_HPGV 
      INTERFACE lasi_HPGV

       SUBROUTINE CHPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE CHPGV

       SUBROUTINE ZHPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), BP(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE ZHPGV

       END INTERFACE
#endif

! 
! lasi_GESVD
! 
#ifdef __LASI_GESVD 
      INTERFACE lasi_GESVD

       SUBROUTINE SGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT,     &
     &                    LDVT, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBVT
         INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE SGESVD

       SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT,     &
     &                    LDVT, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBVT
         INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE DGESVD

       SUBROUTINE CGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT,     &
     &                    LDVT, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBVT
         INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: S(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE CGESVD

       SUBROUTINE ZGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT,     &
     &                    LDVT, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBVT
         INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: S(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
      END SUBROUTINE ZGESVD

       END INTERFACE
#endif

! 
! lasi_GEEVX
! 
#ifdef __LASI_GEEVX 
      INTERFACE lasi_GEEVX

       SUBROUTINE SGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, WR,   &
     &                    WI, VL, LDVL, VR, LDVR, ILO, IHI, SCALE,      &
     &                    ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK,    &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO, ILO, IHI, IWORK(*)
         REAL(WP), INTENT(OUT) :: ABNRM
         REAL(WP), INTENT(OUT) :: SCALE(*), RCONDE(*), RCONDV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), WR(*), WI(*), &
     &                            WORK(*)
      END SUBROUTINE SGEEVX

       SUBROUTINE DGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, WR,   &
     &                    WI, VL, LDVL, VR, LDVR, ILO, IHI, SCALE,      &
     &                    ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK,    &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO, ILO, IHI, IWORK(*)
         REAL(WP), INTENT(OUT) :: ABNRM
         REAL(WP), INTENT(OUT) :: SCALE(*), RCONDE(*), RCONDV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), WR(*), WI(*), &
     &                            WORK(*)
      END SUBROUTINE DGEEVX

       SUBROUTINE CGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, W, VL,&
     &                    LDVL, VR, LDVR, ILO, IHI, SCALE, ABNRM,       &
     &                    RCONDE, RCONDV, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO, ILO, IHI
         REAL(WP), INTENT(OUT) :: ABNRM
         REAL(WP), INTENT(OUT) :: SCALE(*), RCONDE(*), RCONDV(*),       &
     &                            RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), W(*),      &
     &                               WORK(*)
      END SUBROUTINE CGEEVX

       SUBROUTINE ZGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, W, VL,&
     &                    LDVL, VR, LDVR, ILO, IHI, SCALE, ABNRM,       &
     &                    RCONDE, RCONDV, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO, ILO, IHI
         REAL(WP), INTENT(OUT) :: ABNRM
         REAL(WP), INTENT(OUT) :: SCALE(*), RCONDE(*), RCONDV(*),       &
     &                            RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), W(*),      &
     &                               WORK(*)
      END SUBROUTINE ZGEEVX

       END INTERFACE
#endif

! 
! lasi_GGEVX
! 
#ifdef __LASI_GGEVX 
        INTERFACE lasi_GGEVX

       SUBROUTINE SGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B,    &
     &                    LDB, ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR,&
     &                    ILO, IHI, LSCALE, RSCALE, ABNRM, BBNRM,       &
     &                    RCONDE, RCONDV, WORK, LWORK, IWORK, BWORK,    &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT):: ILO, IHI
         REAL(WP), INTENT(OUT) :: ABNRM, BBNRM
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*),      &
     &                            LSCALE(*), RSCALE(*), RCONDE(*),      &
     &                            RCONDV(*)
         INTEGER :: IWORK(*)
         LOGICAL :: BWORK(*) 
        END SUBROUTINE SGGEVX

       SUBROUTINE DGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B,    &
     &                    LDB, ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR,&
     &                    ILO, IHI, LSCALE, RSCALE, ABNRM, BBNRM,       &
     &                    RCONDE, RCONDV, WORK, LWORK, IWORK, BWORK,    &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT):: ILO, IHI
         REAL(WP), INTENT(OUT) :: ABNRM, BBNRM
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*),      &
     &                            LSCALE(*), RSCALE(*), RCONDE(*),      &
     &                            RCONDV(*)
         INTEGER :: IWORK(*)
         LOGICAL :: BWORK(*) 
        END SUBROUTINE DGGEVX

       SUBROUTINE CGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B,    &
     &                    LDB, ALPHA, BETA, VL, LDVL, VR, LDVR, ILO,    &
     &                    IHI, LSCALE, RSCALE, ABNRM, BBNRM, RCONDE,    &
     &                    RCONDV, WORK, LWORK, RWORK, IWORK, BWORK,     &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT):: ILO, IHI
         REAL(WP), INTENT(OUT) :: ABNRM, BBNRM
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VL(LDVL,*),     &
     &                               VR(LDVR,*), WORK(*)
         REAL(WP), INTENT(OUT) :: LSCALE(*), RSCALE(*), RCONDE(*), RCONDV(*)
         INTEGER :: IWORK(*)
         LOGICAL :: BWORK(*)
         REAL(WP) :: RWORK(*)
        END SUBROUTINE CGGEVX

       SUBROUTINE ZGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B,    &
     &                    LDB, ALPHA, BETA, VL, LDVL, VR, LDVR, ILO,    &
     &                    IHI, LSCALE, RSCALE, ABNRM, BBNRM, RCONDE,    &
     &                    RCONDV, WORK, LWORK, RWORK, IWORK, BWORK,     &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT):: ILO, IHI
         REAL(WP), INTENT(OUT) :: ABNRM, BBNRM
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VL(LDVL,*),     &
     &                               VR(LDVR,*), WORK(*)
         REAL(WP), INTENT(OUT) :: LSCALE(*), RSCALE(*), RCONDE(*), RCONDV(*)
         INTEGER :: IWORK(*)
         LOGICAL :: BWORK(*)
         REAL(WP) :: RWORK(*)
        END SUBROUTINE ZGGEVX

        END INTERFACE
#endif

! 
! lasi_GGEV
! 
#ifdef __LASI_GGEV 
        INTERFACE lasi_GGEV
      
       SUBROUTINE SGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR,       &
     &                   ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*)
       END SUBROUTINE SGGEV

       SUBROUTINE DGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR,       &
     &                   ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VL(LDVL,*), VR(LDVR,*), WORK(*)
       END SUBROUTINE DGGEV

       SUBROUTINE CGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA,  &
     &                   VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )
       USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
       CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
       INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
       INTEGER, INTENT(OUT) :: INFO
       COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) ::  ALPHA(*), BETA(*), VL(LDVL,*),    &
     &                                VR(LDVR,*), WORK(*)
       REAL(WP) :: RWORK(*)
      END SUBROUTINE CGGEV
      
       SUBROUTINE ZGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA,  &
     &                   VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )
       USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
       CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
       INTEGER, INTENT(IN) :: LDA, LDB, N, LDVL, LDVR, LWORK
       INTEGER, INTENT(OUT) :: INFO
       COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) ::  ALPHA(*), BETA(*), VL(LDVL,*),    &
     &                                VR(LDVR,*), WORK(*)
       REAL(WP) :: RWORK(*)
      END SUBROUTINE ZGGEV
      
       END INTERFACE
#endif

! 
! lasi_GEEV
! 
#ifdef __LASI_GEEV 
      INTERFACE lasi_GEEV

       SUBROUTINE SGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, &
     &                   LDVR, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), WR(*), WI(*), &
     &                            WORK(*)
      END SUBROUTINE SGEEV

       SUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, &
     &                   LDVR, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), WR(*), WI(*), &
     &                            WORK(*)
      END SUBROUTINE DGEEV

       SUBROUTINE CGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR,&
     &                   WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), W(*),      &
     &                               WORK(*)
      END SUBROUTINE CGEEV

       SUBROUTINE ZGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR,&
     &                   WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVL, JOBVR
         INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: VL(LDVL,*), VR(LDVR,*), W(*),      &
     &                               WORK(*)
      END SUBROUTINE ZGEEV

       END INTERFACE
#endif

! 
! lasi_GEESX
! 
#ifdef __LASI_GEESX 
      INTERFACE lasi_GEESX

       SUBROUTINE SGEESX( JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM,  &
     &                    WR, WI, VS, LDVS, RCONDE, RCONDV, WORK, LWORK,&
     &                    IWORK, LIWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTERFACE
            LOGICAL FUNCTION SELECT(WR, WI)
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               REAL(WP), INTENT(IN) :: WR, WI
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM, IWORK(*)
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: RCONDV, RCONDE
         REAL(WP), INTENT(OUT) :: VS(LDVS,*), WR(*), WI(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE SGEESX

       SUBROUTINE DGEESX( JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM,  &
     &                    WR, WI, VS, LDVS, RCONDE, RCONDV, WORK, LWORK,&
     &                    IWORK, LIWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTERFACE
            LOGICAL FUNCTION SELECT(WR, WI)
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               REAL(WP), INTENT(IN) :: WR, WI
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM, IWORK(*)
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: RCONDV, RCONDE
         REAL(WP), INTENT(OUT) :: VS(LDVS,*), WR(*), WI(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE DGEESX

       SUBROUTINE CGEESX( JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM,  &
     &                    W, VS, LDVS, RCONDE, RCONDV, WORK, LWORK,     &
     &                    RWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTERFACE
            LOGICAL FUNCTION SELECT( W )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               COMPLEX(WP), INTENT(IN) :: W
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM
         LOGICAL, INTENT(OUT) :: BWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: RCONDV, RCONDE
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: VS(LDVS,*), W(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE CGEESX

       SUBROUTINE ZGEESX( JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM,  &
     &                    W, VS, LDVS, RCONDE, RCONDV, WORK, LWORK,     &
     &                    RWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTERFACE
            LOGICAL FUNCTION SELECT( W )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               COMPLEX(WP), INTENT(IN) :: W
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT, SENSE
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM
         LOGICAL, INTENT(OUT) :: BWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: RCONDV, RCONDE
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: VS(LDVS,*), W(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE ZGEESX

       END INTERFACE
#endif
       
! 
! lasi_GGESX
! 
#ifdef __LASI_GGESX 
       INTERFACE lasi_GGESX

       SUBROUTINE SGGESX( JOBVSL, JOBVSR, SORT, SELCTG, SENSE, N, A,    &
     &                    LDA, B, LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, &
     &                    LDVSL, VSR, LDVSR, RCONDE, RCONDV, WORK,      &
     &                    LWORK, IWORK, LIWORK, BWORK, INFO )
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT, SENSE
          INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK, LIWORK
          INTEGER, INTENT(INOUT) :: INFO
          INTEGER, INTENT(OUT) :: SDIM
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
          REAL(WP), INTENT(OUT) :: RCONDE(2), RCONDV(2)
          LOGICAL :: BWORK(*)
          INTEGER :: IWORK (*)
          INTERFACE
           LOGICAL FUNCTION SELCTG(ALPHAR, ALPHAI, BETA)
              USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
              REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
           END FUNCTION SELCTG
          END INTERFACE
          OPTIONAL :: SELCTG
         END SUBROUTINE SGGESX
 
       SUBROUTINE DGGESX( JOBVSL, JOBVSR, SORT, DELCTG, SENSE, N, A,    &
     &                    LDA, B, LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, &
     &                    LDVSL, VSR, LDVSR, RCONDE, RCONDV, WORK,      &
     &                    LWORK, IWORK, LIWORK, BWORK, INFO )
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT, SENSE
          INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK, LIWORK
          INTEGER, INTENT(INOUT) :: INFO
          INTEGER, INTENT(OUT) :: SDIM
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
          REAL(WP), INTENT(OUT) :: RCONDE(2), RCONDV(2)
          LOGICAL :: BWORK(*)
          INTEGER :: IWORK (*)
          INTERFACE
           LOGICAL FUNCTION DELCTG(ALPHAR, ALPHAI, BETA)
              USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
              REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
           END FUNCTION DELCTG
          END INTERFACE
          OPTIONAL :: DELCTG
         END SUBROUTINE DGGESX
 
       SUBROUTINE CGGESX( JOBVSL, JOBVSR, SORT, SELCTG, SENSE, N, A,    &
     &                    LDA, B, LDB, SDIM, ALPHA, BETA, VSL, LDVSL,   &
     &                    VSR, LDVSR, RCONDE, RCONDV, WORK, LWORK,      &
     &                    RWORK, IWORK, LIWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT, SENSE
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK, LIWORK
         INTEGER, INTENT(INOUT) :: INFO
         INTEGER, INTENT(OUT) :: SDIM
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VSL(LDVSL,*),   &
     &                               VSR(LDVSR,*), WORK(*)
         REAL(WP), INTENT(OUT) :: RCONDE(2), RCONDV(2)
         LOGICAL :: BWORK(*)
         INTEGER :: IWORK (*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         INTERFACE
          LOGICAL FUNCTION SELCTG(ALPHA, BETA)
            USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
            COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
          END FUNCTION SELCTG
        END INTERFACE
        OPTIONAL :: SELCTG
      END SUBROUTINE CGGESX

       SUBROUTINE ZGGESX( JOBVSL, JOBVSR, SORT, DELCTG, SENSE, N, A,    &
     &                    LDA, B, LDB, SDIM, ALPHA, BETA, VSL, LDVSL,   &
     &                    VSR, LDVSR, RCONDE, RCONDV, WORK, LWORK,      &
     &                    RWORK, IWORK, LIWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT, SENSE
         INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK, LIWORK
         INTEGER, INTENT(INOUT) :: INFO
         INTEGER, INTENT(OUT) :: SDIM
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VSL(LDVSL,*),   &
     &                               VSR(LDVSR,*), WORK(*)
         REAL(WP), INTENT(OUT) :: RCONDE(2), RCONDV(2)
         LOGICAL :: BWORK(*)
         INTEGER :: IWORK (*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         INTERFACE
          LOGICAL FUNCTION DELCTG(ALPHA, BETA)
            USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
            COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
          END FUNCTION DELCTG
        END INTERFACE
        OPTIONAL :: DELCTG
      END SUBROUTINE ZGGESX

      END INTERFACE 
#endif

       
! 
! lasi_GGES
! 
#ifdef __LASI_GGES 
      INTERFACE lasi_GGES
      
       SUBROUTINE SGGES( JOBVSL, JOBVSR, SORT, SELCTG, N, A, LDA, B,    &
     &                   LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, LDVSL,   &
     &                   VSR, LDVSR, WORK, LWORK, BWORK, INFO )
       USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
       CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT
       INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
       INTEGER, INTENT(INOUT) :: INFO
       INTEGER, INTENT(OUT) :: SDIM
       REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
       LOGICAL :: BWORK(*)
       INTERFACE
         LOGICAL FUNCTION SELCTG(ALPHAR, ALPHAI, BETA)
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
         END FUNCTION SELCTG
       END INTERFACE
       OPTIONAL :: SELCTG
      END SUBROUTINE SGGES
      
       SUBROUTINE DGGES( JOBVSL, JOBVSR, SORT, DELCTG, N, A, LDA, B,    &
     &                   LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, LDVSL,   &
     &                   VSR, LDVSR, WORK, LWORK, BWORK, INFO )
       USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
       CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT
       INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
       INTEGER, INTENT(INOUT) :: INFO
       INTEGER, INTENT(OUT) :: SDIM
       REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: ALPHAR(*), ALPHAI(*), BETA(*),        &
     &                            VSL(LDVSL,*), VSR(LDVSR,*), WORK(*)
       LOGICAL :: BWORK(*)
       INTERFACE
         LOGICAL FUNCTION DELCTG(ALPHAR, ALPHAI, BETA)
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
         END FUNCTION DELCTG
       END INTERFACE
       OPTIONAL :: DELCTG
      END SUBROUTINE DGGES
      
       SUBROUTINE CGGES( JOBVSL, JOBVSR, SORT, SELCTG, N, A, LDA, B,    &
     &                   LDB, SDIM, ALPHA, BETA, VSL, LDVSL, VSR, LDVSR,&
     &                   WORK, LWORK, RWORK, BWORK, INFO )
       USE lasi_kinds, ONLY : WP => sgl
          IMPLICIT NONE
       CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT
       INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
       INTEGER, INTENT(INOUT) :: INFO
       INTEGER, INTENT(OUT) :: SDIM
       COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VSL(LDVSL,*),   &
     &                               VSR(LDVSR,*), WORK(*)
       LOGICAL :: BWORK(*)
       REAL(WP) :: RWORK(*)
       INTERFACE
         LOGICAL FUNCTION SELCTG( ALPHA, BETA)
           USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
           COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
         END FUNCTION SELCTG
       END INTERFACE
       OPTIONAL :: SELCTG
      END SUBROUTINE CGGES
      
       SUBROUTINE ZGGES( JOBVSL, JOBVSR, SORT, DELCTG, N, A, LDA, B,    &
     &                   LDB, SDIM, ALPHA, BETA, VSL, LDVSL, VSR, LDVSR,&
     &                   WORK, LWORK, RWORK, BWORK, INFO )
       USE lasi_kinds, ONLY : WP => dbl
          IMPLICIT NONE
       CHARACTER(LEN=1), INTENT(IN) :: JOBVSL, JOBVSR, SORT
       INTEGER, INTENT(IN) :: LDA, LDB, N, LDVSL, LDVSR, LWORK
       INTEGER, INTENT(INOUT) :: INFO
       INTEGER, INTENT(OUT) :: SDIM
       COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: ALPHA(*), BETA(*), VSL(LDVSL,*),   &
     &                               VSR(LDVSR,*), WORK(*)
       LOGICAL :: BWORK(*)
       REAL(WP) :: RWORK(*)
       INTERFACE
         LOGICAL FUNCTION DELCTG( ALPHA, BETA)
           USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
           COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
         END FUNCTION DELCTG
       END INTERFACE
       OPTIONAL :: DELCTG
      END SUBROUTINE ZGGES
      
        END INTERFACE
#endif

! 
! lasi_GEES
! 
#ifdef __LASI_GEES 
      INTERFACE lasi_GEES

       SUBROUTINE SGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, WR, WI,  &
     &                   VS, LDVS, WORK, LWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTERFACE
            LOGICAL FUNCTION SELECT(WR, WI)
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               REAL(WP), INTENT(IN) :: WR, WI
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VS(LDVS,*), WR(*), WI(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE SGEES

       SUBROUTINE DGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, WR, WI,  &
     &                   VS, LDVS, WORK, LWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTERFACE
            LOGICAL FUNCTION SELECT(WR, WI)
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               REAL(WP), INTENT(IN) :: WR, WI
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: VS(LDVS,*), WR(*), WI(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE DGEES

       SUBROUTINE CGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, W, VS,   &
     &                   LDVS, WORK, LWORK, RWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTERFACE
            LOGICAL FUNCTION SELECT( W )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               COMPLEX(WP), INTENT(IN) :: W
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: VS(LDVS,*), W(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE CGEES

       SUBROUTINE ZGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, W, VS,   &
     &                   LDVS, WORK, LWORK, RWORK, BWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTERFACE
            LOGICAL FUNCTION SELECT( W )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               COMPLEX(WP), INTENT(IN) :: W
            END FUNCTION SELECT
         END INTERFACE
         CHARACTER(LEN=1), INTENT(IN) :: JOBVS, SORT
         INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
         INTEGER, INTENT(OUT) :: INFO, SDIM
         LOGICAL, INTENT(OUT) :: BWORK(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: VS(LDVS,*), W(*), WORK(*)
         OPTIONAL :: SELECT
      END SUBROUTINE ZGEES

       END INTERFACE
#endif

! 
! lasi_STEVR
! 
#ifdef __LASI_STEVR 
        INTERFACE lasi_STEVR

       SUBROUTINE SSTEVR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
     &                    M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,     &
     &                    LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ,  LWORK, LIWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT), TARGET :: ISUPPZ(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*), W(*)
         REAL(WP), INTENT(OUT), TARGET :: Z(LDZ,*)
       END SUBROUTINE SSTEVR

       SUBROUTINE DSTEVR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
     &                    M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,     &
     &                    LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ,  LWORK, LIWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT), TARGET :: ISUPPZ(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: WORK(*), W(*)
         REAL(WP), INTENT(OUT), TARGET :: Z(LDZ,*)
       END SUBROUTINE DSTEVR

       END INTERFACE
#endif

! 
! lasi_STEVX
! 
#ifdef __LASI_STEVX 
      INTERFACE lasi_STEVX

       SUBROUTINE SSTEVX( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
     &                    M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSTEVX

       SUBROUTINE DSTEVX( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL, &
     &                    M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSTEVX

       END INTERFACE
#endif

! 
! lasi_STEVD
! 
#ifdef __LASI_STEVD 
      INTERFACE lasi_STEVD

       SUBROUTINE SSTEVD( JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
     &                    LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSTEVD

       SUBROUTINE DSTEVD( JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,    &
     &                    LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSTEVD

       END INTERFACE
#endif

! 
! lasi_STEV
! 
#ifdef __LASI_STEV 
      INTERFACE lasi_STEV

       SUBROUTINE SSTEV( JOBZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSTEV

       SUBROUTINE DSTEV( JOBZ, N, D, E, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSTEV

       END INTERFACE
#endif

! 
! lasi_SBEVX
! 
#ifdef __LASI_SBEVX 
      INTERFACE lasi_SBEVX

       SUBROUTINE SSBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ,   &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSBEVX

       SUBROUTINE DSBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ,   &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: Q(LDQ,*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSBEVX

       END INTERFACE
#endif

! 
! lasi_HBEVX
! 
#ifdef __LASI_HBEVX 
      INTERFACE lasi_HBEVX

       SUBROUTINE CHBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ,   &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    RWORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Q(LDQ,*), Z(LDZ,*), WORK(*)
      END SUBROUTINE CHBEVX

       SUBROUTINE ZHBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ,   &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    RWORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU, LDQ, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Q(LDQ,*), Z(LDZ,*), WORK(*)
      END SUBROUTINE ZHBEVX

       END INTERFACE
#endif

! 
! lasi_SBEVD
! 
#ifdef __LASI_SBEVD 
      INTERFACE lasi_SBEVD

       SUBROUTINE SSBEVD( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
     &                    LWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE SSBEVD

       SUBROUTINE DSBEVD( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
     &                    LWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE DSBEVD

       END INTERFACE
#endif

! 
! lasi_HBEVD
! 
#ifdef __LASI_HBEVD 
      INTERFACE lasi_HBEVD

       SUBROUTINE CHBEVD( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
     &                    LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK, LRWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE CHBEVD

       SUBROUTINE ZHBEVD( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, &
     &                    LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ, LWORK, LIWORK, LRWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE ZHBEVD

       END INTERFACE
#endif

! 
! lasi_SBEV
! 
#ifdef __LASI_SBEV 
      INTERFACE lasi_SBEV

       SUBROUTINE SSBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSBEV

       SUBROUTINE DSBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSBEV

       END INTERFACE
#endif

! 
! lasi_HBEV
! 
#ifdef __LASI_HBEV 
      INTERFACE lasi_HBEV

       SUBROUTINE CHBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,  &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE CHBEV

       SUBROUTINE ZHBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,  &
     &                   RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: N, KD, LDAB, LDZ
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE ZHBEV

       END INTERFACE
#endif

! 
! lasi_SPEVX
! 
#ifdef __LASI_SPEVX 
      INTERFACE lasi_SPEVX

       SUBROUTINE SSPEVX( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU,     &
     &                    ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL,     &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSPEVX

       SUBROUTINE DSPEVX( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU,     &
     &                    ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL,     &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSPEVX

       END INTERFACE
#endif

! 
! lasi_HPEVX
! 
#ifdef __LASI_HPEVX 
      INTERFACE lasi_HPEVX

       SUBROUTINE CHPEVX( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU,     &
     &                    ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK,     &
     &                    IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE CHPEVX

       SUBROUTINE ZHPEVX( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU,     &
     &                    ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK,     &
     &                    IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO, RANGE
         INTEGER, INTENT(IN) :: LDZ, N, IL, IU
         INTEGER, INTENT(OUT) :: INFO, IWORK(*), M, IFAIL(*)
         REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE ZHPEVX

       END INTERFACE
#endif

! 
! lasi_SPEVD
! 
#ifdef __LASI_SPEVD 
      INTERFACE lasi_SPEVD

       SUBROUTINE SSPEVD( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK,    &
     &                    IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSPEVD

       SUBROUTINE DSPEVD( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK,    &
     &                    IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSPEVD

       END INTERFACE
#endif

! 
! lasi_HPEVD
! 
#ifdef __LASI_HPEVD 
      INTERFACE lasi_HPEVD

       SUBROUTINE CHPEVD( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK,    &
     &                    RWORK, LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK, LRWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE CHPEVD

       SUBROUTINE ZHPEVD( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK,    &
     &                    RWORK, LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N, LWORK, LIWORK, LRWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE ZHPEVD

       END INTERFACE
#endif

! 
! lasi_SPEV
! 
#ifdef __LASI_SPEV 
      INTERFACE lasi_SPEV

       SUBROUTINE SSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE SSPEV

       SUBROUTINE DSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), Z(LDZ,*), WORK(*)
      END SUBROUTINE DSPEV

       END INTERFACE
#endif

! 
! lasi_HPEV
! 
#ifdef __LASI_HPEV 
      INTERFACE lasi_HPEV

       SUBROUTINE CHPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, RWORK,     &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE CHPEV

       SUBROUTINE ZHPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, RWORK,     &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDZ, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: Z(LDZ,*), WORK(*)
      END SUBROUTINE ZHPEV

       END INTERFACE
#endif

! 
! lasi_GGGLM
! 
#ifdef __LASI_GGGLM 
      INTERFACE lasi_GGGLM

       SUBROUTINE SGGGLM( N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), D(*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(*), Y(*)
      END SUBROUTINE SGGGLM

       SUBROUTINE DGGGLM( N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), D(*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(*), Y(*)
      END SUBROUTINE DGGGLM

       SUBROUTINE CGGGLM( N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), D(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), X(*), Y(*)
      END SUBROUTINE CGGGLM

       SUBROUTINE ZGGGLM( N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), D(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), X(*), Y(*)
      END SUBROUTINE ZGGGLM

       END INTERFACE
#endif

! 
! lasi_GGLSE
! 
#ifdef __LASI_GGLSE 
      INTERFACE lasi_GGLSE

       SUBROUTINE SGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), C(*), D(*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(*)
      END SUBROUTINE SGGLSE

       SUBROUTINE DGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), C(*), D(*)
         REAL(WP), INTENT(OUT) :: WORK(*), X(*)
      END SUBROUTINE DGGLSE

       SUBROUTINE CGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), C(*), D(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), X(*)
      END SUBROUTINE CGGLSE

       SUBROUTINE ZGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: P, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*), C(*), D(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), X(*)
      END SUBROUTINE ZGGLSE

       END INTERFACE
#endif

! 
! lasi_GELSY
! 
#ifdef __LASI_GELSY 
       INTERFACE lasi_GELSY

       SUBROUTINE SGELSY(  M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) ::  WORK(*)
       END SUBROUTINE SGELSY

       SUBROUTINE DGELSY(  M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) ::  WORK(*)
       END SUBROUTINE DGELSY

       SUBROUTINE CGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,&
     &                    WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) ::  WORK(*)
         REAL(WP) :: RWORK(*)
       END SUBROUTINE CGELSY

       SUBROUTINE ZGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,&
     &                    WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) ::  WORK(*)
         REAL(WP) :: RWORK(*)
       END SUBROUTINE ZGELSY

        MODULE PROCEDURE SGELSY1
        MODULE PROCEDURE DGELSY1
        MODULE PROCEDURE CGELSY1
        MODULE PROCEDURE ZGELSY1

        END INTERFACE 
#endif

! 
! lasi_GELSD
! 
#ifdef __LASI_GELSD 
        INTERFACE lasi_GELSD

       SUBROUTINE SGELSD(  M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, IWORK, INFO )
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) ::  A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: S(*)
          REAL(WP), INTENT(OUT) :: WORK(*)
          INTEGER :: IWORK(*) 
        END SUBROUTINE SGELSD

       SUBROUTINE DGELSD(  M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, IWORK, INFO )
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) ::  A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: S(*)
          REAL(WP), INTENT(OUT) :: WORK(*)
          INTEGER :: IWORK(*) 
        END SUBROUTINE DGELSD

       SUBROUTINE CGELSD(  M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, RWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: S(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         REAL(WP) :: RWORK(*)
         INTEGER :: IWORK(*)
       END SUBROUTINE CGELSD

       SUBROUTINE ZGELSD(  M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, RWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: S(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         REAL(WP) :: RWORK(*)
         INTEGER :: IWORK(*)
       END SUBROUTINE ZGELSD

          MODULE PROCEDURE SGELSD1
          MODULE PROCEDURE DGELSD1
          MODULE PROCEDURE CGELSD1
          MODULE PROCEDURE ZGELSD1
  
       END INTERFACE
#endif

! 
! lasi_GELSX
! 
#ifdef __LASI_GELSX 
      INTERFACE lasi_GELSX

       SUBROUTINE SGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,&
     &                    WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGELSX

       SUBROUTINE DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,&
     &                    WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGELSX

       SUBROUTINE CGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,&
     &                    WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CGELSX

       SUBROUTINE ZGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,&
     &                    WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZGELSX

      MODULE PROCEDURE SGELSX1
      MODULE PROCEDURE DGELSX1
      MODULE PROCEDURE CGELSX1
      MODULE PROCEDURE ZGELSX1

       END INTERFACE
#endif

! 
! lasi_GELSS
! 
#ifdef __LASI_GELSS 
      INTERFACE lasi_GELSS

       SUBROUTINE SGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,   &
     &                    WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGELSS

       SUBROUTINE DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,   &
     &                    WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGELSS

       SUBROUTINE CGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,   &
     &                    WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: S(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CGELSS

       SUBROUTINE ZGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,   &
     &                    WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: S(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZGELSS

      MODULE PROCEDURE SGELSS1
      MODULE PROCEDURE DGELSS1
      MODULE PROCEDURE CGELSS1
      MODULE PROCEDURE ZGELSS1

       END INTERFACE
#endif

! 
! lasi_GELS
! 
#ifdef __LASI_GELS 
      INTERFACE lasi_GELS

       SUBROUTINE SGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SGELS

       SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DGELS

       SUBROUTINE CGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CGELS

       SUBROUTINE ZGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,&
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZGELS

      MODULE PROCEDURE SGELS1
      MODULE PROCEDURE DGELS1
      MODULE PROCEDURE CGELS1
      MODULE PROCEDURE ZGELS1

       END INTERFACE
#endif

! 
! lasi_SPSV
! 
#ifdef __LASI_SPSV 
      INTERFACE lasi_SPSV

       SUBROUTINE SSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE SSPSV

       SUBROUTINE DSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE DSPSV

       SUBROUTINE CSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE CSPSV

       SUBROUTINE ZSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE ZSPSV

      MODULE PROCEDURE SSPSV1
      MODULE PROCEDURE DSPSV1
      MODULE PROCEDURE CSPSV1
      MODULE PROCEDURE ZSPSV1

       END INTERFACE
#endif

! 
! lasi_HPSV
! 
#ifdef __LASI_HPSV 
      INTERFACE lasi_HPSV

       SUBROUTINE CHPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE CHPSV

       SUBROUTINE ZHPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE ZHPSV

      MODULE PROCEDURE CHPSV1
      MODULE PROCEDURE ZHPSV1

       END INTERFACE
#endif

! 
! lasi_SYSV
! 
#ifdef __LASI_SYSV 
      INTERFACE lasi_SYSV

       SUBROUTINE SSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYSV

       SUBROUTINE DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYSV

       SUBROUTINE CSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CSYSV

       SUBROUTINE ZSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZSYSV

      MODULE PROCEDURE SSYSV1
      MODULE PROCEDURE DSYSV1
      MODULE PROCEDURE CSYSV1
      MODULE PROCEDURE ZSYSV1

       END INTERFACE
#endif

! 
! lasi_HESV
! 
#ifdef __LASI_HESV 
      INTERFACE lasi_HESV

       SUBROUTINE CHESV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHESV

       SUBROUTINE ZHESV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,     &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHESV

      MODULE PROCEDURE CHESV1
      MODULE PROCEDURE ZHESV1

       END INTERFACE
#endif

! 
! lasi_PTSV
! 
#ifdef __LASI_PTSV 
      INTERFACE lasi_PTSV

       SUBROUTINE SPTSV( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         REAL(WP), INTENT(INOUT) :: E(*), B(LDB,*)
      END SUBROUTINE SPTSV

       SUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         REAL(WP), INTENT(INOUT) :: E(*), B(LDB,*)
      END SUBROUTINE DPTSV

       SUBROUTINE CPTSV( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         COMPLEX(WP), INTENT(INOUT) :: E(*), B(LDB,*)
      END SUBROUTINE CPTSV

       SUBROUTINE ZPTSV( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         COMPLEX(WP), INTENT(INOUT) :: E(*), B(LDB,*)
      END SUBROUTINE ZPTSV

      MODULE PROCEDURE SPTSV1
      MODULE PROCEDURE DPTSV1
      MODULE PROCEDURE CPTSV1
      MODULE PROCEDURE ZPTSV1

       END INTERFACE
#endif

! 
! lasi_PBSV
! 
#ifdef __LASI_PBSV 
      INTERFACE lasi_PBSV

       SUBROUTINE SPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE SPBSV

       SUBROUTINE DPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE DPBSV

       SUBROUTINE CPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE CPBSV

       SUBROUTINE ZPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE ZPBSV

      MODULE PROCEDURE SPBSV1
      MODULE PROCEDURE DPBSV1
      MODULE PROCEDURE CPBSV1
      MODULE PROCEDURE ZPBSV1

       END INTERFACE
#endif

! 
! lasi_PPSV
! 
#ifdef __LASI_PPSV 
      INTERFACE lasi_PPSV

       SUBROUTINE SPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE SPPSV

       SUBROUTINE DPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE DPPSV

       SUBROUTINE CPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE CPPSV

       SUBROUTINE ZPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
      END SUBROUTINE ZPPSV

      MODULE PROCEDURE SPPSV1
      MODULE PROCEDURE DPPSV1
      MODULE PROCEDURE CPPSV1
      MODULE PROCEDURE ZPPSV1

       END INTERFACE
#endif

! 
! lasi_POSV
! 
#ifdef __LASI_POSV 
      INTERFACE lasi_POSV

       SUBROUTINE SPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE SPOSV

       SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE DPOSV

       SUBROUTINE CPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE CPOSV

       SUBROUTINE ZPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE ZPOSV

      MODULE PROCEDURE SPOSV1
      MODULE PROCEDURE DPOSV1
      MODULE PROCEDURE CPOSV1
      MODULE PROCEDURE ZPOSV1

       END INTERFACE
#endif

! 
! lasi_GTSV
! 
#ifdef __LASI_GTSV 
      INTERFACE lasi_GTSV

       SUBROUTINE SGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
      END SUBROUTINE SGTSV

       SUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
      END SUBROUTINE DGTSV

       SUBROUTINE CGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
      END SUBROUTINE CGTSV

       SUBROUTINE ZGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
      END SUBROUTINE ZGTSV

      MODULE PROCEDURE SGTSV1
      MODULE PROCEDURE DGTSV1
      MODULE PROCEDURE CGTSV1
      MODULE PROCEDURE ZGTSV1

       END INTERFACE
#endif

! 
! lasi_GBSV
! 
#ifdef __LASI_GBSV 
      INTERFACE lasi_GBSV

       SUBROUTINE SGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE SGBSV

       SUBROUTINE DGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE DGBSV

       SUBROUTINE CGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE CGBSV

       SUBROUTINE ZGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
      END SUBROUTINE ZGBSV

      MODULE PROCEDURE SGBSV1
      MODULE PROCEDURE DGBSV1
      MODULE PROCEDURE CGBSV1
      MODULE PROCEDURE ZGBSV1

       END INTERFACE
#endif

! 
! lasi_GESV
! 
#ifdef __LASI_GESV 
      INTERFACE lasi_GESV

       SUBROUTINE SGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE SGESV

       SUBROUTINE DGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE DGESV

       SUBROUTINE CGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE CGESV

       SUBROUTINE ZGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
      END SUBROUTINE ZGESV

      MODULE PROCEDURE SGESV1
      MODULE PROCEDURE DGESV1
      MODULE PROCEDURE CGESV1
      MODULE PROCEDURE ZGESV1

       END INTERFACE
#endif

! 
! lasi_SPSVX
! 
#ifdef __LASI_SPSVX 
      INTERFACE lasi_SPSVX

       SUBROUTINE SSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X,  &
     &                    LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: A(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: AF(*)
      END SUBROUTINE SSPSVX

       SUBROUTINE DSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X,  &
     &                    LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: A(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: AF(*)
      END SUBROUTINE DSPSVX

       SUBROUTINE CSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X,  &
     &                    LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: AF(*)
      END SUBROUTINE CSPSVX

       SUBROUTINE ZSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X,  &
     &                    LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: AF(*)
      END SUBROUTINE ZSPSVX

      MODULE PROCEDURE SSPSVX1
      MODULE PROCEDURE DSPSVX1
      MODULE PROCEDURE CSPSVX1
      MODULE PROCEDURE ZSPSVX1

       END INTERFACE
#endif

! 
! lasi_HPSVX
! 
#ifdef __LASI_HPSVX 
      INTERFACE lasi_HPSVX

       SUBROUTINE CHPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X,  &
     &                    LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: AF(*)
      END SUBROUTINE CHPSVX

       SUBROUTINE ZHPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X,  &
     &                    LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: AF(*)
      END SUBROUTINE ZHPSVX

      MODULE PROCEDURE CHPSVX1
      MODULE PROCEDURE ZHPSVX1

       END INTERFACE
#endif

! 
! lasi_SYSVX
! 
#ifdef __LASI_SYSVX 
      INTERFACE lasi_SYSVX

       SUBROUTINE SSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV,  &
     &                    B, LDB, X, LDX, RCOND, FERR, BERR, WORK,      &
     &                    LWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE SSYSVX

       SUBROUTINE DSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV,  &
     &                    B, LDB, X, LDX, RCOND, FERR, BERR, WORK,      &
     &                    LWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE DSYSVX

       SUBROUTINE CSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV,  &
     &                    B, LDB, X, LDX, RCOND, FERR, BERR, WORK,      &
     &                    LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE CSYSVX

       SUBROUTINE ZSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV,  &
     &                    B, LDB, X, LDX, RCOND, FERR, BERR, WORK,      &
     &                    LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE ZSYSVX

      MODULE PROCEDURE SSYSVX1
      MODULE PROCEDURE DSYSVX1
      MODULE PROCEDURE CSYSVX1
      MODULE PROCEDURE ZSYSVX1

       END INTERFACE
#endif

! 
! lasi_HESVX
! 
#ifdef __LASI_HESVX 
      INTERFACE lasi_HESVX

       SUBROUTINE CHESVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV,  &
     &                    B, LDB, X, LDX, RCOND, FERR, BERR, WORK,      &
     &                    LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE CHESVX

       SUBROUTINE ZHESVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV,  &
     &                    B, LDB, X, LDX, RCOND, FERR, BERR, WORK,      &
     &                    LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE ZHESVX

      MODULE PROCEDURE CHESVX1
      MODULE PROCEDURE ZHESVX1

       END INTERFACE
#endif

! 
! lasi_PTSVX
! 
#ifdef __LASI_PTSVX 
      INTERFACE lasi_PTSVX

       SUBROUTINE SPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,  &
     &                    RCOND, FERR, BERR, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         REAL(WP), INTENT(INOUT) :: EF(*)
      END SUBROUTINE SPTSVX

       SUBROUTINE DPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,  &
     &                    RCOND, FERR, BERR, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         REAL(WP), INTENT(INOUT) :: EF(*)
      END SUBROUTINE DPTSVX

       SUBROUTINE CPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,  &
     &                    RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(IN) :: E(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         COMPLEX(WP), INTENT(INOUT) :: EF(*)
      END SUBROUTINE CPTSVX

       SUBROUTINE ZPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,  &
     &                    RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(IN) :: E(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         COMPLEX(WP), INTENT(INOUT) :: EF(*)
      END SUBROUTINE ZPTSVX

      MODULE PROCEDURE SPTSVX1
      MODULE PROCEDURE DPTSVX1
      MODULE PROCEDURE CPTSVX1
      MODULE PROCEDURE ZPTSVX1

       END INTERFACE
#endif

! 
! lasi_PBSVX
! 
#ifdef __LASI_PBSVX 
      INTERFACE lasi_PBSVX

       SUBROUTINE SPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,&
     &                    EQUED, S, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                    WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*), FERR(*), BERR(*),  &
     &                            RCOND
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(LDB,*), &
     &                              S(*)
      END SUBROUTINE SPBSVX

       SUBROUTINE DPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,&
     &                    EQUED, S, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                    WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*), FERR(*), BERR(*),  &
     &                            RCOND
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(LDB,*), &
     &                              S(*)
      END SUBROUTINE DPBSVX

       SUBROUTINE CPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,&
     &                    EQUED, S, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                    WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RCOND, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*),        &
     &                                 B(LDB,*)
      END SUBROUTINE CPBSVX

       SUBROUTINE ZPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,&
     &                    EQUED, S, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                    WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RCOND, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*),        &
     &                                 B(LDB,*)
      END SUBROUTINE ZPBSVX

      MODULE PROCEDURE SPBSVX1
      MODULE PROCEDURE DPBSVX1
      MODULE PROCEDURE CPBSVX1
      MODULE PROCEDURE ZPBSVX1

       END INTERFACE
#endif

! 
! lasi_PPSVX
! 
#ifdef __LASI_PPSVX 
      INTERFACE lasi_PPSVX

       SUBROUTINE SPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,    &
     &                    LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK,  &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*), S(*)
      END SUBROUTINE SPPSVX

       SUBROUTINE DPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,    &
     &                    LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK,  &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*), S(*)
      END SUBROUTINE DPPSVX

       SUBROUTINE CPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,    &
     &                    LDB, X, LDX, RCOND, FERR, BERR, WORK, RWORK,  &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*)
      END SUBROUTINE CPPSVX

       SUBROUTINE ZPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,    &
     &                    LDB, X, LDX, RCOND, FERR, BERR, WORK, RWORK,  &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*)
      END SUBROUTINE ZPPSVX

      MODULE PROCEDURE SPPSVX1
      MODULE PROCEDURE DPPSVX1
      MODULE PROCEDURE CPPSVX1
      MODULE PROCEDURE ZPPSVX1

       END INTERFACE
#endif

! 
! lasi_POSVX
! 
#ifdef __LASI_POSVX 
      INTERFACE lasi_POSVX

       SUBROUTINE SPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
     &                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
     &                    IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE SPOSVX

       SUBROUTINE DPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
     &                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
     &                    IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE DPOSVX

       SUBROUTINE CPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
     &                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
     &                    RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE CPOSVX

       SUBROUTINE ZPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
     &                    S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,   &
     &                    RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE ZPOSVX

      MODULE PROCEDURE SPOSVX1
      MODULE PROCEDURE DPOSVX1
      MODULE PROCEDURE CPOSVX1
      MODULE PROCEDURE ZPOSVX1

       END INTERFACE
#endif

! 
! lasi_GTSVX
! 
#ifdef __LASI_GTSVX 
      INTERFACE lasi_GTSVX

       SUBROUTINE SGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
     &                    DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
     &                    WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
         REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
      END SUBROUTINE SGTSVX

       SUBROUTINE DGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
     &                    DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
     &                    WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX,*), WORK(*)
         REAL(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
         REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
      END SUBROUTINE DGTSVX

       SUBROUTINE CGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
     &                    DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
     &                    WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
         COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
      END SUBROUTINE CGTSVX

       SUBROUTINE ZGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,&
     &                    DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, &
     &                    WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
         COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
      END SUBROUTINE ZGTSVX

      MODULE PROCEDURE SGTSVX1
      MODULE PROCEDURE DGTSVX1
      MODULE PROCEDURE CGTSVX1
      MODULE PROCEDURE ZGTSVX1

       END INTERFACE
#endif

! 
! lasi_GBSVX
! 
#ifdef __LASI_GBSVX 
      INTERFACE lasi_GBSVX

       SUBROUTINE SGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE SGBSVX

       SUBROUTINE DGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE DGBSVX

       SUBROUTINE CGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE CGBSVX

       SUBROUTINE ZGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE ZGBSVX

      MODULE PROCEDURE SGBSVX1
      MODULE PROCEDURE DGBSVX1
      MODULE PROCEDURE CGBSVX1
      MODULE PROCEDURE ZGBSVX1

       END INTERFACE
#endif

! 
! lasi_GESVX
! 
#ifdef __LASI_GESVX 
      INTERFACE lasi_GESVX

       SUBROUTINE SGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE SGESVX

       SUBROUTINE DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE DGESVX

       SUBROUTINE CGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE CGESVX

       SUBROUTINE ZGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
      END SUBROUTINE ZGESVX

      MODULE PROCEDURE SGESVX1
      MODULE PROCEDURE DGESVX1
      MODULE PROCEDURE CGESVX1
      MODULE PROCEDURE ZGESVX1

      END INTERFACE
#endif

! 
! lasi_GETRF
! 
#ifdef __LASI_GETRF 
      INTERFACE lasi_GETRF

       SUBROUTINE SGETRF( M, N, A, LDA, PIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT( OUT ) :: PIV( * )
         REAL(WP), INTENT( INOUT ) :: A( LDA, * )
      END SUBROUTINE SGETRF

       SUBROUTINE DGETRF( M, N, A, LDA, PIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT( OUT ) :: PIV( * )
         REAL(WP), INTENT( INOUT ) :: A( LDA, * )
      END SUBROUTINE DGETRF

       SUBROUTINE CGETRF( M, N, A, LDA, PIV, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT( OUT ) :: PIV( * )
         COMPLEX(WP), INTENT( INOUT ) :: A( LDA, * )
      END SUBROUTINE CGETRF

       SUBROUTINE ZGETRF( M, N, A, LDA, PIV, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT( OUT ) :: PIV( * )
         COMPLEX(WP), INTENT( INOUT ) :: A( LDA, * )
      END SUBROUTINE ZGETRF


       END INTERFACE
#endif

! 
! lasi_GETRS
! 
#ifdef __LASI_GETRS 
      INTERFACE lasi_GETRS

       SUBROUTINE SGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE SGETRS

       SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE DGETRS

       SUBROUTINE CGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE CGETRS

       SUBROUTINE ZGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
      END SUBROUTINE ZGETRS

      MODULE PROCEDURE SGETRS1
      MODULE PROCEDURE DGETRS1
      MODULE PROCEDURE CGETRS1
      MODULE PROCEDURE ZGETRS1

       END INTERFACE
#endif

! 
! lasi_GETRI
! 
#ifdef __LASI_GETRI 
      INTERFACE lasi_GETRI

       SUBROUTINE SGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE SGETRI

       SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE DGETRI

       SUBROUTINE CGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE CGETRI

       SUBROUTINE ZGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE ZGETRI

       END INTERFACE
#endif

! 
! lasi_GERFS
! 
#ifdef __LASI_GERFS 
      INTERFACE lasi_GERFS

       SUBROUTINE SGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
      END SUBROUTINE SGERFS

       SUBROUTINE DGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
      END SUBROUTINE DGERFS

       SUBROUTINE CGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
      END SUBROUTINE CGERFS

       SUBROUTINE ZGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
      END SUBROUTINE ZGERFS

      MODULE PROCEDURE SGERFS1
      MODULE PROCEDURE DGERFS1
      MODULE PROCEDURE CGERFS1
      MODULE PROCEDURE ZGERFS1

       END INTERFACE
#endif

! 
! lasi_GEEQU
! 
#ifdef __LASI_GEEQU 
      INTERFACE lasi_GEEQU

       SUBROUTINE SGEEQU( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX,     &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: C( * ), R( * )
      END SUBROUTINE SGEEQU

       SUBROUTINE DGEEQU( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX,     &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: C( * ), R( * )
      END SUBROUTINE DGEEQU

       SUBROUTINE CGEEQU( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX,     &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: C( * ), R( * )
      END SUBROUTINE CGEEQU

       SUBROUTINE ZGEEQU( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX,     &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, M, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: C( * ), R( * )
      END SUBROUTINE ZGEEQU

       END INTERFACE
#endif

! 
! lasi_LANGE
! 
#ifdef __LASI_LANGE 
      INTERFACE lasi_LANGE

       FUNCTION SLANGE( NORM, M, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         REAL(WP) :: SLANGE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION SLANGE

       FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         REAL(WP) :: DLANGE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION DLANGE

       FUNCTION CLANGE( NORM, M, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         REAL(WP) :: CLANGE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION CLANGE

       FUNCTION ZLANGE( NORM, M, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         REAL(WP) :: ZLANGE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION ZLANGE

      MODULE PROCEDURE SLANGE1
      MODULE PROCEDURE DLANGE1
      MODULE PROCEDURE CLANGE1
      MODULE PROCEDURE ZLANGE1

       END INTERFACE
#endif

! 
! lasi_GECON
! 
#ifdef __LASI_GECON 
      INTERFACE lasi_GECON

       SUBROUTINE SGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE SGECON

       SUBROUTINE DGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE DGECON

       SUBROUTINE CGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, RWORK,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         COMPLEX(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE CGECON

       SUBROUTINE ZGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, RWORK,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         COMPLEX(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE ZGECON

       END INTERFACE
#endif

! 
! lasi_SYEV
! 
#ifdef __LASI_SYEV 
      INTERFACE lasi_SYEV

       SUBROUTINE SSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYEV

       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYEV

       END INTERFACE
#endif

! 
! lasi_HEEV
! 
#ifdef __LASI_HEEV 
      INTERFACE lasi_HEEV

       SUBROUTINE CHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHEEV

       SUBROUTINE ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK,  &
     &                   INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHEEV

       END INTERFACE
#endif

! 
! lasi_SYEVD
! 
#ifdef __LASI_SYEVD 
      INTERFACE lasi_SYEVD

       SUBROUTINE SSYEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK, &
     &                    LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LIWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYEVD

       SUBROUTINE DSYEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK, &
     &                    LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LIWORK, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYEVD

       END INTERFACE
#endif

! 
! lasi_HEEVD
! 
#ifdef __LASI_HEEVD 
      INTERFACE lasi_HEEVD

       SUBROUTINE CHEEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, &
     &                    LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LIWORK, LWORK, N, LRWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE CHEEVD

       SUBROUTINE ZHEEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, &
     &                    LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: LDA, LIWORK, LWORK, N, LRWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE ZHEEVD

       END INTERFACE
#endif

! 
! lasi_SYEVR
! 
#ifdef __LASI_SYEVR 
       INTERFACE lasi_SYEVR

       SUBROUTINE SSYEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,    &
     &                    IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LDA, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT) :: ISUPPZ(*)
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
       END SUBROUTINE  SSYEVR    

       SUBROUTINE DSYEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,    &
     &                    IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LDA, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT) :: ISUPPZ(*)
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
       END SUBROUTINE  DSYEVR    

      END INTERFACE
#endif
      
! 
! lasi_HEEVR
! 
#ifdef __LASI_HEEVR 
      INTERFACE lasi_HEEVR
   
       SUBROUTINE CHEEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,    &
     &                    RWORK, LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LDA, LWORK, LIWORK, LRWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT) :: ISUPPZ(*)
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
        END SUBROUTINE CHEEVR 


       SUBROUTINE ZHEEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,    &
     &                    RWORK, LRWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LDA, LWORK, LIWORK, LRWORK
         INTEGER, INTENT(OUT) :: M
         INTEGER, INTENT(OUT) :: ISUPPZ(*)
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
        END SUBROUTINE ZHEEVR 


       END INTERFACE
#endif

! 
! lasi_SYEVX
! 
#ifdef __LASI_SYEVX 
      INTERFACE lasi_SYEVX

       SUBROUTINE SSYEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK,     &
     &                    IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: IL, IU, LDA, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, M
         INTEGER, INTENT(OUT) :: IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
      END SUBROUTINE SSYEVX

       SUBROUTINE DSYEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK,     &
     &                    IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: IL, IU, LDA, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, M
         INTEGER, INTENT(OUT) :: IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
      END SUBROUTINE DSYEVX

       END INTERFACE
#endif

! 
! lasi_HEEVX
! 
#ifdef __LASI_HEEVX 
      INTERFACE lasi_HEEVX

       SUBROUTINE CHEEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK,     &
     &                    IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: IL, IU, LDA, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, M
         INTEGER, INTENT(OUT) :: IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
      END SUBROUTINE CHEEVX

       SUBROUTINE ZHEEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
     &                    ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK,     &
     &                    IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: IL, IU, LDA, LDZ, LWORK, N
         INTEGER, INTENT(OUT) :: INFO, M
         INTEGER, INTENT(OUT) :: IFAIL(*), IWORK(*)
         REAL(WP), INTENT(IN) :: ABSTOL, VL, VU
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
      END SUBROUTINE ZHEEVX

       END INTERFACE
#endif

! 
! lasi_SYGST
! 
#ifdef __LASI_SYGST 
      INTERFACE lasi_SYGST

       SUBROUTINE SSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: B(LDB,*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE SSYGST

       SUBROUTINE DSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: B(LDB,*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE DSYGST

       END INTERFACE
#endif

! 
! lasi_HEGST
! 
#ifdef __LASI_HEGST 
      INTERFACE lasi_HEGST

       SUBROUTINE CHEGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE CHEGST

       SUBROUTINE ZHEGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE ZHEGST

       END INTERFACE
#endif

! 
! lasi_SYGV
! 
#ifdef __LASI_SYGV 
      INTERFACE lasi_SYGV

       SUBROUTINE SSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE SSYGV

       SUBROUTINE DSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, &
     &                   LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
      END SUBROUTINE DSYGV

       END INTERFACE
#endif

! 
! lasi_HEGV
! 
#ifdef __LASI_HEGV 
      INTERFACE lasi_HEGV

       SUBROUTINE CHEGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, &
     &                   LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
       END SUBROUTINE CHEGV

       SUBROUTINE ZHEGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, &
     &                   LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, LDA, LDB, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: W(*), RWORK(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
       END SUBROUTINE ZHEGV

       END INTERFACE
#endif

! 
! lasi_SYGVX
! 
#ifdef __LASI_SYGVX 
        INTERFACE lasi_SYGVX


       SUBROUTINE SSYGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,  &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    LWORK, IWORK, IFAIL, INFO )
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
          INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: M
          REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
          INTEGER, INTENT(OUT) ::  IWORK(*)
          INTEGER, INTENT(OUT) :: INFO
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
          REAL(WP), INTENT(OUT) :: W(*)
          INTEGER, INTENT(IN) :: IFAIL(*)
         END SUBROUTINE SSYGVX
   

       SUBROUTINE DSYGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,  &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    LWORK, IWORK, IFAIL, INFO )
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
          INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: M
          REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
          INTEGER, INTENT(OUT) ::  IWORK(*)
          INTEGER, INTENT(OUT) :: INFO
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
          REAL(WP), INTENT(OUT) :: W(*)
          INTEGER, INTENT(IN) :: IFAIL(*)
         END SUBROUTINE DSYGVX
   
        END INTERFACE
#endif

! 
! lasi_HEGVX
! 
#ifdef __LASI_HEGVX 
        INTERFACE lasi_HEGVX

       SUBROUTINE CHEGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,  &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    LWORK, RWORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
       END SUBROUTINE CHEGVX

       SUBROUTINE ZHEGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,  &
     &                    VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,   &
     &                    LWORK, RWORK, IWORK, IFAIL, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, IL, IU, LDZ, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: M
         REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
         INTEGER, INTENT(OUT) ::  IWORK(*)
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*), Z(LDZ,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         INTEGER, INTENT(IN) :: IFAIL(*)
       END SUBROUTINE ZHEGVX

       END INTERFACE
#endif


! 
! lasi_SYGVD
! 
#ifdef __LASI_SYGVD 
       INTERFACE lasi_SYGVD


       SUBROUTINE SSYGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,&
     &                    LWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
        END SUBROUTINE SSYGVD


       SUBROUTINE DSYGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,&
     &                    LWORK, IWORK, LIWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
         INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK, LIWORK
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         REAL(WP), INTENT(OUT) :: W(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
        END SUBROUTINE DSYGVD

        END INTERFACE
#endif

! 
! lasi_HEGVD
! 
#ifdef __LASI_HEGVD 
        INTERFACE lasi_HEGVD

       SUBROUTINE CHEGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,&
     &                    LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
          INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK, LIWORK, LRWORK
          INTEGER, INTENT(OUT) :: INFO, IWORK(*)
          COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: W(*)
          COMPLEX(WP), INTENT(OUT) :: WORK(*)
          REAL(WP), INTENT(OUT) :: RWORK(*)
         END SUBROUTINE CHEGVD

       SUBROUTINE ZHEGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,&
     &                    LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: JOBZ, UPLO
          INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK, LIWORK, LRWORK
          INTEGER, INTENT(OUT) :: INFO, IWORK(*)
          COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) :: W(*)
          COMPLEX(WP), INTENT(OUT) :: WORK(*)
          REAL(WP), INTENT(OUT) :: RWORK(*)
         END SUBROUTINE ZHEGVD

      END INTERFACE
#endif
            
! 
! lasi_SYTRD
! 
#ifdef __LASI_SYTRD 
      INTERFACE lasi_SYTRD

       SUBROUTINE SSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK,      &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE SSYTRD

       SUBROUTINE DSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK,      &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         REAL(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE DSYTRD

       END INTERFACE
#endif

! 
! lasi_HETRD
! 
#ifdef __LASI_HETRD 
      INTERFACE lasi_HETRD

       SUBROUTINE CHETRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK,      &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE CHETRD

       SUBROUTINE ZHETRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK,      &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: D(*), E(*)
         COMPLEX(WP), INTENT(OUT) :: TAU(*), WORK(LWORK)
      END SUBROUTINE ZHETRD

       END INTERFACE
#endif

! 
! lasi_ORGTR
! 
#ifdef __LASI_ORGTR 
      INTERFACE lasi_ORGTR

       SUBROUTINE SORGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE SORGTR

       SUBROUTINE DORGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: TAU(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
         REAL(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE DORGTR

       END INTERFACE
#endif

! 
! lasi_UNGTR
! 
#ifdef __LASI_UNGTR 
      INTERFACE lasi_UNGTR

       SUBROUTINE CUNGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
         IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE CUNGTR

       SUBROUTINE ZUNGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LWORK, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: TAU(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(LWORK)
      END SUBROUTINE ZUNGTR

       END INTERFACE
#endif

! 
! lasi_LANSY
! 
#ifdef __LASI_LANSY 
      INTERFACE lasi_LANSY

      FUNCTION SLANSY( NORM, UPLO, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         REAL(WP) :: SLANSY
         CHARACTER(LEN=1), INTENT(IN) :: NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION SLANSY

      FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         REAL(WP) :: DLANSY
         CHARACTER(LEN=1), INTENT(IN) :: NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION DLANSY

      FUNCTION CLANSY( NORM, UPLO, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         REAL(WP) :: CLANSY
         CHARACTER(LEN=1), INTENT(IN) :: NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION CLANSY

      FUNCTION ZLANSY( NORM, UPLO, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         REAL(WP) :: ZLANSY
         CHARACTER(LEN=1), INTENT(IN) :: NORM, UPLO
         INTEGER, INTENT(IN) :: LDA, N
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END FUNCTION ZLANSY

       END INTERFACE
#endif

! 
! lasi_POTRF
! 
#ifdef __LASI_POTRF 
      INTERFACE lasi_POTRF

       SUBROUTINE SPOTRF( UPLO, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE SPOTRF

       SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE DPOTRF

       SUBROUTINE CPOTRF( UPLO, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE CPOTRF

       SUBROUTINE ZPOTRF( UPLO, N, A, LDA, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*)
      END SUBROUTINE ZPOTRF

       END INTERFACE
#endif

! 
! lasi_POCON
! 
#ifdef __LASI_POCON 
      INTERFACE lasi_POCON

       SUBROUTINE SPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE SPOCON

       SUBROUTINE DPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, IWORK,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND
         INTEGER, INTENT(OUT) :: IWORK( * )
         REAL(WP), INTENT(IN) :: A( LDA, * )
         REAL(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE DPOCON

       SUBROUTINE CPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, RWORK,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         COMPLEX(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE CPOCON

       SUBROUTINE ZPOCON( UPLO, N, A, LDA, ANORM, RCOND, WORK, RWORK,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: ANORM
         REAL(WP), INTENT(OUT) :: RCOND, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA, * )
         COMPLEX(WP), INTENT(OUT) :: WORK( * )
      END SUBROUTINE ZPOCON

      END INTERFACE
#endif

! 
! lasi_ILAENV
! 
#ifdef __LASI_ILAENV 
      INTERFACE lasi_ILAENV

      FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
         USE lasi_kinds
         IMPLICIT NONE
         INTEGER :: ILAENV
         CHARACTER(LEN=*), INTENT(IN) :: NAME, OPTS
         INTEGER, INTENT(IN) :: ISPEC, N1, N2, N3, N4
      END FUNCTION ILAENV

      END INTERFACE
#endif

! 
! lasi_LAGGE
! 
#ifdef __LASI_LAGGE 
      INTERFACE lasi_LAGGE

       SUBROUTINE SLAGGE( M, N, KL, KU, D, A, LDA, ISEED, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, KL, KU, LDA
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: ISEED(4)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(OUT) :: A(LDA,*), WORK(*)
      END SUBROUTINE SLAGGE

       SUBROUTINE DLAGGE( M, N, KL, KU, D, A, LDA, ISEED, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, KL, KU, LDA
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: ISEED(4)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(OUT) :: A(LDA,*), WORK(*)
      END SUBROUTINE DLAGGE

       SUBROUTINE CLAGGE( M, N, KL, KU, D, A, LDA, ISEED, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, KL, KU, LDA
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: ISEED(4)
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(OUT) :: A(LDA,*), WORK(*)
      END SUBROUTINE CLAGGE

       SUBROUTINE ZLAGGE( M, N, KL, KU, D, A, LDA, ISEED, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, KL, KU, LDA
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: ISEED(4)
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(OUT) :: A(LDA,*), WORK(*)
      END SUBROUTINE ZLAGGE

      END INTERFACE
#endif


      CONTAINS

!
! dummy procedure if NO other subroutines are selected
!
     SUBROUTINE lasi_dummy()
       USE lasi_kinds
       IMPLICIT NONE
     END SUBROUTINE lasi_dummy

!
! all the followinf routines are used to permit passing
! matrix with 1 column as vector to the interfaced lapack
!

#ifdef __LASI_GESV
      SUBROUTINE SGESV1( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE SGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
             IMPLICIT NONE
             INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE SGESV
         END INTERFACE
         CALL SGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
      END SUBROUTINE SGESV1

      SUBROUTINE DGESV1( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE DGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE DGESV
         END INTERFACE
         CALL DGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
      END SUBROUTINE DGESV1

      SUBROUTINE CGESV1( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE CGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE CGESV
         END INTERFACE
         CALL CGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
      END SUBROUTINE CGESV1

      SUBROUTINE ZGESV1( N, NRHS, A, LDA, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE ZGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE ZGESV
         END INTERFACE
         CALL ZGESV( N, NRHS, A, LDA, PIV, B, LDB, INFO )
      END SUBROUTINE ZGESV1
#endif


#ifdef __LASI_GESVX
      SUBROUTINE SGESVX1( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE SGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF,   &
     &                        PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,  &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE SGESVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,      &
     &                EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR, LBERR, &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SGESVX1

      SUBROUTINE DGESVX1( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF,   &
     &                        PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,  &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE DGESVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,      &
     &                EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR, LBERR, &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DGESVX1

      SUBROUTINE CGESVX1( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE CGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF,   &
     &                        PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,  &
     &                        FERR, BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*),        &
     &                                     B(LDB,*)
           END SUBROUTINE CGESVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,      &
     &                EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR, LBERR, &
     &                WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CGESVX1

      SUBROUTINE ZGESVX1( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                    EQUED, R, C, B, LDB, X, LDX, RCOND, FERR,     &
     &                    BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE ZGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF,   &
     &                        PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,  &
     &                        FERR, BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*),        &
     &                                     B(LDB,*)
           END SUBROUTINE ZGESVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,      &
     &                EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR, LBERR, &
     &                WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZGESVX1
#endif


#ifdef __LASI_POSV
      SUBROUTINE SPOSV1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE SPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE SPOSV
         END INTERFACE
         CALL SPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE SPOSV1

      SUBROUTINE DPOSV1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE DPOSV
         END INTERFACE
         CALL DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE DPOSV1

      SUBROUTINE CPOSV1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE CPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE CPOSV
         END INTERFACE
         CALL CPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE CPOSV1

      SUBROUTINE ZPOSV1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         INTERFACE
           SUBROUTINE ZPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, LDA
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           END SUBROUTINE ZPOSV
         END INTERFACE
         CALL ZPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE ZPOSV1
#endif

#ifdef __LASI_LANGE
      FUNCTION SLANGE1( NORM, M, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         REAL(WP) :: SLANGE1
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         REAL(WP), INTENT(IN) :: A( * )
         REAL(WP), INTENT(OUT) :: WORK( * )
         INTERFACE
           FUNCTION SLANGE( NORM, M, N, A, LDA, WORK )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             REAL(WP) :: SLANGE
             CHARACTER(LEN=1), INTENT(IN) :: NORM
             INTEGER, INTENT(IN) :: LDA, M, N
             REAL(WP), INTENT(IN) :: A( LDA, * )
             REAL(WP), INTENT(OUT) :: WORK( * )
           END FUNCTION SLANGE
         END INTERFACE
        SLANGE1 = SLANGE( NORM, M, N, A, LDA, WORK )
      END FUNCTION SLANGE1

      FUNCTION DLANGE1( NORM, M, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         REAL(WP) :: DLANGE1
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         REAL(WP), INTENT(IN) :: A( * )
         REAL(WP), INTENT(OUT) :: WORK( * )
         INTERFACE
           FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             REAL(WP) :: DLANGE
             CHARACTER(LEN=1), INTENT(IN) :: NORM
             INTEGER, INTENT(IN) :: LDA, M, N
             REAL(WP), INTENT(IN) :: A( LDA, * )
             REAL(WP), INTENT(OUT) :: WORK( * )
           END FUNCTION DLANGE
         END INTERFACE
        DLANGE1 = DLANGE( NORM, M, N, A, LDA, WORK )
      END FUNCTION DLANGE1

      FUNCTION CLANGE1( NORM, M, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         REAL(WP) :: CLANGE1
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         COMPLEX(WP), INTENT(IN) :: A( * )
         REAL(WP), INTENT(OUT) :: WORK( * )
         INTERFACE
           FUNCTION CLANGE( NORM, M, N, A, LDA, WORK )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             REAL(WP) :: CLANGE
             CHARACTER(LEN=1), INTENT(IN) :: NORM
             INTEGER, INTENT(IN) :: LDA, M, N
             COMPLEX(WP), INTENT(IN) :: A( LDA, * )
             REAL(WP), INTENT(OUT) :: WORK( * )
           END FUNCTION CLANGE
         END INTERFACE
        CLANGE1 = CLANGE( NORM, M, N, A, LDA, WORK )
      END FUNCTION CLANGE1

      FUNCTION ZLANGE1( NORM, M, N, A, LDA, WORK )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         REAL(WP) :: ZLANGE1
         CHARACTER(LEN=1), INTENT(IN) :: NORM
         INTEGER, INTENT(IN) :: LDA, M, N
         COMPLEX(WP), INTENT(IN) :: A( * )
         REAL(WP), INTENT(OUT) :: WORK( * )
         INTERFACE
           FUNCTION ZLANGE( NORM, M, N, A, LDA, WORK )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             REAL(WP) :: ZLANGE
             CHARACTER(LEN=1), INTENT(IN) :: NORM
             INTEGER, INTENT(IN) :: LDA, M, N
             COMPLEX(WP), INTENT(IN) :: A( LDA, * )
             REAL(WP), INTENT(OUT) :: WORK( * )
           END FUNCTION ZLANGE
         END INTERFACE
        ZLANGE1 = ZLANGE( NORM, M, N, A, LDA, WORK )
      END FUNCTION ZLANGE1
#endif

#ifdef __LASI_GBSV
      SUBROUTINE SGBSV1( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE SGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB,    &
     &                       INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE SGBSV
         END INTERFACE
         CALL SGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
      END SUBROUTINE SGBSV1

      SUBROUTINE DGBSV1( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE DGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB,    &
     &                       INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE DGBSV
         END INTERFACE
         CALL DGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
      END SUBROUTINE DGBSV1

      SUBROUTINE CGBSV1( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE CGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB,    &
     &                       INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE CGBSV
         END INTERFACE
         CALL CGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
      END SUBROUTINE CGBSV1

      SUBROUTINE ZGBSV1( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: PIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE ZGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB,    &
     &                       INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: PIV(*)
             COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE ZGBSV
         END INTERFACE
         CALL ZGBSV( N, KL, KU, NRHS, AB, LDAB, PIV, B, LDB, INFO )
      END SUBROUTINE ZGBSV1
#endif

#ifdef __LASI_GBSVX
      SUBROUTINE SGBSVX1( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE SGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, &
     &                        LDAF, PIV, EQUED, R, C, B, LDB, X, LDX,   &
     &                        RCOND, FERR, BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
             INTEGER, INTENT(OUT) :: INFO
!
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE SGBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, LDAF,   &
     &                PIV, EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR,   &
     &                LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SGBSVX1

      SUBROUTINE DGBSVX1( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE DGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, &
     &                        LDAF, PIV, EQUED, R, C, B, LDB, X, LDX,   &
     &                        RCOND, FERR, BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
             INTEGER, INTENT(OUT) :: INFO
!
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE DGBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, LDAF,   &
     &                PIV, EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR,   &
     &                LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DGBSVX1

      SUBROUTINE CGBSVX1( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE CGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, &
     &                        LDAF, PIV, EQUED, R, C, B, LDB, X, LDX,   &
     &                        RCOND, FERR, BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE CGBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, LDAF,   &
     &                PIV, EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR,   &
     &                LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CGBSVX1

      SUBROUTINE ZGBSVX1( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF,     &
     &                    LDAF, PIV, EQUED, R, C, B, LDB, X, LDX, RCOND,&
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: PIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: R(*), C(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE ZGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, &
     &                        LDAF, PIV, EQUED, R, C, B, LDB, X, LDX,   &
     &                        RCOND, FERR, BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, KL, KU
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: PIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: R(*), C(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE ZGBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZGBSVX( FACT, TRANS, N, KL, KU, NRHS, A, LDA, AF, LDAF,   &
     &                PIV, EQUED, R, C, B, LDB, X, LDX, RCOND, LFERR,   &
     &                LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZGBSVX1
#endif

#ifdef __LASI_GTSV
      SUBROUTINE SGTSV1( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
         INTERFACE
           SUBROUTINE SGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
           END SUBROUTINE SGTSV
         END INTERFACE
         CALL SGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
      END SUBROUTINE SGTSV1

      SUBROUTINE DGTSV1( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
         INTERFACE
           SUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
           END SUBROUTINE DGTSV
         END INTERFACE
         CALL DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
      END SUBROUTINE DGTSV1

      SUBROUTINE CGTSV1( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
         INTERFACE
           SUBROUTINE CGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
           END SUBROUTINE CGTSV
         END INTERFACE
         CALL CGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
      END SUBROUTINE CGTSV1

      SUBROUTINE ZGTSV1( N, NRHS, DL, D, DU, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(*)
         INTERFACE
           SUBROUTINE ZGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: DL(*), D(*), DU(*), B(LDB,*)
           END SUBROUTINE ZGTSV
         END INTERFACE
         CALL ZGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
      END SUBROUTINE ZGTSV1
#endif

#ifdef __LASI_GTSVX
       SUBROUTINE SGTSVX1( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
     &                     DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
     &                     BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, X(*), WORK(*)
         REAL(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*) 
         REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
         INTERFACE
           SUBROUTINE SGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
     &                        DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
             REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
           END SUBROUTINE SGTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
     &                DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SGTSVX1

       SUBROUTINE DGTSVX1( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
     &                     DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
     &                     BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, X(*), WORK(*)
         REAL(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*) 
         REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
         INTERFACE
           SUBROUTINE DGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
     &                        DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
             REAL(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
           END SUBROUTINE DGTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
     &                DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DGTSVX1

       SUBROUTINE CGTSVX1( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
     &                     DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
     &                     BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*) 
         COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
         INTERFACE
           SUBROUTINE CGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
     &                        DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
     &                        FERR, BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
             COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
           END SUBROUTINE CGTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
     &                DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
     &                WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CGTSVX1

       SUBROUTINE ZGTSVX1( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,    &
     &                     DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, &
     &                     BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: B(*), DL(*), D(*), DU(*) 
         COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
         INTERFACE
           SUBROUTINE ZGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, &
     &                        DUF, DU2, IPIV, B, LDB, X, LDX, RCOND,    &
     &                        FERR, BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: B(LDB,*), DL(*), D(*), DU(*) 
             COMPLEX(WP), INTENT(INOUT) :: DF(*), DLF(*), DU2(*), DUF(*)
           END SUBROUTINE ZGTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,    &
     &                DU2, IPIV, B, LDB, X, LDX, RCOND, LFERR, LBERR,   &
     &                WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZGTSVX1
#endif

#ifdef __LASI_POSVX
       SUBROUTINE SPOSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
     &                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
     &                     IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE SPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
     &                        BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE SPOSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
     &                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, IWORK, &
     &                INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SPOSVX1

       SUBROUTINE DPOSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
     &                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
     &                     IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE DPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
     &                        BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE DPOSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
     &                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, IWORK, &
     &                INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DPOSVX1

       SUBROUTINE CPOSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
     &                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
     &                     RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE CPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
     &                        BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE CPOSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
     &                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, RWORK, &
     &                INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CPOSVX1

       SUBROUTINE ZPOSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,&
     &                     S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,  &
     &                     RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(*)
         INTERFACE
           SUBROUTINE ZPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        EQUED, S, B, LDB, X, LDX, RCOND, FERR,    &
     &                        BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
           END SUBROUTINE ZPOSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, S,  &
     &                B, LDB, X, LDX, RCOND, LFERR, LBERR, WORK, RWORK, &
     &                INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZPOSVX1
#endif

#ifdef __LASI_PPSV
       SUBROUTINE SPPSV1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE SPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE SPPSV
         END INTERFACE
         CALL SPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE SPPSV1

       SUBROUTINE DPPSV1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE DPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE DPPSV
         END INTERFACE
         CALL DPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE DPPSV1

       SUBROUTINE CPPSV1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE CPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE CPPSV
         END INTERFACE
         CALL CPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE CPPSV1

       SUBROUTINE ZPPSV1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE ZPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE ZPPSV
         END INTERFACE
         CALL ZPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE ZPPSV1
#endif

#ifdef __LASI_PPSVX
       SUBROUTINE SPPSVX1( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,   &
     &                     LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK, &
     &                     INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(*), S(*)
         INTERFACE
           SUBROUTINE SPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,&
     &                        LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                        IWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*), S(*)
           END SUBROUTINE SPPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B, LDB, X,&
     &                LDX, RCOND, LFERR, LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SPPSVX1

       SUBROUTINE DPPSVX1( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,   &
     &                     LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK, &
     &                     INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(*), S(*)
         INTERFACE
           SUBROUTINE DPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,&
     &                        LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                        IWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*), S(*)
           END SUBROUTINE DPPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B, LDB, X,&
     &                LDX, RCOND, LFERR, LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DPPSVX1

       SUBROUTINE CPPSVX1( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,   &
     &                     LDB, X, LDX, RCOND, FERR, BERR, WORK, RWORK, &
     &                     INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), AFP(*), B(*)
         INTERFACE
           SUBROUTINE CPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,&
     &                        LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                        RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             COMPLEX(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*)
           END SUBROUTINE CPPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B, LDB, X,&
     &                LDX, RCOND, LFERR, LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CPPSVX1

       SUBROUTINE ZPPSVX1( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,   &
     &                     LDB, X, LDX, RCOND, FERR, BERR, WORK, RWORK, &
     &                     INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), AFP(*), B(*)
         INTERFACE
           SUBROUTINE ZPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B,&
     &                        LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                        RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             COMPLEX(WP), INTENT(INOUT) :: AP(*), AFP(*), B(LDB,*)
           END SUBROUTINE ZPPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B, LDB, X,&
     &                LDX, RCOND, LFERR, LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZPPSVX1
#endif

#ifdef __LASI_PBSV
       SUBROUTINE SPBSV1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE SPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,       &
     &                       INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE SPBSV
         END INTERFACE
         CALL SPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE SPBSV1

       SUBROUTINE DPBSV1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE DPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,       &
     &                       INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE DPBSV
         END INTERFACE
         CALL DPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE DPBSV1

       SUBROUTINE CPBSV1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE CPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,       &
     &                       INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE CPBSV
         END INTERFACE
         CALL CPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE CPBSV1

       SUBROUTINE ZPBSV1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(*)
         INTERFACE
           SUBROUTINE ZPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,       &
     &                       INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB, KD, LDAB
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), B(LDB,*)
           END SUBROUTINE ZPBSV
         END INTERFACE
         CALL ZPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE ZPBSV1
#endif

#ifdef __LASI_PBSVX
       SUBROUTINE SPBSVX1( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,      &
     &                     LDAFB, EQUED, S, B, LDB, X, LDX, RCOND, FERR,&
     &                     BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: X(*), WORK(*), FERR, BERR, RCOND
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(*),     &
     &                              S(*)
         INTERFACE
           SUBROUTINE SPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,   &
     &                        LDAFB, EQUED, S, B, LDB, X, LDX, RCOND,   &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
             INTEGER, INTENT(OUT) :: INFO, IWORK(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*), FERR(*),       &
     &                                BERR(*), RCOND
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*),       &
     &                                  B(LDB,*), S(*)
           END SUBROUTINE SPBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,    &
     &                EQUED, S, B, LDB, X, LDX, RCOND, LFERR, LBERR,    &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SPBSVX1

       SUBROUTINE DPBSVX1( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,      &
     &                     LDAFB, EQUED, S, B, LDB, X, LDX, RCOND, FERR,&
     &                     BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: X(*), WORK(*), FERR, BERR, RCOND
         REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(*),     &
     &                              S(*)
         INTERFACE
           SUBROUTINE DPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,   &
     &                        LDAFB, EQUED, S, B, LDB, X, LDX, RCOND,   &
     &                        FERR, BERR, WORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
             INTEGER, INTENT(OUT) :: INFO, IWORK(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*), FERR(*),       &
     &                                BERR(*), RCOND
             REAL(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*),       &
     &                                  B(LDB,*), S(*)
           END SUBROUTINE DPBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,    &
     &                EQUED, S, B, LDB, X, LDX, RCOND, LFERR, LBERR,    &
     &                WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DPBSVX1

       SUBROUTINE CPBSVX1( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,      &
     &                     LDAFB, EQUED, S, B, LDB, X, LDX, RCOND, FERR,&
     &                     BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: FERR, BERR, RCOND, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(*)
         INTERFACE
           SUBROUTINE CPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,   &
     &                        LDAFB, EQUED, S, B, LDB, X, LDX, RCOND,   &
     &                        FERR, BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RCOND, RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*),    &
     &                                     B(LDB,*)
           END SUBROUTINE CPBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,    &
     &                EQUED, S, B, LDB, X, LDX, RCOND, LFERR, LBERR,    &
     &                WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CPBSVX1

       SUBROUTINE ZPBSVX1( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,      &
     &                     LDAFB, EQUED, S, B, LDB, X, LDX, RCOND, FERR,&
     &                     BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
         CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
         INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: FERR, BERR, RCOND, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(INOUT) :: S(*)
         COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*), B(*)
         INTERFACE
           SUBROUTINE ZPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB,   &
     &                        LDAFB, EQUED, S, B, LDB, X, LDX, RCOND,   &
     &                        FERR, BERR, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT, UPLO
             CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
             INTEGER, INTENT(IN) :: LDAB, LDAFB, LDB, LDX, NRHS, N, KD
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RCOND, RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(INOUT) :: S(*)
             COMPLEX(WP), INTENT(INOUT) :: AB(LDAB,*), AFB(LDAFB,*),    &
     &                                     B(LDB,*)
           END SUBROUTINE ZPBSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,    &
     &                EQUED, S, B, LDB, X, LDX, RCOND, LFERR, LBERR,    &
     &                WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZPBSVX1
#endif

#ifdef __LASI_PTSV
       SUBROUTINE SPTSV1( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         REAL(WP), INTENT(INOUT) :: E(*), B(*)
         INTERFACE
           SUBROUTINE SPTSV( N, NRHS, D, E, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: D(*)
             REAL(WP), INTENT(INOUT) :: E(*), B(LDB,*)
           END SUBROUTINE SPTSV
         END INTERFACE
         CALL SPTSV( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE SPTSV1

       SUBROUTINE DPTSV1( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         REAL(WP), INTENT(INOUT) :: E(*), B(*)
         INTERFACE
           SUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: D(*)
             REAL(WP), INTENT(INOUT) :: E(*), B(LDB,*)
           END SUBROUTINE DPTSV
         END INTERFACE
         CALL DPTSV( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE DPTSV1

       SUBROUTINE CPTSV1( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         COMPLEX(WP), INTENT(INOUT) :: E(*), B(*)
         INTERFACE
           SUBROUTINE CPTSV( N, NRHS, D, E, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: D(*)
             COMPLEX(WP), INTENT(INOUT) :: E(*), B(LDB,*)
           END SUBROUTINE CPTSV
         END INTERFACE
         CALL CPTSV( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE CPTSV1

       SUBROUTINE ZPTSV1( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: D(*)
         COMPLEX(WP), INTENT(INOUT) :: E(*), B(*)
         INTERFACE
           SUBROUTINE ZPTSV( N, NRHS, D, E, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: D(*)
             COMPLEX(WP), INTENT(INOUT) :: E(*), B(LDB,*)
           END SUBROUTINE ZPTSV
         END INTERFACE
         CALL ZPTSV( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE ZPTSV1
#endif

#ifdef __LASI_PTSVX
       SUBROUTINE SPTSVX1( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX, &
     &                     RCOND, FERR, BERR, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*), B(*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         REAL(WP), INTENT(INOUT) :: EF(*)
         INTERFACE
           SUBROUTINE SPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X,   &
     &                        LDX, RCOND, FERR, BERR, WORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: D(*)
             REAL(WP), INTENT(IN) :: E(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: DF(*)
             REAL(WP), INTENT(INOUT) :: EF(*)
           END SUBROUTINE SPTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,      &
     &                RCOND, LFERR, LBERR, WORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SPTSVX1

       SUBROUTINE DPTSVX1( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX, &
     &                     RCOND, FERR, BERR, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*), B(*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         REAL(WP), INTENT(INOUT) :: EF(*)
         INTERFACE
           SUBROUTINE DPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X,   &
     &                        LDX, RCOND, FERR, BERR, WORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: D(*)
             REAL(WP), INTENT(IN) :: E(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: DF(*)
             REAL(WP), INTENT(INOUT) :: EF(*)
           END SUBROUTINE DPTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,      &
     &                RCOND, LFERR, LBERR, WORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DPTSVX1

       SUBROUTINE CPTSVX1( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX, &
     &                     RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(IN) :: E(*), B(*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         COMPLEX(WP), INTENT(INOUT) :: EF(*)
         INTERFACE
           SUBROUTINE CPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X,   &
     &                        LDX, RCOND, FERR, BERR, WORK, RWORK,      &
     &                        INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: D(*)
             COMPLEX(WP), INTENT(IN) :: E(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: DF(*)
             COMPLEX(WP), INTENT(INOUT) :: EF(*)
           END SUBROUTINE CPTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,      &
     &                RCOND, LFERR, LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CPTSVX1

       SUBROUTINE ZPTSVX1( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX, &
     &                     RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(IN) :: E(*), B(*)
         REAL(WP), INTENT(INOUT) :: DF(*)
         COMPLEX(WP), INTENT(INOUT) :: EF(*)
         INTERFACE
           SUBROUTINE ZPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X,   &
     &                        LDX, RCOND, FERR, BERR, WORK, RWORK,      &
     &                        INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: D(*)
             COMPLEX(WP), INTENT(IN) :: E(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: DF(*)
             COMPLEX(WP), INTENT(INOUT) :: EF(*)
           END SUBROUTINE ZPTSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,      &
     &                RCOND, LFERR, LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZPTSVX1
#endif

#ifdef __LASI_SYSV
       SUBROUTINE SSYSV1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE SSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE SSYSV
         END INTERFACE
         CALL SSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK,  &
     &               INFO )
      END SUBROUTINE SSYSV1

       SUBROUTINE DSYSV1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE DSYSV
         END INTERFACE
         CALL DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK,  &
     &               INFO )
      END SUBROUTINE DSYSV1

       SUBROUTINE CSYSV1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE CSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE CSYSV
         END INTERFACE
         CALL CSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK,  &
     &               INFO )
      END SUBROUTINE CSYSV1

       SUBROUTINE ZSYSV1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE ZSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE ZSYSV
         END INTERFACE
         CALL ZSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK,  &
     &               INFO )
      END SUBROUTINE ZSYSV1
#endif

#ifdef __LASI_HESV
       SUBROUTINE CHESV1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE CHESV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE CHESV
         END INTERFACE
         CALL CHESV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK,  &
     &               INFO )
      END SUBROUTINE CHESV1

       SUBROUTINE ZHESV1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE ZHESV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE ZHESV
         END INTERFACE
         CALL ZHESV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK,  &
     &               INFO )
      END SUBROUTINE ZHESV1
#endif

#ifdef __LASI_SYSVX
       SUBROUTINE SSYSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &                     B, LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                     LWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(*)
         REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
         INTERFACE
           SUBROUTINE SSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                        WORK, LWORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
           END SUBROUTINE SSYSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,   &
     &                LDB, X, LDX, RCOND, LFERR, LBERR, WORK, LWORK,    &
     &                IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SSYSVX1

       SUBROUTINE DSYSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &                     B, LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                     LWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: A(LDA,*), B(*)
         REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
         INTERFACE
           SUBROUTINE DSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                        WORK, LWORK, IWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: AF(LDAF,*)
           END SUBROUTINE DSYSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,   &
     &                LDB, X, LDX, RCOND, LFERR, LBERR, WORK, LWORK,    &
     &                IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DSYSVX1

       SUBROUTINE CSYSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &                     B, LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                     LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
         INTERFACE
           SUBROUTINE CSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                        WORK, LWORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
           END SUBROUTINE CSYSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,   &
     &                LDB, X, LDX, RCOND, LFERR, LBERR, WORK, LWORK,    &
     &                RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CSYSVX1

       SUBROUTINE ZSYSVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &                     B, LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                     LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
         INTERFACE
           SUBROUTINE ZSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                        WORK, LWORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
           END SUBROUTINE ZSYSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,   &
     &                LDB, X, LDX, RCOND, LFERR, LBERR, WORK, LWORK,    &
     &                RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZSYSVX1
#endif

#ifdef __LASI_HESVX
       SUBROUTINE CHESVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &                     B, LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                     LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
         INTERFACE
           SUBROUTINE CHESVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                        WORK, LWORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE CHESVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CHESVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,   &
     &                LDB, X, LDX, RCOND, LFERR, LBERR, WORK, LWORK,    &
     &                RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CHESVX1

       SUBROUTINE ZHESVX1( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &                     B, LDB, X, LDX, RCOND, FERR, BERR, WORK,     &
     &                     LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
         INTERFACE
           SUBROUTINE ZHESVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF,    &
     &                        IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,  &
     &                        WORK, LWORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N, LWORK
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(INOUT) :: AF(LDAF,*)
      END SUBROUTINE ZHESVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZHESVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,   &
     &                LDB, X, LDX, RCOND, LFERR, LBERR, WORK, LWORK,    &
     &                RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZHESVX1
#endif

#ifdef __LASI_SPSV
       SUBROUTINE SSPSV1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE SSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE SSPSV
         END INTERFACE
         CALL SSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE SSPSV1

       SUBROUTINE DSPSV1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE DSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             REAL(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE DSPSV
         END INTERFACE
         CALL DSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE DSPSV1

       SUBROUTINE CSPSV1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE CSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE CSPSV
         END INTERFACE
         CALL CSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE CSPSV1

       SUBROUTINE ZSPSV1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE ZSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE ZSPSV
         END INTERFACE
         CALL ZSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE ZSPSV1
#endif

#ifdef __LASI_HPSV
       SUBROUTINE CHPSV1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE CHPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE CHPSV
         END INTERFACE
         CALL CHPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE CHPSV1

       SUBROUTINE ZHPSV1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: NRHS, N, LDB
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AP(*), B(*)
         INTERFACE
           SUBROUTINE ZHPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO
             INTEGER, INTENT(IN) :: NRHS, N, LDB
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(IN) :: IPIV(*)
             COMPLEX(WP), INTENT(INOUT) :: AP(*), B(LDB,*)
           END SUBROUTINE ZHPSV
         END INTERFACE
         CALL ZHPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE ZHPSV1
#endif

#ifdef __LASI_SPSVX
       SUBROUTINE SSPSVX1( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
     &                     LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: A(*), B(*)
         REAL(WP), INTENT(INOUT) :: AF(*)
         INTERFACE
           SUBROUTINE SSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
     &                        X, LDX, RCOND, FERR, BERR, WORK, IWORK,   &
     &                        INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: A(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: AF(*)
           END SUBROUTINE SSPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL SSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
     &                RCOND, LFERR, LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE SSPSVX1

       SUBROUTINE DSPSVX1( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
     &                     LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IWORK(*)
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR
         REAL(WP), INTENT(OUT) :: X(*), WORK(*)
         REAL(WP), INTENT(IN) :: A(*), B(*)
         REAL(WP), INTENT(INOUT) :: AF(*)
         INTERFACE
           SUBROUTINE DSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
     &                        X, LDX, RCOND, FERR, BERR, WORK, IWORK,   &
     &                        INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(OUT) :: IWORK(*)
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
             REAL(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             REAL(WP), INTENT(IN) :: A(*), B(LDB,*)
             REAL(WP), INTENT(INOUT) :: AF(*)
           END SUBROUTINE DSPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL DSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
     &                RCOND, LFERR, LBERR, WORK, IWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE DSPSVX1

       SUBROUTINE CSPSVX1( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
     &                     LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: AF(*)
         INTERFACE
           SUBROUTINE CSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
     &                        X, LDX, RCOND, FERR, BERR, WORK, RWORK,   &
     &                        INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: A(*), B(LDB,*)
             COMPLEX(WP), INTENT(INOUT) :: AF(*)
           END SUBROUTINE CSPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
     &                RCOND, LFERR, LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CSPSVX1

       SUBROUTINE ZSPSVX1( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
     &                     LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: AF(*)
         INTERFACE
           SUBROUTINE ZSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
     &                        X, LDX, RCOND, FERR, BERR, WORK, RWORK,   &
     &                        INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: A(*), B(LDB,*)
             COMPLEX(WP), INTENT(INOUT) :: AF(*)
           END SUBROUTINE ZSPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZSPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
     &                RCOND, LFERR, LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZSPSVX1
#endif

#ifdef __LASI_HPSVX
       SUBROUTINE CHPSVX1( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
     &                     LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: AF(*)
         INTERFACE
           SUBROUTINE CHPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
     &                        X, LDX, RCOND, FERR, BERR, WORK, RWORK,   &
     &                        INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: A(*), B(LDB,*)
             COMPLEX(WP), INTENT(INOUT) :: AF(*)
           END SUBROUTINE CHPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL CHPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
     &                RCOND, LFERR, LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE CHPSVX1

       SUBROUTINE ZHPSVX1( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, &
     &                     LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
         INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: RCOND
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: X(*), WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: AF(*)
         INTERFACE
           SUBROUTINE ZHPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, &
     &                        X, LDX, RCOND, FERR, BERR, WORK, RWORK,   &
     &                        INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: UPLO, FACT
             INTEGER, INTENT(IN) :: LDB, LDX, NRHS, N
             INTEGER, INTENT(OUT) :: INFO
             INTEGER, INTENT(INOUT) :: IPIV(*)
             REAL(WP), INTENT(OUT) :: RCOND
             REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: X(LDX,*), WORK(*)
             COMPLEX(WP), INTENT(IN) :: A(*), B(LDB,*)
             COMPLEX(WP), INTENT(INOUT) :: AF(*)
           END SUBROUTINE ZHPSVX
         END INTERFACE
         REAL(WP) :: LFERR(1), LBERR(1)
         CALL ZHPSVX( FACT, UPLO, N, NRHS, A, AF, IPIV, B, LDB, X, LDX, &
     &                RCOND, LFERR, LBERR, WORK, RWORK, INFO )
         FERR = LFERR(1); BERR = LBERR(1)
      END SUBROUTINE ZHPSVX1
#endif

#ifdef __LASI_GELS
       SUBROUTINE SGELS1( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,      &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE SGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,   &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE SGELS
         END INTERFACE
         CALL SGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,    &
     &               INFO )
      END SUBROUTINE SGELS1

       SUBROUTINE DGELS1( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,      &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,   &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE DGELS
         END INTERFACE
         CALL DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,    &
     &               INFO )
      END SUBROUTINE DGELS1

       SUBROUTINE CGELS1( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,      &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE CGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,   &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE CGELS
         END INTERFACE
         CALL CGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,    &
     &               INFO )
      END SUBROUTINE CGELS1

       SUBROUTINE ZGELS1( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,      &
     &                    LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE ZGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK,   &
     &                       LWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             CHARACTER(LEN=1), INTENT(IN) :: TRANS
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE ZGELS
         END INTERFACE
         CALL ZGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,    &
     &               INFO )
      END SUBROUTINE ZGELS1
#endif

#ifdef __LASI_GELSY
       SUBROUTINE SGELSY1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, INFO )
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          INTEGER, INTENT(INOUT) :: JPVT(*)
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
          REAL(WP), INTENT(OUT) ::  WORK(*)
          INTERFACE
           SUBROUTINE SGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, LWORK, INFO )
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          INTEGER, INTENT(INOUT) :: JPVT(*)
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) ::  WORK(*)
         END SUBROUTINE SGELSY
        END INTERFACE
         CALL SGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, LWORK, INFO )
       END SUBROUTINE SGELSY1
       SUBROUTINE DGELSY1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, INFO )
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          INTEGER, INTENT(INOUT) :: JPVT(*)
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
          REAL(WP), INTENT(OUT) ::  WORK(*)
          INTERFACE
           SUBROUTINE DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, LWORK, INFO )
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
          INTEGER, INTENT(OUT) :: INFO, RANK
          INTEGER, INTENT(INOUT) :: JPVT(*)
          REAL(WP), INTENT(IN) :: RCOND
          REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
          REAL(WP), INTENT(OUT) ::  WORK(*)
         END SUBROUTINE DGELSY
        END INTERFACE
         CALL DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, LWORK, INFO )
       END SUBROUTINE DGELSY1
       SUBROUTINE CGELSY1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(OUT) ::  WORK(*)
         REAL(WP) :: RWORK(*)
         INTERFACE
           SUBROUTINE CGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) ::  WORK(*)
         REAL(WP) :: RWORK(*)
        END SUBROUTINE CGELSY
       END INTERFACE
         CALL CGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, LWORK, RWORK, INFO )
       END SUBROUTINE CGELSY1
       SUBROUTINE ZGELSY1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(OUT) ::  WORK(*)
         REAL(WP) :: RWORK(*)
         INTERFACE
           SUBROUTINE ZGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
         COMPLEX(WP), INTENT(OUT) ::  WORK(*)
         REAL(WP) :: RWORK(*)
        END SUBROUTINE ZGELSY
       END INTERFACE
         CALL ZGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, LWORK, RWORK, INFO )
       END SUBROUTINE ZGELSY1
#endif

#ifdef __LASI_GELSD
       SUBROUTINE SGELSD1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND 
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTEGER :: IWORK(*)  
         INTERFACE
           SUBROUTINE SGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, IWORK, INFO )
           USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
           INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
           INTEGER, INTENT(OUT) :: INFO, RANK
           REAL(WP), INTENT(IN) :: RCOND
           REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           REAL(WP), INTENT(OUT) :: S(*)
           REAL(WP), INTENT(OUT) :: WORK(*)
           INTEGER :: IWORK(*)
         END SUBROUTINE SGELSD
       END INTERFACE
         CALL SGELSD ( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK,&
     &                 LWORK, IWORK, INFO )
       END SUBROUTINE SGELSD1
       SUBROUTINE DGELSD1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND 
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTEGER :: IWORK(*)  
         INTERFACE
           SUBROUTINE DGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, IWORK, INFO )
           USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
           INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
           INTEGER, INTENT(OUT) :: INFO, RANK
           REAL(WP), INTENT(IN) :: RCOND
           REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           REAL(WP), INTENT(OUT) :: S(*)
           REAL(WP), INTENT(OUT) :: WORK(*)
           INTEGER :: IWORK(*)
         END SUBROUTINE DGELSD
       END INTERFACE
         CALL DGELSD ( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK,&
     &                 LWORK, IWORK, INFO )
       END SUBROUTINE DGELSD1
       SUBROUTINE CGELSD1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, RWORK, IWORK, INFO )
       USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
       INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
       INTEGER, INTENT(OUT) :: INFO, RANK
       REAL(WP), INTENT(IN) :: RCOND
       COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
       REAL(WP), INTENT(OUT) :: S(*)
       COMPLEX(WP), INTENT(OUT) :: WORK(*)
       INTEGER :: IWORK(*)
       REAL(WP) :: RWORK(*)
       INTERFACE
           SUBROUTINE CGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, RWORK, IWORK, INFO )
           USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
           INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
           INTEGER, INTENT(OUT) :: INFO, RANK
           REAL(WP), INTENT(IN) :: RCOND
           COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           REAL(WP), INTENT(OUT) :: S(*)
           COMPLEX(WP), INTENT(OUT) :: WORK(*)
           INTEGER :: IWORK(*)
           REAL(WP) :: RWORK(*)
         END SUBROUTINE CGELSD
       END INTERFACE
         CALL CGELSD ( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK,&
     &                 LWORK, RWORK, IWORK, INFO )
      END SUBROUTINE CGELSD1
       SUBROUTINE ZGELSD1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, RWORK, IWORK, INFO )
       USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
       INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
       INTEGER, INTENT(OUT) :: INFO, RANK
       REAL(WP), INTENT(IN) :: RCOND
       COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
       REAL(WP), INTENT(OUT) :: S(*)
       COMPLEX(WP), INTENT(OUT) :: WORK(*)
       INTEGER :: IWORK(*)
       REAL(WP) :: RWORK(*)
       INTERFACE
           SUBROUTINE ZGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, RWORK, IWORK, INFO )
           USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
           INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
           INTEGER, INTENT(OUT) :: INFO, RANK
           REAL(WP), INTENT(IN) :: RCOND
           COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
           REAL(WP), INTENT(OUT) :: S(*)
           COMPLEX(WP), INTENT(OUT) :: WORK(*)
           INTEGER :: IWORK(*)
           REAL(WP) :: RWORK(*)
         END SUBROUTINE ZGELSD
       END INTERFACE
         CALL ZGELSD ( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK,&
     &                 LWORK, RWORK, IWORK, INFO )
      END SUBROUTINE ZGELSD1
#endif

#ifdef __LASI_GELSX
       SUBROUTINE SGELSX1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE SGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
             INTEGER, INTENT(OUT) :: INFO, RANK
             INTEGER, INTENT(INOUT) :: JPVT(*)
             REAL(WP), INTENT(IN) :: RCOND
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE SGELSX
         END INTERFACE
         CALL SGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, INFO )
      END SUBROUTINE SGELSX1

       SUBROUTINE DGELSX1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
             INTEGER, INTENT(OUT) :: INFO, RANK
             INTEGER, INTENT(INOUT) :: JPVT(*)
             REAL(WP), INTENT(IN) :: RCOND
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE DGELSX
         END INTERFACE
         CALL DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, INFO )
      END SUBROUTINE DGELSX1

       SUBROUTINE CGELSX1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE CGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
             INTEGER, INTENT(OUT) :: INFO, RANK
             INTEGER, INTENT(INOUT) :: JPVT(*)
             REAL(WP), INTENT(IN) :: RCOND
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE CGELSX
         END INTERFACE
         CALL CGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, RWORK, INFO )
      END SUBROUTINE CGELSX1

       SUBROUTINE ZGELSX1( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,     &
     &                     RANK, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
         INTEGER, INTENT(OUT) :: INFO, RANK
         INTEGER, INTENT(INOUT) :: JPVT(*)
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE ZGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,  &
     &                        RANK, WORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB
             INTEGER, INTENT(OUT) :: INFO, RANK
             INTEGER, INTENT(INOUT) :: JPVT(*)
             REAL(WP), INTENT(IN) :: RCOND
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE ZGELSX
         END INTERFACE
         CALL ZGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,    &
     &                WORK, RWORK, INFO )
      END SUBROUTINE ZGELSX1
#endif

#ifdef __LASI_GELSS
       SUBROUTINE SGELSS1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE SGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO, RANK
             REAL(WP), INTENT(IN) :: RCOND
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: S(*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE SGELSS
         END INTERFACE
         CALL SGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK, &
     &                LWORK, INFO )
      END SUBROUTINE SGELSS1

       SUBROUTINE DGELSS1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         REAL(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO, RANK
             REAL(WP), INTENT(IN) :: RCOND
             REAL(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: S(*)
             REAL(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE DGELSS
         END INTERFACE
         CALL DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK, &
     &                LWORK, INFO )
      END SUBROUTINE DGELSS1

       SUBROUTINE CGELSS1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE CGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO, RANK
             REAL(WP), INTENT(IN) :: RCOND
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: S(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE CGELSS
         END INTERFACE
         CALL CGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK, &
     &                LWORK, RWORK, INFO )
      END SUBROUTINE CGELSS1

       SUBROUTINE ZGELSS1( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,  &
     &                     WORK, LWORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
         INTEGER, INTENT(OUT) :: INFO, RANK
         REAL(WP), INTENT(IN) :: RCOND
         COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(*)
         REAL(WP), INTENT(OUT) :: S(*), RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
           SUBROUTINE ZGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND,     &
     &                        RANK, WORK, LWORK, RWORK, INFO )
             USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
             INTEGER, INTENT(IN) :: NRHS, M, N, LDA, LDB, LWORK
             INTEGER, INTENT(OUT) :: INFO, RANK
             REAL(WP), INTENT(IN) :: RCOND
             COMPLEX(WP), INTENT(INOUT) :: A(LDA,*), B(LDB,*)
             REAL(WP), INTENT(OUT) :: S(*), RWORK(*)
             COMPLEX(WP), INTENT(OUT) :: WORK(*)
           END SUBROUTINE ZGELSS
         END INTERFACE
         CALL ZGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK, WORK, &
     &                LWORK, RWORK, INFO )
      END SUBROUTINE ZGELSS1
#endif

#ifdef __LASI_GETRS
       SUBROUTINE SGETRS1( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: TRANS
          INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
          INTEGER, INTENT(OUT) :: INFO
          INTEGER, INTENT(IN) :: PIV(*)
          REAL(WP), INTENT(IN) :: A(LDA,*)
          REAL(WP), INTENT(INOUT) :: B(*)
          INTERFACE
             SUBROUTINE SGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB,    &
     &                          INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: PIV(*)
               REAL(WP), INTENT(IN) :: A(LDA,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE SGETRS
          END INTERFACE
          CALL SGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
       END SUBROUTINE SGETRS1
       SUBROUTINE DGETRS1( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: TRANS
          INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
          INTEGER, INTENT(OUT) :: INFO
          INTEGER, INTENT(IN) :: PIV(*)
          REAL(WP), INTENT(IN) :: A(LDA,*)
          REAL(WP), INTENT(INOUT) :: B(*)
          INTERFACE
             SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB,    &
     &                          INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: PIV(*)
               REAL(WP), INTENT(IN) :: A(LDA,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DGETRS
          END INTERFACE
          CALL DGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
       END SUBROUTINE DGETRS1
       SUBROUTINE CGETRS1( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
          USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: TRANS
          INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
          INTEGER, INTENT(OUT) :: INFO
          INTEGER, INTENT(IN) :: PIV(*)
          COMPLEX(WP), INTENT(IN) :: A(LDA,*)
          COMPLEX(WP), INTENT(INOUT) :: B(*)
          INTERFACE
             SUBROUTINE CGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB,    &
     &                          INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: PIV(*)
               COMPLEX(WP), INTENT(IN) :: A(LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE CGETRS
          END INTERFACE
          CALL CGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
       END SUBROUTINE CGETRS1
       SUBROUTINE ZGETRS1( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
          USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
          CHARACTER(LEN=1), INTENT(IN) :: TRANS
          INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
          INTEGER, INTENT(OUT) :: INFO
          INTEGER, INTENT(IN) :: PIV(*)
          COMPLEX(WP), INTENT(IN) :: A(LDA,*)
          COMPLEX(WP), INTENT(INOUT) :: B(*)
          INTERFACE
             SUBROUTINE ZGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB,    &
     &                          INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDA, LDB, NRHS, N
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: PIV(*)
               COMPLEX(WP), INTENT(IN) :: A(LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE ZGETRS
          END INTERFACE
          CALL ZGETRS( TRANS, N, NRHS, A, LDA, PIV, B, LDB, INFO )
       END SUBROUTINE ZGETRS1
#endif

#ifdef __LASI_GERFS
       SUBROUTINE SGERFS1( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B,    &
     &                     LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
           USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
           CHARACTER(LEN=1), INTENT(IN) :: TRANS
           INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
           INTEGER, INTENT(OUT) :: INFO
           INTEGER, INTENT(IN) :: PIV(*)
           INTEGER, INTENT(OUT) :: IWORK(*)
           REAL(WP), INTENT(OUT) :: FERR, BERR
           REAL(WP), INTENT(OUT) :: WORK(*)
           REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(*)
           REAL(WP), INTENT(INOUT) :: X(*)
           INTERFACE
              SUBROUTINE SGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, &
     &                           B, LDB, X, LDX, FERR, BERR, WORK,      &
     &                           IWORK, INFO )
                 USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
                 CHARACTER(LEN=1), INTENT(IN) :: TRANS
                 INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
                 INTEGER, INTENT(OUT) :: INFO
                 INTEGER, INTENT(IN) :: PIV(*)
                 INTEGER, INTENT(OUT) :: IWORK(*)
                 REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
                 REAL(WP), INTENT(OUT) :: WORK(*)
                 REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
                 REAL(WP), INTENT(INOUT) :: X(LDX,*)
              END SUBROUTINE SGERFS
           END INTERFACE
           REAL(WP) FERR1(1), BERR1(1)
           CALL SGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,  &
     &                  X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
           FERR = FERR1(1); BERR = BERR1(1)
        END SUBROUTINE SGERFS1

       SUBROUTINE DGERFS1( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B,    &
     &                     LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
           USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
           CHARACTER(LEN=1), INTENT(IN) :: TRANS
           INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
           INTEGER, INTENT(OUT) :: INFO
           INTEGER, INTENT(IN) :: PIV(*)
           INTEGER, INTENT(OUT) :: IWORK(*)
           REAL(WP), INTENT(OUT) :: FERR, BERR
           REAL(WP), INTENT(OUT) :: WORK(*)
           REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(*)
           REAL(WP), INTENT(INOUT) :: X(*)
           INTERFACE
              SUBROUTINE DGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, &
     &                           B, LDB, X, LDX, FERR, BERR, WORK,      &
     &                           IWORK, INFO )
                 USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
                 CHARACTER(LEN=1), INTENT(IN) :: TRANS
                 INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
                 INTEGER, INTENT(OUT) :: INFO
                 INTEGER, INTENT(IN) :: PIV(*)
                 INTEGER, INTENT(OUT) :: IWORK(*)
                 REAL(WP), INTENT(OUT) :: FERR(*), BERR(*)
                 REAL(WP), INTENT(OUT) :: WORK(*)
                 REAL(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
                 REAL(WP), INTENT(INOUT) :: X(LDX,*)
              END SUBROUTINE DGERFS
           END INTERFACE
           REAL(WP) FERR1(1), BERR1(1)
           CALL DGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB,  &
     &                  X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
           FERR = FERR1(1); BERR = BERR1(1)
        END SUBROUTINE DGERFS1

       SUBROUTINE CGERFS1( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B,    &
     &                     LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         INTERFACE
             SUBROUTINE CGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                          B, LDB, X, LDX, FERR, BERR, WORK, RWORK,&
     &                          INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: PIV(*)
               REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
               COMPLEX(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
            END SUBROUTINE CGERFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
            CALL CGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB, &
     &                   X, LDX, FERR1, BERR1, WORK, RWORK, INFO )
           FERR = FERR1(1); BERR = BERR1(1)
        END SUBROUTINE CGERFS1

       SUBROUTINE ZGERFS1( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B,    &
     &                     LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: PIV(*)
         REAL(WP), INTENT(OUT) :: FERR, BERR, RWORK(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         INTERFACE
             SUBROUTINE ZGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV,  &
     &                          B, LDB, X, LDX, FERR, BERR, WORK, RWORK,&
     &                          INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, NRHS, N
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: PIV(*)
               REAL(WP), INTENT(OUT) :: FERR(*), BERR(*), RWORK(*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
               COMPLEX(WP), INTENT(IN) :: A(LDA,*), AF(LDAF,*), B(LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
            END SUBROUTINE ZGERFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
            CALL ZGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, PIV, B, LDB, &
     &                   X, LDX, FERR1, BERR1, WORK, RWORK, INFO )
           FERR = FERR1(1); BERR = BERR1(1)
        END SUBROUTINE ZGERFS1
#endif

#ifdef __LASI_GBTRS
      SUBROUTINE SGBTRS1( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B,    &
     &                    LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AB( LDAB,*), B(*)
         INTERFACE
            SUBROUTINE SGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV,  &
     &                         B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(INOUT) :: IPIV(*)
               REAL(WP), INTENT(INOUT) :: AB( LDAB,*), B(LDB,*)
            END SUBROUTINE SGBTRS
         END INTERFACE
         CALL SGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,   &
     &                INFO )
      END SUBROUTINE SGBTRS1
      SUBROUTINE DGBTRS1( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B,    &
     &                    LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         REAL(WP), INTENT(INOUT) :: AB( LDAB,*), B(*)
         INTERFACE
            SUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV,  &
     &                         B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(INOUT) :: IPIV(*)
               REAL(WP), INTENT(INOUT) :: AB( LDAB,*), B(LDB,*)
            END SUBROUTINE DGBTRS
         END INTERFACE
         CALL DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,   &
     &                INFO )
      END SUBROUTINE DGBTRS1
      SUBROUTINE CGBTRS1( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B,    &
     &                    LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AB( LDAB,*), B(*)
         INTERFACE
            SUBROUTINE CGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV,  &
     &                         B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(INOUT) :: IPIV(*)
               COMPLEX(WP), INTENT(INOUT) :: AB( LDAB,*), B(LDB,*)
            END SUBROUTINE CGBTRS
         END INTERFACE
         CALL CGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,   &
     &                INFO )
      END SUBROUTINE CGBTRS1
      SUBROUTINE ZGBTRS1( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B,    &
     &                    LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(INOUT) :: IPIV(*)
         COMPLEX(WP), INTENT(INOUT) :: AB( LDAB,*), B(*)
         INTERFACE
            SUBROUTINE ZGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV,  &
     &                         B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(INOUT) :: IPIV(*)
               COMPLEX(WP), INTENT(INOUT) :: AB( LDAB,*), B(LDB,*)
            END SUBROUTINE ZGBTRS
         END INTERFACE
         CALL ZGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB,   &
     &                INFO )
      END SUBROUTINE ZGBTRS1
#endif

#ifdef __LASI_GBRFS
      SUBROUTINE SGBRFS1( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         INTERFACE
            SUBROUTINE SGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,   &
     &                         LDAFB, IPIV, B, LDB, X, LDX, FERR, BERR, &
     &                         WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, &
     &                                NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               INTEGER, INTENT(OUT) :: IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*),      &
     &                                 B( LDB,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
            END SUBROUTINE SGBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL SGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SGBRFS1
      SUBROUTINE DGBRFS1( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         INTEGER, INTENT(OUT) :: IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         INTERFACE
            SUBROUTINE DGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,   &
     &                         LDAFB, IPIV, B, LDB, X, LDX, FERR, BERR, &
     &                         WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, &
     &                                NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               INTEGER, INTENT(OUT) :: IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*),      &
     &                                 B( LDB,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
            END SUBROUTINE DGBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL DGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DGBRFS1

      SUBROUTINE CGBRFS1( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         INTERFACE
            SUBROUTINE CGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,   &
     &                         LDAFB, IPIV, B, LDB, X, LDX, FERR, BERR, &
     &                         WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, &
     &                                NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*),   &
     &                                    B( LDB,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
            END SUBROUTINE CGBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL CGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, RWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CGBRFS1
      SUBROUTINE ZGBRFS1( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*), B(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         INTERFACE
            SUBROUTINE ZGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,   &
     &                         LDAFB, IPIV, B, LDB, X, LDX, FERR, BERR, &
     &                         WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: KL, KU, LDAB, LDAFB, LDB, LDX, N, &
     &                                NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AB( LDAB,*), AFB( LDAFB,*),   &
     &                                    B( LDB,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
            END SUBROUTINE ZGBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL ZGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, RWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZGBRFS1
#endif

#ifdef __LASI_GTTRS
      SUBROUTINE SGTTRS1( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
     &                         LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(OUT) :: IPIV(*)
               REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SGTTRS
         END INTERFACE
         CALL SGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
     &                INFO )
      END SUBROUTINE SGTTRS1
      SUBROUTINE DGTTRS1( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
     &                         LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(OUT) :: IPIV(*)
               REAL(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DGTTRS
         END INTERFACE
         CALL DGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
     &                INFO )
      END SUBROUTINE DGTTRS1
      SUBROUTINE CGTTRS1( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
     &                         LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(OUT) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE CGTTRS
         END INTERFACE
         CALL CGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
     &                INFO )
      END SUBROUTINE CGTTRS1
      SUBROUTINE ZGTTRS1( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(OUT) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, &
     &                         LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(OUT) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: D(*), DL(*), DU(*), DU2(*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE ZGTTRS
         END INTERFACE
         CALL ZGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB,     &
     &                INFO )
      END SUBROUTINE ZGTTRS1
#endif

#ifdef __LASI_GTRFS
      SUBROUTINE SGTRFS1( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),      &
     &                           DU(*), DU2(*), DUF(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
     &                         DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
     &                         WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*),   &
     &                                 DLF(*), DU(*), DU2(*), DUF(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SGTRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL SGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SGTRFS1

      SUBROUTINE DGTRFS1( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, IWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),      &
     &                           DU(*), DU2(*), DUF(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
     &                         DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
     &                         WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*),   &
     &                                 DLF(*), DU(*), DU2(*), DUF(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DGTRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL DGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, IWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DGTRFS1

      SUBROUTINE CGTRFS1( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),   &
     &                              DU(*), DU2(*), DUF(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
     &                         DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
     &                         WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*),&
     &                                    DLF(*), DU(*), DU2(*), DUF(*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CGTRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL CGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, RWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CGTRFS1

      SUBROUTINE ZGTRFS1( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2, &
     &                    IPIV, B, LDB, X, LDX, FERR, BERR, WORK, RWORK,&
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: TRANS
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: B(*), D(*), DF(*), DL(*), DLF(*),   &
     &                              DU(*), DU2(*), DUF(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, &
     &                         DU2, IPIV, B, LDB, X, LDX, FERR, BERR,   &
     &                         WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: TRANS
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS, IPIV(*)
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: B( LDB,*), D(*), DF(*), DL(*),&
     &                                    DLF(*), DU(*), DU2(*), DUF(*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZGTRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL ZGTRFS( TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF, DU2,     &
     &                IPIV, B, LDB, X, LDX, FERR1, BERR1, WORK, RWORK,  &
     &                INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZGTRFS1
#endif

#ifdef __LASI_POTRS
      SUBROUTINE SPOTRS1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: A( LDA,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SPOTRS
         END INTERFACE
         CALL SPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE SPOTRS1

      SUBROUTINE DPOTRS1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: A( LDA,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DPOTRS
         END INTERFACE
         CALL DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE DPOTRS1

      SUBROUTINE CPOTRS1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: A( LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE CPOTRS
         END INTERFACE
         CALL CPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE CPOTRS1

      SUBROUTINE ZPOTRS1( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: A( LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE ZPOTRS
         END INTERFACE
         CALL ZPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
      END SUBROUTINE ZPOTRS1
#endif

#ifdef __LASI_PORFS
      SUBROUTINE SPORFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: A(*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
     &                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*),          &
     &                                 B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SPORFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL SPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE SPORFS1

      SUBROUTINE DPORFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: A(*), AF( LDAF,*), B( LDB,*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
     &                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*),          &
     &                                 B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DPORFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL DPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE DPORFS1

      SUBROUTINE CPORFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
     &                    LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), AF( LDAF,*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
     &                         X, LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*),       &
     &                                    B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CPORFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL CPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
     &                FERR1, BERR1, WORK, RWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE CPORFS1

      SUBROUTINE ZPORFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X,   &
     &                    LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(*), AF( LDAF,*), B( LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, &
     &                         X, LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: A( LDA,*), AF( LDAF,*),       &
     &                                    B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZPORFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL ZPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,  &
     &                FERR1, BERR1, WORK, RWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE ZPORFS1
#endif

#ifdef __LASI_PPTRS
      SUBROUTINE SPPTRS1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SPPTRS
         END INTERFACE
         CALL SPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE SPPTRS1

      SUBROUTINE DPPTRS1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DPPTRS
         END INTERFACE
         CALL DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE DPPTRS1

      SUBROUTINE CPPTRS1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: AP(*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE CPPTRS
         END INTERFACE
         CALL CPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE CPPTRS1

      SUBROUTINE ZPPTRS1( UPLO, N, NRHS, AP, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: AP(*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE ZPPTRS
         END INTERFACE
         CALL ZPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE ZPPTRS1
#endif

#ifdef __LASI_PPRFS
      SUBROUTINE SPPRFS1( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR, &
     &                    BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX,  &
     &                         FERR, BERR, WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SPPRFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL SPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR1,    &
     &                BERR1, WORK, IWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE SPPRFS1

      SUBROUTINE DPPRFS1( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR, &
     &                    BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX,  &
     &                         FERR, BERR, WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DPPRFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL DPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR1,    &
     &                BERR1, WORK, IWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE DPPRFS1

      SUBROUTINE CPPRFS1( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR, &
     &                    BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX,  &
     &                         FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CPPRFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL CPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR1,    &
     &                BERR1, WORK, RWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE CPPRFS1

      SUBROUTINE ZPPRFS1( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR, &
     &                    BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX,  &
     &                         FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZPPRFS
         END INTERFACE
         REAL(WP) :: BERR1(1), FERR1(1)
         CALL ZPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR1,    &
     &                BERR1, WORK, RWORK, INFO )
         BERR = BERR1(1); FERR = FERR1(1)
      END SUBROUTINE ZPPRFS1
#endif

#ifdef __LASI_PBTRS
      SUBROUTINE SPBTRS1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AB( LDAB,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SPBTRS
         END INTERFACE
         CALL SPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE SPBTRS1

      SUBROUTINE DPBTRS1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB( LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AB( LDAB,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DPBTRS
         END INTERFACE
         CALL DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE DPBTRS1

      SUBROUTINE CPBTRS1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE CPBTRS
         END INTERFACE
         CALL CPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE CPBTRS1

      SUBROUTINE ZPBTRS1( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: AB( LDAB,*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE ZPBTRS
         END INTERFACE
         CALL ZPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
      END SUBROUTINE ZPBTRS1
#endif

#ifdef __LASI_PBRFS
      SUBROUTINE SPBRFS1( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,   &
     &                    LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, &
     &                         B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N,    &
     &                                 NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*),     &
     &                                  B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SPBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1) 
         CALL SPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B, LDB,  &
     &                X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SPBRFS1

      SUBROUTINE DPBRFS1( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,   &
     &                    LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, &
     &                         B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N,    &
     &                                 NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*),     &
     &                                  B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DPBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1) 
         CALL DPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B, LDB,  &
     &                X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DPBRFS1

      SUBROUTINE CPBRFS1( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,   &
     &                    LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, &
     &                         B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N,    &
     &                                 NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*),  &
     &                                     B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CPBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1) 
         CALL CPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B, LDB,  &
     &                X, LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CPBRFS1

      SUBROUTINE ZPBRFS1( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B,   &
     &                    LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, &
     &                         B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) ::  KD, LDAB, LDAFB, LDB, LDX, N,    &
     &                                 NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) ::  AB( LDAB,*), AFB( LDAFB,*),  &
     &                                     B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZPBRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1) 
         CALL ZPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B, LDB,  &
     &                X, LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZPBRFS1
#endif

#ifdef __LASI_PTTRS
      SUBROUTINE SPTTRS1( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*)
         REAL(WP), INTENT(OUT) :: B(*)
         INTERFACE
            SUBROUTINE SPTTRS( N, NRHS, D, E, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*)
               REAL(WP), INTENT(IN) :: E(*)
               REAL(WP), INTENT(OUT) :: B( LDB,*)
            END SUBROUTINE SPTTRS
         END INTERFACE
         CALL SPTTRS( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE SPTTRS1

      SUBROUTINE DPTTRS1( N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         REAL(WP), INTENT(IN) :: E(*)
         REAL(WP), INTENT(OUT) :: B(*)
         INTERFACE
            SUBROUTINE DPTTRS( N, NRHS, D, E, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*)
               REAL(WP), INTENT(IN) :: E(*)
               REAL(WP), INTENT(OUT) :: B( LDB,*)
            END SUBROUTINE DPTTRS
         END INTERFACE
         CALL DPTTRS( N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE DPTTRS1

      SUBROUTINE CPTTRS1( UPLO, N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(IN) :: E(*)
         COMPLEX(WP), INTENT(OUT) :: B(*)
         INTERFACE
            SUBROUTINE CPTTRS( UPLO, N, NRHS, D, E, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*)
               COMPLEX(WP), INTENT(IN) :: E(*)
               COMPLEX(WP), INTENT(OUT) :: B( LDB,*)
            END SUBROUTINE CPTTRS
         END INTERFACE
         CALL CPTTRS( UPLO, N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE CPTTRS1

      SUBROUTINE ZPTTRS1( UPLO, N, NRHS, D, E, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*)
         COMPLEX(WP), INTENT(IN) :: E(*)
         COMPLEX(WP), INTENT(OUT) :: B(*)
         INTERFACE
            SUBROUTINE ZPTTRS( UPLO, N, NRHS, D, E, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*)
               COMPLEX(WP), INTENT(IN) :: E(*)
               COMPLEX(WP), INTENT(OUT) :: B( LDB,*)
            END SUBROUTINE ZPTTRS
         END INTERFACE
         CALL ZPTTRS( UPLO, N, NRHS, D, E, B, LDB, INFO )
      END SUBROUTINE ZPTTRS1
#endif

#ifdef __LASI_PTRFS
      SUBROUTINE SPTRFS1( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR,  &
     &                    BERR, WORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: B(*), E(*), EF(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX,   &
     &                         FERR, BERR, WORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*), DF(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SPTRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL SPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR1,     &
     &                BERR1, WORK, INFO )
      FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SPTRFS1

      SUBROUTINE DPTRFS1( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR,  &
     &                    BERR, WORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: B(*), E(*), EF(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX,   &
     &                         FERR, BERR, WORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*), DF(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DPTRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DPTRFS( N, NRHS, D, E, DF, EF, B, LDB, X, LDX, FERR1,     &
     &                BERR1, WORK, INFO )
      FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DPTRFS1

      SUBROUTINE CPTRFS1( UPLO, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,  &
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: B(*), E(*), EF(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CPTRFS( UPLO, N, NRHS, D, E, DF, EF, B, LDB, X,  &
     &                         LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*), DF(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CPTRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL CPTRFS( UPLO, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,      &
     &                FERR1, BERR1, WORK, RWORK, INFO )
      FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CPTRFS1

      SUBROUTINE ZPTRFS1( UPLO, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,  &
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: D(*), DF(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: B(*), E(*), EF(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZPTRFS( UPLO, N, NRHS, D, E, DF, EF, B, LDB, X,  &
     &                         LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: D(*), DF(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: B( LDB,*), E(*), EF(*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZPTRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL ZPTRFS( UPLO, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,      &
     &                FERR1, BERR1, WORK, RWORK, INFO )
      FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZPTRFS1
#endif

#ifdef __LASI_SYTRS
      SUBROUTINE SSYTRS1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER , INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(IN) :: A( LDA,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE SSYTRS
         END INTERFACE
         CALL SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      END SUBROUTINE SSYTRS1

      SUBROUTINE DSYTRS1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: A( LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER , INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(IN) :: A( LDA,*)
               REAL(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE DSYTRS
         END INTERFACE
         CALL DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      END SUBROUTINE DSYTRS1

      SUBROUTINE CSYTRS1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER , INTENT(IN) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: A( LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE CSYTRS
         END INTERFACE
         CALL CSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      END SUBROUTINE CSYTRS1

      SUBROUTINE ZSYTRS1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER , INTENT(IN) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: A( LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE ZSYTRS
         END INTERFACE
         CALL ZSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      END SUBROUTINE ZSYTRS1
#endif

#ifdef __LASI_HETRS
      SUBROUTINE CHETRS1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CHETRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER , INTENT(IN) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: A( LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE CHETRS
         END INTERFACE
         CALL CHETRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      END SUBROUTINE CHETRS1

      SUBROUTINE ZHETRS1( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER , INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: A( LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZHETRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,     &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER , INTENT(IN) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: A( LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B( LDB,*)
            END SUBROUTINE ZHETRS
         END INTERFACE
         CALL ZHETRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
      END SUBROUTINE ZHETRS1
#endif

#ifdef __LASI_HERFS
      SUBROUTINE CHERFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CHERFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,&
     &                         LDB, X, LDX, FERR, BERR, WORK, RWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*),      &
     &                                     B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CHERFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL CHERFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
     &                LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CHERFS1

      SUBROUTINE ZHERFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZHERFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,&
     &                         LDB, X, LDX, FERR, BERR, WORK, RWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*),      &
     &                                     B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZHERFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL ZHERFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
     &                LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZHERFS1
#endif

#ifdef __LASI_SYRFS
      SUBROUTINE CSYRFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,&
     &                         LDB, X, LDX, FERR, BERR, WORK, RWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*),      &
     &                                     B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CSYRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL CSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
     &                LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CSYRFS1

      SUBROUTINE ZSYRFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,&
     &                         LDB, X, LDX, FERR, BERR, WORK, RWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*),      &
     &                                     B( LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X( LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZSYRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL ZSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
     &                LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZSYRFS1

      SUBROUTINE SSYRFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,&
     &                         LDB, X, LDX, FERR, BERR, WORK, IWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*),         &
     &                                  B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SSYRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL SSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
     &                LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SSYRFS1

      SUBROUTINE DSYRFS1( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB,&
     &                    X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,&
     &                         LDB, X, LDX, FERR, BERR, WORK, IWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDA, LDAF, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) ::  A( LDA,*), AF( LDAF,*),         &
     &                                  B( LDB,*)
               REAL(WP), INTENT(INOUT) :: X( LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DSYRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X, &
     &                LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DSYRFS1
#endif

#ifdef __LASI_SPTRS
      SUBROUTINE SSPTRS1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE SSPTRS
         ENDINTERFACE
         CALL SSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE SSPTRS1

      SUBROUTINE DSPTRS1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DSPTRS
         ENDINTERFACE
         CALL DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE DSPTRS1

      SUBROUTINE CSPTRS1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: AP(*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE CSPTRS
         ENDINTERFACE
         CALL CSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE CSPTRS1

      SUBROUTINE ZSPTRS1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: AP(*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE ZSPTRS
         ENDINTERFACE
         CALL ZSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE ZSPTRS1
#endif

#ifdef __LASI_HPTRS
      SUBROUTINE CHPTRS1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CHPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: AP(*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE CHPTRS
         END INTERFACE
         CALL CHPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE CHPTRS1

      SUBROUTINE ZHPTRS1( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZHPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               COMPLEX(WP), INTENT(IN) :: AP(*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE ZHPTRS
         END INTERFACE
         CALL ZHPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
      END SUBROUTINE ZHPTRS1
#endif

#ifdef __LASI_HPRFS
      SUBROUTINE CHPRFS1( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CHPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
     &                         LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CHPRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL CHPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,     &
     &                FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CHPRFS1

      SUBROUTINE ZHPRFS1( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZHPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
     &                         LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZHPRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL ZHPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,     &
     &                FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZHPRFS1
#endif

#ifdef __LASI_SPRFS
      SUBROUTINE CSPRFS1( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
     &                         LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CSPRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL CSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,     &
     &                FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CSPRFS1

      SUBROUTINE ZSPRFS1( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
     &                    FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
     &                         LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZSPRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL ZSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,     &
     &                FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZSPRFS1

      SUBROUTINE SSPRFS1( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE SSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
     &                         LDX, FERR, BERR, WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
               REAL(WP), INTENT(INOUT) :: X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE SSPRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL SSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,     &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE SSPRFS1

      SUBROUTINE DSPRFS1( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, &
     &                    FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         INTEGER, INTENT(IN) :: IPIV(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
         REAL(WP), INTENT(INOUT) :: X(LDX,*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, &
     &                         LDX, FERR, BERR, WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               INTEGER, INTENT(IN) :: IPIV(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AFP(*), AP(*), B(LDB,*)
               REAL(WP), INTENT(INOUT) :: X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DSPRFS
         END INTERFACE
         REAL(WP) :: FERR1(1), BERR1(1)
         CALL DSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,     &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DSPRFS1
#endif

#ifdef __LASI_TRTRS
      SUBROUTINE STRTRS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE STRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: A(LDA,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE STRTRS
         END INTERFACE
         CALL STRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,       &
     &                INFO )
      END SUBROUTINE STRTRS1

      SUBROUTINE DTRTRS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: A(LDA,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: A(LDA,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DTRTRS
         END INTERFACE
         CALL DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,       &
     &                INFO )
      END SUBROUTINE DTRTRS1

      SUBROUTINE CTRTRS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: A(LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE CTRTRS
         END INTERFACE
         CALL CTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,       &
     &                INFO )
      END SUBROUTINE CTRTRS1

      SUBROUTINE ZTRTRS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,   &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: A(LDA,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: A(LDA,*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE ZTRTRS
         END INTERFACE
         CALL ZTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,       &
     &                INFO )
      END SUBROUTINE ZTRTRS1
#endif

#ifdef __LASI_TRRFS
      SUBROUTINE STRRFS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,&
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: A(LDA,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE STRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, X, LDX, FERR, BERR, WORK, IWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
               REAL(WP), INTENT(INOUT) :: X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE STRRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL STRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,    &
     &                LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE STRRFS1

      SUBROUTINE DTRRFS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,&
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: A(LDA,*), B(*)
         REAL(WP), INTENT(INOUT) :: X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, X, LDX, FERR, BERR, WORK, IWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
               REAL(WP), INTENT(INOUT) :: X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DTRRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,    &
     &                LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DTRRFS1

      SUBROUTINE CTRRFS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,&
     &                    LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, X, LDX, FERR, BERR, WORK, RWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CTRRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL CTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,    &
     &                LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CTRRFS1

      SUBROUTINE ZTRRFS1( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,&
     &                    LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(*)
         COMPLEX(WP), INTENT(INOUT) :: X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B,   &
     &                         LDB, X, LDX, FERR, BERR, WORK, RWORK,    &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDA, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: A(LDA,*), B(LDB,*)
               COMPLEX(WP), INTENT(INOUT) :: X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZTRRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL ZTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,    &
     &                LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZTRRFS1
#endif

#ifdef __LASI_TPTRS
      SUBROUTINE STPTRS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,       &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE STPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE STPTRS
         END INTERFACE
         CALL STPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE STPTRS1

      SUBROUTINE DTPTRS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,       &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AP(*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AP(*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DTPTRS
         END INTERFACE
         CALL DTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE DTPTRS1

      SUBROUTINE CTPTRS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,       &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: AP(*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE CTPTRS
         END INTERFACE
         CALL CTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE CTPTRS1

      SUBROUTINE ZTPTRS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,       &
     &                    INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AP(*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: AP(*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE ZTPTRS
         END INTERFACE
         CALL ZTPTRS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, INFO )
      END SUBROUTINE ZTPTRS1
#endif

#ifdef __LASI_TPRFS
      SUBROUTINE STPRFS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X,    &
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AP(*), B(*), X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE STPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE STPRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL STPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,   &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE STPRFS1

      SUBROUTINE DTPRFS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X,    &
     &                    LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AP(*), B(*), X(*)
         REAL(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE DTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         X, LDX, FERR, BERR, WORK, IWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
               REAL(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE DTPRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,   &
     &                FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DTPRFS1

      SUBROUTINE CTPRFS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X,    &
     &                    LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AP(*), B(*), X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         X, LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CTPRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL CTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,   &
     &                FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CTPRFS1

      SUBROUTINE ZTPRFS1( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X,    &
     &                    LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AP(*), B(*), X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB,  &
     &                         X, LDX, FERR, BERR, WORK, RWORK, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AP(*), B(LDB,*), X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZTPRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL ZTPRFS( UPLO, TRANS, DIAG, N, NRHS, AP, B, LDB, X, LDX,   &
     &                FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZTPRFS1
#endif

#ifdef __LASI_TBTRS
      SUBROUTINE STBTRS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE STBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AB(LDAB,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE STBTRS
         END INTERFACE
         CALL STBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                INFO )
      END SUBROUTINE STBTRS1

      SUBROUTINE DTBTRS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(IN) :: AB(LDAB,*)
         REAL(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE DTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(IN) :: AB(LDAB,*)
               REAL(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE DTBTRS
         END INTERFACE
         CALL DTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                INFO )
      END SUBROUTINE DTBTRS1

      SUBROUTINE CTBTRS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE CTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: AB(LDAB,*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE CTBTRS
         END INTERFACE
         CALL CTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                INFO )
      END SUBROUTINE CTBTRS1

      SUBROUTINE ZTBTRS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*)
         COMPLEX(WP), INTENT(INOUT) :: B(*)
         INTERFACE
            SUBROUTINE ZTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               COMPLEX(WP), INTENT(IN) :: AB(LDAB,*)
               COMPLEX(WP), INTENT(INOUT) :: B(LDB,*)
            END SUBROUTINE ZTBTRS
         END INTERFACE
         CALL ZTBTRS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                INFO )
      END SUBROUTINE ZTBTRS1
#endif

#ifdef __LASI_TBRFS
      SUBROUTINE STBRFS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AB(LDAB,*), B(*), X(*)
         REAL(WP), INTENT(IN) :: WORK(*)
         INTERFACE
            SUBROUTINE STBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
               REAL(WP), INTENT(IN) :: WORK(*)
            END SUBROUTINE STBRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL STBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE STBRFS1

      SUBROUTINE DTBRFS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO, IWORK(*)
         REAL(WP), INTENT(OUT) :: BERR, FERR
         REAL(WP), INTENT(IN) :: AB(LDAB,*), B(*), X(*)
         REAL(WP), INTENT(IN) :: WORK(*)
         INTERFACE
            SUBROUTINE DTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, X, LDX, FERR, BERR, WORK, IWORK, &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO, IWORK(*)
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*)
               REAL(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
               REAL(WP), INTENT(IN) :: WORK(*)
            END SUBROUTINE DTBRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL DTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                X, LDX, FERR1, BERR1, WORK, IWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE DTBRFS1

      SUBROUTINE CTBRFS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*), B(*), X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE CTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => sgl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE CTBRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL CTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                X, LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE CTBRFS1

      SUBROUTINE ZTBRFS1( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B,  &
     &                    LDB, X, LDX, FERR, BERR, WORK, RWORK, INFO )
         USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
         CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
         INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
         INTEGER, INTENT(OUT) :: INFO
         REAL(WP), INTENT(OUT) :: BERR, FERR, RWORK(*)
         COMPLEX(WP), INTENT(IN) :: AB(LDAB,*), B(*), X(*)
         COMPLEX(WP), INTENT(OUT) :: WORK(*)
         INTERFACE
            SUBROUTINE ZTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB,&
     &                         B, LDB, X, LDX, FERR, BERR, WORK, RWORK, &
     &                         INFO )
               USE lasi_kinds, ONLY: WP => dbl
          IMPLICIT NONE
               CHARACTER(LEN=1), INTENT(IN) :: DIAG, TRANS, UPLO
               INTEGER, INTENT(IN) :: KD, LDAB, LDB, LDX, N, NRHS
               INTEGER, INTENT(OUT) :: INFO
               REAL(WP), INTENT(OUT) :: BERR(*), FERR(*), RWORK(*)
               COMPLEX(WP), INTENT(IN) :: AB(LDAB,*), B(LDB,*), X(LDX,*)
               COMPLEX(WP), INTENT(OUT) :: WORK(*)
            END SUBROUTINE ZTBRFS
         END INTERFACE
         REAL(WP) FERR1(1), BERR1(1)
         CALL ZTBRFS( UPLO, TRANS, DIAG, N, KD, NRHS, AB, LDAB, B, LDB, &
     &                X, LDX, FERR1, BERR1, WORK, RWORK, INFO )
         FERR = FERR1(1); BERR = BERR1(1)
      END SUBROUTINE ZTBRFS1
#endif

END MODULE lasi_module

