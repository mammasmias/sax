
!------------------------------------------------------------------------------!
! CONFIGURATION FILE FOR PTK 0.1.2
!------------------------------------------------------------------------------!
! The following lines map some commonly defined system macro to the internal
! ptk macros.
! Ptk macros which are not defined take their default values.
! See the manual for a list of iotk macros.

#ifndef __PTK_CONFIG_H
#define __PTK_CONFIG_H

#ifdef __PARA
#  define __PTK_MPI
#endif

#ifdef __AIX
#  define __PTK_LOGICAL1 1
#  define __PTK_MPI_LOGICAL1 MPI_LOGICAL1
#  define __PTK_LOGICAL2 2
#  define __PTK_MPI_LOGICAL2 MPI_LOGICAL2
#  define __PTK_LOGICAL3 4
#  define __PTK_MPI_LOGICAL3 MPI_LOGICAL4
#  define __PTK_LOGICAL4 8
#  define __PTK_MPI_LOGICAL4 MPI_LOGICAL8
#  define __PTK_INTEGER1 1
#  define __PTK_MPI_INTEGER1 MPI_INTEGER1
#  define __PTK_INTEGER2 2
#  define __PTK_MPI_INTEGER2 MPI_INTEGER2
#  define __PTK_INTEGER3 4
#  define __PTK_MPI_INTEGER3 MPI_INTEGER4
#  define __PTK_INTEGER4 8
#  define __PTK_MPI_INTEGER4 MPI_INTEGER8
#  define __PTK_REAL1    4
#  define __PTK_MPI_REAL1    MPI_REAL4
#  define __PTK_REAL2    8
#  define __PTK_MPI_REAL2    MPI_REAL8
#  define __PTK_REAL3    16
#  define __PTK_MPI_REAL3    MPI_REAL16
#  define __PTK_COMPLEX1 4
#  define __PTK_MPI_COMPLEX1 MPI_COMPLEX8
#  define __PTK_COMPLEX2 8
#  define __PTK_MPI_COMPLEX2 MPI_COMPLEX16
#  define __PTK_COMPLEX3 16
#  define __PTK_MPI_COMPLEX3 MPI_COMPLEX32
#endif


#ifdef __INTEL
#         define __PTK_LOGICAL3 4
#         define __PTK_MPI_LOGICAL3 MPI_LOGICAL
#         define __PTK_INTEGER1 1
#         define __PTK_MPI_INTEGER1 MPI_INTEGER1
#         define __PTK_INTEGER2 2
#         define __PTK_MPI_INTEGER2 MPI_INTEGER2
#         define __PTK_INTEGER3 4
#         define __PTK_MPI_INTEGER3 MPI_INTEGER4
#         define __PTK_REAL1    4
#         define __PTK_MPI_REAL1 MPI_REAL
#         define __PTK_REAL2    8
#         define __PTK_MPI_REAL2 MPI_DOUBLE_PRECISION
#         define __PTK_COMPLEX1 4
#         define __PTK_MPI_COMPLEX1 MPI_COMPLEX
#         define __PTK_COMPLEX2 8
#         define __PTK_MPI_COMPLEX2 MPI_DOUBLE_COMPLEX
#endif
#ifdef __G95
#         define __PTK_LOGICAL1 1
#         define __PTK_LOGICAL2 2
#         define __PTK_LOGICAL3 4
#         define __PTK_LOGICAL4 8
#         define __PTK_INTEGER1 1
#         define __PTK_INTEGER2 2
#         define __PTK_INTEGER3 4
#         define __PTK_INTEGER4 8
#         define __PTK_REAL1    4
#         define __PTK_REAL2    8
#         define __PTK_COMPLEX1    4
#         define __PTK_COMPLEX2    8
#endif
#ifdef __PGI
#         define __PTK_LOGICAL3 4
#         define __PTK_MPI_LOGICAL3 MPI_LOGICAL
#         define __PTK_INTEGER1 1
#         define __PTK_MPI_INTEGER1 MPI_INTEGER1
#         define __PTK_INTEGER2 2
#         define __PTK_MPI_INTEGER2 MPI_INTEGER2
#         define __PTK_INTEGER3 4
#         define __PTK_MPI_INTEGER3 MPI_INTEGER4
#         define __PTK_INTEGER4 8
#         define __PTK_MPI_INTEGER4 MPI_INTEGER8
#         define __PTK_REAL1    4
#         define __PTK_MPI_REAL1 MPI_REAL
#         define __PTK_REAL2    8
#         define __PTK_MPI_REAL2 MPI_DOUBLE_PRECISION
#         define __PTK_COMPLEX1    4
#         define __PTK_MPI_COMPLEX1 MPI_COMPLEX
#         define __PTK_COMPLEX2    8
#         define __PTK_MPI_COMPLEX2 MPI_DOUBLE_COMPLEX
#endif

#ifdef __EKO
#         define __PTK_LOGICAL3 4
#         define __PTK_MPI_LOGICAL3 MPI_LOGICAL
#         define __PTK_INTEGER1 1
#         define __PTK_MPI_INTEGER1 MPI_INTEGER1
#         define __PTK_INTEGER2 2
#         define __PTK_MPI_INTEGER2 MPI_INTEGER2
#         define __PTK_INTEGER3 4
#         define __PTK_MPI_INTEGER3 MPI_INTEGER4
#         define __PTK_INTEGER4 8
#         define __PTK_MPI_INTEGER4 MPI_INTEGER8
#         define __PTK_REAL1    4
#         define __PTK_MPI_REAL1 MPI_REAL
#         define __PTK_REAL2    8
#         define __PTK_MPI_REAL2 MPI_DOUBLE_PRECISION
#         define __PTK_COMPLEX1    4
#         define __PTK_MPI_COMPLEX1 MPI_COMPLEX
#         define __PTK_COMPLEX2    8
#         define __PTK_MPI_COMPLEX2 MPI_DOUBLE_COMPLEX
#endif


#ifdef __SGI
#   define __PTK_LOGICAL1 1
#   define __PTK_LOGICAL2 2
#   define __PTK_LOGICAL3 4
#   define __PTK_LOGICAL4 8
#   define __PTK_INTEGER1 1
#   define __PTK_INTEGER2 2
#   define __PTK_INTEGER3 4
#   define __PTK_INTEGER4 8
#   define __PTK_REAL1    4
#   define __PTK_REAL2    8
#   define __PTK_REAL3    16
#endif

#endif



!------------------------------------------------------------------------------!

