#ifndef __IOTK_CONFIG_H
#define __IOTK_CONFIG_H

! Max rank

! End of Max rank


! Type definitions:
#define __IOTK_INTEGER1 4
#define __IOTK_INTEGER2 1
#define __IOTK_INTEGER3 2
#define __IOTK_INTEGER4 8
#define __IOTK_LOGICAL1 4
#define __IOTK_LOGICAL2 1
#define __IOTK_LOGICAL3 2
#define __IOTK_LOGICAL4 8
#define __IOTK_REAL1 4
#define __IOTK_REAL2 8
#define __IOTK_REAL3 10

! End of type definitions

#ifdef __IOTK_SAFEST
    !
    ! force to define all the workarounds
    !
#   define __IOTK_WORKAROUND1
#   define __IOTK_WORKAROUND2
#   define __IOTK_WORKAROUND3
#   define __IOTK_WORKAROUND4
#   define __IOTK_WORKAROUND5
#   define __IOTK_WORKAROUND6
#   define __IOTK_WORKAROUND7
#   define __IOTK_WORKAROUND9
#else
    !
    ! proceed with a machine dependent def where available
    !
#   if defined(__XLF)
#      define __IOTK_WORKAROUND5
#      define __IOTK_WORKAROUND9
#   elif defined(__INTEL)
#      define __IOTK_WORKAROUND1
#      define __IOTK_WORKAROUND3
#      define __IOTK_WORKAROUND5
#   elif defined(__PGI)
#      define __IOTK_WORKAROUND2
#      define __IOTK_WORKAROUND4
#   elif defined(__NAG)
#      define __IOTK_WORKAROUND4
#   elif defined(__ALPHA)
#      define __IOTK_WORKAROUND1
#      define __IOTK_WORKAROUND6
#   elif defined(__SX6)
#      define __IOTK_WORKAROUND5
#      define __IOTK_WORKAROUND7
#   else
#      define __IOTK_SAFEST
#   endif
#endif


!! Workarounds for bugs:
!
!
!
!
!
!
!
!
!

#endif

