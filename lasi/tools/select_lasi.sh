#!/bin/sh 
#
# Linear Algebra Subroutines Interfaces (LASI)
# select_lasi.sh -- script that determines which are the INTERFACED LASI routines
#                   used by some code
#
# written 17.08.04 by Andrea Ferretti

MANUAL="
 Script select_lasi.sh
 select_lasi.sh [-ah] [-l <lib>] [<list>]

 -h        :    Print this help
 -a        :    all the implemented interfaced routines are selected
 <list>    :    list of the directories containing F90 files using interfaced routines
 -l <lib>  :    only the LASI interfaces from library <lib> are selected
                <lib> can be 'blas', 'lapack', 'other'
                If -a is present all the routines of the current lib
                are printed

 The interfaced LASI are named as    LASI_<name> 
 where <name> is the original blas name without the first character
 which usually indicates the precision 

"
ALL=
dir_list=
lib_name=

while getopts :ahl: OPT
do
  case $OPT in
  (a) ALL='.TRUE.' ;;
  (l) lib_name=$OPTARG ;;
  (h) echo "$MANUAL" ; exit 0 ;;
  (?) echo "$0 error: unkwown option $OPTARG" ; exit 1 ;;
  esac
done
dir_list=` echo "$*" | sed 's/ *-a / /
                            s/ *-l  *[a-z]* / /'
                            s/ *-al  *[a-z]* / /'`

if [ -z "$dir_list" -a -z "$ALL"  ] ; then echo "$MANUAL" ; exit 0 ; fi

#
# list of avalilable INTERFACES
#
blas="\
    lasi_IAMAX  lasi_NRM2   lasi_ASUM   lasi_DOTU   lasi_DOTC
    lasi_DOT    lasi_DSDOT  lasi_AXPY   lasi_COPY   lasi_ROTG
    lasi_ROT    lasi_SCAL   lasi_SWAP   lasi_GBMV   lasi_GEMV
    lasi_GERC   lasi_GERU   lasi_HBMV   lasi_HEMV   lasi_HER
    lasi_HER2   lasi_HPR    lasi_HPR2   lasi_HPMV   lasi_TBMV
    lasi_TPMV   lasi_TRMV   lasi_SYMV   lasi_SBMV   lasi_SPMV
    lasi_TBSV   lasi_TPSV   lasi_TRSV   lasi_GER    lasi_SPR
    lasi_SPR2   lasi_SYR    lasi_SYR2   lasi_GEMM   lasi_HEMM
    lasi_HER2K  lasi_HERK   lasi_SYMM   lasi_SYR2K  lasi_SYRK
    lasi_TRMM   lasi_TRSM   "

lapack="\
    lasi_LANGB  lasi_TGSEN  lasi_TGSNA  lasi_TGSYL  lasi_TGEXC
    lasi_BDSDC  lasi_STEGR  lasi_ORMRZ  lasi_UNMRZ  lasi_TZRZF
    lasi_GEQP3  lasi_GESDD  lasi_GGRQF  lasi_GGQRF  lasi_DISNA
    lasi_TGSJA  lasi_GGSVP  lasi_TGEVC  lasi_HGEQZ  lasi_GGBAK
    lasi_GGBAL  lasi_GGHRD  lasi_PBSTF  lasi_SBGST  lasi_HBGST
    lasi_SPGST  lasi_HPGST  lasi_BDSQR  lasi_ORMBR  lasi_UNMBR
    lasi_ORGBR  lasi_UNGBR  lasi_GBBRD  lasi_GEBRD  lasi_TRSEN
    lasi_TRSNA  lasi_TRSYL  lasi_TREXC  lasi_TREVC  lasi_HSEIN
    lasi_HSEQR  lasi_ORMHR  lasi_UNMHR  lasi_ORGHR  lasi_UNGHR
    lasi_GEBAK  lasi_GEBAL  lasi_GEHRD  lasi_PTEQR  lasi_STEIN
    lasi_STEBZ  lasi_STEDC  lasi_STERF  lasi_STEQR  lasi_OPMTR
    lasi_UPMTR  lasi_OPGTR  lasi_UPGTR  lasi_ORMTR  lasi_UNMTR
    lasi_SBTRD  lasi_HBTRD  lasi_SPTRD  lasi_HPTRD  lasi_TZRQF
    lasi_ORMRQ  lasi_UNMRQ  lasi_ORGRQ  lasi_UNGRQ  lasi_GERQF
    lasi_ORMQL  lasi_UNMQL  lasi_ORGQL  lasi_UNGQL  lasi_GEQLF
    lasi_ORMLQ  lasi_UNMLQ  lasi_ORGLQ  lasi_UNGLQ  lasi_GELQF
    lasi_ORMQR  lasi_UNMQR  lasi_ORGQR  lasi_UNGQR  lasi_GEQRF
    lasi_GEQPF  lasi_TBRFS  lasi_TBCON  lasi_TBTRS  lasi_TPTRI
    lasi_TPRFS  lasi_TPCON  lasi_TPTRS  lasi_TRTRI  lasi_TRRFS
    lasi_TRCON  lasi_TRTRS  lasi_SPTRI  lasi_HPTRI  lasi_SPRFS
    lasi_HPRFS  lasi_HPCON  lasi_SPCON  lasi_SPTRS  lasi_HPTRS
    lasi_HPTRF  lasi_SPTRF  lasi_SYTRI  lasi_HETRI  lasi_SYRFS
    lasi_HERFS  lasi_SYCON  lasi_HECON  lasi_HETRS  lasi_SYTRS
    lasi_HETRF  lasi_SYTRF  lasi_PTRFS  lasi_PTCON  lasi_PTTRS
    lasi_PTTRF  lasi_PBEQU  lasi_PBRFS  lasi_PBCON  lasi_PBTRS
    lasi_PBTRF  lasi_PPEQU  lasi_PPTRI  lasi_PPRFS  lasi_PPCON
    lasi_PPTRS  lasi_PPTRF  lasi_POEQU  lasi_POTRI  lasi_PORFS
    lasi_POTRS  lasi_GTRFS  lasi_GTCON  lasi_GTTRS  lasi_GTTRF
    lasi_GBEQU  lasi_GBRFS  lasi_GBCON  lasi_GBTRS  lasi_GBTRF
    lasi_GGSVD  lasi_GEGV   lasi_GEGS   lasi_SBGVX  lasi_HBGVX
    lasi_SBGVD  lasi_HBGVD  lasi_SBGV   lasi_HBGV   lasi_SPGVX
    lasi_HPGVX  lasi_SPGVD  lasi_HPGVD  lasi_SPGV   lasi_HPGV
    lasi_GESVD  lasi_GEEVX  lasi_GGEVX  lasi_GGEV   lasi_GEEV
    lasi_GEESX  lasi_GGESX  lasi_GGES   lasi_GEES   lasi_STEVR
    lasi_STEVX  lasi_STEVD  lasi_STEV   lasi_SBEVX  lasi_HBEVX
    lasi_SBEVD  lasi_HBEVD  lasi_SBEV   lasi_HBEV   lasi_SPEVX
    lasi_HPEVX  lasi_SPEVD  lasi_HPEVD  lasi_SPEV   lasi_HPEV
    lasi_GGGLM  lasi_GGLSE  lasi_GELSY  lasi_GELSD  lasi_GELSX
    lasi_GELSS  lasi_GELS   lasi_SPSV   lasi_HPSV   lasi_SYSV
    lasi_HESV   lasi_PTSV   lasi_PBSV   lasi_PPSV   lasi_POSV
    lasi_GTSV   lasi_GBSV   lasi_GESV   lasi_SPSVX  lasi_HPSVX
    lasi_SYSVX  lasi_HESVX  lasi_PTSVX  lasi_PBSVX  lasi_PPSVX
    lasi_POSVX  lasi_GTSVX  lasi_GBSVX  lasi_GESVX  lasi_GETRF
    lasi_GETRS  lasi_GETRI  lasi_GERFS  lasi_GEEQU  lasi_LANGE
    lasi_GECON  lasi_SYEV   lasi_HEEV   lasi_SYEVD  lasi_HEEVD
    lasi_SYEVR  lasi_HEEVR  lasi_SYEVX  lasi_HEEVX  lasi_SYGST
    lasi_HEGST  lasi_SYGV   lasi_HEGV   lasi_SYGVX  lasi_HEGVX
    lasi_SYGVD  lasi_HEGVD  lasi_SYTRD  lasi_HETRD  lasi_ORGTR
    lasi_UNGTR  lasi_LANSY  lasi_POTRF  lasi_POCON  lasi_ILAENV
    lasi_LAGGE  lasi_LAMCH"

other=" "

#
# select the library
#
if [ -z "$lib_name" ] ; then
   INTERFACES=`echo $blas $lapack $other`
elif [ "$lib_name" = "blas" ] ; then
   INTERFACES=`echo $blas `
elif [ "$lib_name" = "lapack" ] ; then
   INTERFACES=`echo $lapack `
elif [ "$lib_name" = "other" ] ; then
   INTERFACES=`echo $other `
else
   echo "Invalid LIB_NAME = $lib_name; use -h option for help" 
   exit 1
fi
 

# files that may contain calls to INTERFACED routines
# extra directories can be specified on the command line

FOUND_LIST=
if [ "$ALL" != ".TRUE."  ] ; then 
   sources=`ls *.f90 2> /dev/null`

   for dir in $dir_list
   do
     sources="$sources `ls $dir/*.f90 2> /dev/null`"
   done


#
# main loop over routines
   for routine in $INTERFACES
   do
       tmp=`egrep -i "[^!][CALL=] *$routine" $sources`
       if [ -n "$tmp" ] ; then  FOUND_LIST=`echo $FOUND_LIST $routine` ; fi
   done

else

   FOUND_LIST=$INTERFACES
fi
#
tmp=$FOUND_LIST
FOUND_LIST=`echo $tmp | tr " " "\n" | uniq`


#
# writing
#
for item in $FOUND_LIST
do
    capital_name=`echo $item | tr [:lower:] [:upper:] ` 
    echo '#define ' __$capital_name  $item 
done

exit 0



