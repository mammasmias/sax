 include -s ptk_version.sh
# Configurations on maximum number of kinds and maximum value for PTK_MAXRANK
 types='LOGICAL INTEGER REAL COMPLEX CHARACTER'
 nkinds=4
 maxrank=7

 kinds=''
 for ((kind = 1 ; $kind <= $nkinds ; kind++)) ; do
   kinds="$kinds $kind"
 done
 ranks=''
 for ((rank = 0 ; $rank <= $maxrank ; rank++)) ; do
   ranks="$ranks $rank"
 done

 SHAPE[0]=''
 for ((rank = 1 ; $rank <= $maxrank ; rank++)) ; do
   SHAPE[$rank]='(:'
   for ((rank1 = 2 ; $rank1 <= $rank ; rank1++)) ; do
     SHAPE[$rank]="${SHAPE[$rank]},:"
   done
   SHAPE[$rank]="${SHAPE[$rank]})"
 done

 function BOUNDS () {
   local index
   local rank
   local argument
   local output
   rank=$1
   argument=$2
   (($rank==0)) && return
   output="(lbound($argument,1):ubound($argument,1)"
   for ((index=2;$index<=$rank;index++)) ; do
     output="${output},"
     if ((${#output}>60)) ; then
       echo "${output} &"
       output=""
     fi
     output="${output}lbound($argument,$index):ubound($argument,$index)"
   done
   output="${output})"
   echo "$output"
 }

 LENSTAR_LOGICAL=''
 LENSTAR_INTEGER=''
 LENSTAR_REAL=''
 LENSTAR_COMPLEX=''
 LENSTAR_CHARACTER=',len=*'

