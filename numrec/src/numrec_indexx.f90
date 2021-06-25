# 3 "numrec_indexx.spp"
subroutine numrec_indexx_sgl(n,arr,indx)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: arr(n)
  integer,     intent(out) :: indx(n)
  integer, parameter :: m=7
  integer, parameter :: nstack=50
  integer :: i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
  REAL(sgl) :: a
  do j=1,n
    indx(j)=j
  end do
  jstack=0
  l=1
  ir=n
1     if(ir-l<M)then
    do j=l+1,ir
      indxt=indx(j)
      a=arr(indxt)
      do i=j-1,1,-1
        if(arr(indx(i))<=a)goto 2
        indx(i+1)=indx(i)
      end do
      i=0
2         indx(i+1)=indxt
    end do
    if(jstack==0)return
    ir=istack(jstack)
    l=istack(jstack-1)
    jstack=jstack-2
  else
    k=(l+ir)/2
    itemp=indx(k)
    indx(k)=indx(l+1)
    indx(l+1)=itemp
    if(arr(indx(l+1))>arr(indx(ir)))then
      itemp=indx(l+1)
      indx(l+1)=indx(ir)
      indx(ir)=itemp
    endif
    if(arr(indx(l))>arr(indx(ir)))then
      itemp=indx(l)
      indx(l)=indx(ir)
      indx(ir)=itemp
    endif
    if(arr(indx(l+1))>arr(indx(l)))then
      itemp=indx(l+1)
      indx(l+1)=indx(l)
      indx(l)=itemp
    endif
    i=l+1
    j=ir
    indxt=indx(l)
    a=arr(indxt)
3       continue
      i=i+1
    if(arr(indx(i))<a)goto 3
4       continue
      j=j-1
    if(arr(indx(j))>a)goto 4
    if(j<i)goto 5
    itemp=indx(i)
    indx(i)=indx(j)
    indx(j)=itemp
    goto 3
5       indx(l)=indx(j)
    indx(j)=indxt
    jstack=jstack+2
    if(jstack>NSTACK) stop 'NSTACK too small in nr_indexx'
    if(ir-i+1>=j-l)then
  istack(jstack)=ir
      istack(jstack-1)=i
      ir=j-1
    else
      istack(jstack)=j-1
      istack(jstack-1)=l
      l=i
    endif
  endif
  goto 1
end subroutine numrec_indexx_sgl
# 3 "numrec_indexx.spp"
subroutine numrec_indexx_dbl(n,arr,indx)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: arr(n)
  integer,     intent(out) :: indx(n)
  integer, parameter :: m=7
  integer, parameter :: nstack=50
  integer :: i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
  REAL(dbl) :: a
  do j=1,n
    indx(j)=j
  end do
  jstack=0
  l=1
  ir=n
1     if(ir-l<M)then
    do j=l+1,ir
      indxt=indx(j)
      a=arr(indxt)
      do i=j-1,1,-1
        if(arr(indx(i))<=a)goto 2
        indx(i+1)=indx(i)
      end do
      i=0
2         indx(i+1)=indxt
    end do
    if(jstack==0)return
    ir=istack(jstack)
    l=istack(jstack-1)
    jstack=jstack-2
  else
    k=(l+ir)/2
    itemp=indx(k)
    indx(k)=indx(l+1)
    indx(l+1)=itemp
    if(arr(indx(l+1))>arr(indx(ir)))then
      itemp=indx(l+1)
      indx(l+1)=indx(ir)
      indx(ir)=itemp
    endif
    if(arr(indx(l))>arr(indx(ir)))then
      itemp=indx(l)
      indx(l)=indx(ir)
      indx(ir)=itemp
    endif
    if(arr(indx(l+1))>arr(indx(l)))then
      itemp=indx(l+1)
      indx(l+1)=indx(l)
      indx(l)=itemp
    endif
    i=l+1
    j=ir
    indxt=indx(l)
    a=arr(indxt)
3       continue
      i=i+1
    if(arr(indx(i))<a)goto 3
4       continue
      j=j-1
    if(arr(indx(j))>a)goto 4
    if(j<i)goto 5
    itemp=indx(i)
    indx(i)=indx(j)
    indx(j)=itemp
    goto 3
5       indx(l)=indx(j)
    indx(j)=indxt
    jstack=jstack+2
    if(jstack>NSTACK) stop 'NSTACK too small in nr_indexx'
    if(ir-i+1>=j-l)then
  istack(jstack)=ir
      istack(jstack-1)=i
      ir=j-1
    else
      istack(jstack)=j-1
      istack(jstack-1)=l
      l=i
    endif
  endif
  goto 1
end subroutine numrec_indexx_dbl
