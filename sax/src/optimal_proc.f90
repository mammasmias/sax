program optimal_proc

implicit none

integer :: a,b,m,n,P,ai,bi
real :: F, fold
integer :: nba, resa, nbb, resb
integer :: iii, dimiii
integer :: nmin,nmax,mmax,mmin
integer :: i,j
integer, pointer :: itrans(:,:)
integer, pointer :: carica(:)
integer :: maxcarica

read(*,*) mmin,mmax
read(*,*) nmin,nmax
read(*,*) P

allocate(itrans(mmin:mmax,nmin:nmax))
allocate(carica(0:P-1))

itrans(:,:) = 0
carica(:) = 0

n=(nmax-nmin) + 1
m=(mmax-mmin) + 1

fold=100000

do ai=1,P
if (real(P)/real(ai) .ne. real(P/ai)) cycle
F=abs(ai*n - m*(P/ai))
 write(*,*) ai, P/ai, F 

if(F<fold) then
 fold=F
 a=ai
endif

enddo
b=P/a
 write(*,*) "a: ", a
 write(*,*) "b :", b

nba = m/a
resa = m-nba*a

nbb = n/b
resb = n-nbb*b

write(*,*) "nba, resa: ", nba, resa
write(*,*) "nbb, resb: ", nbb, resb

if(nbb==0.or.nba==0) write(*,*) "incompatible number of proc, try with less proc"

do ai=1,a
  do bi=1,b
!   iii=(bi-1)*a+(ai-1)
    iii=modulo(ai-1,a)*b + modulo(bi-1,b)
!   write(*,*) ai,bi,iii 
  enddo
enddo

dimiii = nba*nbb
!write(*,*) "dimiii", dimiii

do i=mmin,mmin+nba*a-1
  do j=nmin,nmin+nbb*b-1
    iii=(i-mmin)/nba*b + (j-nmin)/nbb
    itrans(i,j)=iii
    carica(iii)=carica(iii)+1
  enddo
enddo

!do i=mmin,mmin+nba*a-1
!  do j=nmin,nmin+nbb*b-1
!     write(*,*) "itrans: ",i,j,itrans(i,j)
!  enddo
!enddo

do i=mmin+nba*a, mmax
  do j=nmin,nmin+nbb*b-1
    iii=modulo(i-nmin,a)*b + (j-nmin)/nbb
    itrans(i,j)=iii
    carica(iii)=carica(iii)+1
  enddo
enddo

!do i=mmin+nba*a, mmax
!  do j=nmin,nmin+nbb*b-1
!    write (*,*) "itrans, resto line: ", i, j, itrans(i,j)
!  enddo
!enddo

do i=mmin,mmin+nba*a-1
  do j=nmin+nbb*b, nmax
    iii=(i-nmin)/nba*b + modulo(j-nmin,b)
    itrans(i,j)=iii
    carica(iii)=carica(iii)+1
  enddo
enddo

!do i=mmin,mmin+nba*a-1
!  do j=nmin+nbb*b, nmax
!    write (*,*) "itrans, resto columm: ", i, j, itrans(i,j)
!  enddo
!enddo

maxcarica=ceiling(real(m)*real(n)/real(P))
iii=P-1
do i=mmin+nba*a, mmax
  do j=nmin+nbb*b, nmax
    do while (carica(iii)>=maxcarica)  
      iii = iii - 1
    enddo
      itrans(i,j) = iii
      carica(iii) = carica(iii) + 1
  enddo
enddo

!do i=mmin+nba*a, mmax
!  do j=nmin+nbb*b, nmax
!    write (*,*) "itrans, resto resto: ", i, j, itrans(i,j)
!  enddo
!enddo

do i=mmin,mmax
  write (*,'(20I2)') (itrans(i,j), j=nmin,nmax)
enddo

deallocate(itrans)
deallocate(carica)

end program optimal_proc
