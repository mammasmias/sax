! Self-energies and eXcitations (SaX)
! Copyright (C) 2010 SaX developers team
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

#include "tools_error.h"

module optimal_proc_module
implicit none
private

public :: optimal_proc_find 
public :: optimal_proc_mycarica
public :: optimal_proc_maxcarica

contains

subroutine optimal_proc_mycarica(mycarica,itrans,myrank)
implicit none

integer, intent(out) :: mycarica
integer, pointer, intent(in) :: itrans(:,:)
integer, intent(in) :: myrank

integer :: i,j
integer :: mmin,mmax,nmin,nmax

mycarica = 0

mmin=lbound(itrans,1)
mmax=ubound(itrans,1)
nmin=lbound(itrans,2)
nmax=ubound(itrans,2)

do i=mmin,mmax
do j=nmin,nmax
  if(itrans(i,j)==myrank) mycarica = mycarica + 1
enddo 
enddo
return
end subroutine optimal_proc_mycarica

subroutine optimal_proc_maxcarica(maxcarica,itrans,P)

integer, intent(out) :: maxcarica
integer, pointer, intent(in) :: itrans(:,:)
integer, intent(in) :: P

integer :: tmp, i, j, ip
integer :: mmin, mmax,nmin,nmax

mmin=lbound(itrans,1)
mmax=ubound(itrans,1)
nmin=lbound(itrans,2)
nmax=ubound(itrans,2)

maxcarica = 0

do ip = 0, P-1
tmp=0
do i=mmin,mmax
do j=nmin,nmax
 if(itrans(i,j)==ip) tmp=tmp + 1
enddo
enddo

if(tmp > maxcarica) maxcarica = tmp

enddo 
return
end subroutine optimal_proc_maxcarica

subroutine optimal_proc_find(itrans,P)
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

mmin=lbound(itrans,1)
mmax=ubound(itrans,1)
nmin=lbound(itrans,2)
nmax=ubound(itrans,2)

allocate(carica(0:P-1))

itrans(:,:) = 0
carica(:) = 0

n=(nmax-nmin) + 1
m=(mmax-mmin) + 1

fold=1E14

do ai=1,P
if (real(P)/real(ai) .ne. real(P/ai)) cycle
F=abs(ai*n - m*(P/ai))
! write(*,*) ai, P/ai, F 

if(F<fold) then
 fold=F
 a=ai
endif

enddo
b=P/a
! write(*,*) "a: ", a
! write(*,*) "b :", b

nba = m/a
resa = m-nba*a

nbb = n/b
resb = n-nbb*b

!write(*,*) "nba, resa: ", nba, resa
!write(*,*) "nbb, resb: ", nbb, resb

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
write(15,*) "nmin,,,", mmin,nmin,mmax,nmax
write(15,*) "1miii"
do i=mmin,mmin+nba*a-1
  do j=nmin,nmin+nbb*b-1
    iii=(i-mmin)/nba*b + (j-nmin)/nbb
    itrans(i,j)=iii
    carica(iii)=carica(iii)+1
  enddo
enddo
write(15,*) "2miii"

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
write(15,*) "3miii"
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
write(15,*) "4miii"
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
write(15,*) "5miii"

!do i=mmin+nba*a, mmax
!  do j=nmin+nbb*b, nmax
!    write (*,*) "itrans, resto resto: ", i, j, itrans(i,j)
!  enddo
!enddo

!do i=mmin,mmax
!  write (*,'(20I2)') (itrans(i,j), j=nmin,nmax)
!enddo

deallocate(carica)

end subroutine optimal_proc_find

end module optimal_proc_module

