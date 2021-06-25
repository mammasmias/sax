
! Copyright (C) 2005 Giovanni Bussi
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .

program test
use ffti_module
implicit none

integer :: i1,i2,i3,n1,n2,n3,p
complex*16, allocatable :: array(:),array_save(:)
character(10) :: library

n1 = 6
n2 = 8
n3 = 5

do n1=4,12,2
do n2=4,12,2
do n3=4,12,2
allocate(array(n1*n2*n3),array_save(n1*n2*n3))

do i3 = 1 , n3
  do i2 = 1 , n2
    do i1 = 1 , n1
      p = i1 + (i2-1)*n1 + (i3-1)*n1*n2
      if(p<1 .or. p>n1*n2*n3) stop
      array(p) = i1 -i2*3 + sin(0.01 * i3 - i2)
    end do
  end do
end do
array_save = array
!write(0,*) "__"
!write(299,"(2f25.18)") array
!write(0,*) "++"

call ffti_set("SINGLETON")
call ffti_which(library)
write(*,*) n1,n2,n3
write(*,*) library
call ffti_3d(array,n1,n2,n3,n1,n2,n3,+1)
write(*,*) "PRE"
write(300,"(2f25.18)") array
write(*,*) "POST"

call ffti_set("FFTW")
call ffti_which(library)
write(*,*) library
call ffti_3d(array_save,n1,n2,n3,n1,n2,n3,+1)
write(301,"(2f25.18)") array_save

write(302,"(2f25.18)") array_save - array

deallocate(array,array_save)
end do
end do
end do


end program test
