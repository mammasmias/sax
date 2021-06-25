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

#include "tools_error.h"

module memory_tools_module
use num_module
implicit none
private

public :: calc_npw, calc_test_processors
public :: test_npe_init, test_npe_destroy
public :: calc_scalable_memory
public :: calc_gmax, calc_gmin
public :: total_memory_init, total_memory_destroy
public :: memory_max_init, memory_max_destroy
contains

subroutine calc_npw(a,b,k,cutoff,npw)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: k(3)
  real, intent(in) :: cutoff
  integer, intent(out) :: npw
  integer :: i1, i2, i3
  integer :: maxvec(3), minvec(3)
  
  maxvec(1) = - k(1) + sqrt( cutoff * sum(a(:,1)**2) ) / num_2pi +2
  minvec(1) = - k(1) - sqrt( cutoff * sum(a(:,1)**2) ) / num_2pi -2
  maxvec(2) = - k(2) + sqrt( cutoff * sum(a(:,2)**2) ) / num_2pi +2
  minvec(2) = - k(2) - sqrt( cutoff * sum(a(:,2)**2) ) / num_2pi -2
  maxvec(3) = - k(3) + sqrt( cutoff * sum(a(:,3)**2) ) / num_2pi +2
  minvec(3) = - k(3) - sqrt( cutoff * sum(a(:,3)**2) ) / num_2pi -2
              
  npw = 0
  do i3=minvec(3),maxvec(3)
   do i2=minvec(2),maxvec(2)
    do i1=minvec(1),maxvec(1)
     if(sum(num_matmul(b,(/i1,i2,i3/)+k)**2)<=cutoff) then
      npw=npw+1
     end if
    end do
   end do
  end do
                                                                
end subroutine calc_npw


subroutine calc_test_processors(nb_test_npe,nbandmin,nbandmax,test_npe)
  integer, intent(in) :: nbandmin, nbandmax
  integer, intent(out) :: test_npe(nb_test_npe)
  integer, intent(inout) :: nb_test_npe
  integer :: i,npe, npemax

  i = 1
  npe = 1
  npemax = nbandmax-nbandmin+1
  do while((i<nb_test_npe+1).AND.(npe<npemax))
      if(modulo(nbandmax-nbandmin+1,npe)==0) then
        i = i+1
        test_npe(i) = npe
        npe = npe + npe
        else
          npe = npe + 1
      end if
  end do
  if(i<nb_test_npe+1) then
    write(*,*) "not find", nb_test_npe, "processors for the calculation of memory"
  end if
  nb_test_npe = i-1
end subroutine calc_test_processors


subroutine test_npe_init(nb_test_npe,test_npe)
  integer, intent(in) :: nb_test_npe
  integer, pointer :: test_npe(:)

  allocate(test_npe(nb_test_npe))
  test_npe(:) = 0
end subroutine test_npe_init


subroutine test_npe_destroy(test_npe)
  integer, pointer :: test_npe(:)

  deallocate(test_npe)
  nullify(test_npe)
end subroutine test_npe_destroy


subroutine total_memory_init(nmax,npe,total_s_memory, total_ns_memory)
  integer, intent(in) :: nmax, npe
  real, pointer :: total_s_memory(:,:), total_ns_memory(:)

  allocate(total_s_memory(npe,nmax))
  allocate(total_ns_memory(nmax))
  total_s_memory(:,:) = 0
  total_ns_memory(:) = 0
end subroutine total_memory_init


subroutine total_memory_destroy(total_s_memory, total_ns_memory)
  real, pointer :: total_ns_memory(:), total_s_memory(:,:)

  deallocate(total_s_memory, total_ns_memory)
  nullify(total_s_memory, total_ns_memory) 
end subroutine total_memory_destroy


subroutine calc_scalable_memory(mem_unit,mem_tot,npe,scalable)
  real, intent(in) :: mem_unit, mem_tot
  integer, intent(in) :: npe
  real, intent(out) :: scalable
  real :: k
  integer :: i, mem

  k = mem_tot / mem_unit
  i = ceiling(k)

  mem = i / npe
  if(modulo(i,npe)/=0) then
    mem = mem + 1
  end if
  scalable = mem*mem_unit
end subroutine calc_scalable_memory

subroutine calc_gmax(a,b,k,cutoff,gmax)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: k(3)
  real, intent(in) :: cutoff
  integer, intent(out) :: gmax(3)
  integer :: i1, i2, i3, npwmax, npw
  integer :: maxvec(3), minvec(3)
  integer, allocatable :: tmp(:,:)
  
  maxvec(1) = - k(1) + sqrt( cutoff * sum(a(:,1)**2) ) / num_2pi +2
  minvec(1) = - k(1) - sqrt( cutoff * sum(a(:,1)**2) ) / num_2pi -2
  maxvec(2) = - k(2) + sqrt( cutoff * sum(a(:,2)**2) ) / num_2pi +2
  minvec(2) = - k(2) - sqrt( cutoff * sum(a(:,2)**2) ) / num_2pi -2
  maxvec(3) = - k(3) + sqrt( cutoff * sum(a(:,3)**2) ) / num_2pi +2
  minvec(3) = - k(3) - sqrt( cutoff * sum(a(:,3)**2) ) / num_2pi -2
  
  npwmax = product(maxvec-minvec+1)
  allocate(tmp(3,npwmax))
  tmp(:,:)=0
  npw = 0
     
  do i3=minvec(3),maxvec(3)
   do i2=minvec(2),maxvec(2)
    do i1=minvec(1),maxvec(1)
     if(sum(num_matmul(b,(/i1,i2,i3/)+k)**2)<=cutoff) then
      npw=npw+1
      tmp(:,npw)=(/i1,i2,i3/)
     end if
    end do
   end do
  end do
  gmax(1) = maxval(tmp(1,:))
  gmax(2) = maxval(tmp(2,:))
  gmax(3) = maxval(tmp(3,:))
  
  deallocate(tmp)
end subroutine calc_gmax

subroutine calc_gmin(a,b,k,cutoff,gmin)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: k(3)
  real, intent(in) :: cutoff
  integer, intent(out) :: gmin(3)
  integer :: i1, i2, i3, npwmax, npw
  integer :: maxvec(3), minvec(3)
  integer, allocatable :: tmp(:,:)
  
  maxvec(1) = - k(1) + sqrt( cutoff * sum(a(:,1)**2) ) / num_2pi +2
  minvec(1) = - k(1) - sqrt( cutoff * sum(a(:,1)**2) ) / num_2pi -2
  maxvec(2) = - k(2) + sqrt( cutoff * sum(a(:,2)**2) ) / num_2pi +2
  minvec(2) = - k(2) - sqrt( cutoff * sum(a(:,2)**2) ) / num_2pi -2
  maxvec(3) = - k(3) + sqrt( cutoff * sum(a(:,3)**2) ) / num_2pi +2
  minvec(3) = - k(3) - sqrt( cutoff * sum(a(:,3)**2) ) / num_2pi -2
  
  npwmax = product(maxvec-minvec+1)
  allocate(tmp(3,npwmax))
  tmp(:,:)=0
  npw = 0
  
  do i3=minvec(3),maxvec(3)
   do i2=minvec(2),maxvec(2)
    do i1=minvec(1),maxvec(1)
     if(sum(num_matmul(b,(/i1,i2,i3/)+k)**2)<=cutoff) then
      npw=npw+1
      tmp(:,npw)=(/i1,i2,i3/)
     end if
    end do
   end do
  end do
  
  gmin(1) = minval(tmp(1,:))
  gmin(2) = minval(tmp(2,:))
  gmin(3) = minval(tmp(3,:))
  
  deallocate(tmp)
end subroutine calc_gmin


subroutine memory_max_init(nb_test_npe,memory_max)
  integer, intent(in) :: nb_test_npe
  real, pointer :: memory_max(:)

  allocate(memory_max(nb_test_npe))
  memory_max(:) = 0
end subroutine memory_max_init


subroutine memory_max_destroy(memory_max)
  real, pointer :: memory_max(:)

  deallocate(memory_max)
  nullify(memory_max)
end subroutine memory_max_destroy


end module memory_tools_module
