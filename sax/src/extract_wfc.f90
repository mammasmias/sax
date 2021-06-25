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

program extract_wfc
use iotk_module
use tools_module
implicit none

integer, parameter :: real_kind = 8
integer :: nb,nk,npw,ib,ik
character(100) :: file,argument
integer :: iostat
character(len=iotk_attlenx) :: attr
integer, allocatable :: grid(:,:)
complex(real_kind), allocatable :: wfc(:)
real   (real_kind), allocatable :: eigenvalues(:,:)

read(*,"(a)",iostat=iostat) file

call iotk_open_read(10,file = trim(file))

call iotk_scan_begin(10,"Dimensions")
call iotk_scan_empty(10,"Kpoints",attr=attr)
call iotk_scan_attr (attr,"nktot",nk)
call iotk_scan_empty(10,"Bands",attr=attr)
call iotk_scan_attr (attr,"nbnd",nb)
call iotk_scan_end  (10,"Dimensions")
write(0,*) "Number of k points :",nk
write(0,*) "Number of bands    :",nb
write(0,*) "Converting grids"
call iotk_scan_begin(10,"Wfc_grids")
do ik = 1 , nk
  call iotk_scan_begin(10,"Kpoint"//trim(iotk_index(ik)),attr=attr)
  call iotk_scan_attr (attr,"npw",npw)
  allocate(grid(3,npw))
  call iotk_scan_dat  (10,"grid",grid)
  call iotk_scan_end  (10,"Kpoint"//trim(iotk_index(ik)))
  open(11,file="grid."//tools_char(ik,3),form="unformatted")
  write(11) npw
  write(11) grid
  close(11)
  deallocate(grid)
end do
call iotk_scan_end  (10,"Wfc_grids")
write(*,*) "Reading eigenvalues"
call iotk_scan_begin(10,"Eigenvalues")
allocate(eigenvalues(nb,nk))
do ik = 1 , nk
  call iotk_scan_dat(10,"e"//trim(iotk_index(ik)),eigenvalues(:,ik))
end do
call iotk_scan_end  (10,"Eigenvalues")
write(*,*) "Converting eigenvectors"
call iotk_scan_begin(10,"Eigenvectors")
do ik = 1 , nk
  call iotk_scan_begin(10,"Kpoint"//trim(iotk_index(ik)))
  call iotk_scan_empty(10,"Info",attr=attr)
  call iotk_scan_attr (attr,"igwx",npw)
  allocate(wfc(npw))
  do ib = 1 , nb
    call iotk_scan_dat(10,"Wfc"//trim(iotk_index(ib)),wfc)
    open(11,file="wfc."//tools_char(ib,3)//"-"//tools_char(ik,3), &
            form="unformatted")
    write(11) npw,eigenvalues(ib,ik)
    write(11) wfc
    close(11)
  end do
  deallocate(wfc)
  call iotk_scan_end  (10,"Kpoint"//trim(iotk_index(ik)))
end do
call iotk_scan_end  (10,"Eigenvectors")
deallocate(eigenvalues)
call iotk_close_read(10)

end program extract_wfc
