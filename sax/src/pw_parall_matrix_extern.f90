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



! ----------------------------------------------------------------------
! The subroutines handel with parallel (square) real or complex 
! matrix distributed on proc with the line index.
! ----------------------------------------------------------------------



#include "tools_error.h"


subroutine pw_parall_complex_m_init_x(matrix,imin,imax,root,comm)

use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
use pw_parall_matrix_type, only : pw_parall_complex_matrix
implicit none

type(pw_parall_complex_matrix),intent(out) :: matrix
integer, intent(in) :: imin,imax
integer, intent(in) :: root
type(ptk_comm), intent(in) :: comm

integer :: i

call ptk_comm_rank(comm,matrix%rank)
call ptk_comm_size(comm,matrix%npe)

matrix%incomplete=.false.
matrix%m_dim=(imax-imin)+1
matrix%imin=imin
matrix%imax=imax
matrix%comm=comm
matrix%root=root

matrix%m_dim_locmax=ceiling(real(matrix%m_dim)/real(matrix%npe))

allocate(matrix%where_line(2,imin:imax))
matrix%where_line = 0

do i=imin,imax
   matrix%where_line(1,i)=modulo((i-imin),matrix%npe)
!    matrix%where_line(1,i)=matrix%rank+(i-1)*matrix%npe
   matrix%where_line(2,i)=count(matrix%where_line(1,imin:i)==matrix%where_line(1,i)) 
enddo

matrix%m_dim_loc=count(matrix%where_line(1,:)==matrix%rank)

if(matrix%m_dim_locmax.ne.matrix%m_dim_loc) matrix%incomplete=.true.

allocate(matrix%val(count(matrix%where_line(1,:)==matrix%rank),imin:imax))
matrix%val(:,:)= (0.0,0.0)
allocate(matrix%eigenval(imin:imax))
matrix%eigenval(:)=0.0
 
end subroutine pw_parall_complex_m_init_x

subroutine pw_parall_complex_m_destroy_x(matrix)
use pw_parall_matrix_type, only : pw_parall_complex_matrix

type(pw_parall_complex_matrix), intent(inout) :: matrix

deallocate(matrix%where_line)
deallocate(matrix%eigenval)
deallocate(matrix%val) 
end subroutine pw_parall_complex_m_destroy_x

subroutine pw_parall_real_m_init_x(matrix,imin,imax,root,comm)
use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm
use pw_parall_matrix_type, only : pw_parall_real_matrix

implicit none

type(pw_parall_real_matrix),intent(out) :: matrix
integer, intent(in) :: imin,imax
integer, intent(in) :: root
type(ptk_comm), intent(in) :: comm

integer :: i

call ptk_comm_size(comm,matrix%npe)
call ptk_comm_rank(comm,matrix%rank)

matrix%incomplete=.false.
matrix%m_dim=(imax-imin)+1
matrix%imin=imin
matrix%imax=imax
matrix%comm=comm
matrix%root=root
matrix%m_dim_locmax=ceiling(real(matrix%m_dim)/real(matrix%npe))

allocate(matrix%where_line(2,imin:imax))
matrix%where_line = 0

do i=imin,imax
   matrix%where_line(1,i)=modulo((i-imin),matrix%npe)
!    matrix%where_line(1,i)=matrix%rank+(i-1)*matrix%npe
   matrix%where_line(2,i)=count(matrix%where_line(1,1:i)==matrix%where_line(1,i)) 
enddo

matrix%m_dim_loc=count(matrix%where_line(1,:)==matrix%rank)

if(matrix%m_dim_locmax.ne.matrix%m_dim_loc) matrix%incomplete=.true.

allocate(matrix%val(count(matrix%where_line(1,:)==matrix%rank),imin:imax))
matrix%val(:,:)= (0.0,0.0)
allocate(matrix%eigenval(imin:imax))
matrix%eigenval(:)=0.0

end subroutine pw_parall_real_m_init_x

subroutine pw_parall_real_m_destroy_x(matrix)
use pw_parall_matrix_type, only : pw_parall_real_matrix

type(pw_parall_real_matrix), intent(inout) :: matrix

deallocate(matrix%where_line)
deallocate(matrix%eigenval)
deallocate(matrix%val) 
end subroutine pw_parall_real_m_destroy_x


subroutine pw_parall_complex_m_write_x(matrix,unit,name)
use ptk_module
use pw_parall_matrix_type, only : pw_parall_complex_matrix
use iotk_module

implicit none

type(pw_parall_complex_matrix), intent(in) :: matrix
integer, intent(in) :: unit
character(*), intent(in) :: name

integer :: i, iline1
complex, allocatable :: line_tmp(:)
character(iotk_attlenx) :: attr

integer :: owner, location

if(matrix%rank==matrix%root) then
   call iotk_write_attr(attr,"type","pw_parall_complex_matrix",first=.true.)
   call iotk_write_begin(unit,trim(name),attr)  
   call iotk_write_dat(unit,"imin",matrix%imin)
   call iotk_write_dat(unit,"imax",matrix%imax)
   call iotk_write_dat(unit,"eigenvalues",matrix%eigenval) 
endif

do iline1=matrix%imin,matrix%imax
  owner=matrix%where_line(1,iline1)
  location=matrix%where_line(2,iline1)
  if(matrix%rank==matrix%root.and.matrix%rank==owner) then
     call iotk_write_dat(unit,"line"//trim(iotk_index(iline1)),matrix%val(location,:))
  endif
  if(matrix%rank/=matrix%root.and.matrix%rank==owner) then
    call ptk_send(matrix%val(location,:),matrix%root,10,matrix%comm)
  endif
  if(matrix%rank==matrix%root.and.matrix%rank/=owner) then
     allocate(line_tmp(matrix%imin:matrix%imax))
     line_tmp(:)=0.0
     call ptk_recv(line_tmp,owner,10,matrix%comm)
     call iotk_write_dat(unit,"line"//trim(iotk_index(iline1)),line_tmp)
     deallocate(line_tmp)
  endif
enddo
if(matrix%rank==matrix%root) then
   call iotk_write_end(unit,name)     
endif   

end subroutine pw_parall_complex_m_write_x

subroutine pw_parall_real_m_write_x(matrix,unit,name)
use ptk_module
use pw_parall_matrix_type, only : pw_parall_real_matrix
use iotk_module

implicit none

type(pw_parall_real_matrix), intent(in) :: matrix
integer, intent(in) :: unit
character(*), intent(in) :: name

integer :: iline1
real, allocatable :: line_tmp(:)
character(iotk_attlenx) :: attr

integer :: owner, location

if(matrix%rank==matrix%root) then
   call iotk_write_attr(attr,"type","pw_parall_real_matrix",first=.true.)
   call iotk_write_begin(unit,trim(name),attr)  
   call iotk_write_dat(unit,"imin",matrix%imin)
   call iotk_write_dat(unit,"imax",matrix%imax)
   call iotk_write_dat(unit,"eigenvalues",matrix%eigenval) 
endif

do iline1=matrix%imin,matrix%imax
  owner=matrix%where_line(1,iline1)
  location=matrix%where_line(2,iline1)
  if(matrix%rank==matrix%root.and.matrix%rank==owner) then
     call iotk_write_dat(unit,"line"//trim(iotk_index(iline1)),matrix%val(location,:))
  endif
  if(matrix%rank==matrix%root.and.matrix%rank/=owner) then
     allocate(line_tmp(matrix%imin:matrix%imax))
     line_tmp(:)=0.0
     call ptk_recv(line_tmp,owner,10,matrix%comm)
     call iotk_write_dat(unit,"line"//trim(iotk_index(iline1)),line_tmp)
     deallocate(line_tmp)
  endif
  if(matrix%rank/=matrix%root.and.matrix%rank==owner) then
     call ptk_send(matrix%val(location,:),matrix%root,10,matrix%comm)
  endif     
enddo
if(matrix%rank==matrix%root) then
   call iotk_write_end(unit,name)     
endif   

end subroutine pw_parall_real_m_write_x

subroutine pw_parall_complex_m_read_x(matrix,unit,name)
use ptk_module
use pw_parall_matrix_type, only : pw_parall_complex_matrix
use iotk_module

implicit none

type(pw_parall_complex_matrix), intent(inout) :: matrix
integer, intent(in) :: unit
character(*), intent(in) :: name

integer :: imin, imax, iline1
complex, allocatable :: line_tmp(:)

character(len=iotk_attlenx) :: attr
character(len=iotk_vallenx) :: rtype

integer :: owner, location
!write(0,*) "enetring matrix read"
if(matrix%rank==matrix%root) then
   call iotk_scan_begin(unit,trim(name),attr)
   call iotk_scan_attr(attr,"type",rtype,default="pw_parall_complex_matrix")  
   call iotk_scan_dat(unit,"imin",imin)
   if(imin/=matrix%imin) ERROR(" Inconsistent matrix dimension imin")
   call iotk_scan_dat(unit,"imax",imax)
   if(imax/=matrix%imax) ERROR(" Inconsistent matrix dimension imax")
   call iotk_scan_dat(unit,"eigenvalues",matrix%eigenval)
endif
!write(0,*) "before bcast m_dim and eigenval"
call ptk_bcast(matrix%m_dim,matrix%root,matrix%comm)
call ptk_bcast(matrix%eigenval,matrix%root,matrix%comm)
!write(0,*) "after bcast"
do iline1=matrix%imin,matrix%imax
  owner=matrix%where_line(1,iline1)
  location=matrix%where_line(2,iline1)
!write(0,*) "root scan and is owner"
  if(matrix%rank==matrix%root.and.matrix%rank==owner) then
!write(0,*) "before scan dat"
     call iotk_scan_dat(unit,"line"//trim(iotk_index(iline1)),matrix%val(location,:))
!write(0,*) "after scan dat"
  endif
!write(0,*) "root scan and is not owner"
  if(matrix%rank==matrix%root.and.matrix%rank/=owner) then
     allocate(line_tmp(matrix%imin:matrix%imax))
     line_tmp(:)=0.0
     call iotk_scan_dat(unit,"line"//trim(iotk_index(iline1)),line_tmp)
     call ptk_send(line_tmp,owner,10,matrix%comm)
     deallocate(line_tmp)
  endif
!write(0,*) "recv"
  if(matrix%rank/=matrix%root.and.matrix%rank==owner) then
     call ptk_recv(matrix%val(location,:),matrix%root,10,matrix%comm)
  endif     
enddo
!write(0,*) "before scan end"
if(matrix%rank==matrix%root) then
   call iotk_scan_end(unit,name)     
endif   

end subroutine pw_parall_complex_m_read_x

subroutine pw_parall_real_m_read_x(matrix,unit,name)
use ptk_module
use pw_parall_matrix_type, only : pw_parall_real_matrix
use iotk_module

implicit none

type(pw_parall_real_matrix), intent(inout) :: matrix
integer, intent(in) :: unit
character(*), intent(in) :: name

integer :: imin, imax, iline1
real, allocatable :: line_tmp(:)

character(len=iotk_attlenx) :: attr
character(len=iotk_vallenx) :: rtype

integer :: owner, location

if(matrix%rank==matrix%root) then
   call iotk_scan_begin(unit,trim(name),attr)
   call iotk_scan_attr(attr,"type",rtype,default="pw_parall_real_matrix")
   call iotk_scan_dat(unit,"imin",imin)
   if(imin/=matrix%imin) ERROR(" Inconsistent matrix dimension imin")
   call iotk_scan_dat(unit,"imax",imax)
   if(imax/=matrix%imax) ERROR(" Inconsistent matrix dimension imax")  
   call iotk_scan_dat(unit,"eigenvalues",matrix%eigenval)
endif

call ptk_bcast(matrix%m_dim,matrix%root,matrix%comm)
call ptk_bcast(matrix%eigenval,matrix%root,matrix%comm)

do iline1=matrix%imin,matrix%imax
  owner=matrix%where_line(1,iline1)
  location=matrix%where_line(2,iline1)
  if(matrix%rank==matrix%root.and.matrix%rank==owner) then
     call iotk_scan_dat(unit,"line"//trim(iotk_index(iline1)),matrix%val(location,:))
  endif
  if(matrix%rank==matrix%root.and.matrix%rank/=owner) then
     allocate(line_tmp(matrix%imin:matrix%imax))
     line_tmp(:)=0.0
     call iotk_scan_dat(unit,"line"//trim(iotk_index(iline1)),line_tmp)
     call ptk_send(line_tmp,owner,10,matrix%comm)
     deallocate(line_tmp)
  endif
  if(matrix%rank/=matrix%root.and.matrix%rank==owner) then
     call ptk_recv(matrix%val(location,:),matrix%root,10,matrix%comm)
  endif     
enddo
if(matrix%rank==matrix%root) then
   call iotk_scan_end(unit,name)     
endif   

end subroutine pw_parall_real_m_read_x

subroutine pw_parall_complex_m_distr_x(fullmatrix,matrix,root,comm)
use ptk_module
use pw_parall_matrix_type, only : pw_parall_complex_matrix
! given a fullmatrix allocated by root or all procs
! root send the lines of fullmatrix to the other procs
! to add the lines to the structure parall_matrix (matrix distributed on procs)
implicit none
complex, intent(in) :: fullmatrix(:,:)
type(pw_parall_complex_matrix),intent(inout) :: matrix
integer, intent(in) :: root
type(ptk_comm), intent(in) :: comm

integer :: iline1
complex, allocatable :: line_tmp(:)
complex, allocatable :: line_tmp2(:)

integer :: owner, location

if(ubound(fullmatrix,1)/=matrix%imax) ERROR("Inconsistent matrix dimension imax")
if(ubound(fullmatrix,1)/=matrix%imax) ERROR("Inconsistent matrix dimension imin")

do iline1=matrix%imin,matrix%imax
  owner=matrix%where_line(1,iline1)
  location=matrix%where_line(2,iline1)
  if(matrix%rank==matrix%root.and.matrix%rank==owner) then
     matrix%val(location,:)=matrix%val(location,:)+fullmatrix(iline1,:)     
  endif
  if(matrix%rank==root.and.matrix%rank/=owner) then
     allocate(line_tmp(matrix%imin:matrix%imax))
     line_tmp(:)=fullmatrix(iline1,:)
     call ptk_send(line_tmp,owner,10,matrix%comm)
     deallocate(line_tmp)
  endif
  if(matrix%rank/=root.and.matrix%rank==owner) then
     allocate(line_tmp2(matrix%imin:matrix%imax))     
     call ptk_recv(line_tmp2(:),root,10,matrix%comm)
     matrix%val(location,:)=matrix%val(location,:)+line_tmp2(:)
     deallocate(line_tmp2)
  endif     
enddo
end subroutine pw_parall_complex_m_distr_x

subroutine pw_parall_real_m_distr_x(fullmatrix,matrix,root,comm)
use ptk_module
use pw_parall_matrix_type, only : pw_parall_real_matrix

implicit none
real, intent(in) :: fullmatrix(:,:)
type(pw_parall_real_matrix),intent(inout) :: matrix
integer, intent(in) :: root
type(ptk_comm), intent(in) :: comm

integer :: iline1
real, allocatable :: line_tmp(:)
real, allocatable :: line_tmp2(:)

integer :: owner, location

if(ubound(fullmatrix,1)/=matrix%imax) ERROR("Inconsistent matrix dimension imax")
if(lbound(fullmatrix,1)/=matrix%imin) ERROR("Inconsistent matrix dimension imin")

do iline1=matrix%imin,matrix%imax
  owner=matrix%where_line(1,iline1)
  location=matrix%where_line(2,iline1)
  if(matrix%rank==root.and.matrix%rank==owner) then
     matrix%val(location,:)=matrix%val(location,:)+fullmatrix(iline1,:)     
  endif
  if(matrix%rank==root.and.matrix%rank/=owner) then
     allocate(line_tmp(matrix%imin:matrix%imax))
     line_tmp(:)=fullmatrix(iline1,:)
     call ptk_send(line_tmp,owner,10,matrix%comm)
     deallocate(line_tmp)
  endif
  if(matrix%rank/=root.and.matrix%rank==owner) then
     allocate(line_tmp2(matrix%imin:matrix%imax))     
     call ptk_recv(line_tmp2(:),root,10,matrix%comm)
     matrix%val(location,:)=matrix%val(location,:)+line_tmp2(:)
     deallocate(line_tmp2)
  endif     
enddo
end subroutine pw_parall_real_m_distr_x

subroutine pw_parall_complex_m_collect_x(fullmatrix,matrix,root,comm)
use ptk_module
use pw_parall_matrix_type, only : pw_parall_complex_matrix
! given a fullmatrix allocated by root or all procs
! root receive (collect) the lines of the structure parall_matrix 
! and add it to fullmatrix
implicit none
type(pw_parall_complex_matrix),intent(in) :: matrix
complex, intent(inout) :: fullmatrix(matrix%imin:matrix%imax,matrix%imin:matrix%imax)
integer, intent(in) :: root
type(ptk_comm), intent(in) :: comm

integer :: iline1
complex, allocatable :: line_tmp(:)

integer :: owner, location

if(ubound(fullmatrix,1)/=matrix%imax) ERROR("Inconsistent matrix dimension imax")
if(lbound(fullmatrix,1)/=matrix%imin) ERROR("Inconsistent matrix dimension imin")
do iline1=matrix%imin,matrix%imax
  owner=matrix%where_line(1,iline1)
  location=matrix%where_line(2,iline1)
  if(matrix%rank==root.and.matrix%rank==owner) then
     fullmatrix(iline1,:)=fullmatrix(iline1,:)+matrix%val(location,:)     
  endif
  if(matrix%rank/=root.and.matrix%rank==owner) then
     call ptk_send(matrix%val(location,:),root,10,matrix%comm)
  endif     
  if(matrix%rank==root.and.matrix%rank/=owner) then
     allocate(line_tmp(matrix%imin:matrix%imax))
     call ptk_recv(line_tmp,owner,10,matrix%comm)
     fullmatrix(iline1,:)=fullmatrix(iline1,:)+line_tmp(:)
     deallocate(line_tmp)
  endif
enddo
end subroutine pw_parall_complex_m_collect_x

subroutine pw_parall_real_m_collect_x(fullmatrix,matrix,root,comm)
use ptk_module
use pw_parall_matrix_type, only : pw_parall_real_matrix
! given a fullmatrix allocated by root or all procs
! root receive (collect) the lines of the structure parall_matrix 
! and add it to fullmatrix
implicit none
type(pw_parall_real_matrix),intent(in) :: matrix
real, intent(inout) :: fullmatrix(matrix%imin:matrix%imax,matrix%imin:matrix%imax)
integer, intent(in) :: root
type(ptk_comm), intent(in) :: comm

integer :: iline1
real, allocatable :: line_tmp(:)

integer :: owner, location

if(ubound(fullmatrix,1)/=matrix%imax) ERROR("Inconsistent matrix dimension imax")
if(lbound(fullmatrix,1)/=matrix%imin) ERROR("Inconsistent matrix dimension imin")

do iline1=matrix%imin,matrix%imax
  owner=matrix%where_line(1,iline1)
  location=matrix%where_line(2,iline1)
  if(matrix%rank==root.and.matrix%rank==owner) then
     fullmatrix(iline1,:)=fullmatrix(iline1,:)+matrix%val(location,:)     
  endif
  if(matrix%rank/=root.and.matrix%rank==owner) then
     call ptk_send(matrix%val(location,:),root,10,matrix%comm)
  endif     
  if(matrix%rank==root.and.matrix%rank/=owner) then
     allocate(line_tmp(matrix%imin:matrix%imax))
     call ptk_recv(line_tmp,owner,10,matrix%comm)
     fullmatrix(iline1,:)=fullmatrix(iline1,:)+line_tmp(:)
     deallocate(line_tmp)
  endif
enddo
end subroutine pw_parall_real_m_collect_x

subroutine pw_parall_complex_m_borrow_line_x(matrix,line,il)

use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm, &
  ptk_allgather, ptk_barrier, ptk_send, ptk_recv, ptk_bcast
use pw_parall_matrix_type, only : pw_parall_complex_matrix
implicit none

type(pw_parall_complex_matrix),intent(in) :: matrix
complex, pointer, optional :: line(:)
integer, optional, intent(in) :: il

integer, allocatable :: request(:)
integer :: ierr, ip, size, rank, ill, where_is, illl
integer :: i

call ptk_comm_rank(matrix%comm,rank)
call ptk_comm_size(matrix%comm,size)

ill=huge(il)
if((present(il).neqv.present(line))) ERROR(" ")
if(present(il)) then
    illl = il
end if
where_is = matrix%where_line(1,illl)
  allocate(request(0:size-1))
  call ptk_allgather(illl,request,matrix%comm)
  if(all(request(:)==request(0))) then
    where_is = matrix%where_line(1,illl)
    if(rank==where_is) then
      line => matrix%val(matrix%where_line(2,illl),:)
    else
        allocate(line(1:matrix%m_dim))
        line(:)=0.0
    end if
    call ptk_bcast(line,where_is,matrix%comm)
  else
    do ip=0,size-1
      ill = request(ip)
      where_is = matrix%where_line(1,ill)
      if(present(il)) then
        if(rank==ip .and. rank==where_is) then
          if(ill/=il) ERROR("")
          line => matrix%val(matrix%where_line(2,ill),:)
        end if
        if(rank==ip .and. rank/=where_is) then
          if(ill/=il) ERROR("")
          allocate(line(1:matrix%m_dim))
          line(:)=0.0
          call ptk_recv(line,where_is,il,matrix%comm)
        end if
      end if
      if(rank/=ip .and. rank==where_is) then
        call ptk_send(matrix%val(matrix%where_line(2,ill),:),ip,ill,matrix%comm)
      end if
    end do
  end if
  deallocate(request)
 
end subroutine pw_parall_complex_m_borrow_line_x

subroutine pw_parall_complex_m_giveback_line_x(matrix,line)
use pw_parall_matrix_type, only : pw_parall_complex_matrix
implicit none

type(pw_parall_complex_matrix),intent(in) :: matrix
complex, pointer, optional :: line(:)

integer :: il
logical :: found

found=.false.
do il=1,matrix%m_dim
  if(associated(line,matrix%val(il,:))) found=.true.
  if(found) exit
enddo 
if(found) then
  nullify(line)
else
  deallocate(line)
endif
end subroutine pw_parall_complex_m_giveback_line_x

