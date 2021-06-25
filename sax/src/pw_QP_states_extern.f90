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

subroutine pw_QP_states_init_x(QP_states,states,QP,root,comm)
  use pw_QP_states_type
  use pw_QP_states_interf
  use pw_QP_module
  use pw_states_type
  use pw_states_interf
  use pw_struct_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type (pw_states),      intent(out) :: QP_states
  type (pw_QP),           intent(in)  :: QP
  type (pw_states), target, intent(in)  :: states
  integer,                  intent(in)  :: root
  type (ptk_comm),          intent(in)  :: comm

  integer :: ib,ik,ib_tmp,nk,nbmin,nbmax
  type (pw_wfc), pointer :: wfc_tmp
  real :: cutoff

  integer :: iboh

  QP_states%struct   => states%struct
  QP_states%symmlist => states%symmlist
  QP_states%kmesh    => states%kmesh
  QP_states%nbmin    =  QP%nbmin 
  QP_states%nbmax    =  QP%nbmax
  QP_states%root     =  states%root
  QP_states%comm     =  comm

  call ptk_comm_size(comm,QP_states%npe)
  call ptk_comm_rank(comm,QP_states%rank)
  if(QP_states%root >= QP_states%npe) ERROR("")

  nbmin=QP%nbmin
  nbmax=QP%nbmax

  QP_states%incomplete=.false.
  QP_states%nband = (nbmax-nbmin)+1
  QP_states%nband_locmax=ceiling(real(QP_states%nband)/real(QP_states%npe))
  
  nk=ubound(states%wfc,2)
  cutoff=pw_states_get_cutoff(states)

  allocate(QP_states%where_band(2,QP_states%nbmin:QP_states%nbmax))
  call pw_allocate(QP_states%where_band)
  do ib=QP_states%nbmin,QP_states%nbmax
      QP_states%where_band(1,ib)=modulo((ib-1)-states%root,QP_states%npe)
      QP_states%where_band(2,ib)=count(QP_states%where_band(1,QP_states%nbmin:ib)==QP_states%where_band(1,ib))
  enddo

  QP_states%nband_loc=(count(QP_states%where_band(1,:)==QP_states%rank))
  if(QP_states%nband_locmax/=QP_states%nband_loc) QP_states%incomplete=.true.

  allocate(QP_states%e(QP_states%nbmin:QP_states%nbmax,states%kmesh%nkibz))
  call pw_allocate(QP_states%e)
  allocate(QP_states%occupation(QP_states%nbmin:QP_states%nbmax,states%kmesh%nkibz))
  call pw_allocate(QP_states%occupation)
  allocate(QP_states%weight(QP_states%nbmin:QP_states%nbmax,states%kmesh%nkibz))
  call pw_allocate(QP_states%weight)
  QP_states%e(:,:) = 0.0
  QP_states%occupation(:,:)=0.0
  QP_states%weight(:,:)=1.0
  do ib=QP_states%nbmin,QP_states%nbmax
     QP_states%e(ib,:)=QP%energies(ib,:)
     QP_states%occupation(ib,:)=states%occupation(ib,:)
     QP_states%weight(ib,:)=states%weight(ib,:)
  enddo

  allocate(QP_states%basis(QP_states%kmesh%nkbz))
  call pw_basis_init(QP_states%basis(:),QP_states%struct)
  do ik=1,nk
    call pw_basis_create(QP_states%basis(ik),QP_states%kmesh%kbz(:,ik),cutoff)
  enddo

  allocate(QP_states%wfc(count(QP_states%where_band(1,:)==QP_states%rank),QP_states%kmesh%nkibz))
  if(ubound(QP_states%wfc,1)>0) call pw_wfc_init(QP_states%wfc(:,:))
  do ik=1,nk
      call pw_wfc_set_basis(QP_states%wfc(:,ik),QP_states%basis(ik))
  enddo

  do ik=1,nk
     iboh=1
     do ib=QP_states%nbmin,QP_states%nbmax
        if(states%where_band(1,ib)==states%rank.or.iboh==(QP_states%nband_loc+1)) then
          do ib_tmp=nbmin,nbmax
             call pw_states_borrow_wfc(states,wfc_tmp,ib_tmp,ik)
             if(QP_states%where_band(1,ib)==states%rank)  &
               QP_states%wfc(QP_states%where_band(2,ib),ik)%val(:)= &
               QP_states%wfc(QP_states%where_band(2,ib),ik)%val(:) + &
               QP%eigenvec(ib_tmp,ib,ik)*wfc_tmp%val(:)
             call pw_states_giveback_wfc(states,wfc_tmp)
          enddo
          if(QP_states%incomplete) iboh=iboh+1
        endif
     enddo
  enddo

end subroutine pw_QP_states_init_x

subroutine pw_QP_states_destroy_x(QP_states)
  use pw_states_type
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  implicit none
  type (pw_states), intent(inout) :: QP_states
  nullify(QP_states%struct)
  nullify(QP_states%kmesh)
  nullify(QP_states%symmlist)
  call pw_deallocate(QP_states%where_band)
  deallocate(QP_states%where_band)
  call pw_wfc_destroy(QP_states%wfc(:,:))
  deallocate(QP_states%wfc)
  call pw_basis_destroy(QP_states%basis(:))
  deallocate(QP_states%basis)
  call pw_deallocate(QP_states%e)
  deallocate(QP_states%e)
  call pw_deallocate(QP_states%occupation)
  deallocate(QP_states%occupation)
  call pw_Deallocate(QP_states%weight)
  deallocate(QP_states%weight)

end subroutine pw_QP_states_destroy_x

function pw_QP_states_is_local_x(QP_states,ib,ik)
  use pw_QP_states_type
  implicit none
  logical :: pw_QP_states_is_local_x
  type (pw_QP_states), intent(in) :: QP_states
  integer, intent(in) :: ib,ik
  pw_QP_states_is_local_x = QP_states%where_band(1,ib)==QP_states%rank
end function pw_QP_states_is_local_x

subroutine pw_QP_states_borrow_basis_x(QP_states,basis,ik)
  use pw_QP_states_type
  use pw_basis_module
  implicit none
  type(pw_QP_states),  target,                 intent(in)  :: QP_states
  type(pw_basis), pointer, optional :: basis
  integer,                 optional, intent(in)  :: ik
  if(present(ik).neqv.present(basis)) ERROR("")
  if(present(ik)) basis => QP_states%basis(ik)
end subroutine pw_QP_states_borrow_basis_x

subroutine pw_QP_states_giveback_basis_x(QP_states,basis)
  use pw_QP_states_type
  use pw_basis_module
  implicit none
  type(pw_QP_states),         intent(in)    :: QP_states
  type(pw_basis), pointer :: basis
  nullify(basis)
end subroutine pw_QP_states_giveback_basis_x

subroutine pw_QP_states_borrow_wfc_x(QP_states,wfc,ib,ik)
  use pw_QP_states_type
  use pw_wfc_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_allgather
  implicit none
  type(pw_QP_states),  target,     intent(in)            :: QP_states
  type(pw_wfc), pointer, optional :: wfc
  integer,               optional, intent(in)  :: ib,ik
  integer, allocatable :: request(:,:)
  integer :: ierr,ip,size,rank,ibb,ikk,where_is,ibl,ikl
  call ptk_comm_size(QP_states%comm,size)
  call ptk_comm_rank(QP_states%comm,rank)
  ibl = huge(ib)
  ikl = huge(ik)
  if((present(ib).neqv.present(ik)) .or. present(ib).neqv.present(wfc)) ERROR("")
  if(present(ib) .and. present(ik)) then
    ibl = ib
    ikl = ik
  end if
  allocate(request(2,0:size-1))
  call ptk_allgather((/ibl,ikl/),request,QP_states%comm)
  if(all(request(1,:)==request(1,0) .and. all(request(2,:)==request(2,0)))) then
    where_is = QP_states%where_band(1,ibl)
    if(rank==where_is) then
      wfc => QP_states%wfc(QP_states%where_band(2,ibl),ikl)
    else
        allocate(wfc)
        call pw_wfc_init(wfc,QP_states%basis(ik))
    end if
    call pw_wfc_bcast(wfc,where_is,QP_states%comm)
  else
    do ip=0,size-1
      ibb = request(1,ip)
      ikk = request(2,ip)
      where_is = QP_states%where_band(1,ibb)
      if(present(ib)) then
        if(rank==ip .and. rank==where_is) then
          if(ibb/=ib) ERROR("")
          if(ikk/=ik) ERROR("")
          wfc => QP_states%wfc(QP_states%where_band(2,ibb),ikk)
        end if
        if(rank==ip .and. rank/=where_is) then
          if(ibb/=ib) ERROR("")
          if(ikk/=ik) ERROR("")
          allocate(wfc)
          call pw_wfc_init(wfc,QP_states%basis(ik))
          call pw_wfc_recv(wfc,where_is,ib,QP_states%comm)
        end if
      end if
      if(rank/=ip .and. rank==where_is) then
        call pw_wfc_send(QP_states%wfc(QP_states%where_band(2,ibb),ikk),ip,ibb,QP_states%comm)
      end if
    end do
  end if
  deallocate(request)
end subroutine pw_QP_states_borrow_wfc_x

subroutine pw_QP_states_giveback_wfc_x(QP_states,wfc)
  use pw_QP_states_type
  use pw_wfc_module
  implicit none
  type(pw_QP_states), intent(in) :: QP_states  
  type(pw_wfc), pointer, optional :: wfc
  integer :: ib,ik
  logical :: found
  found = .false.
  do ik=lbound(QP_states%wfc,2),ubound(QP_states%wfc,2)
    do ib=lbound(QP_states%wfc,1),ubound(QP_states%wfc,1)
      if(associated(wfc,QP_states%wfc(ib,ik))) found = .true.
      if(found) exit
    end do
    if(found) exit
  end do
  if(found) then
    nullify(wfc)
  else
    call pw_wfc_destroy(wfc)
    deallocate(wfc)
  end if
end subroutine pw_QP_states_giveback_wfc_x

function pw_QP_states_get_cutoff_x(states)
  use pw_QP_states_type
  use pw_basis_module
  implicit none
  real :: pw_QP_states_get_cutoff_x
  type (pw_QP_states), intent(in) :: states
  pw_QP_states_get_cutoff_x = pw_basis_get_cutoff(states%basis)
end function pw_QP_states_get_cutoff_x
