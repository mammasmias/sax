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

subroutine pw_states_init_x(states,struct,symmlist,kmesh,nbmin,nbmax,root,comm,do_not_alloc,superparall)
  use pw_states_type
  use pw_states_interf
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use pw_smearing_module
  implicit none
  type (pw_states),           intent(out) :: states
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: kmesh
  integer,                    intent(in)  :: nbmin,nbmax,root
  type (ptk_comm),            intent(in)  :: comm
  logical,         optional, intent(in) :: do_not_alloc
  logical,         optional, intent(in) :: superparall

  integer :: ib

  logical :: do_not_alloc_loc
  logical :: superparall_loc

  states%struct   => struct
  states%symmlist => symmlist
  states%kmesh    => kmesh
  states%nbmin    =  nbmin
  states%nbmax    =  nbmax
  states%root     =  root
  states%comm     =  comm

  states%smearing%smearing_kind = "none"

  call ptk_comm_size(comm,states%npe)
  call ptk_comm_rank(comm,states%rank)

  if(states%root >= states%npe) ERROR("")

  do_not_alloc_loc = .false.
  if(present(do_not_alloc)) do_not_alloc_loc = do_not_alloc
  states%allocated_basis_wfc = (.not.do_not_alloc_loc)

  superparall_loc = .false.
  if(present(superparall)) superparall_loc = superparall
  states%superparall = superparall_loc


  if(.not.do_not_alloc_loc) call pw_states_init_basis_wfc(states,struct,symmlist,kmesh,nbmin,nbmax,root,comm)

!  if(.not.associated(states%e)) then
    allocate(states%e(nbmin:nbmax,kmesh%nkibz))
    call pw_allocate(states%e)
    states%e(:,:) = 0.0
!  endif

!  if(.not.associated(states%occupation)) then
    allocate(states%occupation(nbmin:nbmax,kmesh%nkibz))
    call pw_allocate(states%occupation)
    states%occupation = 0.0
!  endif

!  if(.not.associated(states%weight)) then
    allocate(states%weight(nbmin:nbmax,kmesh%nkibz))
    call pw_allocate(states%weight)
    states%weight = 1.0
!  endif

end subroutine pw_states_init_x

subroutine pw_states_init_basis_wfc_x(states,struct,symmlist,kmesh,nbmin,nbmax,root,comm)
  use pw_states_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use pw_smearing_module
  use optimal_proc_module
  implicit none
  type (pw_states),           intent(out) :: states
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: kmesh
  integer,                    intent(in)  :: nbmin,nbmax,root
  type (ptk_comm),            intent(in)  :: comm

  integer :: ib, ik
  integer :: nbvmin,  nbvmax, nbcmin, nbcmax

  if(.not.associated(states%struct)) states%struct  => struct
  if(.not.associated(states%symmlist)) states%symmlist => symmlist
  if(.not.associated(states%kmesh)) states%kmesh    => kmesh
  states%nbmin    =  nbmin
  states%nbmax    =  nbmax
  states%root     =  root
  states%comm     =  comm

  states%smearing%smearing_kind = "none"

  call ptk_comm_size(comm,states%npe)
  call ptk_comm_rank(comm,states%rank)

  if(states%root >= states%npe) ERROR("")

  if(.not.states%superparall) then
    states%incomplete=.false.
    states%nband=(nbmax-nbmin)+1
    states%nband_locmax=ceiling(real(states%nband)/real(states%npe))

    allocate(states%where_band(2,nbmin:nbmax))
    call pw_allocate(states%where_band)
    do ib=nbmin,nbmax
      states%where_band(1,ib) = modulo((ib-1)-root,states%npe)
      states%where_band(2,ib) = count(states%where_band(1,nbmin:ib)==states%where_band(1,ib))
    end do

    states%nband_loc=count(states%where_band(1,:)==states%rank)
    if(states%nband_locmax/=states%nband_loc) states%incomplete=.true.

    allocate(states%basis(kmesh%nkibz))
    call pw_basis_init(states%basis(:),states%struct)

    allocate(states%wfc(count(states%where_band(1,:)==states%rank),kmesh%nkibz))
    if(ubound(states%wfc,1) > 0) &
      call pw_wfc_init(states%wfc(:,:))
  else
   states%incomplete=.false.
   states%nband=(nbmax-nbmin) + 1

!------------------------------------------
! determination of nbvmax nbvmin nbcmin nbcmax
   if(states%occupation(nbmin,1) > 1 ) then
     nbvmin = nbmin
   else
     ERROR("firts band is unoccupied!!!!! WRONG!!!")
   endif
   ik=1
   nbvmax = nbmin-1
   do ib=nbmin,nbmax
     if(states%occupation(ib,ik) > 1 ) nbvmax = nbvmax +1
   enddo
   nbcmin = nbvmax + 1
   if(states%occupation(nbmax,1) > 1) ERROR("last band is occupied!!!! WRONG!!!")
   nbcmax = nbmax
! ----------------------------------------
    allocate(states%where_band(2,nbmin:nbmax))
    call pw_allocate(states%where_band)
    states%where_band(1,:) = -1
    states%where_band(2,:) = 0
   allocate (states%transitions(nbvmin:nbvmax,nbcmin:nbcmax))
   states%transitions(:,:) = 0
   call optimal_proc_find(states%transitions,states%npe)
   do ib =nbvmin,nbvmax
     if(ANY(states%transitions(ib,:)==states%rank)) then
        states%where_band(1,ib) = states%rank
        states%where_band(2,ib) = count(states%where_band(1,nbvmin:ib)==states%where_band(1,ib))
     endif
   enddo
   do ib =nbcmin,nbcmax
     if(ANY(states%transitions(:,ib)==states%rank)) then
        states%where_band(1,ib) = states%rank
        states%where_band(2,ib) = count(states%where_band(1,nbvmin:ib)==states%where_band(1,ib))
     endif
   enddo

    states%nband_loc=count(states%where_band(1,:)==states%rank)
!    if(states%nband_locmax/=states%nband_loc) states%incomplete=.true.

    allocate(states%basis(kmesh%nkibz))
    call pw_basis_init(states%basis(:),states%struct)

    allocate(states%wfc(count(states%where_band(1,:)==states%rank),kmesh%nkibz))
    if(ubound(states%wfc,1) > 0) &
      call pw_wfc_init(states%wfc(:,:))

  endif
   
end subroutine pw_states_init_basis_wfc_x


subroutine pw_states_init_smearing_x(states,smearing,struct,symmlist,kmesh,nbmin,nbmax,root,comm)
  use pw_states_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use pw_smearing_module
  implicit none
  type (pw_states),           intent(out) :: states
  type (pw_smearing),         intent(in)  :: smearing
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: kmesh
  integer,                    intent(in)  :: nbmin,nbmax,root
  type (ptk_comm),            intent(in)  :: comm

  integer :: ib

  states%struct   => struct
  states%symmlist => symmlist
  states%kmesh    => kmesh
  states%nbmin    =  nbmin
  states%nbmax    =  nbmax
  states%root     =  root
  states%comm     =  comm

  states%smearing%smearing = smearing%smearing
  states%smearing%smearing_kind = smearing%smearing_kind

  call ptk_comm_size(comm,states%npe)
  call ptk_comm_rank(comm,states%rank)
  if(states%root >= states%npe) ERROR("")

  states%incomplete=.false.
  states%nband=(nbmax-nbmin)+1
  states%nband_locmax=ceiling(real(states%nband)/real(states%npe))

  allocate(states%where_band(2,nbmin:nbmax))
  call pw_allocate(states%where_band)
  do ib=nbmin,nbmax
    states%where_band(1,ib) = modulo((ib-1)-root,states%npe)
    states%where_band(2,ib) = count(states%where_band(1,nbmin:ib)==states%where_band(1,ib))
  end do

  states%nband_loc=count(states%where_band(1,:)==states%rank)
  if(states%nband_locmax/=states%nband_loc) states%incomplete=.true.

  allocate(states%basis(kmesh%nkibz))
  call pw_basis_init(states%basis(:),states%struct)

  allocate(states%wfc(count(states%where_band(1,:)==states%rank),kmesh%nkibz))
  if(ubound(states%wfc,1) > 0) &
    call pw_wfc_init(states%wfc(:,:))

  allocate(states%e(nbmin:nbmax,kmesh%nkibz))
  call pw_allocate(states%e)
  states%e(:,:) = 0.0

  allocate(states%occupation(nbmin:nbmax,kmesh%nkibz))
  call pw_allocate(states%occupation)
  states%occupation = 0.0

  allocate(states%weight(nbmin:nbmax,kmesh%nkibz))
  call pw_allocate(states%weight)
  states%weight = 1.0

end subroutine pw_states_init_smearing_x

subroutine pw_states_destroy_x(states)
  use pw_states_type
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  implicit none
  type (pw_states), intent(inout) :: states
  if(associated(states%struct)) nullify(states%struct)
  if(associated(states%symmlist)) nullify(states%symmlist)
  if(associated(states%kmesh)) nullify(states%kmesh)
  call pw_states_destroy_basis_wfc_x(states)
!  call pw_deallocate(states%e)
  if(associated(states%e)) then
    deallocate(states%e)
    nullify(states%e)
  endif
!  call pw_deallocate(states%occupation)
  if(associated(states%occupation)) then
    deallocate(states%occupation)
    nullify(states%occupation)
  endif
!  call pw_deallocate(states%weight)
  if(associated(states%weight)) then
    deallocate(states%weight)
    nullify(states%weight)
  endif
end subroutine pw_states_destroy_x

subroutine pw_states_destroy_basis_wfc_x(states)
  use pw_states_type
  use pw_common_module
  use pw_wfc_module
  use pw_basis_module
  implicit none
  type (pw_states), intent(inout) :: states
  if(associated(states%struct)) nullify(states%struct)
  if(associated(states%symmlist)) nullify(states%symmlist)
  if(associated(states%kmesh)) nullify(states%kmesh)

!  call pw_deallocate(states%where_band)
  if(associated(states%where_band)) then
     deallocate(states%where_band)
     nullify(states%where_band)
  endif
!  call pw_wfc_destroy(states%wfc(:,:))
  if(associated(states%wfc)) then
     call pw_wfc_destroy(states%wfc(:,:))
     deallocate(states%wfc)
     nullify(states%wfc)
  endif
!  call pw_basis_destroy(states%basis(:))
  if(associated(states%basis)) then
     call pw_basis_destroy(states%basis(:))
     deallocate(states%basis)
     nullify(states%basis)
   endif
end subroutine pw_states_destroy_basis_wfc_x

function pw_states_get_cutoff_x(states)
  use pw_states_type
  use pw_basis_module
  implicit none
  real :: pw_states_get_cutoff_x
  type (pw_states), intent(in) :: states
  pw_states_get_cutoff_x = pw_basis_get_cutoff(states%basis)
end function pw_states_get_cutoff_x

function pw_states_is_local_x(states,ib,ik)
  use pw_states_type
  implicit none
  logical :: pw_states_is_local_x
  type (pw_states), intent(in) :: states
  integer, intent(in) :: ib,ik
  pw_states_is_local_x = states%where_band(1,ib)==states%rank
end function pw_states_is_local_x

function pw_trans_is_local_x(trans,ib,jb,rank)
  implicit none
  logical :: pw_trans_is_local_x
  integer, pointer,  intent(in) :: trans(:,:)
  integer, intent(in) :: ib,jb
  integer, intent(in) :: rank
  pw_trans_is_local_x = trans(ib,jb)==rank
end function pw_trans_is_local_x

subroutine pw_states_read_x(states,unit,name)
  use pw_states_type
  use pw_wfc_module
  use pw_basis_module
  use ptk_module, only : ptk_bcast
  use iotk_module
  implicit none
  type (pw_states), intent(inout) :: states
  integer,          intent(in)    :: unit
  character(*),     intent(in)    :: name
  integer :: ib,ik,ierr
  type (pw_wfc) :: wfc_tmp
  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype

  if(states%rank == states%root) then
    call iotk_scan_begin(unit,name,attr)
    call iotk_scan_attr (attr,"type",rtype,default="pw_states")
    if(rtype/="pw_states") ERROR("")
  end if
  if(states%allocated_basis_wfc) call pw_states_read_basis_wfc_x(states,unit,name)
  if(states%rank == states%root) then
    call iotk_scan_dat(unit,"efermi",states%efermi,default=0.0)
    call iotk_scan_dat(unit,"eband",states%e)
    call iotk_scan_dat(unit,"occupation",states%occupation)
    call iotk_scan_dat(unit,"weight",states%weight)
    call iotk_scan_end(unit,name)
  end if
  call ptk_bcast(states%e,states%root,states%comm)
  call ptk_bcast(states%occupation,states%root,states%comm)
  call ptk_bcast(states%weight,states%root,states%comm)
  call ptk_bcast(states%efermi,states%root,states%comm)
end subroutine pw_states_read_x

subroutine pw_states_read_basis_wfc_x(states,unit,name)
  use pw_states_type
  use pw_wfc_module
  use pw_basis_module
  use ptk_module, only : ptk_bcast
  use iotk_module
  implicit none
  type (pw_states), intent(inout) :: states
  integer,          intent(in)    :: unit
  character(*),     intent(in)    :: name
  integer :: ib,ik,ierr
  type (pw_wfc) :: wfc_tmp
  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype

  if(.not.states%superparall) then

    do ik=lbound(states%wfc,2),ubound(states%wfc,2)
      if(states%rank == states%root) then
        call pw_basis_read(states%basis(ik),unit,"iotk",name="basis"//trim(iotk_index(ik)))
      end if
      call pw_basis_bcast(states%basis(ik),states%root,states%comm)
      call pw_wfc_set_basis(states%wfc(:,ik),states%basis(ik))
      if(states%rank == states%root) call pw_wfc_init(wfc_tmp,states%basis(ik))
      do ib=states%nbmin,states%nbmax
        if(states%rank == states%root) then
          call pw_wfc_read(wfc_tmp,unit,"iotk",name="wfc"//trim(iotk_index((/ik,ib/))))
!          call pw_wfc_fix_phase(wfc_tmp)
          if(states%where_band(1,ib) == states%rank) then
            states%wfc(states%where_band(2,ib),ik) = wfc_tmp
          else
            call pw_wfc_send(wfc_tmp,states%where_band(1,ib),ib,states%comm)
          end if
        else
          if(states%where_band(1,ib) == states%rank) then
            call pw_wfc_recv(states%wfc(states%where_band(2,ib),ik), &
                 states%root,ib,states%comm)
          end if
        end if
      end do
      if(states%rank == states%root) call pw_wfc_destroy(wfc_tmp)
    end do

 else
    if(states%rank/=states%root) then
      call iotk_scan_begin(unit,name,attr)
      call iotk_scan_attr (attr,"type",rtype,default="pw_states")
      if(rtype/="pw_states") ERROR("")
    endif
    do ik=lbound(states%wfc,2),ubound(states%wfc,2)
      if(states%rank == states%root) then
        call pw_basis_read(states%basis(ik),unit,"iotk",name="basis"//trim(iotk_index(ik)))
      end if
      call pw_basis_bcast(states%basis(ik),states%root,states%comm)
      call pw_wfc_set_basis(states%wfc(:,ik),states%basis(ik))

      call pw_wfc_init(wfc_tmp,states%basis(ik))
      do ib=states%nbmin,states%nbmax
        if(states%rank==states%where_band(1,ib))  then 
          call pw_wfc_read(wfc_tmp,unit,"iotk",name="wfc"//trim(iotk_index((/ik,ib/))))
!          call pw_wfc_fix_phase(wfc_tmp)
          states%wfc(states%where_band(2,ib),ik) = wfc_tmp
        end if
      end do
      call pw_wfc_destroy(wfc_tmp)
    end do
    if(states%rank/=states%root) call iotk_scan_end(unit,name)
 endif
end subroutine pw_states_read_basis_wfc_x

subroutine pw_states_write_x(states,unit,name)
  use pw_states_type
  use pw_wfc_module
  use pw_basis_module
  use iotk_module
  implicit none
  type (pw_states), intent(in) :: states
  integer,          intent(in) :: unit
  character(*),     intent(in) :: name

  integer ::ik,ib,unit_tmp,ierr
  type (pw_wfc) :: wfc_tmp
  character(len=iotk_attlenx) :: attr

  integer :: location, owner

  if(states%rank == states%root) then
    call iotk_write_attr (attr,"type","pw_states",first=.true.)
    call iotk_write_begin(unit,name,attr)
  end if

  do ik=lbound(states%wfc,2),ubound(states%wfc,2)
    if(states%rank == states%root) then
      call pw_wfc_init(wfc_tmp,states%basis(ik))
      call pw_basis_write(states%basis(ik),unit,"iotk",name="basis"//trim(iotk_index(ik)))
    end if
    do ib=states%nbmin,states%nbmax
!       owner=states%where_band(1,ib)
!       location=states%where_band(2,ib)
!       if(states%rank==states%root.and.states%rank==owner) then
!         wfc_tmp = states%wfc(location,ik)
!         call pw_wfc_write(wfc_tmp,unit,"iotk",name="wfc"//trim(iotk_index((/ik,ib/))))
!       endif
!       if(states%rank==states%root.and.states%rank/=owner) then
!         call pw_wfc_recv(wfc_tmp,owner,ib,states%comm)
!       endif
!       if(states%rank/=states%root.and.states%rank==owner) then
!         call pw_wfc_send(states%wfc(location,ik),states%root,ib,states%comm)
!       endif
      if(states%rank == states%root) then
        if(states%where_band(1,ib) == states%rank) then
          wfc_tmp = states%wfc(states%where_band(2,ib),ik)
        else
          call pw_wfc_recv(wfc_tmp,states%where_band(1,ib),ib,states%comm)
        end if
        call pw_wfc_write(wfc_tmp,unit,"iotk",name="wfc"//trim(iotk_index((/ik,ib/))))
      else
        if(states%where_band(1,ib) == states%rank) then
          call pw_wfc_send(states%wfc(states%where_band(2,ib),ik),states%root,ib,states%comm)
        end if
      end if
    end do
    if(states%rank == states%root) call pw_wfc_destroy(wfc_tmp)
  end do
  if(states%rank == states%root) then
    call iotk_write_dat(unit,"efermi",states%efermi)
    call iotk_write_dat(unit,"eband",states%e)
    call iotk_write_dat(unit,"occupation",states%occupation)
    call iotk_write_dat(unit,"weight",states%weight)
    call iotk_write_end(unit,name)
  end if

end subroutine pw_states_write_x

subroutine pw_states_convert_from_newp_x(states,file,unit,nelec,name)
  use pw_states_interf
  use pw_states_type
  use pw_basis_module
  use pw_wfc_module
  use iotk_module
  use ptk_module, only : ptk_barrier
  implicit none
  type (pw_states), intent(in) :: states ! serve per dedurre i vari parametri (nbande, mesh, etc)
  character(len=*), intent(in) :: file   ! punch file
  integer,          intent(in) :: unit   ! unita' di scrittura
  real,             intent(in) :: nelec
  character(len=*), intent(in) :: name
  integer :: ik,ib
  real, allocatable            :: eband(:,:)
  real, allocatable            :: occupation(:,:)
  real, allocatable            :: weight(:,:)
  type (pw_basis), allocatable :: basis(:)
  type (pw_wfc)   :: wfc
  integer :: read_unit,nbnd_tmp
  real, allocatable :: eband_tmp(:)
  integer,allocatable :: index(:)
  real :: efermi
  character(iotk_attlenx) :: attr
  integer :: nbmin, nbmax, nkmin,nkmax
  if(states%rank == states%root) then

    nbmin=states%nbmin
    nbmax=states%nbmax
    nkmin=lbound(states%wfc,2)
    nkmax=ubound(states%wfc,2)

    allocate(eband(nbmin:nbmax,nkmin:nkmax))
    allocate(occupation(nbmin:nbmax,nkmin:nkmax))
    allocate(weight(nbmin:nbmax,nkmin:nkmax))

    eband(:,:)=0.0
    occupation(:,:)=0.0
    weight(:,:)=0.0  

    call iotk_free_unit(read_unit)
    call iotk_open_read(read_unit,file=trim(file))
    allocate(basis(lbound(states%wfc,2):ubound(states%wfc,2)))
    call pw_basis_init(basis,states%struct)

    write(0,*) "init read Wfc_grids"

    call iotk_scan_begin(read_unit,"Wfc_grids")
    do ik=lbound(states%wfc,2),ubound(states%wfc,2)
      call pw_basis_read(basis(ik),read_unit,"pw_punch",rec=ik)
      basis(ik)%k(:) = states%kmesh%kibz(:,ik)
    end do
    call iotk_scan_end  (read_unit,"Wfc_grids")

    write(0,*) "end read Wfc_grids"

    call iotk_scan_begin(read_unit,"Eigenvalues",attr=attr)
    call iotk_scan_attr(attr,"nbnd",nbnd_tmp)
    allocate(eband_tmp(nbnd_tmp))
    do ik=lbound(states%wfc,2),ubound(states%wfc,2)
      call iotk_scan_dat(read_unit,"e"//trim(iotk_index(ik)),eband_tmp)
      eband(:,ik) = eband_tmp(states%nbmin:states%nbmax)
    end do
    deallocate(eband_tmp)
    call iotk_scan_end  (read_unit,"Eigenvalues")

    write(0,*) "end read Eigenvalues"

    call iotk_write_attr (attr,"type","pw_states",first=.true.)
    call iotk_write_begin(unit,name,attr)

    call iotk_scan_begin(read_unit,"Eigenvectors")
    do ik=lbound(states%wfc,2),ubound(states%wfc,2)
      allocate(index(basis(ik)%npw))
      call pw_basis_sort_index(basis(ik),index)
      call pw_basis_sort(basis(ik),index)
      call pw_basis_write(basis(ik),unit,"iotk",name="basis"//trim(iotk_index(ik)))
      call pw_wfc_init(wfc,basis(ik))
      call iotk_scan_begin(read_unit,"Kpoint"//iotk_index(ik))
      do ib=states%nbmin,states%nbmax
        call pw_wfc_read    (wfc,read_unit,"pw_punch",rec=ib)
        call pw_wfc_sort(wfc,index)
        call pw_wfc_write(wfc,unit,"iotk",name="wfc"//trim(iotk_index((/ik,ib/))))
      end do
      call iotk_scan_end  (read_unit,"Kpoint"//iotk_index(ik))
      call pw_wfc_destroy(wfc)
      deallocate(index)
    end do
    call iotk_scan_end  (read_unit,"Eigenvectors")
    call pw_basis_destroy(basis)
    deallocate(basis)

    write(0,*) "end read Eigenvectors"

    call iotk_close_read(read_unit)
    weight(:,:) = 1.0/states%kmesh%nkibz
    call pw_states_occupation(occupation,weight,eband,efermi,nelec,states%smearing)

    write(0,*) "efermi from newp ", efermi

    call iotk_write_dat(unit,"efermi",efermi)
    call iotk_write_dat(unit,"eband",eband)
    call iotk_write_dat(unit,"occupation",occupation)
    call iotk_write_dat(unit,"weight",weight)

    call iotk_write_end(unit,name)

    deallocate(eband)
    deallocate(occupation)
    deallocate(weight)
  endif

  call ptk_barrier(states%comm)

end subroutine pw_states_convert_from_newp_x
    
! NOTA:
! fatta sulla falsariga di pw_states_convert_from_newp, ma non ancora debuggata, OCCHIO!
subroutine pw_states_convert_from_pw104_x(states,unit,nelec,name)
  use pw_states_interf
  use pw_states_type
  use pw_wfc_module
  use pw_basis_module
  use tools_module
  use iotk_module
  implicit none
  type (pw_states), intent(in) :: states ! serve per dedurre i vari parametri (nbande, mesh, etc)
  integer,          intent(in) :: unit
  real,             intent(in) :: nelec
  character(len=*), intent(in) :: name
!@ END MANUAL
  character(iotk_attlenx) :: attr
  integer :: ik,ib
  real            :: eband(states%nbmin:states%nbmax,lbound(states%wfc,2):ubound(states%wfc,2))
  real            :: occupation(states%nbmin:states%nbmax,lbound(states%wfc,2):ubound(states%wfc,2))
  real            :: weight(states%nbmin:states%nbmax,lbound(states%wfc,2):ubound(states%wfc,2))
  type (pw_basis) :: basis
  type (pw_wfc)   :: wfc
  integer :: read_unit
  integer,allocatable :: index(:)
  real :: efermi
  if(states%rank /= states%root) return
! SOLO ROOT
  call iotk_write_attr (attr,"type","pw_states",first=.true.)
  call iotk_write_begin(unit,name,attr)

  call iotk_free_unit(read_unit)
  call pw_basis_init(basis,states%struct)
  do ik=lbound(states%wfc,2),ubound(states%wfc,2)
    open(read_unit,file="grid."//tools_char(ik,3),form="unformatted",status="old")
    call pw_basis_read(basis,read_unit,"pw104")
    close(read_unit)
    allocate(index(basis%npw))
    basis%k(:) = states%kmesh%kibz(:,ik)
    call pw_basis_sort_index(basis,index)
    call pw_basis_sort(basis,index)
    call pw_basis_write(basis,unit,"iotk",name="basis"//trim(iotk_index(ik)))
    call pw_wfc_init(wfc,basis)
    do ib=states%nbmin,states%nbmax
      open(read_unit,file="wfc."//tools_char(ib,3)//"-"//tools_char(ik,3), &
           form="unformatted",status="old")
      call pw_wfc_read(wfc,read_unit,"pw104",eband=eband(ib,ik))
      close(read_unit)
      call pw_wfc_sort(wfc,index)
      call pw_wfc_write(wfc,unit,"iotk",name="wfc"//trim(iotk_index((/ik,ib/))))
    end do
    deallocate(index)
    call pw_wfc_destroy(wfc)
  end do
  call pw_basis_destroy(basis)
  
  weight(:,:) = 1.0/states%kmesh%nkibz
  call pw_states_occupation(occupation,weight,eband,efermi,nelec,states%smearing)

  call iotk_write_dat(unit,"eband",eband)
  call iotk_write_dat(unit,"occupation",occupation)
  call iotk_write_dat(unit,"weight",weight)

  call iotk_write_end(unit,name)
end subroutine pw_states_convert_from_pw104_x

subroutine pw_states_occupation_x(occupation,weight,eband,efermi,nelec,smearing)
  use pw_states_type
  use pw_smearing_module
  use num_module
  implicit none
  real, intent(out) :: occupation(:,:)
  real, intent(in)  :: eband(:,:), weight(:,:)
  real, intent(in)  :: nelec
  real, intent(out) :: efermi
  type(pw_smearing) :: smearing 
  real              :: homo,lumo,sum,scalefactor
  integer           :: ib,ik,irec,Ngauss

  if(any(shape(occupation)/=shape(eband))) ERROR("Bad allocation of occupations array")

  if(smearing%smearing_kind /="none")then

      write(0,*) 
      write(0,*) "the system is a metal"
      write(0,*)
      
      select case (smearing%smearing_kind)
      case ("gauss","gaussian")
      write(0,*) "smearing kind in use: gaussian"  
      Ngauss=0
      case ("methfessel-paxton","m-p","mp")
      write(0,*) "smearing kind in use: Methfessel-Paxton"  
      Ngauss=1
      case ("marzari-vanderbilt","cold","m-v","mv")  
      write(0,*) "smearing kind in use: Marzari-Vanderbilt"  
      Ngauss=-1
      case ("fermi-dirac","f-d","fd") 
      write(0,*) "smearing kind in use: Fermi-Dirac"  
      Ngauss=-99
      case default
      ERROR("Stay cool, it's just a wrong smearing_kind")
      end select
!
! Fermi energy calculation via bisection method
!
      efermi = efermig (eband, ubound(eband,1), ubound(eband,2), &
                        nelec, weight, smearing%smearing, Ngauss) 

      write(0,*) 
      write(0,*) "Fermi energy: ",efermi*num_ry2ev
      write(0,*) 
!
! Comparison between Fermi energy and eigenvalues: fractionary occupations calculation
!   
      do ib=lbound(eband,1),ubound(eband,1)
         do ik=lbound(eband,2),ubound(eband,2)

             occupation(ib,ik) = 2.0 * wgauss((efermi-eband(ib,ik))/smearing%smearing,Ngauss) 

         enddo
      enddo 

  else
      
      write(0,*) 
      write(0,*) "the system is a dielectric" 
      write(0,*)
!
! Detection of the last occupied state
!
      homo = maxval(eband(lbound(eband,1):int(nelec)/2,:))
!
! Detection of the first unoccupied state
! 
      lumo = minval(eband(int(nelec)/2+1:ubound(eband,1),:))

      efermi = homo + (lumo-homo) / 2.0
 
      write(0,*) 
      write(0,*) "Fermi energy: ",efermi*num_ry2ev
      write(0,*) "HOMO: ",homo*num_ry2ev
      write(0,*) "LUMO: ",lumo*num_ry2ev
      write(0,*) 
!
! Comparison between Fermi energy and eigenvalues: integer occupations calculation
!
      do ib=lbound(eband,1),ubound(eband,1)
         do ik=lbound(eband,2),ubound(eband,2)

            if(eband(ib,ik) <= efermi) then
               occupation(ib,ik) = 2.0
            else
               occupation(ib,ik) = 0.0
            end if

         end do
      end do

  endif

!
! Check on occupations
!
  sum = 0.0
  do ib=lbound(eband,1),ubound(eband,1)
      do ik=lbound(eband,2),ubound(eband,2)
               
             sum = sum + occupation(ib,ik) 

      end do
  end do
   
  write(0,*) ubound(eband,1),ubound(eband,2),sum,nelec

  if (nint(sum/ubound(eband,2)) /= nelec) ERROR("Bad occupations assignment") 

end subroutine pw_states_occupation_x

subroutine pw_states_time_reversal_x(states)
  use pw_states_type
  use pw_wfc_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_symm_module
  use pw_symmlist_module
  use pw_qtools_module
  implicit none
  type(pw_states), intent(inout) :: states
  integer :: ik1,ik2,nk
  real    :: k1(3),k2(3)
  integer :: isymm
  type (pw_symmlist) :: symmlist
  call pw_symmlist_init(symmlist,states%struct)
  call pw_symmlist_set_nsymm(symmlist,2)
  call pw_symm_set(symmlist%symm(1),.false.,reshape((/1,0,0,0,1,0,0,0,1/),(/3,3/)),(/0.0,0.0,0.0/),.true.)
  call pw_symm_set(symmlist%symm(2),.true., reshape((/1,0,0,0,1,0,0,0,1/),(/3,3/)),(/0.0,0.0,0.0/),.true.)

  nk = states%kmesh%nkbz
  do ik1 = 1,nk
    do ik2 = ik1+1,nk
      k1 = states%kmesh%kbz(:,ik1)
      k2 = states%kmesh%kbz(:,ik2)
      call pw_qtools_equivalent(k1,k2,symmlist%symm,isymm)
      if(isymm==2) then
        call pw_wfc_set_basis(states%wfc(:,ik2),states%basis(ik1))
        states%wfc(:,ik2) = states%wfc(:,ik1)
        call pw_basis_apply_symm(states%basis(ik2),states%basis(ik1),symmlist%symm(isymm))
        call pw_wfc_change_basis(states%wfc(:,ik2),states%basis(ik2))
      end if
    end do
  end do
  
  call pw_symmlist_destroy(symmlist)
end subroutine pw_states_time_reversal_x

subroutine pw_states_borrow_basis_x(states,basis,ik)
  use pw_states_type
  use pw_basis_module
  implicit none
  type(pw_states),                   intent(in)  :: states
  type(pw_basis), pointer, optional :: basis
  integer,                 optional, intent(in)  :: ik
  if(present(ik).neqv.present(basis)) ERROR("")
  if(present(ik)) basis => states%basis(ik)
end subroutine pw_states_borrow_basis_x

subroutine pw_states_giveback_basis_x(states,basis)
  use pw_states_type
  use pw_basis_module
  implicit none
  type(pw_states),         intent(in)    :: states
  type(pw_basis), pointer :: basis
  nullify(basis)
end subroutine pw_states_giveback_basis_x

subroutine pw_states_borrow_wfc_x(states,wfc,ib,ik)
  use pw_states_type
  use pw_wfc_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_allgather, &
    ptk_barrier
  implicit none
  type(pw_states),       intent(in)            :: states
  type(pw_wfc), pointer, optional :: wfc
  integer,               optional, intent(in)  :: ib,ik
  integer, allocatable :: request(:,:)
  integer :: ierr,ip,size,rank,ibb,ikk,where_is,ibl,ikl
  call ptk_comm_size(states%comm,size)
  call ptk_comm_rank(states%comm,rank)
  ibl = huge(ib)
  ikl = huge(ik)
  if((present(ib).neqv.present(ik)) .or. present(ib).neqv.present(wfc)) ERROR("")
  if(present(ib) .and. present(ik)) then
    ibl = ib
    ikl = ik
  end if
   where_is = states%where_band(1,ibl)
  allocate(request(2,0:size-1))
  call ptk_allgather((/ibl,ikl/),request,states%comm)
  if(all(request(1,:)==request(1,0) .and. all(request(2,:)==request(2,0)))) then
    where_is = states%where_band(1,ibl)
    if(rank==where_is) then
      wfc => states%wfc(states%where_band(2,ibl),ikl)
    else
        allocate(wfc)
        call pw_wfc_init(wfc,states%basis(ik))
    end if
    call pw_wfc_bcast(wfc,where_is,states%comm)
  else
    do ip=0,size-1
      ibb = request(1,ip)
      ikk = request(2,ip)
      where_is = states%where_band(1,ibb)
      if(present(ib)) then
        if(rank==ip .and. rank==where_is) then
          if(ibb/=ib) ERROR("")
          if(ikk/=ik) ERROR("")
          wfc => states%wfc(states%where_band(2,ibb),ikk)
        end if
        if(rank==ip .and. rank/=where_is) then
          if(ibb/=ib) ERROR("")
          if(ikk/=ik) ERROR("")
          allocate(wfc)
          call pw_wfc_init(wfc,states%basis(ik))
          call pw_wfc_recv(wfc,where_is,ib,states%comm)
        end if
      end if
      if(rank/=ip .and. rank==where_is) then
        call pw_wfc_send(states%wfc(states%where_band(2,ibb),ikk),ip,ibb,states%comm)
      end if
    end do
  end if
  deallocate(request)
end subroutine pw_states_borrow_wfc_x

subroutine pw_states_giveback_wfc_x(states,wfc)
  use pw_states_type
  use pw_wfc_module
  implicit none
  type(pw_states), intent(in) :: states  
  type(pw_wfc), pointer, optional :: wfc
  integer :: ib,ik
  logical :: found
  found = .false.
  do ik=lbound(states%wfc,2),ubound(states%wfc,2)
    do ib=lbound(states%wfc,1),ubound(states%wfc,1)
      if(associated(wfc,states%wfc(ib,ik))) found = .true.
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
end subroutine pw_states_giveback_wfc_x

subroutine pw_states_density_x(states,density)
  use pw_states_type
  use pw_wfc_module
  use pw_density_module
  use pw_dipole_module
  implicit none
  type (pw_states),  intent(in)    :: states
  type (pw_density), intent(inout) :: density
  real,allocatable :: weights(:,:)
  integer :: ib,ik,ib_loc
  allocate(weights(lbound(states%wfc,1):ubound(states%wfc,1), &
                   lbound(states%wfc,2):ubound(states%wfc,2)))

  weights = -10.0
  do ik=lbound(states%wfc,2),ubound(states%wfc,2)
    do ib=states%nbmin,states%nbmax
      if(states%where_band(1,ib) == states%rank) then
        ib_loc = states%where_band(2,ib)
        weights(ib_loc,ik) = states%occupation(ib,ik) * &
                             states%weight(ib,ik)
!        write(*,*) ik,ib,ib_loc
      end if
    end do
  end do

  if(any(weights==-10.0)) ERROR("")
  
  call pw_density_from_wfc(density,states%wfc,weights)
  call pw_wfc_allreduce_inplace(density%wfc,states%comm)

  deallocate(weights)
end subroutine pw_states_density_x

subroutine pw_states_calc_expect_x(states,nbmin,nbmax,diagonal,atoms,density)
  use num_module
  use mp_global
  use pw_states_type
  use pw_states_interf
  use pw_density_module
  use pw_atoms_module
  use pw_wfcb_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm_rank,ptk_comm_world, &
      ptk_barrier, ptk_comm, ptk_comm_size
  use iotk_module    
  use pw_basis_module
  use pw_wfc_module
  use pw_vnloc_module
  use pw_hartree_module
  use pw_coulomb_module
  use pw_dipole_module
  use pw_vloc_interf
  use pw_QP_module
  use pw_parall_matrix_module
  use num_la_parall_module
  
  implicit none

  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  logical, intent(in) :: diagonal
  type(pw_atoms),  intent(in)    :: atoms
  type(pw_density),intent(in)    :: density

  type(pw_parall_complex_matrix) :: matrix

  type(pw_coulomb) :: coulomb
  type(pw_hartree) :: hartree
  type(pw_wfcb)    :: vxc
  real             :: cutoff_density
  integer          :: ib,ik,ibp
  type(pw_wfc)     :: wfc_tmp1
  type(pw_wfc),pointer :: wfc_tmp,wfc_tmp2
  type(pw_basis), pointer :: basis_tmp
  integer :: rank

  type(pw_vnloc) :: vnloc
  type(pw_wfcb)  :: vloc

  type(ptk_comm) :: comm
  integer :: root, iboh, location

  integer :: m_dim

  real :: a

  integer :: my_pool_id, me_pool, npool_local

  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  call ptk_comm_rank(intra_pool_comm,me_pool)
  call ptk_comm_size(inter_pool_comm,npool_local)

  comm=states%comm

  cutoff_density = pw_basis_get_cutoff(density%basis)

  root=states%root
  
  call pw_parall_matrix_init(matrix,nbmin,nbmax,root,comm)
  call pw_hartree_init(hartree,states%struct,cutoff_density)
  call pw_wfcb_init   (vxc,states%struct,(/0.0,0.0,0.0/),cutoff_density)
  call pw_wfcb_init   (vloc,states%struct,(/0.0,0.0,0.0/),cutoff_density)
  call pw_coulomb_init(coulomb,states%struct%b)
  call pw_hartree_from_density(hartree,density,coulomb)
  call pw_coulomb_destroy(coulomb)
  call pw_vloc_calc(vloc%wfc,atoms)
  call pw_vxc_calc(vxc%wfc,density%wfc,"pz")

if(diagonal) then
   do ik=1,states%kmesh%nkibz
     if(mod(ik,npool_local).ne.my_pool_id) cycle
     call pw_states_borrow_basis(states,basis_tmp,ik=ik)
     call pw_vnloc_init(vnloc,basis_tmp,atoms)
     call pw_wfc_init(wfc_tmp1,basis_tmp)
     matrix%val(:,:)=0.0
     matrix%eigenval(:)=0.0
     do ib=nbmin,nbmax
        if(matrix%where_line(1,ib)/=matrix%rank) cycle
        location=matrix%where_line(2,ib)
        matrix%val(location,ib)=1.0
     enddo
     iboh=1
     do ib=nbmin,nbmax
     if(matrix%where_line(1,ib)==matrix%rank.or.iboh==(matrix%m_dim_loc+1)) then
         location=matrix%where_line(2,ib) 
         a=0.0   
         call pw_states_borrow_wfc(states,wfc_tmp,ib=ib,ik=ik)
         call pw_wfc_kinetic(wfc_tmp1,wfc_tmp)
         a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
         call pw_vloc_apply(wfc_tmp1,vloc%wfc,wfc_tmp)
         a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
         call pw_vnloc_apply(wfc_tmp1,vnloc,wfc_tmp)
         a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
         call pw_vloc_apply(wfc_tmp1,hartree%wfc,wfc_tmp)
         a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
         call pw_vloc_apply(wfc_tmp1,vxc%wfc,wfc_tmp)
         a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
         call pw_states_giveback_wfc(states,wfc_tmp)
         if(matrix%incomplete) iboh=iboh+1
         if(matrix%where_line(1,ib)==matrix%rank) matrix%eigenval(ib)=real(a)
     end if
     enddo

! reduction -> all procs get matrix%eigenval
    call ptk_allreduce_inplace(matrix%eigenval,ptk_sum,comm)
    
! writing dft expectation values on file exp_dft
     if(matrix%rank==matrix%root) call iotk_open_write(10,file="exp_dft"//trim(iotk_index(ik)),binary=.false.)
     call pw_parall_matrix_write(matrix,10,name="exp_dft"//trim(iotk_index(ik)))
     if(matrix%rank==matrix%root) call iotk_close_write(10)

     call pw_vnloc_destroy(vnloc)  
     call pw_wfc_destroy(wfc_tmp1)
     call pw_states_giveback_basis(states,basis_tmp)
   enddo
else        
  do ik=1, states%kmesh%nkibz
    if(mod(ik,npool_local).ne.my_pool_id) cycle
    call pw_states_borrow_basis(states,basis_tmp,ik=ik)
    call pw_vnloc_init(vnloc,basis_tmp,atoms)
    call pw_wfc_init(wfc_tmp1,basis_tmp)
    matrix%val(:,:)=0.0
    matrix%eigenval(:)=0.0
    iboh=1
    do ib=nbmin,nbmax
    if(matrix%where_line(1,ib)==matrix%rank.or.iboh==(matrix%m_dim_loc+1)) then
            location=matrix%where_line(2,ib)    
        call pw_states_borrow_wfc(states,wfc_tmp,ib=ib,ik=ik)
        call pw_wfc_kinetic(wfc_tmp1,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
               matrix%val(location,ibp) + pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_vloc_apply(wfc_tmp1,vloc%wfc,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
               matrix%val(location,ibp) + pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_vnloc_apply(wfc_tmp1,vnloc,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
               matrix%val(location,ibp) + pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_vloc_apply(wfc_tmp1,hartree%wfc,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
              matrix%val(location,ibp)+  pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_vloc_apply(wfc_tmp1,vxc%wfc,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
             matrix%val(location,ibp) + pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_states_giveback_wfc(states,wfc_tmp)
        if(matrix%incomplete) iboh=iboh+1
      end if
    end do
    call pw_wfc_destroy(wfc_tmp1)
    call pw_states_giveback_basis(states,basis_tmp)
    call pw_vnloc_destroy(vnloc)
    
! diagonalization of dft matrix
    call num_parall_he_diag(matrix,matrix%root,matrix%comm)
! writing dft expectation values 
    if(matrix%rank==matrix%root) call iotk_open_write(10,file="exp_dft"//trim(iotk_index(ik)),binary=.false.)
    call pw_parall_matrix_write(matrix,10,name="exp_dft"//trim(iotk_index(ik)))
    if(matrix%rank==matrix%root) call iotk_close_write(10)
  enddo
endif  

call pw_wfcb_destroy(vloc)
call pw_wfcb_destroy(vxc)
call pw_hartree_destroy(hartree)

call pw_parall_matrix_destroy(matrix)

end subroutine pw_states_calc_expect_x

subroutine pw_states_calc_sp_hmatrix_x(system_type,states,nbmin,nbmax,diagonal,atoms,density,outdir)
  use num_module
  use mp_global
  use pw_states_type
  use pw_states_interf
  use pw_density_module
  use pw_atoms_module
  use pw_wfcb_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm_rank,ptk_comm_world, &
      ptk_barrier, ptk_comm, ptk_comm_size
  use iotk_module    
  use pw_basis_module
  use pw_wfc_module
  use pw_vnloc_module
  use pw_hartree_module
  use pw_coulomb_module
  use pw_dipole_module
  use pw_vloc_interf
  use pw_QP_module
  use pw_parall_matrix_module
  use num_la_parall_module
  use coulomb_vcut_module, only : vcut_type
  
  implicit none

  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  logical, intent(in) :: diagonal
  type(pw_atoms),  intent(in)    :: atoms
  type(pw_density),intent(in)    :: density
  integer, intent(in) :: system_type
  character(256), intent(in) :: outdir

  type(pw_parall_complex_matrix) :: matrix

  type(pw_coulomb) :: coulomb
  type(pw_hartree) :: hartree
  real             :: cutoff_density
  integer          :: ib,ik,ibp
  type(pw_wfc)     :: wfc_tmp1
  type(pw_wfc),pointer :: wfc_tmp,wfc_tmp2
  type(pw_basis), pointer :: basis_tmp
  integer :: rank

  type(pw_vnloc) :: vnloc
  type(pw_wfcb)  :: vloc
  type(vcut_type) :: vcut

  type(ptk_comm) :: comm

  integer :: m_dim
  integer :: root, location, iboh

  real :: a
  complex :: b

  integer :: my_pool_id, me_pool, npool_local

  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  call ptk_comm_rank(intra_pool_comm,me_pool)
  call ptk_comm_size(inter_pool_comm,npool_local)

  comm=states%comm

  cutoff_density = pw_basis_get_cutoff(density%basis)

  root=states%root

  if(system_type==0) then
    call pw_vcut_init(vcut,states%struct,states%kmesh,cutoff=15.0)
  endif

  call pw_parall_matrix_init(matrix,nbmin,nbmax,root,comm)

  call pw_hartree_init(hartree,states%struct,cutoff_density)
  call pw_wfcb_init   (vloc,states%struct,(/0.0,0.0,0.0/),cutoff_density)
  call pw_coulomb_init(coulomb,states%struct%b)
  call pw_hartree_from_density(hartree,density,coulomb)
  if(system_type==0) call pw_hartree_corr(hartree,density,vcut)
!  write(0,*) "after hartree corr"
  call pw_coulomb_destroy(coulomb)
!  write(0,*) "before vloc calc"
  if(system_type==0) then
    call pw_vloc_calc0(vloc%wfc,atoms)
    call pw_vloc_corr(vloc%wfc,atoms,vcut)
  else
    call pw_vloc_calc(vloc%wfc,atoms)
  endif
  if(system_type==0) call pw_vcut_destroy(vcut)
!  write(0,*) "after vloc corr"
if(diagonal) then
  do ik=1,states%kmesh%nkibz
    if(mod(ik,npool_local).ne.my_pool_id) cycle
    call pw_states_borrow_basis(states,basis_tmp,ik=ik)
    call pw_vnloc_init(vnloc,basis_tmp,atoms)
    call pw_wfc_init(wfc_tmp1,basis_tmp)
    matrix%val(:,:)=0.0
    matrix%eigenval(:)=0.0
    do ib=nbmin,nbmax
       if(matrix%where_line(1,ib)/=matrix%rank) cycle  
       location=matrix%where_line(2,ib)
       matrix%val(location,ib)=1.0
    enddo
    iboh=1
    do ib=nbmin,nbmax
    if(matrix%where_line(1,ib)==matrix%rank.or.iboh==(matrix%m_dim_loc+1)) then
            location=matrix%where_line(2,ib) 
        a=0.0    
        a=0.0
        call pw_states_borrow_wfc(states,wfc_tmp,ib=ib,ik=ik)
        call pw_wfc_kinetic(wfc_tmp1,wfc_tmp)
        a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
        call pw_vloc_apply(wfc_tmp1,vloc%wfc,wfc_tmp)
        a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
        call pw_vnloc_apply(wfc_tmp1,vnloc,wfc_tmp)
        a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
        call pw_vloc_apply(wfc_tmp1,hartree%wfc,wfc_tmp)
        a = a + pw_wfc_braket(wfc_tmp,wfc_tmp1)
        call pw_states_giveback_wfc(states,wfc_tmp)
        if(matrix%incomplete) iboh=iboh+1
        if(matrix%where_line(1,ib)==matrix%rank) then
             matrix%eigenval(ib)=real(a)
             write(0,*) ib, real(a)
        endif
      end if
    end do
    call pw_vnloc_destroy(vnloc)
    call pw_wfc_destroy(wfc_tmp1)
    call pw_states_giveback_basis(states,basis_tmp)

! reduction -> all procs get matrix%eigenval
    call ptk_allreduce_inplace(matrix%eigenval,ptk_sum,comm)
    
! writing single particle expectation values 
    if(matrix%rank==matrix%root) call iotk_open_write(10,file=trim(outdir)//"exp_sp"//trim(iotk_index(ik)),binary=.false.)
    call pw_parall_matrix_write(matrix,10,name="exp_sp"//trim(iotk_index(ik)))
    if(matrix%rank==matrix%root) call iotk_close_write(10)

  enddo

else        
  do ik=1,states%kmesh%nkibz
    if(mod(ik,npool_local).ne.my_pool_id) cycle
    call pw_states_borrow_basis(states,basis_tmp,ik=ik)
    call pw_vnloc_init(vnloc,basis_tmp,atoms)
    call pw_wfc_init(wfc_tmp1,basis_tmp)
    matrix%val(:,:)=0.0
    matrix%eigenval(:)=0.0
    iboh=1
    do ib=nbmin,nbmax
    if(matrix%where_line(1,ib)==matrix%rank.or.iboh==(matrix%m_dim_loc+1)) then
            location=matrix%where_line(2,ib)    
        call pw_states_borrow_wfc(states,wfc_tmp,ib=ib,ik=ik)
        call pw_wfc_kinetic(wfc_tmp1,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
               matrix%val(location,ibp) + pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_vloc_apply(wfc_tmp1,vloc%wfc,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
               matrix%val(location,ibp) + pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_vnloc_apply(wfc_tmp1,vnloc,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
               matrix%val(location,ibp) + pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_vloc_apply(wfc_tmp1,hartree%wfc,wfc_tmp)
        do ibp=nbmin,nbmax
          call pw_states_borrow_wfc(states,wfc_tmp2,ib=ibp,ik=ik)
          if(matrix%where_line(1,ib)==matrix%rank) matrix%val(location,ibp) = &
              matrix%val(location,ibp)+ pw_wfc_braket(wfc_tmp2,wfc_tmp1)
          call pw_states_giveback_wfc(states,wfc_tmp2)
        end do
        call pw_states_giveback_wfc(states,wfc_tmp)
        if(matrix%incomplete) iboh=iboh+1
! now fill matrix%eigenval with diagonal elements just as indication
        if(matrix%where_line(1,ib)==matrix%rank) matrix%eigenval(ib)=matrix%val(location,ib)
      end if
    end do
    call pw_vnloc_destroy(vnloc)
    call pw_wfc_destroy(wfc_tmp1)
    call pw_states_giveback_basis(states,basis_tmp)

! XXX
! DEBUG FIX: AF
!
! Now these off-diagonal matrix elements are consistent with those
! computed for the self-energy.
!
    matrix%val(:,:) = CONJG( matrix%val(:,:) )
! XXX

! reduce matrix%eigenval

    call ptk_allreduce_inplace(matrix%eigenval,ptk_sum,comm)
    
! writing single particle expectation values 
    if(matrix%rank==matrix%root) call iotk_open_write(10,file=trim(outdir)//"exp_sp"//trim(iotk_index(ik)),binary=.false.)
    call pw_parall_matrix_write(matrix,10,name="exp_sp"//trim(iotk_index(ik)))
    if(matrix%rank==matrix%root) call iotk_close_write(10)

  enddo
endif  

call pw_wfcb_destroy(vloc)
call pw_hartree_destroy(hartree)

call pw_parall_matrix_destroy(matrix)
end subroutine pw_states_calc_sp_hmatrix_x

subroutine pw_states_calc_expect_sp_x(exp_sp,nbmin,nbmax,states,atoms,density)
  use num_module
  use pw_states_type
  use pw_states_interf
  use pw_density_module
  use pw_atoms_module
  use pw_wfcb_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm_rank,ptk_comm_world, &
      ptk_barrier, ptk_comm_size
  use pw_basis_module
  use pw_wfc_module
  use pw_vnloc_module
  use pw_hartree_module
  use pw_coulomb_module
  use pw_dipole_module
  use pw_vloc_interf
  use pw_QP_module
  implicit none
  type(pw_QP), intent(out) :: exp_sp
  integer, intent(in) :: nbmin,nbmax
  type(pw_states), intent(in) :: states
  type(pw_atoms),  intent(in)    :: atoms
  type(pw_density),intent(in)    :: density

  real             :: cutoff_density
  integer          :: ib,ik,ibp
  type(pw_wfc)     :: wfc_tmp1
  type(pw_wfc),pointer :: wfc_tmp
  type(pw_basis), pointer :: basis_tmp
  integer :: rank
  type(pw_coulomb) :: coulomb
  type(pw_hartree) :: hartree
  type(pw_vnloc) :: vnloc
  type(pw_wfcb)  :: vloc

  integer :: ierr, myrank, npe

  call ptk_comm_rank(states%comm,myrank)
  call ptk_comm_size(states%comm,npe)

  cutoff_density = pw_basis_get_cutoff(density%basis)

  call pw_hartree_init(hartree,states%struct,cutoff_density)
  call pw_coulomb_init(coulomb,states%struct%b)
  call pw_hartree_from_density(hartree,density,coulomb)
  call pw_coulomb_destroy(coulomb)

!  write(0,*) "sp: hartree from density done" 

  call pw_wfcb_init   (vloc,states%struct,(/0.0,0.0,0.0/),cutoff_density)
  call pw_vloc_calc(vloc%wfc,atoms)

  do ik=lbound(states%wfc,2),ubound(states%wfc,2)
    call pw_states_borrow_basis(states,basis_tmp,ik=ik)
    call pw_vnloc_init(vnloc,basis_tmp,atoms)
    call pw_wfc_init(wfc_tmp1,basis_tmp)
    do ib=nbmin,nbmax
       if(states%where_band(1,ib)==myrank) then 
!         call pw_states_borrow_wfc(states,wfc_tmp,ib=ib,ik=ik)
         wfc_tmp => states%wfc(states%where_band(2,ib),ik)
         call pw_wfc_kinetic(wfc_tmp1,wfc_tmp)
         exp_sp%energies(ib,ik) = exp_sp%energies(ib,ik) +&
              pw_wfc_braket(wfc_tmp,wfc_tmp1)
         call pw_vloc_apply(wfc_tmp1,vloc%wfc,wfc_tmp)
         exp_sp%energies(ib,ik) = exp_sp%energies(ib,ik) + &
              pw_wfc_braket(wfc_tmp,wfc_tmp1)
         call pw_vnloc_apply(wfc_tmp1,vnloc,wfc_tmp)
         exp_sp%energies(ib,ik) = exp_sp%energies(ib,ik) + &
              pw_wfc_braket(wfc_tmp,wfc_tmp1)
         call pw_vloc_apply(wfc_tmp1,hartree%wfc,wfc_tmp)
         exp_sp%energies(ib,ik) = exp_sp%energies(ib,ik)+ &
              pw_wfc_braket(wfc_tmp,wfc_tmp1)
!         call pw_states_giveback_wfc(states,wfc_tmp)
         nullify(wfc_tmp)
      endif 
    end do
    call pw_vnloc_destroy(vnloc)
    call pw_wfc_destroy(wfc_tmp1)
    call pw_states_giveback_basis(states,basis_tmp)
  end do
  call pw_wfcb_destroy(vloc)
  call pw_hartree_destroy(hartree)
  call ptk_allreduce_inplace(exp_sp%energies,comm=states%comm,op=ptk_sum)

end subroutine pw_states_calc_expect_sp_x

subroutine pw_states_calc_expect_hartree_x(exp_hartree,nbmin,nbmax,states,atoms,density)
  use num_module
  use pw_states_type
  use pw_states_interf
  use pw_density_module
  use pw_atoms_module
  use pw_wfcb_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm_rank,ptk_comm_world, &
      ptk_barrier
  use pw_basis_module
  use pw_wfc_module
  use pw_vnloc_module
  use pw_hartree_module
  use pw_coulomb_module
  use pw_dipole_module
  use pw_vloc_interf
  use pw_QP_module
  implicit none
  type(pw_QP), intent(out) :: exp_hartree
  integer, intent(in) :: nbmin, nbmax
  type(pw_states), intent(in) :: states
  type(pw_atoms),  intent(in)    :: atoms
  type(pw_density),intent(in)    :: density

  type(pw_coulomb) :: coulomb
  type(pw_hartree) :: hartree
  real             :: cutoff_density
  integer          :: ib,ik,ibp
  type(pw_wfc)     :: wfc_tmp1
  type(pw_wfc),pointer :: wfc_tmp,wfc_tmp2
  type(pw_basis), pointer :: basis_tmp
  integer :: rank

  cutoff_density = pw_basis_get_cutoff(density%basis)

  call pw_hartree_init(hartree,states%struct,cutoff_density)
  call pw_coulomb_init(coulomb,states%struct%b)
  call pw_hartree_from_density(hartree,density,coulomb)
  call pw_coulomb_destroy(coulomb)

!  write(0,*) "hartree from density done"

  do ik=lbound(states%wfc,2),ubound(states%wfc,2)
    call pw_states_borrow_basis(states,basis_tmp,ik=ik)
    call pw_wfc_init(wfc_tmp1,basis_tmp)
    do ib=nbmin,nbmax
       if(.not.(pw_states_is_local(states,ib,ik))) cycle
!       call pw_states_borrow_wfc(states,wfc_tmp,ib=ib,ik=ik)
       wfc_tmp => states%wfc(states%where_band(2,ib),ik)
       call pw_vloc_apply(wfc_tmp1,hartree%wfc,wfc_tmp)
       exp_hartree%energies(ib,ik) = pw_wfc_braket(wfc_tmp,wfc_tmp1)*2.0
!       call pw_states_giveback_wfc(states,wfc_tmp)
       nullify(wfc_tmp)
    end do
    call pw_wfc_destroy(wfc_tmp1)
    call pw_states_giveback_basis(states,basis_tmp)
  end do

  call ptk_allreduce_inplace(exp_hartree%energies,comm=states%comm,op=ptk_sum)
  
  call pw_hartree_destroy(hartree)

end subroutine pw_states_calc_expect_hartree_x

subroutine pw_states_calc_expect_vxc_x(exp_vxc,nbmin,nbmax,states,atoms,density)
  use num_module
  use pw_states_type
  use pw_states_interf
  use pw_density_module
  use pw_atoms_module
  use pw_wfcb_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm_rank,ptk_comm_world, &
      ptk_barrier
  use pw_basis_module
  use pw_wfc_module
  use pw_vnloc_module
  use pw_hartree_module
  use pw_coulomb_module
  use pw_dipole_module
  use pw_vloc_interf
  use pw_QP_module
  implicit none

  type(pw_QP), intent(out) :: exp_vxc
  integer, intent(in) :: nbmin,nbmax
  type(pw_states), intent(in) :: states
  type(pw_atoms),  intent(in)    :: atoms
  type(pw_density),intent(in)    :: density

  type(pw_wfcb)    :: vxc
  real             :: cutoff_density
  integer          :: ib,ik,ibp
  type(pw_wfc)     :: wfc_tmp1
  type(pw_wfc),pointer :: wfc_tmp,wfc_tmp2
  type(pw_basis), pointer :: basis_tmp
  integer :: rank

  cutoff_density = pw_basis_get_cutoff(density%basis)

  call pw_wfcb_init   (vxc,states%struct,(/0.0,0.0,0.0/),cutoff_density)
  call pw_vxc_calc(vxc%wfc,density%wfc,"pz")

  do ik=lbound(states%wfc,2),ubound(states%wfc,2)
    call pw_states_borrow_basis(states,basis_tmp,ik=ik)
    call pw_wfc_init(wfc_tmp1,basis_tmp)
    do ib=nbmin,nbmax
      if(pw_states_is_local(states,ib,ik)) then
!        call pw_states_borrow_wfc(states,wfc_tmp,ib=ib,ik=ik)
        wfc_tmp => states%wfc(states%where_band(2,ib),ik)
        call pw_vloc_apply(wfc_tmp1,vxc%wfc,wfc_tmp)
        exp_vxc%energies(ib,ik) = pw_wfc_braket(wfc_tmp,wfc_tmp1)
!        call pw_states_giveback_wfc(states,wfc_tmp)
        nullify(wfc_tmp)
      end if
    end do
    call pw_wfc_destroy(wfc_tmp1)
    call pw_states_giveback_basis(states,basis_tmp)
  end do

  call ptk_allreduce_inplace(exp_vxc%energies,comm=states%comm,op=ptk_sum)

  call pw_wfcb_destroy(vxc)

end subroutine pw_states_calc_expect_vxc_x

