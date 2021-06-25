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

subroutine pw_polar_init_x(pol,struct,symmlist,qmesh,omegamax,nomega,cutoff,emax,degauss, &
    gw_integration_method,root,comm)
  use pw_polar_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm, ptk_barrier
  use tools_module
  use num_module
  implicit none
  type (pw_polar),   intent(out) :: pol
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
!  real,                       intent(in)  :: cutoff,omegamax
  real,                       intent(in)  :: cutoff
  complex,                       intent(in)  :: omegamax
  real,                       intent(in)  :: emax, degauss
  integer,                    intent(in)  :: root,nomega
  type (ptk_comm),            intent(in)  :: comm
  character(len=*),           intent(in)  :: gw_integration_method
  integer iq, iomega

  pol%struct   => struct
  pol%symmlist => symmlist
  pol%qmesh    => qmesh
  pol%cutoff   =  cutoff
  pol%root     =  root
  pol%comm     =  comm
  pol%nomega   = nomega 
  pol%emax     = emax
  pol%degauss  = degauss

  call ptk_comm_size(comm,pol%npe)
  call ptk_comm_rank(comm,pol%rank)
  if(pol%root >= pol%npe) ERROR("")

  allocate(pol%where_q(2,pol%qmesh%nkibz))
  call pw_allocate(pol%where_q)
  pol%where_q = 0
  do iq=1,qmesh%nkibz
    pol%where_q(1,iq) = modulo(iq-pol%root,pol%npe)
    pol%where_q(2,iq) = count(pol%where_q(1,1:iq)==pol%where_q(1,iq))
  end do

  allocate(pol%basis(qmesh%nkibz))
  call pw_basis_init(pol%basis(:),pol%struct)
  call pw_basis_create(pol%basis,pol%qmesh,pol%cutoff)

  write(0,*)"The aprox number of plane waves for P is:", pol%basis(1)%npw

  allocate(pol%val(count(pol%where_q(1,:)==pol%rank),0:nomega))
  if(ubound(pol%val,1) > 0) then
    do iq=1,qmesh%nkibz
      if(pol%where_q(1,iq)==pol%rank) then
        do iomega = 0,pol%nomega
         call pw_wfc6d_init(pol%val(pol%where_q(2,iq),iomega),pol%basis(iq))
        end do
      end if
    end do
  end if
  pol%val_initialized = .true.

  pol%iqgamma = pw_kmesh_kibz_index(pol%qmesh,(/0.0,0.0,0.0/))

  allocate(pol%gradients_l(3,0:nomega))
  allocate(pol%gradients_r(3,0:nomega))
  call pw_wfc_init(pol%gradients_l(:,:),pol%basis(pol%iqgamma))
  call pw_wfc_init(pol%gradients_r(:,:),pol%basis(pol%iqgamma))
  allocate(pol%macroscopic(3,3,0:nomega))
  pol%macroscopic = 0.0

  allocate(pol%omega(0:nomega))
  allocate(pol%weight(0:nomega))
  if(gw_integration_method=="plasmon_pole") then
    pol%weight(:)=1.0
    pol%omega(0)=0.0 
    pol%omega(1)=omegamax
  elseif(gw_integration_method=="Farid") then 
!    call num_gaussmesh(omegamax,pol%omega(0:),pol%weight(0:))
  elseif(gw_integration_method=="cohsex".or.gw_integration_method=="sex".or.gw_integration_method=="coh") then
    pol%weight(:)=1.0
    pol%omega(0)=omegamax
  elseif(gw_integration_method=="sshf") then
    pol%weight(:)=1.0
    pol%omega(0)=0.0 
  endif  

  call ptk_barrier(pol%comm)

end subroutine pw_polar_init_x

subroutine pw_polar_calc_x(pol,states,atoms)
use ptk_module
use mp_global
use tools_module
use pw_polar_type
use pw_polar_interf 
use pw_states_module
use pw_atoms_module
use pw_pseudovelocity_module
use pw_wfc_module
use pw_dipole_module
use pw_kmesh_module
use pw_basis_module
use pw_field_module
use pw_fft_module
use num_module
use optimal_proc_module, only : optimal_proc_mycarica, optimal_proc_maxcarica

implicit none

type(pw_polar), intent(inout) :: pol
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms

type(pw_pseudovelocity) :: pseudovelocity
complex :: scaled_velocity(3)
type(pw_wfc), pointer :: wfc_val, wfc_con
type(pw_wfc) :: wfc_dip 
type(pw_basis), pointer :: basis_con 
type(pw_basis) :: basis_dip

real :: deltae, polar_weight
logical :: lflag
integer :: iq, ik1, ik2
integer :: ib1,ib2
real :: k1(3), k2(3)
integer :: dim(3)
integer :: owner, location
integer :: iomega, i
integer :: my_pool_id, me_pool, npool_local

integer :: mycarica, maxcarica, icarica

call ptk_comm_rank(inter_pool_comm,my_pool_id)
call ptk_comm_rank(intra_pool_comm,me_pool)
call ptk_comm_size(inter_pool_comm,npool_local)

!write(0,*) "npool_local ", npool_local
  if(states%superparall) then
    call optimal_proc_mycarica(mycarica,states%transitions,states%rank)
    call optimal_proc_maxcarica(maxcarica,states%transitions,states%npe)
  endif
  do ik1=1,states%kmesh%nkibz
    !
    ! AF
    call tools_log("Computing P("//TRIM(tools_char(ik1,5))//") ..." ,advance=.false.)
    !
    if(mod(ik1,npool_local).ne.my_pool_id) cycle
!    write(0,*) "mod.. my_pool_id ", mod(ik1,npool_local), my_pool_id
    k1 = states%kmesh%kibz(:,ik1)
!WRITE(0,*) "before borrow states"
    call pw_states_borrow_basis(states,basis_con,ik1)
!WRITE(0,*) "before look ik2"
    do ik2=1,states%kmesh%nkibz
      k2 = states%kmesh%kibz(:,ik2)
      iq = pw_kmesh_kibz_index(pol%qmesh,(states%kmesh%kibz(:,ik1)-states%kmesh%kibz(:,ik2)))
!WRITE(0,*) "pw_pseudovel"
      if(iq==pol%iqgamma) then
        call pw_pseudovelocity_init(pseudovelocity,basis_con,atoms)
      end if
      call pw_basis_init(basis_dip,states%struct)
      call pw_basis_create(basis_dip,pol%qmesh%kbz(:,iq),pol%cutoff)
      call pw_wfc_init(wfc_dip,basis_dip)

! loop on occupied states
      do ib2=states%nbmin, states%nbmax 
        if(.not.states%superparall) then
          call pw_states_borrow_wfc(states,wfc_val,ib2,ik2)
        else
          wfc_val => states%wfc(states%where_band(2,ib2),ik2)
        endif

        if(states%occupation(ib2,ik2)<1.0) cycle
! AF
!write(0,*) "ib2: ", ib2
        if(states%superparall .and. (states%where_band(1,ib2)/=states%rank)) cycle
!WRITE(0,*) "after superparall 2"

! loop on unoccupied states
        do ib1=states%nbmin,states%nbmax 
          if(states%superparall) then
            if(states%occupation(ib1,ik1)>1.0) cycle
            if(states%transitions(ib2,ib1)/=states%rank) cycle
            polar_weight = 0.5 * num_erfc((abs(states%e(ib1,ik1) - states%e(ib2,ik2)) &
                     - pol%emax) / (2.0*pol%degauss)) 
          else
            if(.not.pw_states_is_local(states,ib1,ik1)) cycle
! AF
!write(0,*) "ib1: ", ib1
            polar_weight = 0.5 * num_erfc((abs(states%e(ib1,ik1) - states%e(ib2,ik2)) &
                     - pol%emax) / (2.0*pol%degauss))* &
                     0.5 * num_erfc( (states%occupation(ib1,ik1)-1.0)*1000.0 )
            call ptk_allreduce( polar_weight < 0.01 , lflag, ptk_land,intra_pool_comm)
            if(lflag) cycle
          endif
          if(.not.states%superparall) then 
            call pw_states_borrow_wfc(states,wfc_con,ib1,ik1)
          else
            wfc_con => states%wfc(states%where_band(2,ib1),ik1)
          endif
! pw_dipole compute FFT(conjg(field_val)*field_con)
          call pw_dipole(wfc_dip,wfc_val,wfc_con)
          deltae = states%e(ib1,ik1) - states%e(ib2,ik2)
          if(iq==pol%iqgamma) then
            scaled_velocity = (pw_wfc_p_braket(wfc_val,wfc_con) &
                              +pw_pseudovelocity_braket(pseudovelocity,wfc_val,wfc_con))
          end if
          call pw_polar_add_dipole(pol,deltae,polar_weight,wfc_dip,iq, &
                   velocity=scaled_velocity)
          if(.not.states%superparall) then
            call pw_states_giveback_wfc(states,wfc_con)
          else
            nullify(wfc_con)
          endif

        end do
        if(.not.states%superparall) then
          call pw_states_giveback_wfc(states,wfc_val)
        else
          nullify(wfc_val)
        endif
      end do
      if(iq==pol%iqgamma) then
        call pw_pseudovelocity_destroy(pseudovelocity)
      end if
      if(states%superparall) then
        do icarica= mycarica, maxcarica-1 
          deltae=1.0
          polar_weight = 0.0
          call pw_polar_add_dipole(pol,deltae,polar_weight,wfc_dip,iq, &
               velocity=scaled_velocity)
        enddo
      endif
      call pw_wfc_destroy(wfc_dip)
      call pw_basis_destroy(basis_dip)
    end do
    !
    ! AF
    call tools_log("done")
    !
    call pw_states_giveback_basis(states,basis_con)
  end do
  call ptk_allreduce_inplace(pol%macroscopic,ptk_sum,inter_pool_comm)
  do i=1,3
    do iomega=0, pol%nomega
      call pw_wfc_allreduce_inplace(pol%gradients_r(i,iomega),inter_pool_comm)
      call pw_wfc_allreduce_inplace(pol%gradients_l(i,iomega),inter_pool_comm)
    enddo
  enddo 

  write(0,*) "Macro", pol%macroscopic(1,1,0), pol%macroscopic(2,2,0), pol%macroscopic(3,3,0)

end subroutine pw_polar_calc_x

subroutine pw_polar_borrow_basis_x(pol,basis,iq)
  use pw_polar_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use tools_module
  use num_module
  implicit none
  type(pw_polar),           intent(in)  :: pol
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
  if(present(basis) .neqv. present(iq)) ERROR("")
  if(present(basis)) basis => pol%basis(iq)
end subroutine pw_polar_borrow_basis_x

subroutine pw_polar_giveback_basis_x(pol,basis)
  use pw_polar_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use tools_module
  use num_module
  implicit none
  type(pw_polar), intent(in)    :: pol
  type(pw_basis), pointer, optional :: basis
  if(.not.associated(pol%basis)) ERROR("")
  if(present(basis)) nullify(basis)
end  subroutine pw_polar_giveback_basis_x

subroutine pw_polar_add_dipole_x(pol,deltae,weight,wfc_dip,iq,velocity)
  use pw_polar_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_allgather,ptk_allreduce_inplace,ptk_sum,ptk_recv,ptk_send
  use mp_global
  use tools_module
  use num_module
  implicit none
  type(pw_polar), intent(inout) :: pol
  real,            intent(in)    :: deltae, weight
  type(pw_wfc),    intent(in)    :: wfc_dip
  integer,         intent(in)    :: iq
  complex,optional,intent(in)    :: velocity(3) ! needed only if iq=pol%iqgamma

  integer      :: owner,location,ip
  integer, allocatable      :: iq_array(:)
  complex      :: macro(3,3)
  type(pw_wfc) :: gradients_tmp
  complex :: scaled_velocity_loc(3)

  integer :: i
  integer :: iomega

  type(pw_wfc), allocatable :: wfc_queue(:)
  real,         allocatable :: deltae_queue(:)
  integer,      allocatable :: location_queue(:)
  real,         allocatable :: weight_queue(:)
  integer :: nqueue,iqueue
  complex :: scale

  scaled_velocity_loc = 0.0
  if(iq==pol%iqgamma .and. present(velocity)) scaled_velocity_loc = velocity / deltae

  allocate(iq_array(0:pol%npe-1))

  call ptk_allgather(iq,iq_array,pol%comm)

  if((iq_array(pol%rank)==pol%iqgamma)) then
    call pw_wfc_init(gradients_tmp,pol%basis(iq))
    do iomega = 0,pol%nomega
!      scale = -2.0*2.0*deltae/(deltae**2+pol%omega(iomega)**2)/ &
!                   pol%struct%a_omega / pol%qmesh%nkbz * weight
! general for omega imaginary or not
      scale = -2.0*2.0*deltae/(deltae**2-pol%omega(iomega)**2)/ &
                   pol%struct%a_omega / pol%qmesh%nkbz * weight

      do i = 1 , 3
        macro(i,:) = scale * (conjg(scaled_velocity_loc(i)) * scaled_velocity_loc(:)+ &
                  scaled_velocity_loc(i)*conjg(scaled_velocity_loc(:)))/2.0
  
        gradients_tmp = wfc_dip
        call pw_wfc_scale(gradients_tmp,scale*conjg(scaled_velocity_loc(i)))
        if(weight<0.01) gradients_tmp%val = 0.0
        call pw_wfc_allreduce_inplace(gradients_tmp,intra_pool_comm)
        call pw_wfc_conjg(gradients_tmp)
        call pw_wfc_axpy(pol%gradients_l(i,iomega),(1.0,0.0),gradients_tmp)
! gradients_r is defined in the same way as gradients r but it is just
! because on pw_w_calc gemv performs the conjg

        gradients_tmp = wfc_dip
        call pw_wfc_scale(gradients_tmp,scale*conjg(scaled_velocity_loc(i)))
        if(weight<0.01) gradients_tmp%val = 0.0
        call pw_wfc_allreduce_inplace(gradients_tmp,intra_pool_comm)
        call pw_wfc_conjg(gradients_tmp)
        call pw_wfc_axpy(pol%gradients_r(i,iomega),(1.0,0.0),gradients_tmp)
      end do

      if(weight < 0.01) macro = 0.0
      call ptk_allreduce_inplace(macro,ptk_sum,intra_pool_comm)
      pol%macroscopic(:,:,iomega) = pol%macroscopic(:,:,iomega) + macro(:,:)
    end do
    call pw_wfc_destroy(gradients_tmp)
  end if

  iqueue = 0
  do ip=0,pol%npe-1
    owner = pol%where_q(1,iq_array(ip))
    if(owner==pol%rank .and. ip/=pol%rank) iqueue = iqueue+1
  end do
  nqueue = iqueue
  allocate(wfc_queue(nqueue))
  allocate(deltae_queue(nqueue))
  allocate(location_queue(nqueue))
  allocate(weight_queue(nqueue))
  iqueue = 0
  do ip=0,pol%npe-1
    owner = pol%where_q(1,iq_array(ip))
    location = pol%where_q(2,iq_array(ip))
    if(owner==pol%rank .and. ip/=pol%rank) then
      iqueue = iqueue+1
      call pw_wfc_init(wfc_queue(iqueue),pol%basis(iq_array(ip)))
      location_queue(iqueue) = location
    end if
  end do
  if(iqueue/=nqueue) ERROR("")

  iqueue = 0
  do ip=0,pol%npe-1
    owner = pol%where_q(1,iq_array(ip))
    if(ip /= pol%rank .and. pol%rank == owner) then
      iqueue = iqueue + 1
      call ptk_recv(weight_queue(iqueue),ip,owner,pol%comm)
      call ptk_recv(deltae_queue(iqueue),ip,owner,pol%comm)
      call pw_wfc_recv(wfc_queue(iqueue),ip,owner,pol%comm)
    end if
    if(ip == pol%rank .and. pol%rank /= owner) then
      call ptk_send(weight,owner,owner,pol%comm)
      call ptk_send(deltae,owner,owner,pol%comm)
      call pw_wfc_send(wfc_dip,owner,owner,pol%comm)
    end if
  end do
  if(iqueue/=nqueue) ERROR("")

  do iomega = 0,pol%nomega
    owner = pol%where_q(1,iq_array(pol%rank))
    location = pol%where_q(2,iq_array(pol%rank))
    if(owner == pol%rank) then
!      scale = -2.0*2.0*deltae/(deltae**2+pol%omega(iomega)**2)/ &
!                   pol%struct%a_omega / pol%qmesh%nkbz * weight 
      scale = -2.0*2.0*deltae/(deltae**2-pol%omega(iomega)**2)/ &
                   pol%struct%a_omega / pol%qmesh%nkbz * weight 

      if(weight>=0.01) call pw_wfc6d_add_from_wfc(pol%val(location,iomega),scale,wfc_dip)
    end if
    do iqueue = 1,nqueue
!      scale = -2.0*2.0*deltae_queue(iqueue)/(deltae_queue(iqueue)**2+pol%omega(iomega)**2)/ &
!                   pol%struct%a_omega / pol%qmesh%nkbz * weight_queue(iqueue) 
      scale = -2.0*2.0*deltae_queue(iqueue)/(deltae_queue(iqueue)**2-pol%omega(iomega)**2)/ &
                   pol%struct%a_omega / pol%qmesh%nkbz * weight_queue(iqueue) 

      if(weight_queue(iqueue)>=0.01) call pw_wfc6d_add_from_wfc(pol%val(location_queue(iqueue),iomega),scale,wfc_queue(iqueue))
    end do
  end do

  deallocate(deltae_queue,location_queue)
  deallocate(weight_queue)
  call pw_wfc_destroy(wfc_queue)
  deallocate(wfc_queue)

  deallocate(iq_array)

end subroutine pw_polar_add_dipole_x

subroutine pw_polar_write_x(pol,unit,name)
  use pw_polar_type
  use pw_polar_interf, dont_use => pw_polar_write
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use tools_module
  use num_module
  use iotk_module
  implicit none
  type(pw_polar),  intent(in) :: pol
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  type(pw_basis), pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq,iomega
  character(iotk_attlenx) :: attr

  if(pol%rank == pol%root) then
    call iotk_write_attr(attr,"type","pw_polar",first=.true.)
    call iotk_write_begin(unit,trim(name),attr)
  
    call iotk_write_attr(attr,"nomega",pol%nomega,first=.true.)
    call iotk_write_attr(attr,"nq",    pol%qmesh%nkbz)
    call iotk_write_empty(unit,"info",attr)
  end if

  do iomega=0,pol%nomega
    if(pol%rank == pol%root) then
      call iotk_write_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_write_dat(unit,"macro1",pol%macroscopic(1,:,iomega))
      call iotk_write_dat(unit,"macro2",pol%macroscopic(2,:,iomega))
      call iotk_write_dat(unit,"macro3",pol%macroscopic(3,:,iomega))
      call pw_wfc_write(pol%gradients_l(1,iomega),unit,"iotk",name="gradients_l1")
      call pw_wfc_write(pol%gradients_l(2,iomega),unit,"iotk",name="gradients_l2")
      call pw_wfc_write(pol%gradients_l(3,iomega),unit,"iotk",name="gradients_l3")
      call pw_wfc_write(pol%gradients_r(1,iomega),unit,"iotk",name="gradients_r1")
      call pw_wfc_write(pol%gradients_r(2,iomega),unit,"iotk",name="gradients_r2")
      call pw_wfc_write(pol%gradients_r(3,iomega),unit,"iotk",name="gradients_r3")
    end if

    do iq=1,pol%qmesh%nkibz
      owner = pol%where_q(1,iq)
      location = pol%where_q(2,iq)
      if(pol%rank == pol%root .and. pol%rank == owner) then
        call pw_wfc6d_write(pol%val(location,iomega),unit,"iotk",name="val"//trim(iotk_index(iq)))
      end if
      if(pol%rank == pol%root .and. pol%rank /= owner) then
        call pw_polar_borrow_basis(pol,basis_tmp,iq)
        call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
        call pw_wfc6d_recv(val_tmp,owner,iq,pol%comm)
        call pw_wfc6d_write(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
        call pw_wfc6d_destroy(val_tmp)
        call pw_polar_giveback_basis(pol,basis_tmp)
      end if
      if(pol%rank /= pol%root .and. pol%rank == owner) then
        call pw_wfc6d_send(pol%val(location,iomega),pol%root,iq,pol%comm)
      end if
    end do
    if(pol%rank == pol%root) then
      call iotk_write_end(unit,"omega"//trim(iotk_index(iomega)))
    end if
  end do

  if(pol%rank == pol%root) then
    call iotk_write_end(unit,name)
  end if

end subroutine pw_polar_write_x

subroutine pw_polar_read_x(pol,unit,name)
  use pw_polar_type
  use pw_polar_interf, dont_use => pw_polar_read
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_bcast
  use tools_module
  use num_module
  use iotk_module
  implicit none
  type(pw_polar),  intent(inout) :: pol
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  type(pw_basis), pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq
  integer :: iomega,nomega,nq

  if(pol%rank == pol%root) then
    call iotk_scan_begin(unit,trim(name),attr)
    call iotk_scan_attr(attr,"type",rtype,default="pw_polar")
    if(rtype/="pw_polar") ERROR("")

    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr(attr,"nomega",nomega)
    if(nomega/=pol%nomega) ERROR("")
    call iotk_scan_attr(attr,"nq",    nq)
    if(nq/=pol%qmesh%nkbz) ERROR("")
  end if

  do iomega = 0,pol%nomega
    if(pol%rank == pol%root) then
      call iotk_scan_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_scan_dat(unit,"macro1",pol%macroscopic(1,:,iomega))
      call iotk_scan_dat(unit,"macro2",pol%macroscopic(2,:,iomega))
      call iotk_scan_dat(unit,"macro3",pol%macroscopic(3,:,iomega))
      call pw_wfc_read(pol%gradients_l(1,iomega),unit,"iotk",name="gradients_l1")
      call pw_wfc_read(pol%gradients_l(2,iomega),unit,"iotk",name="gradients_l2")
      call pw_wfc_read(pol%gradients_l(3,iomega),unit,"iotk",name="gradients_l3")
      call pw_wfc_read(pol%gradients_r(1,iomega),unit,"iotk",name="gradients_r1")
      call pw_wfc_read(pol%gradients_r(2,iomega),unit,"iotk",name="gradients_r2")
      call pw_wfc_read(pol%gradients_r(3,iomega),unit,"iotk",name="gradients_r3")
    end if

    call ptk_bcast(pol%macroscopic(:,:,iomega),pol%root,pol%comm)
    call pw_wfc_bcast(pol%gradients_l(1,iomega),pol%root,pol%comm)
    call pw_wfc_bcast(pol%gradients_l(2,iomega),pol%root,pol%comm)
    call pw_wfc_bcast(pol%gradients_l(3,iomega),pol%root,pol%comm)
    call pw_wfc_bcast(pol%gradients_r(1,iomega),pol%root,pol%comm)
    call pw_wfc_bcast(pol%gradients_r(2,iomega),pol%root,pol%comm)
    call pw_wfc_bcast(pol%gradients_r(3,iomega),pol%root,pol%comm)

    do iq=1,pol%qmesh%nkibz
      owner = pol%where_q(1,iq)
      location = pol%where_q(2,iq)
      if(pol%rank == pol%root .and. pol%rank == owner) then
        call pw_wfc6d_read(pol%val(location,iomega),unit,"iotk",name="val"//trim(iotk_index(iq)))
      end if
      if(pol%rank == pol%root .and. pol%rank /= owner) then
! OCCHIO AL BARRIER
        call pw_polar_borrow_basis(pol,basis_tmp,iq)
        call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
        call pw_wfc6d_read(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
        call pw_wfc6d_send(val_tmp,owner,iq,pol%comm)
        call pw_wfc6d_destroy(val_tmp)
      else
        call pw_polar_borrow_basis(pol,basis_tmp,iq) !per il barrier
      end if
      if(pol%rank /= pol%root .and. pol%rank == owner) then
        call pw_wfc6d_recv(pol%val(location,iomega),pol%root,iq,pol%comm)
      end if
      call pw_polar_giveback_basis(pol,basis_tmp)
    end do
    if(pol%rank == pol%root) then
      call iotk_scan_end(unit,"omega"//trim(iotk_index(iomega)))
    end if
  end do
  if(pol%rank == pol%root) then
    call iotk_scan_end(unit,name)
  end if

end subroutine pw_polar_read_x

subroutine pw_polar_destroy_x(pol)
  use pw_polar_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use tools_module
  use num_module
  implicit none
  type (pw_polar), intent(inout) :: pol
  nullify(pol%struct)
  nullify(pol%symmlist)
  nullify(pol%qmesh)
  call pw_deallocate(pol%where_q)
  deallocate(pol%where_q)
  if(size(pol%val)>0 .and. pol%val_initialized) call pw_wfc6d_destroy(pol%val(:,:))
  deallocate(pol%val)
  call pw_wfc_destroy(pol%gradients_l(:,:))
  call pw_wfc_destroy(pol%gradients_r(:,:))
  deallocate(pol%gradients_l,pol%gradients_r)
  deallocate(pol%macroscopic)
  call pw_basis_destroy(pol%basis(:))
  deallocate(pol%basis)
  deallocate(pol%omega)
  deallocate(pol%weight)
end subroutine pw_polar_destroy_x
