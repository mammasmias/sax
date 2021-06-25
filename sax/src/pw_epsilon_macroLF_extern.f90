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

subroutine pw_epsilon_macroLF_init_x(elf,struct,symmlist,qmesh,omegamax,&
                                    nomega,cutoff,emax,degauss,nkpt,imaginary_axis,root,comm)
  use pw_epsilon_macroLF_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use tools_module
  use num_module

  implicit none

  type (pw_epsilon_macroLF),  intent(out) :: elf 
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
  real,                       intent(in)  :: cutoff,omegamax
  real,                       intent(in)  :: emax, degauss
  integer,                    intent(in)  :: root,nomega,nkpt
  type (ptk_comm),            intent(in)  :: comm
  logical,                    intent(in)  :: imaginary_axis
  integer iq, iomega

  elf%struct   => struct
  elf%symmlist => symmlist
  elf%qmesh    => qmesh
  elf%cutoff   =  cutoff
  elf%root     =  root
  elf%comm     =  comm
  elf%nomega   = nomega
  elf%omegamax = omegamax
  elf%emax     = emax
  elf%degauss  = degauss
  elf%imaginary_axis = imaginary_axis 
  elf%nkpt     = nkpt

  call ptk_comm_size(comm,elf%npe)
  call ptk_comm_rank(comm,elf%rank)
  if(elf%root >= elf%npe) ERROR("")

  allocate(elf%where_q(2,elf%qmesh%nkibz))
  call pw_allocate(elf%where_q)
  elf%where_q = 0
!
! divide q vectors on different tasks
! 
  do iq=1,qmesh%nkibz
    elf%where_q(1,iq) = modulo(iq-elf%root,elf%npe)
    elf%where_q(2,iq) = count(elf%where_q(1,1:iq)==elf%where_q(1,iq))
  end do

  allocate(elf%basis(qmesh%nkibz))
  call pw_basis_init(elf%basis(:),elf%struct)
  call pw_basis_create(elf%basis,elf%qmesh,elf%cutoff)

  write(0,*)"The aprox number of plane waves for P is:", elf%basis(1)%npw

  elf%iqgamma = pw_kmesh_kibz_index(elf%qmesh,(/0.0,0.0,0.0/))

  allocate(elf%val(elf%where_q(1,elf%iqgamma):elf%where_q(1,elf%iqgamma),0:nomega))
!
! only the task holding q=0 allocate the big matrx below 
!
  if(elf%where_q(1,elf%iqgamma)==elf%rank) then
     do iomega = 0,elf%nomega
        call pw_wfc6d_init(elf%val(elf%where_q(2,elf%iqgamma),iomega),elf%basis(elf%iqgamma))
     end do
  end if

  allocate(elf%gradients_l(3,0:nomega))
  allocate(elf%gradients_r(3,0:nomega))
  call pw_wfc_init(elf%gradients_l(:,:),elf%basis(elf%iqgamma))
  call pw_wfc_init(elf%gradients_r(:,:),elf%basis(elf%iqgamma))
!
! buffers for vector matrix vector operation
!
! allocate(elf%lambda(3,0:nomega))
  allocate(elf%rho(3,0:nomega))
!  call pw_wfc_init(elf%lambda,elf%basis(elf%iqgamma))
  call pw_wfc_init(elf%rho,elf%basis(elf%iqgamma))
  
  allocate(elf%macroscopic(3,3,0:nomega))
  allocate(elf%wbwprod(3,3,0:nomega))
  elf%macroscopic = 0.0

  allocate(elf%omega(0:nomega))
  allocate(elf%weight(0:nomega))
  
  call num_mesh(elf%omegamax,elf%omega,elf%weight)

end subroutine pw_epsilon_macroLF_init_x

subroutine pw_epsilon_macroLF_calc_x(elf,states,atoms,broadening)
use ptk_module
use tools_module
use pw_epsilon_macroLF_type
use pw_epsilon_macroLF_interf 
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

implicit none

type(pw_epsilon_macroLF), intent(inout) :: elf 
type(pw_states), intent(in)             :: states
type(pw_atoms), intent(in)              :: atoms
type(pw_pseudovelocity) :: pseudovelocity
complex                 :: scaled_velocity(3)
type(pw_wfc), pointer   :: wfc_val, wfc_con
type(pw_wfc)            :: wfc_dip 
type(pw_field)          :: field_val,field_con
type(pw_field)          :: field_dip
type(pw_basis), pointer :: basis_val, basis_con 
type(pw_basis)          :: basis_dip

real                    :: deltae, switch, broadening 
logical                 :: lflag
integer                 :: ik 
integer                 :: ib1,ib2
real                    :: vk(3)
integer                 :: dim(3)
integer                 :: owner, location
integer                 :: iomega

  call pw_field_init(field_dip,states%struct)
  call pw_field_init(field_val,states%struct)
  call pw_field_init(field_con,states%struct)

  elf%broadening=broadening
!
! loop over kpts
!
  do ik=1,states%kmesh%nkibz
      vk = states%kmesh%kibz(:,ik)
      call pw_states_borrow_basis(states,basis_con,ik)
      call pw_pseudovelocity_init(pseudovelocity,basis_con,atoms)
      call pw_basis_init(basis_dip,states%struct)
      call pw_basis_create(basis_dip,elf%qmesh%kbz(:,elf%iqgamma),elf%cutoff)
      dim = pw_field_dim_from_dipole(basis_dip,states%basis(ik),states%basis(ik))

      call pw_field_set_dim(field_val, dim, k=vk,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(field_con, dim, k=vk,r0=(/0.0,0.0,0.0/))
      call pw_field_set_dim(field_dip, dim, k=vk-vk ,r0=(/0.0,0.0,0.0/))

      call pw_wfc_init(wfc_dip,basis_dip)
!
! loop on occupied states
!
      do ib2=states%nbmin, states%nbmax 

!? non va bene, ci sono transizioni intrabanda fra occupazioni semipiene nei metalli!!!! 

        if(states%occupation(ib2,ik) < 1.99) cycle
        call pw_states_borrow_wfc(states,wfc_val,ib2,ik)
        call pw_wfc2field(field_val,wfc_val)
!
! loop on unoccupied states
!
        do ib1=states%nbmin,states%nbmax 
          if(.not.pw_states_is_local(states,ib1,ik)) cycle
          switch = 0.5 * num_erfc((abs(states%e(ib1,ik) - states%e(ib2,ik)) &
                   - elf%emax) / (2.0*elf%degauss))* &
                   0.5 * num_erfc( (states%occupation(ib1,ik) - 1.99)*1000.0 )
 
          call ptk_allreduce( switch < 0.01 , lflag, ptk_land,elf%comm)
          if(lflag) cycle

          call pw_states_borrow_wfc(states,wfc_con,ib1,ik)
          call pw_wfc2field(field_con,wfc_con)
!
! compute dipole elements for the HEAD (directly in reciprocal space)  
!
          deltae = states%e(ib1,ik) - states%e(ib2,ik)

          scaled_velocity = (pw_wfc_p_braket(wfc_val,wfc_con) &
                              +pw_pseudovelocity_braket(pseudovelocity,wfc_val,wfc_con))
!
! compute dipole elements for WINGS and BODY in real space and than goes back in reciprocal one 
!
          call pw_field_mul(field_dip,field_val,field_con)
          call pw_field2wfc(wfc_dip,field_dip)
!
! recover bands parallelism
!
          call pw_epsilon_macroLF_add_dipole(elf,deltae,switch,wfc_dip,&
                   velocity=scaled_velocity) 

          call pw_states_giveback_wfc(states,wfc_con)
        end do

        call pw_states_giveback_wfc(states,wfc_val)
      end do
      call pw_pseudovelocity_destroy(pseudovelocity)
      call pw_wfc_destroy(wfc_dip)
      call pw_basis_destroy(basis_dip)
      call pw_states_giveback_basis(states,basis_con)
  end do

  call pw_field_destroy(field_val)
  call pw_field_destroy(field_con)
  call pw_field_destroy(field_dip)

end subroutine pw_epsilon_macroLF_calc_x

subroutine pw_epsilon_macroLF_borrow_basis_x(elf,basis,iq)
  use pw_epsilon_macroLF_type
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
  type(pw_epsilon_macroLF),           intent(in)  :: elf 
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
  if(present(basis) .neqv. present(iq)) ERROR("")
  if(present(basis)) basis => elf%basis(iq)
end subroutine pw_epsilon_macroLF_borrow_basis_x

subroutine pw_epsilon_macroLF_giveback_basis_x(elf,basis)
  use pw_epsilon_macroLF_type
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
  type(pw_epsilon_macroLF), intent(in)    :: elf 
  type(pw_basis), pointer, optional :: basis
  if(.not.associated(elf%basis)) ERROR("")
  if(present(basis)) nullify(basis)
end  subroutine pw_epsilon_macroLF_giveback_basis_x

subroutine pw_epsilon_macroLF_add_dipole_x(elf,deltae,switch,wfc_dip,velocity)
  use pw_epsilon_macroLF_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_allgather,ptk_allreduce_inplace,&
                         ptk_sum,ptk_recv,ptk_send
  use tools_module
  use num_module
  implicit none
  type(pw_epsilon_macroLF), intent(inout) :: elf 
  real,            intent(in)             :: deltae, switch
  type(pw_wfc),    intent(in)             :: wfc_dip
  complex,optional,intent(in)             :: velocity(3)

  integer                                 :: owner,location,ip
  integer, allocatable                    :: iq_array(:)
  complex                                 :: macro(3,3)
  type(pw_wfc)                            :: gradients_tmp
  complex                                 :: scaled_velocity_loc(3)

  integer                                 :: i
  integer                                 :: iomega

  type(pw_wfc), allocatable               :: wfc_queue(:)
  real,         allocatable               :: deltae_queue(:)
  integer,      allocatable               :: location_queue(:)
  real,         allocatable               :: switch_queue(:)
  integer                                 :: nqueue,iqueue
  complex                                 :: scale

  scaled_velocity_loc = 0.0
  if(present(velocity)) scaled_velocity_loc = velocity / deltae 

  allocate(iq_array(0:elf%npe-1))
  call ptk_allgather(elf%iqgamma,iq_array,elf%comm)

  call pw_wfc_init(gradients_tmp,elf%basis(elf%iqgamma))
!
! Loop on frequency
!
  do iomega = 0,elf%nomega
!
! polarizzability HEAD complete calculation and storage in final variable macro
!
      if(elf%imaginary_axis) then
          scale = elf%weight(iomega)*(2.0/((0.0,1.0)*elf%omega(iomega)-deltae+(0.0,1.0)*&
                  elf%broadening)- 2.0/((0.0,1.0)*elf%omega(iomega)+deltae+(0.0,1.0)*&
                  elf%broadening))/(elf%struct%a_omega * elf%nkpt)* switch 
      else
          scale = elf%weight(iomega)*(2.0/(elf%omega(iomega)-deltae+(0.0,1.0)*elf%broadening)- &
                  2.0/(elf%omega(iomega)+deltae+(0.0,1.0)*elf%broadening))&
                  /(elf%struct%a_omega * elf%nkpt)* switch 
      endif

      do i = 1 , 3
        macro(i,:) = scale * (conjg(scaled_velocity_loc(i)) * scaled_velocity_loc(:)+ &
                  scaled_velocity_loc(i)*conjg(scaled_velocity_loc(:)))/2.0
!
! polarizzability WINGS complete calculation and storage in gradients_l and gradients_r 
!
! left wing
!
        gradients_tmp = wfc_dip
        call pw_wfc_scale(gradients_tmp,scale*CONJG(scaled_velocity_loc(i)))
        if(switch<0.01) gradients_tmp%val = 0.0
        call pw_wfc_allreduce_inplace(gradients_tmp,elf%comm)
!        call pw_wfc_conjg(gradients_tmp)

        call pw_wfc_axpy(elf%gradients_l(i,iomega),(1.0,0.0),gradients_tmp)

! gradients_r is defined in the same way as gradients r but it is just
! because on pw_w_calc gemv performs the conjg
!
! right wing
!
        gradients_tmp = wfc_dip
        call pw_wfc_conjg(gradients_tmp)
        call pw_wfc_scale(gradients_tmp,scale*(scaled_velocity_loc(i)))
        if(switch<0.01) gradients_tmp%val = 0.0 
        call pw_wfc_allreduce_inplace(gradients_tmp,elf%comm)
!        call pw_wfc_conjg(gradients_tmp)

        call pw_wfc_axpy(elf%gradients_r(i,iomega),(1.0,0.0),gradients_tmp)
      end do
!
! Now we are out of components loop: recover parallelism on bands for the HEAD
!
      if(switch < 0.01) macro = 0.0
      call ptk_allreduce_inplace(macro,ptk_sum,elf%comm)
      elf%macroscopic(:,:,iomega) = elf%macroscopic(:,:,iomega) + macro(:,:)
    end do
    call pw_wfc_destroy(gradients_tmp)
!
! exchange of index arrays for wave functions localizzation  
!
  iqueue = 0
  do ip=0,elf%npe-1
    owner = elf%where_q(1,iq_array(ip))
    if(owner==elf%rank .and. ip/=elf%rank) iqueue = iqueue+1
  end do
  nqueue = iqueue
  allocate(wfc_queue(nqueue))
  allocate(deltae_queue(nqueue))
  allocate(location_queue(nqueue))
  allocate(switch_queue(nqueue))
  iqueue = 0
  do ip=0,elf%npe-1
    owner = elf%where_q(1,iq_array(ip))
    location = elf%where_q(2,iq_array(ip))
    if(owner==elf%rank .and. ip/=elf%rank) then
      iqueue = iqueue+1
      call pw_wfc_init(wfc_queue(iqueue),elf%basis(iq_array(ip)))
      location_queue(iqueue) = location
    end if
  end do
  if(iqueue/=nqueue) ERROR("")

  iqueue = 0
  do ip=0,elf%npe-1
    owner = elf%where_q(1,iq_array(ip))
    if(ip /= elf%rank .and. elf%rank == owner) then
      iqueue = iqueue + 1
      call ptk_recv(switch_queue(iqueue),ip,owner,elf%comm)
      call ptk_recv(deltae_queue(iqueue),ip,owner,elf%comm)
      call pw_wfc_recv(wfc_queue(iqueue),ip,owner,elf%comm)
    end if
    if(ip == elf%rank .and. elf%rank /= owner) then
      call ptk_send(switch,owner,owner,elf%comm)
      call ptk_send(deltae,owner,owner,elf%comm)
      call pw_wfc_send(wfc_dip,owner,owner,elf%comm)
    end if
  end do
  if(iqueue/=nqueue) ERROR("")

!
! BODY calculation
!
  do iomega = 0,elf%nomega
    owner = elf%where_q(1,iq_array(elf%rank))
    location = elf%where_q(2,iq_array(elf%rank))
    if(owner == elf%rank) then
      if(elf%imaginary_axis) then
          scale = elf%weight(iomega)*(2.0/((0.0,1.0)*elf%omega(iomega)-deltae+(0.0,1.0)*&
                  elf%broadening)- 2.0/((0.0,1.0)*elf%omega(iomega)+deltae+(0.0,1.0)*&
                  elf%broadening)) * switch /(elf%struct%a_omega * elf%nkpt) 
      else
          scale = elf%weight(iomega)*(2.0/(elf%omega(iomega)-deltae+(0.0,1.0)*elf%broadening)- &
                  2.0/(elf%omega(iomega)+deltae+(0.0,1.0)*elf%broadening)) *switch &
                  /(elf%struct%a_omega * elf%nkpt)
      endif

      if(switch>=0.01) call pw_wfc6d_add_from_wfc(elf%val(location,iomega),scale,wfc_dip)

    end if

    do iqueue = 1,nqueue
        if(elf%imaginary_axis) then
            scale = elf%weight(iomega)*(2.0/((0.0,1.0)*elf%omega(iomega)-deltae_queue(iqueue)+(0.0,1.0)*&
                    elf%broadening)- 2.0/((0.0,1.0)*elf%omega(iomega)+deltae_queue(iqueue)+(0.0,1.0)*&
                    elf%broadening)) * switch_queue(iqueue) /(elf%struct%a_omega * elf%nkpt) 
        else    
            scale = elf%weight(iomega)*(2.0/(elf%omega(iomega)-deltae_queue(iqueue)+(0.0,1.0)*&
                    elf%broadening)-2.0/(elf%omega(iomega)+deltae_queue(iqueue)+& 
                    (0.0,1.0)*elf%broadening)) *switch_queue(iqueue) /(elf%struct%a_omega * elf%nkpt) 
        endif   

       if(switch_queue(iqueue)>=0.01) call pw_wfc6d_add_from_wfc(elf%val(location_queue(iqueue),iomega)&
                                      ,scale,wfc_queue(iqueue))
    end do  

  end do

  deallocate(deltae_queue,location_queue)
  deallocate(switch_queue)
  call pw_wfc_destroy(wfc_queue)
  deallocate(wfc_queue)
  deallocate(iq_array)

end subroutine pw_epsilon_macroLF_add_dipole_x

subroutine pw_epsilon_macroLF_write_x(elf,unit,name)
  use pw_epsilon_macroLF_type
  use pw_epsilon_macroLF_interf, dont_use => pw_epsilon_macroLF_write
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
  type(pw_epsilon_macroLF),  intent(in) :: elf 
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  type(pw_basis), pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq,iomega
  character(iotk_attlenx) :: attr

  if(elf%rank == elf%root) then
    call iotk_write_attr(attr,"type","pw_epsilon_macroLF",first=.true.)
    call iotk_write_begin(unit,trim(name),attr)
  
    call iotk_write_attr(attr,"nomega",elf%nomega,first=.true.)
    call iotk_write_attr(attr,"nq",    elf%qmesh%nkbz)
    call iotk_write_empty(unit,"info",attr)
  end if

  do iomega=0,elf%nomega
    if(elf%rank == elf%root) then
      call iotk_write_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_write_dat(unit,"macro1",elf%macroscopic(1,:,iomega))
      call iotk_write_dat(unit,"macro2",elf%macroscopic(2,:,iomega))
      call iotk_write_dat(unit,"macro3",elf%macroscopic(3,:,iomega))
      call pw_wfc_write(elf%gradients_l(1,iomega),unit,"iotk",name="gradients_l1")
      call pw_wfc_write(elf%gradients_l(2,iomega),unit,"iotk",name="gradients_l2")
      call pw_wfc_write(elf%gradients_l(3,iomega),unit,"iotk",name="gradients_l3")
      call pw_wfc_write(elf%gradients_r(1,iomega),unit,"iotk",name="gradients_r1")
      call pw_wfc_write(elf%gradients_r(2,iomega),unit,"iotk",name="gradients_r2")
      call pw_wfc_write(elf%gradients_r(3,iomega),unit,"iotk",name="gradients_r3")
    end if

    do iq=1,elf%qmesh%nkibz
      owner = elf%where_q(1,iq)
      location = elf%where_q(2,iq)
      if(elf%rank == elf%root .and. elf%rank == owner) then
        call pw_wfc6d_write(elf%val(location,iomega),unit,"iotk",name="val"//trim(iotk_index(iq)))
      end if
      if(elf%rank == elf%root .and. elf%rank /= owner) then
        call pw_epsilon_macroLF_borrow_basis(elf,basis_tmp,iq)
        call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
        call pw_wfc6d_recv(val_tmp,owner,iq,elf%comm)
        call pw_wfc6d_write(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
        call pw_wfc6d_destroy(val_tmp)
        call pw_epsilon_macroLF_giveback_basis(elf,basis_tmp)
      end if
      if(elf%rank /= elf%root .and. elf%rank == owner) then
        call pw_wfc6d_send(elf%val(location,iomega),elf%root,iq,elf%comm)
      end if
    end do
    if(elf%rank == elf%root) then
      call iotk_write_end(unit,"omega"//trim(iotk_index(iomega)))
    end if
  end do

  if(elf%rank == elf%root) then
    call iotk_write_end(unit,name)
  end if

end subroutine pw_epsilon_macroLF_write_x

subroutine pw_epsilon_macroLF_read_x(elf,unit,name)
  use pw_epsilon_macroLF_type
  use pw_epsilon_macroLF_interf, dont_use => pw_epsilon_macroLF_read
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
  type(pw_epsilon_macroLF),  intent(inout) :: elf 
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  type(pw_basis), pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq
  integer :: iomega,nomega,nq

  if(elf%rank == elf%root) then
    call iotk_scan_begin(unit,trim(name),attr)
    call iotk_scan_attr(attr,"type",rtype,default="pw_epsilon_macroLF")
    if(rtype/="pw_epsilon_macroLF") ERROR("")

    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr(attr,"nomega",nomega)
    if(nomega/=elf%nomega) ERROR("")
    call iotk_scan_attr(attr,"nq",    nq)
    if(nq/=elf%qmesh%nkbz) ERROR("")
  end if

  do iomega = 0,elf%nomega
    if(elf%rank == elf%root) then
      call iotk_scan_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_scan_dat(unit,"macro1",elf%macroscopic(1,:,iomega))
      call iotk_scan_dat(unit,"macro2",elf%macroscopic(2,:,iomega))
      call iotk_scan_dat(unit,"macro3",elf%macroscopic(3,:,iomega))
      call pw_wfc_read(elf%gradients_l(1,iomega),unit,"iotk",name="gradients_l1")
      call pw_wfc_read(elf%gradients_l(2,iomega),unit,"iotk",name="gradients_l2")
      call pw_wfc_read(elf%gradients_l(3,iomega),unit,"iotk",name="gradients_l3")
      call pw_wfc_read(elf%gradients_r(1,iomega),unit,"iotk",name="gradients_r1")
      call pw_wfc_read(elf%gradients_r(2,iomega),unit,"iotk",name="gradients_r2")
      call pw_wfc_read(elf%gradients_r(3,iomega),unit,"iotk",name="gradients_r3")
    end if

    call ptk_bcast(elf%macroscopic(:,:,iomega),elf%root,elf%comm)
    call pw_wfc_bcast(elf%gradients_l(1,iomega),elf%root,elf%comm)
    call pw_wfc_bcast(elf%gradients_l(2,iomega),elf%root,elf%comm)
    call pw_wfc_bcast(elf%gradients_l(3,iomega),elf%root,elf%comm)
    call pw_wfc_bcast(elf%gradients_r(1,iomega),elf%root,elf%comm)
    call pw_wfc_bcast(elf%gradients_r(2,iomega),elf%root,elf%comm)
    call pw_wfc_bcast(elf%gradients_r(3,iomega),elf%root,elf%comm)

    do iq=1,elf%qmesh%nkibz
      owner = elf%where_q(1,iq)
      location = elf%where_q(2,iq)
      if(elf%rank == elf%root .and. elf%rank == owner) then
        call pw_wfc6d_read(elf%val(location,iomega),unit,"iotk",name="val"//trim(iotk_index(iq)))
      end if
      if(elf%rank == elf%root .and. elf%rank /= owner) then
! OCCHIO AL BARRIER
        call pw_epsilon_macroLF_borrow_basis(elf,basis_tmp,iq)
        call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
        call pw_wfc6d_read(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
        call pw_wfc6d_send(val_tmp,owner,iq,elf%comm)
        call pw_wfc6d_destroy(val_tmp)
      else
        call pw_epsilon_macroLF_borrow_basis(elf,basis_tmp,iq) !per il barrier
      end if
      if(elf%rank /= elf%root .and. elf%rank == owner) then
        call pw_wfc6d_recv(elf%val(location,iomega),elf%root,iq,elf%comm)
      end if
      call pw_epsilon_macroLF_giveback_basis(elf,basis_tmp)
    end do
    if(elf%rank == elf%root) then
      call iotk_scan_end(unit,"omega"//trim(iotk_index(iomega)))
    end if
  end do
  if(elf%rank == elf%root) then
    call iotk_scan_end(unit,name)
  end if

end subroutine pw_epsilon_macroLF_read_x

subroutine pw_epsilon_macroLF_destroy_x(elf)
  use pw_epsilon_macroLF_type
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
  type (pw_epsilon_macroLF), intent(inout) :: elf 
  nullify(elf%struct)
  nullify(elf%symmlist)
  nullify(elf%qmesh)
  call pw_deallocate(elf%where_q)
  deallocate(elf%where_q)
  if(elf%rank == elf%iqgamma) call pw_wfc6d_destroy(elf%val(:,:))
  deallocate(elf%val)
  call pw_wfc_destroy(elf%gradients_l(:,:))
  call pw_wfc_destroy(elf%gradients_r(:,:))
  deallocate(elf%gradients_l,elf%gradients_r)
!  deallocate(elf%lambda,elf%rho) 
  deallocate(elf%macroscopic)
  deallocate(elf%wbwprod)
  call pw_basis_destroy(elf%basis(:))
  deallocate(elf%basis)
  deallocate(elf%omega)
  deallocate(elf%weight)
end subroutine pw_epsilon_macroLF_destroy_x
