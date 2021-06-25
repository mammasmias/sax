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

subroutine pw_epsilon_macro_init_x(epsilon,struct,symmlist,qmesh,omegamax,nomega,cutoff, &
    emax,degauss,energy_shift,broadening,imagomega,root,comm)
  use pw_epsilon_macro_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_common_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use tools_module
  use num_module
  implicit none
  type (pw_epsilon_macro),   intent(out) :: epsilon
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh), target,    intent(in)  :: qmesh
  real,                       intent(in)  :: cutoff,omegamax
  real,                       intent(in)  :: emax, degauss, energy_shift
  integer,                    intent(in)  :: nomega
  real,                       intent(in)  :: broadening
  logical,                    intent(in)  :: imagomega
  integer,                    intent(in)  :: root
  type(ptk_comm),             intent(in)  :: comm
  integer iq, iomega

  epsilon%struct   => struct
  epsilon%symmlist => symmlist
  epsilon%qmesh => qmesh
  epsilon%cutoff   =  cutoff
  epsilon%nomega   = nomega 
  epsilon%omegamax = omegamax
  epsilon%emax     = emax
  epsilon%degauss  = degauss
  epsilon%energy_shift = energy_shift
  epsilon%broadening = broadening
  epsilon%imaginary_axis = imagomega

  epsilon%comm = comm
  epsilon%root = root

  epsilon%iqgamma=pw_kmesh_kibz_index(epsilon%qmesh,(/0.0,0.0,0.0/))

  allocate(epsilon%macroscopic(3,3,0:nomega))
  epsilon%macroscopic = 0.0

  allocate(epsilon%omega(0:nomega))
  allocate(epsilon%weight(0:nomega))

  call num_mesh(epsilon%omegamax,epsilon%omega,epsilon%weight)

end subroutine pw_epsilon_macro_init_x

subroutine pw_epsilon_macro_init2_x(epsilon,omegamax,nomega,imagomega,root,comm)
  use pw_epsilon_macro_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_common_module
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use tools_module
  use num_module
  implicit none
  type (pw_epsilon_macro),   intent(out) :: epsilon
  real,                       intent(in)  :: omegamax
  integer,                    intent(in)  :: nomega
  logical,                    intent(in)  :: imagomega
  integer,                    intent(in)  :: root
  type(ptk_comm),             intent(in)  :: comm
  integer iomega

  epsilon%nomega   = nomega 
  epsilon%omegamax = omegamax
  epsilon%imaginary_axis = imagomega

  epsilon%comm = comm
  epsilon%root = root

  allocate(epsilon%macroscopic(3,3,0:nomega))
  epsilon%macroscopic = 0.0

  allocate(epsilon%omega(0:nomega))
  allocate(epsilon%weight(0:nomega))

  call num_mesh(epsilon%omegamax,epsilon%omega,epsilon%weight)

end subroutine pw_epsilon_macro_init2_x


subroutine pw_epsilon_macro_rpa_calc_x(epsilon,states,atoms)
use ptk_module
use mp_global
use tools_module
use pw_epsilon_macro_type
use pw_epsilon_macro_interf 
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

type(pw_epsilon_macro), intent(inout) :: epsilon
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms

type(pw_pseudovelocity) :: pseudovelocity
complex :: scaled_velocity(3)
type(pw_wfc), pointer :: wfc_val, wfc_con
type(pw_basis), pointer :: basis_con, basis_val 

real :: deltae, epsilon_weight, switch
complex :: scale
logical :: lflag
integer :: iq, ik
integer :: ib1,ib2
real :: k(3)
integer :: dim(3)
integer :: owner, location
integer :: iomega
real :: identity(3,3)
complex :: macro(3,3)
integer :: my_pool_id, me_pool, npool_local

call ptk_comm_rank(inter_pool_comm,my_pool_id)
call ptk_comm_rank(intra_pool_comm,me_pool)
call ptk_comm_size(inter_pool_comm,npool_local)

  identity(:,:)=0.0
  identity(1,1)=1.0
  identity(2,2)=1.0
  identity(3,3)=1.0

!
! kpt loop
!
  do ik=1,states%kmesh%nkibz
    if(mod(ik,npool_local).ne.my_pool_id) cycle
    k = states%kmesh%kibz(:,ik)
    call pw_states_borrow_basis(states,basis_con,ik)
      
    call pw_pseudovelocity_init(pseudovelocity,basis_con,atoms)

!
! loop on occupied states
!
    do ib2=states%nbmin, states%nbmax 

!? occhio alle interbanda fra occupazioni semintere, questo non va bene!!!!

      call pw_states_borrow_wfc(states,wfc_val,ib2,ik)

      if(states%occupation(ib2,ik) < 1.99) cycle
!
! loop on unoccupied states
!
      do ib1=states%nbmin,states%nbmax 
        if(.not.pw_states_is_local(states,ib1,ik)) cycle
        switch = 0.5 * num_erfc((abs(states%e(ib1,ik) - states%e(ib2,ik)) &
                     - epsilon%emax) / (2.0*epsilon%degauss))* &
                     0.5 * num_erfc( (states%occupation(ib1,ik)- 1.99)*1000.0 ) 
        call ptk_allreduce( switch < 0.01 , lflag, ptk_land,epsilon%comm)
        if(lflag) cycle
        
        call pw_states_borrow_wfc(states,wfc_con,ib1,ik)
        deltae = states%e(ib1,ik) - states%e(ib2,ik) + epsilon%energy_shift
!
! compute dipole matrix element
!        
        scaled_velocity = (pw_wfc_p_braket(wfc_val,wfc_con) & 
                           +pw_pseudovelocity_braket(pseudovelocity,wfc_val,wfc_con))

        do iomega=0,epsilon%nomega

          if(epsilon%imaginary_axis) then
             scale = 2.0*(1.0/((0.0,1.0)*epsilon%omega(iomega)-deltae+(0.0,1.0)*epsilon%broadening)- &
                     1.0/((0.0,1.0)*epsilon%omega(iomega)+deltae+(0.0,1.0)*epsilon%broadening)) / &
                     epsilon%struct%a_omega / states%kmesh%nkibz
          else
             scale = 2.0*(1.0/(epsilon%omega(iomega)-deltae+(0.0,1.0)*epsilon%broadening)- &
                     1.0/(epsilon%omega(iomega)+deltae+(0.0,1.0)*epsilon%broadening)) / &
                     epsilon%struct%a_omega / states%kmesh%nkibz
          endif
          macro(:,:)=0.0
          call pw_epsilon_macro_add_dipole(macro,deltae,scale,1.0,switch, &
                   epsilon%iqgamma,velocity=scaled_velocity,comm=epsilon%comm)
          epsilon%macroscopic(:,:,iomega) = epsilon%macroscopic(:,:,iomega) + macro(:,:)
        enddo
        call pw_states_giveback_wfc(states,wfc_con) 
      end do
      call pw_states_giveback_wfc(states,wfc_val)
    end do
    call pw_pseudovelocity_destroy(pseudovelocity)
    call pw_states_giveback_basis(states,basis_con)
  end do

  do iomega=0,epsilon%nomega
    epsilon%macroscopic(:,:,iomega)=identity(:,:)/num_8pi-epsilon%macroscopic(:,:,iomega)
  enddo

!
! intraband
!
!  do ik=1,states%kmesh%nkibz
!    k = states%kmesh%kibz(:,ik)
!    call pw_states_borrow_basis(states,basis_con,ik)
      
!    call pw_pseudovelocity_init(pseudovelocity,basis_con,atoms)
!
! loop on occupied states
!
!    do ib2=states%nbmin, states%nbmax 
!      if(.not.pw_states_is_local(states,ib2,ik)) cycle
!      call pw_states_borrow_wfc(states,wfc_val,ib2,ik)

!        scaled_velocity = (pw_wfc_p_braket(wfc_val,wfc_val) & 
!                           +pw_pseudovelocity_braket(pseudovelocity,wfc_val,wfc_val))

!        do iomega=0,epsilon%nomega

!          if(epsilon%imaginary_axis) then
!             scale = (1.0/((0.0,1.0)*(epsilon%omega(iomega)+0.001)+(0.0,1.0)*0.001))/ &
!                     ((0.0,1.0)*(epsilon%omega(iomega)+0.001)) 
!          else
!             scale = (1.0/((epsilon%omega(iomega)+(0.0,1.0)*0.001)+(0.0,1.0)*0.001))/ &
!                     ((epsilon%omega(iomega)+(0.0,1.0)*0.001))
!          endif
          
!          macro(:,:)=0.0
!          if(epsilon_weight > 0.01) call pw_epsilon_macro_add_dipole(macro,deltae,scale,epsilon_weight,&
!                               epsilon%iqgamma,scaled_velocity,epsilon%comm) 
!          epsilon%macroscopic(:,:,iomega) = epsilon%macroscopic(:,:,iomega) + macro(:,:)
!        enddo
!     end do
!    call pw_states_giveback_wfc(states,wfc_val)
!    call pw_pseudovelocity_destroy(pseudovelocity)
!    call pw_states_giveback_basis(states,basis_con)
!  end do

end subroutine pw_epsilon_macro_rpa_calc_x

subroutine pw_oscillator_strengh_calc_x(nstatemin,nstatemax,bse,states,atoms)
use ptk_module
use tools_module
use pw_bse_type
use pw_parall_matrix_module
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

integer, intent(in) :: nstatemin, nstatemax
type(pw_bse), intent(in) :: bse
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms

type(pw_pseudovelocity) :: pseudovelocity
complex :: scaled_velocity(3)
type(pw_wfc), pointer :: wfc_val, wfc_con
type(pw_basis), pointer :: basis_val, basis_con

real :: deltae
complex :: epsilon_weight
complex :: scale
logical :: lflag
integer :: iq, ik
integer :: ib1,ib2
real :: k(3)
integer :: dim(3)
integer :: itrans1,itrans2
!----------------------------------------
! variables controling parall flux
integer :: owner, location, iboh
!-----------------------------------------
integer :: iomega
real :: identity(3,3)
complex, allocatable :: macro(:,:,:)
integer :: i, ndim
complex :: epsilon_weight2

  ndim = (nstatemax-nstatemin)+1

  allocate(macro(1:3,1:3,nstatemin:nstatemax))
  macro(:,:,:) = 0.0

  identity(:,:)=0.0
  identity(1,1)=1.0
  identity(2,2)=1.0
  identity(3,3)=1.0

  scale = 2.0 / states%struct%a_omega / states%kmesh%nkibz

  iboh=1
  do itrans1=1,bse%ntrans
    if(itrans1>nstatemax) cycle
    if(itrans1<nstatemin) cycle
    owner = bse%matrix%where_line(1,itrans1)
    location = bse%matrix%where_line(2,itrans1)
    if(owner==bse%matrix%rank) then
write(0,*) "itrans1: ", itrans1
      do itrans2=1,bse%ntrans
write(0,*) "itrans2: ", itrans2
        ik=bse%iktrans(itrans2)
        k = states%kmesh%kibz(:,ik)
        epsilon_weight = 0.0
        epsilon_weight2 = 0.0
        epsilon_weight = bse%matrix%val(location,itrans2)
        epsilon_weight2 = epsilon_weight * conjg(epsilon_weight)
        if(real(epsilon_weight2)<0.01) cycle
write(0,*) "epsilon_weight2 after: ", itrans2, epsilon_weight, epsilon_weight2
        call pw_states_borrow_basis(states,basis_con,ik)
        call pw_pseudovelocity_init(pseudovelocity,basis_con,atoms)
        scaled_velocity = 0.0

! occupied states
        ib2=bse%ib2trans(itrans2)
        call pw_states_borrow_wfc(states,wfc_val,ib2,ik)
! unoccupied states
        ib1=bse%ib1trans(itrans2)
        call pw_states_borrow_wfc(states,wfc_con,ib1,ik)
! pw_dipole compute FFT(conjg(field_val)*field_con)
        if(owner==bse%matrix%rank) scaled_velocity = (pw_wfc_p_braket(wfc_val,wfc_con) &
                             +pw_pseudovelocity_braket(pseudovelocity,wfc_val,wfc_con))
        call pw_states_giveback_wfc(states,wfc_con)
       deltae = states%e(ib1,ik) - states%e(ib2,ik)
     
! should add energy_shift for running with scissor shift
!       deltae = states%e(ib1,ik) - states%e(ib2,ik) + epsilon%energy_shift
        scaled_velocity = scaled_velocity * epsilon_weight / deltae
        if(owner==bse%matrix%rank) then
          do i=1,3
            macro(i,:,itrans1) = macro(i,:,itrans1) + scale * &
                   conjg(scaled_velocity(i)) *  &
                   scaled_velocity(:)
          enddo
        endif 
        call pw_states_giveback_wfc(states,wfc_val)
        call pw_pseudovelocity_destroy(pseudovelocity)
        call pw_states_giveback_basis(states,basis_con)
      enddo
    else
      call pw_states_borrow_wfc(states,wfc_con,states%nbmin,1)
      call pw_states_borrow_wfc(states,wfc_val,states%nbmin,1)
      call pw_states_giveback_wfc(states,wfc_con)
      call pw_states_giveback_wfc(states,wfc_val) 
    endif

    if(bse%matrix%rank==owner) write(0,*) itrans1, bse%matrix%eigenval(itrans1),  &
       macro(1,1,itrans1), macro(2,2,itrans1), macro(3,3,itrans1)
  enddo


    call ptk_allreduce_inplace(macro,ptk_sum,bse%matrix%comm)


  if(bse%matrix%rank==bse%matrix%root) then
    do itrans1=1,bse%ntrans
      if(itrans1<nstatemin) cycle
      if(itrans1>nstatemax) cycle
      write(25,'("Exciton: ",i5,2x,f12.6,2x,3f12.6)') itrans1, bse%matrix%eigenval(itrans1), real(macro(1,1,itrans1)), &
      real(macro(2,2,itrans1)), real(macro(3,3,itrans1))
    enddo
  endif 
  deallocate(macro)
end subroutine pw_oscillator_strengh_calc_x

subroutine pw_oscillator_strengh_bis_calc_x(nstatemin,nstatemax,bse,states,atoms)
use ptk_module
use tools_module
use pw_bse_type
use pw_parall_matrix_module
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

integer, intent(in) :: nstatemin, nstatemax
type(pw_bse), intent(in) :: bse
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms

type(pw_pseudovelocity) :: pseudovelocity
complex :: scaled_velocity(3), scaled_velocity_tmp(3)
type(pw_wfc), pointer :: wfc_val, wfc_con
type(pw_basis), pointer :: basis_val, basis_con

real :: deltae
complex :: epsilon_weight
complex :: scale
logical :: lflag
integer :: iq, ik
integer :: ib1,ib2
real :: k(3)
integer :: dim(3)
integer :: itrans1,itrans2
!----------------------------------------
! variables controling parall flux
integer :: owner, location, iboh
!-----------------------------------------
integer :: iomega
real :: identity(3,3)
complex, allocatable :: macro(:,:,:)
integer :: i, ndim, itrans

  ndim = (nstatemax-nstatemin)+1

  allocate(macro(1:3,1:3,1:bse%ntrans))
  macro(:,:,:) = 0.0
  itrans = 1

  identity(:,:)=0.0
  identity(1,1)=1.0
  identity(2,2)=1.0
  identity(3,3)=1.0

  scale = 2.0 / states%struct%a_omega / states%kmesh%nkibz
  do ik=bse%nkmin,bse%nkmax
        k = states%kmesh%kibz(:,ik)
        call pw_states_borrow_basis(states,basis_con,ik)
        call pw_pseudovelocity_init(pseudovelocity,basis_con,atoms)
  do ib1=bse%nbcmin,bse%nbcmax
! unoccupied states
    if(pw_states_is_local(states,ib1,ik)) then
        call pw_states_borrow_wfc(states,wfc_con,ib1,ik)
! occupied states
    do ib2=bse%nbvmin,bse%nbvmax
        deltae = states%e(ib1,ik) - states%e(ib2,ik)
        call pw_states_borrow_wfc(states,wfc_val,ib2,ik)
        scaled_velocity = 0.0
! pw_dipole compute FFT(conjg(field_val)*field_con)
        scaled_velocity = (pw_wfc_p_braket(wfc_val,wfc_con) &
                             +pw_pseudovelocity_braket(pseudovelocity,wfc_val,wfc_con))
        call pw_states_giveback_wfc(states,wfc_val)
     
        scaled_velocity = scaled_velocity / deltae
          do itrans2=1,bse%ntrans
            if((ib2/=bse%ib2trans(itrans2)).or. &
               (ib1/=bse%ib1trans(itrans2)).or.(ik/=bse%iktrans(itrans2))) cycle
            do itrans1=1,bse%ntrans
             epsilon_weight = 0.0
             epsilon_weight = bse%matrix%val(itrans1,itrans2)
             scaled_velocity_tmp = scaled_velocity * epsilon_weight
          do i=1,3
            macro(i,:,itrans1) = macro(i,:,itrans1)+ &
                   conjg(scaled_velocity_tmp(i)) *  &
                   scaled_velocity_tmp(:)
          enddo
!write(0,*) itrans1,itrans2,deltae, real(macro(1,1,itrans1))
          enddo
!do itrans1=1,3
!write(0,*) itrans2,deltae, real(macro(1,1,itrans1))
!enddo
          enddo
     enddo
        call pw_states_giveback_wfc(states,wfc_con)
!write(0,*) "finished ib1: ", ib1
     else
      call pw_states_borrow_wfc(states,wfc_con,states%nbmin,1)
      call pw_states_giveback_wfc(states,wfc_con)
      do itrans=bse%nbvmin, bse%nbvmax
        call pw_states_borrow_wfc(states,wfc_val,states%nbmin,1)
        call pw_states_giveback_wfc(states,wfc_val)
      enddo 
     endif
     enddo
        call pw_pseudovelocity_destroy(pseudovelocity)
        call pw_states_giveback_basis(states,basis_con)
     enddo

do itrans1=1,3
write(0,*) deltae, real(macro(1,1,itrans1))
enddo

     call ptk_allreduce_inplace(macro,ptk_sum,states%comm)
  if(states%rank==states%root) then
    do itrans1=1,bse%ntrans
write(0,*) itrans1,bse%matrix%eigenval(itrans1), real(macro(1,1,itrans1)), real(macro(2,2,itrans1)), &
     real(macro(3,3,itrans1))
      write(25,'("Exciton: ",i5,2x,f12.6,2x,3f12.6)') itrans1, bse%matrix%eigenval(itrans1), real(macro(1,1,itrans1)), &
      real(macro(2,2,itrans1)), real(macro(3,3,itrans1))
    enddo
  endif 
  deallocate(macro)
end subroutine pw_oscillator_strengh_bis_calc_x

subroutine pw_epsilon_macro_rpaexc_calc_x(epsilon,bse,states,atoms)
use ptk_module
use tools_module
use pw_epsilon_macro_type
use pw_epsilon_macro_interf
use pw_bse_type
use pw_parall_matrix_module
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

type(pw_epsilon_macro), intent(inout) :: epsilon
type(pw_bse), intent(in) :: bse
type(pw_states), intent(in) :: states
type(pw_atoms), intent(in) :: atoms

type(pw_pseudovelocity) :: pseudovelocity
complex :: scaled_velocity(3)
type(pw_wfc), pointer :: wfc_val, wfc_con
type(pw_basis), pointer :: basis_val, basis_con

real :: deltae
complex :: epsilon_weight
complex :: scale
logical :: lflag
integer :: iq, ik
integer :: ib1,ib2
real :: k(3)
integer :: dim(3)
integer :: itrans1,itrans2
!----------------------------------------
! variables controling parall flux
integer :: owner, location, iboh
!-----------------------------------------
integer :: iomega
real :: identity(3,3)
complex :: macro(3,3)

  identity(:,:)=0.0
  identity(1,1)=1.0
  identity(2,2)=1.0
  identity(3,3)=1.0

  iboh=1

  do itrans1=1,bse%ntrans
    owner = bse%matrix%where_line(1,itrans1)
    location = bse%matrix%where_line(2,itrans1)
    if(owner==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then

      do itrans2=1,bse%ntrans
        ik=bse%iktrans(itrans2)
        k = states%kmesh%kibz(:,ik)
        call pw_states_borrow_basis(states,basis_con,ik)
        call pw_pseudovelocity_init(pseudovelocity,basis_con,atoms)
        scaled_velocity = 0.0
        epsilon_weight = 0.0

! occupied states
        ib2=bse%ib2trans(itrans2)
        call pw_states_borrow_wfc(states,wfc_val,ib2,ik)

! unoccupied states
        ib1=bse%ib1trans(itrans2)
        call pw_states_borrow_wfc(states,wfc_con,ib1,ik)

        if(owner==bse%matrix%rank) epsilon_weight = bse%matrix%val(location,itrans2)

! pw_dipole compute FFT(conjg(field_val)*field_con)
        deltae = states%e(ib1,ik) - states%e(ib2,ik) + epsilon%energy_shift
        if(owner==bse%matrix%rank) scaled_velocity = (pw_wfc_p_braket(wfc_val,wfc_con) &
                             +pw_pseudovelocity_braket(pseudovelocity,wfc_val,wfc_con))

        call pw_states_giveback_wfc(states,wfc_con)

        do iomega=0,epsilon%nomega

          if(epsilon%imaginary_axis) then
             scale = 2.0*(1.0/((0.0,1.0)*epsilon%omega(iomega)-bse%matrix%eigenval(itrans1)+(0.0,1.0)*epsilon%broadening)- &
                        1.0/((0.0,1.0)*epsilon%omega(iomega)+bse%matrix%eigenval(itrans1)+(0.0,1.0)*epsilon%broadening)) / &
                        epsilon%struct%a_omega / states%kmesh%nkibz
          else
              scale = 2.0*(1.0/(epsilon%omega(iomega)-bse%matrix%eigenval(itrans1)+(0.0,1.0)*epsilon%broadening)- &
                         1.0/(epsilon%omega(iomega)+bse%matrix%eigenval(itrans1)+(0.0,1.0)*epsilon%broadening)) / &
                         epsilon%struct%a_omega / states%kmesh%nkibz
          endif

          macro(:,:)=0.0
          call pw_epsilon_macro_add_dipole(macro,deltae,scale,epsilon_weight,1.0, &
                  epsilon%iqgamma,scaled_velocity,epsilon%comm)
          epsilon%macroscopic(:,:,iomega) = epsilon%macroscopic(:,:,iomega) + macro(:,:)
        enddo

        call pw_states_giveback_wfc(states,wfc_val)
        call pw_pseudovelocity_destroy(pseudovelocity)
        call pw_states_giveback_basis(states,basis_con)
      enddo
      if(bse%matrix%incomplete) iboh=iboh+1
    endif
  enddo

  do iomega=0,epsilon%nomega
    epsilon%macroscopic(:,:,iomega)=identity(:,:)/num_8pi-epsilon%macroscopic(:,:,iomega)
  enddo

end subroutine pw_epsilon_macro_rpaexc_calc_x

subroutine pw_epsilon_macro_project_on_q_x(epsilon,q)
use tools_module
use pw_epsilon_macro_type
use pw_epsilon_macro_interf 
use num_module

implicit none

type(pw_epsilon_macro), intent(inout) :: epsilon
integer, intent(in) :: q(3)

complex :: epsq(3)
integer :: iomega
complex :: epsilon_tmp(3,3)
real :: modq
real :: qxyz(3), tmpq(3)

modq=sqrt(sum(num_matmul(real(q),epsilon%struct%a)**2))
qxyz = num_matmul(real(q),epsilon%struct%a)

tmpq(:)=qxyz(:)/modq


do iomega=0,epsilon%nomega
  epsq(:)=0.0
  epsilon_tmp(:,:)=epsilon%macroscopic(:,:,iomega)
  epsq = num_matmul(epsilon_tmp,cmplx(tmpq))
  epsilon%macroscopic(1,1,iomega)=cmplx(tmpq(1))*epsq(1)+cmplx(tmpq(2))*epsq(2)+cmplx(tmpq(3))*epsq(3)
  epsilon%macroscopic(1,2,iomega)=0.0
  epsilon%macroscopic(1,3,iomega)=0.0
  epsilon%macroscopic(2,1,iomega)=0.0
  epsilon%macroscopic(2,2,iomega)=0.0
  epsilon%macroscopic(2,3,iomega)=0.0
  epsilon%macroscopic(3,1,iomega)=0.0
  epsilon%macroscopic(3,2,iomega)=0.0
  epsilon%macroscopic(3,3,iomega)=0.0
enddo

end subroutine pw_epsilon_macro_project_on_q_x

subroutine pw_epsilon_macro_spherical_mean_x(epsilon)
use tools_module
use pw_epsilon_macro_type
use pw_epsilon_macro_interf 
use num_module

implicit none

type(pw_epsilon_macro), intent(inout) :: epsilon

integer :: iomega

do iomega=0,epsilon%nomega
  epsilon%macroscopic(1,1,iomega)=num_trace(epsilon%macroscopic(:,:,iomega))/3.0
!  epsilon%macroscopic(1,1,iomega)=1.0/(num_trace(num_inverse_tensor(epsilon%macroscopic(:,:,iomega)))/3.0)
  epsilon%macroscopic(1,2,iomega)=0.0
  epsilon%macroscopic(1,3,iomega)=0.0
  epsilon%macroscopic(2,1,iomega)=0.0
  epsilon%macroscopic(2,2,iomega)=0.0
  epsilon%macroscopic(2,3,iomega)=0.0
  epsilon%macroscopic(3,1,iomega)=0.0
  epsilon%macroscopic(3,2,iomega)=0.0
  epsilon%macroscopic(3,3,iomega)=0.0
enddo

end subroutine pw_epsilon_macro_spherical_mean_x

subroutine pw_epsilon_macro_add_dipole_rx(macro,deltae,scale,weight,switch,iq,velocity,comm)
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_comm, ptk_sum
  use tools_module
  use num_module
  implicit none
  complex,         intent(inout) :: macro(3,3)
  real,            intent(in)    :: deltae, weight, switch
  complex,         intent(in)    :: scale
  integer,         intent(in)    :: iq
  complex,         intent(in)    :: velocity(3)
  type(ptk_comm), intent(in)     :: comm

  complex :: scaled_velocity_loc(3)

  integer :: i

  scaled_velocity_loc = 0.0
  scaled_velocity_loc = velocity / deltae*weight*switch
  do i = 1 , 3
    macro(i,:) = scale * conjg(scaled_velocity_loc(i)) * scaled_velocity_loc(:)
  enddo

  if(switch<0.01) macro = 0.0

  call ptk_allreduce_inplace(macro,ptk_sum,comm)

end subroutine pw_epsilon_macro_add_dipole_rx

subroutine pw_epsilon_macro_add_dipole_cx(macro,deltae,scale,weight,switch,iq,velocity,comm)
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_allreduce_inplace,ptk_comm, ptk_sum
  use tools_module
  use num_module
  implicit none
  complex,         intent(inout) :: macro(3,3)
  real,            intent(in)    :: deltae, switch
  complex,         intent(in)    :: scale, weight
  integer,         intent(in)    :: iq
  complex,         intent(in)    :: velocity(3)
  type(ptk_comm), intent(in)     :: comm

  complex :: scaled_velocity_loc(3)

  integer :: i

  scaled_velocity_loc = 0.0
  scaled_velocity_loc = velocity / deltae*weight*switch
  do i = 1 , 3
    macro(i,:) = scale * conjg(scaled_velocity_loc(i)) * scaled_velocity_loc(:)
  enddo

  if(switch<0.01) macro = 0.0

  call ptk_allreduce_inplace(macro,ptk_sum,comm)

end subroutine pw_epsilon_macro_add_dipole_cx

subroutine pw_epsilon_macro_write_x(epsilon,unit,name,kindout,fmt)
  use pw_epsilon_macro_type
  use pw_epsilon_macro_interf
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
  type(pw_epsilon_macro),  intent(in) :: epsilon
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
  character(*), optional, intent(in) :: kindout
  character(*), optional, intent(in) :: fmt

  type(pw_basis), pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: iomega
  real, allocatable :: epsilon_tmp(:) 
  character(iotk_attlenx) :: attr
  character(20) :: fmt_local
  character(20) :: kind_local

  integer :: i,j

  if(present(kindout)) kind_local = kindout
  if(.not.present(kindout)) kind_local = "full"

  if(present(fmt)) fmt_local = fmt
  if(.not.present(fmt)) fmt_local = "iotk"

  select case(kind_local)
  case("full")
    if(fmt_local=="iotk") then
      call iotk_open_write(unit,file=trim(name),binary=.false.)
      call iotk_write_attr(attr,"type","pw_epsilon",first=.true.)
      call iotk_write_begin(unit,"epsilon",attr)
  
      call iotk_write_attr(attr,"nomega",epsilon%nomega,first=.true.)
      call iotk_write_empty(unit,"info",attr)

    do iomega=0,epsilon%nomega
      call iotk_write_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_write_dat(unit,"macro1",epsilon%macroscopic(1,:,iomega))
      call iotk_write_dat(unit,"macro2",epsilon%macroscopic(2,:,iomega))
      call iotk_write_dat(unit,"macro3",epsilon%macroscopic(3,:,iomega))
      call iotk_write_end(unit,"omega"//trim(iotk_index(iomega)))
    end do

    call iotk_write_end(unit,"epsilon")
    call iotk_close_write(unit)

  elseif(fmt_local=="txt") then
    open(unit,file=trim(name),status="unknown")
    do iomega=0,epsilon%nomega
      do i=1,3
         write(unit,*) epsilon%omega(iomega), (epsilon%macroscopic(i,j,iomega)*num_8pi, j=1,3)
      enddo
    enddo
    close(unit)
  else
    ERROR("wrong format on epsilon_macro_write")
  endif
  case("eels")
    allocate(epsilon_tmp(0:epsilon%nomega))
    epsilon_tmp(:)=0.0
    epsilon_tmp(:)=aimag(epsilon%macroscopic(1,1,:))/ &
        (real(epsilon%macroscopic(1,1,:))**2+aimag(epsilon%macroscopic(1,1,:))**2)
    if(fmt_local=="iotk") then
      call iotk_open_write(unit,file=trim(name),binary=.false.)
      call iotk_write_attr(attr,"type","pw_epsilon",first=.true.)
      call iotk_write_begin(unit,trim(name),attr)
  
      call iotk_write_attr(attr,"nomega",epsilon%nomega,first=.true.)
      call iotk_write_empty(unit,"info",attr)

    do iomega=0,epsilon%nomega
      call iotk_write_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_write_dat(unit,"eels",epsilon_tmp(iomega))
      call iotk_write_end(unit,"omega"//trim(iotk_index(iomega)))
    end do

    call iotk_write_end(unit,name)
    call iotk_close_write(unit)

  elseif(fmt_local=="txt") then
    open(unit,file=trim(name),status="unknown")
    do iomega=0,epsilon%nomega
       write(unit,*) epsilon%omega(iomega), epsilon_tmp(iomega)*num_8pi
    enddo
    close(unit)
  else
    ERROR("wrong format on epsilon_macro_write")
  endif
    deallocate(epsilon_tmp)
  case("real")
    if(fmt_local=="iotk") then
      call iotk_open_write(unit,file=trim(name),binary=.false.)
      call iotk_write_attr(attr,"type","pw_epsilon",first=.true.)
      call iotk_write_begin(unit,trim(name),attr)
  
      call iotk_write_attr(attr,"nomega",epsilon%nomega,first=.true.)
      call iotk_write_empty(unit,"info",attr)

    do iomega=0,epsilon%nomega
      call iotk_write_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_write_dat(unit,"macro1",real(epsilon%macroscopic(1,:,iomega)))
      call iotk_write_dat(unit,"macro2",real(epsilon%macroscopic(2,:,iomega)))
      call iotk_write_dat(unit,"macro3",real(epsilon%macroscopic(3,:,iomega)))
      call iotk_write_end(unit,"omega"//trim(iotk_index(iomega)))
    end do

    call iotk_write_end(unit,name)
    call iotk_close_write(unit)

  elseif(fmt_local=="txt") then
    open(unit,file=trim(name),status="unknown")
    do iomega=0,epsilon%nomega
       write(unit,*) epsilon%omega(iomega), real(epsilon%macroscopic(1,1,iomega))*num_8pi
    enddo
    close(unit)
  else
    ERROR("wrong format on epsilon_macro_write")
  endif
  case("imag")
    if(fmt_local=="iotk") then
      call iotk_open_write(unit,file=trim(name),binary=.false.)
      call iotk_write_attr(attr,"type","pw_epsilon",first=.true.)
      call iotk_write_begin(unit,trim(name),attr)
  
      call iotk_write_attr(attr,"nomega",epsilon%nomega,first=.true.)
      call iotk_write_empty(unit,"info",attr)

    do iomega=0,epsilon%nomega
      call iotk_write_begin(unit,"omega"//trim(iotk_index(iomega)))
      call iotk_write_dat(unit,"macro1",imag(epsilon%macroscopic(1,:,iomega)))
      call iotk_write_dat(unit,"macro2",imag(epsilon%macroscopic(2,:,iomega)))
      call iotk_write_dat(unit,"macro3",imag(epsilon%macroscopic(3,:,iomega)))
      call iotk_write_end(unit,"omega"//trim(iotk_index(iomega)))
    end do

    call iotk_write_end(unit,name)
    call iotk_close_write(unit)

  elseif(fmt_local=="txt") then
    open(unit,file=trim(name),status="unknown")
    do iomega=0,epsilon%nomega
       write(unit,*) epsilon%omega(iomega), aimag(epsilon%macroscopic(1,1,iomega))*num_8pi
    enddo
    close(unit)
  else
    ERROR("wrong format on epsilon_macro_write")
  endif
  end select

end subroutine pw_epsilon_macro_write_x

subroutine pw_epsilon_macro_read_x(epsilon,unit,name,fmt)
  use pw_epsilon_macro_type
  use pw_epsilon_macro_interf
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_common_module
  use ptk_module, only : ptk_bcast
  use tools_module
  use num_module
  use iotk_module
  implicit none
  type(pw_epsilon_macro),  intent(inout) :: epsilon
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
  character(*), optional, intent(in) :: fmt

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  integer :: iomega,nomega,nq

  character(20) :: fmt_local

  integer :: i,j

  if(present(fmt)) fmt_local = fmt
  if(.not.present(fmt)) fmt_local = "iotk"

  select case(fmt_local)
  case("iotk")
    call iotk_open_read(unit,file=trim(name),binary=.false.)
    call iotk_scan_begin(unit,"epsilon",attr)
    call iotk_scan_attr(attr,"type",rtype,default="pw_epsilon")
    if(rtype/="pw_epsilon") ERROR("")

    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr(attr,"nomega",nomega)
    if(nomega/=epsilon%nomega) ERROR("")

    do iomega = 0,epsilon%nomega
      call iotk_scan_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_scan_dat(unit,"macro1",epsilon%macroscopic(1,:,iomega))
      call iotk_scan_dat(unit,"macro2",epsilon%macroscopic(2,:,iomega))
      call iotk_scan_dat(unit,"macro3",epsilon%macroscopic(3,:,iomega))
      call iotk_scan_end(unit,"omega"//trim(iotk_index(iomega)))
    end do

    call iotk_scan_end(unit,"epsilon")
    call iotk_close_read(unit)
  case("txt")
    open(unit,file=trim(name),status="old")
    do iomega=0,epsilon%nomega
       do i=1,3
          read(unit,*) epsilon%omega(iomega), epsilon%macroscopic(i,1,iomega), &
             epsilon%macroscopic(i,2,iomega), epsilon%macroscopic(i,3,iomega)
       enddo
    enddo
    close(unit) 
    epsilon%macroscopic(:,:,:)=epsilon%macroscopic(:,:,:)/num_8pi 
  end select

end subroutine pw_epsilon_macro_read_x

subroutine pw_epsilon_macro_readbcast_x(epsilon,unit,name,root,comm,fmt)
  use pw_epsilon_macro_type
  use pw_epsilon_macro_interf
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_bcast, ptk_comm_rank, ptk_comm
  use tools_module
  use num_module
  use iotk_module
  implicit none
  type(pw_epsilon_macro),  intent(inout) :: epsilon
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name
  integer, intent(in) :: root
  type(ptk_comm), intent(in) :: comm
  character(*), optional, intent(in) :: fmt

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  integer :: iomega,nomega

  integer :: rank

  call ptk_comm_rank(comm,rank)

  if(rank==root) then
    call pw_epsilon_macro_read(epsilon,unit,name,fmt) 
  endif
  call ptk_bcast(epsilon%macroscopic(:,:,:),epsilon%root,epsilon%comm)

end subroutine pw_epsilon_macro_readbcast_x


subroutine pw_epsilon_macro_destroy_x(epsilon)
  use pw_epsilon_macro_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_common_module
  use tools_module
  use num_module
  implicit none
  type (pw_epsilon_macro), intent(inout) :: epsilon
  nullify(epsilon%struct)
  nullify(epsilon%symmlist)
  nullify(epsilon%qmesh)
  deallocate(epsilon%macroscopic)
  deallocate(epsilon%omega)
  deallocate(epsilon%weight)
end subroutine pw_epsilon_macro_destroy_x


