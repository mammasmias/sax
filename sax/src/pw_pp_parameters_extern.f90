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

subroutine pw_pp_parameters_init_x(pp_parameters,plasmon_energy,broadening,struct,symmlist,qmesh,cutoff,root,comm)
  use pw_pp_parameters_type
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_comm
  use tools_module
  use num_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type (pw_pp_parameters), intent(out) :: pp_parameters
  real,                       intent(in) :: plasmon_energy
  real,                       intent(in) :: broadening
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
  real,                       intent(in)  :: cutoff
  integer,                    intent(in)  :: root
  type (ptk_comm),            intent(in)  :: comm
!@ END MANUAL
  integer iq,iomega

  pp_parameters%struct   => struct
  pp_parameters%symmlist => symmlist
  pp_parameters%qmesh    => qmesh
  pp_parameters%cutoff   =  cutoff
  pp_parameters%root     =  root
  pp_parameters%comm     =  comm

  pp_parameters%plasmon_energy = plasmon_energy
  pp_parameters%broadening = broadening

!-----------------------
! test new new
!  pp_parameters%discontinuity_parameters(:,:)=0.0
! ---------------------

  call ptk_comm_size(comm,pp_parameters%npe)
  call ptk_comm_rank(comm,pp_parameters%rank)
  if(pp_parameters%root >= pp_parameters%npe) ERROR("")

  allocate(pp_parameters%where_q(2,pp_parameters%qmesh%nkibz))
  call pw_allocate(pp_parameters%where_q)
  pp_parameters%where_q = 0
  do iq=1,qmesh%nkibz
    pp_parameters%where_q(1,iq) = modulo(iq-pp_parameters%root,pp_parameters%npe)
    pp_parameters%where_q(2,iq) = count(pp_parameters%where_q(1,1:iq)==pp_parameters%where_q(1,iq))
  end do


  allocate(pp_parameters%basis(qmesh%nkibz))
  call pw_basis_init(pp_parameters%basis(:),pp_parameters%struct)
  call pw_basis_create(pp_parameters%basis,pp_parameters%qmesh,pp_parameters%cutoff)

  allocate(pp_parameters%val(count(pp_parameters%where_q(1,:)==pp_parameters%rank),0:1))
  do iomega=0,1
    do iq=1,qmesh%nkibz
      if(pp_parameters%where_q(1,iq)==pp_parameters%rank) &
       call pw_wfc6d_init(pp_parameters%val(pp_parameters%where_q(2,iq),iomega),pp_parameters%basis(iq))
    end do
  end do

  pp_parameters%iqgamma = pw_kmesh_kibz_index(pp_parameters%qmesh,(/0.0,0.0,0.0/))

end subroutine pw_pp_parameters_init_x

subroutine pw_pp_parameters_borrow_basis_x(pp_parameters,basis,iq)
  use pw_w_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use pw_pp_parameters_type
  implicit none
  type(pw_pp_parameters),                        intent(in)  :: pp_parameters
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
  if(present(basis) .neqv. present(iq)) ERROR("")
  if(present(basis)) basis => pp_parameters%basis(iq)
end subroutine pw_pp_parameters_borrow_basis_x

subroutine pw_pp_parameters_giveback_basis_x(pp_parameters,basis)
  use pw_w_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use pw_pp_parameters_type
  implicit none
  type(pw_pp_parameters),              intent(in)    :: pp_parameters
  type(pw_basis), pointer :: basis
  if(.not.associated(pp_parameters%basis)) ERROR("")
  nullify(basis)
end  subroutine pw_pp_parameters_giveback_basis_x

subroutine pw_pp_parameters_write_x(pp_parameters,unit,name)
  use pw_pp_parameters_type
  use pw_pp_parameters_interf
  use iotk_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use ptk_module, only : ptk_barrier
  implicit none
  type(pw_pp_parameters),  intent(in) :: pp_parameters
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  type(pw_basis),pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq
  character(iotk_attlenx) :: attr

  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_write_attr(attr,"type","pw_pp_parameters",first=.true.)
    call iotk_write_begin(unit,trim(name),attr)

    call iotk_write_attr(attr,"nq",    pp_parameters%qmesh%nkbz)
    call iotk_write_empty(unit,"info",attr)
  end if

!-----------------------
! test new new
!-----------------------
!  if(pp_parameters%rank==pp_parameters%root) then
!    call iotk_write_dat(unit,"discontinuity_parameters",pp_parameters%discontinuity_parameters(:,:))
!  endif
!-----------------------

  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_write_begin(unit,"pp_energies")
  end if

  do iq=1,pp_parameters%qmesh%nkibz
    owner = pp_parameters%where_q(1,iq)
    location = pp_parameters%where_q(2,iq)
    if(pp_parameters%rank == pp_parameters%root .and. pp_parameters%rank == owner) then
      call pw_wfc6d_write(pp_parameters%val(location,0),unit,"iotk",name="val"//trim(iotk_index(iq)))
    end if
    if(pp_parameters%rank == pp_parameters%root .and. pp_parameters%rank /= owner) then
      call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq)
      call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
      call pw_wfc6d_recv(val_tmp,owner,iq,pp_parameters%comm)
      call pw_wfc6d_write(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
      call pw_wfc6d_destroy(val_tmp)
    else
      call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq)
! with this call all proc do the same number of calls to parall routines
! --> agains dead locks      
    end if
    if(pp_parameters%rank /= pp_parameters%root .and. pp_parameters%rank == owner) then
      call pw_wfc6d_send(pp_parameters%val(location,0),pp_parameters%root,iq,pp_parameters%comm)
    end if
    call pw_pp_parameters_giveback_basis(pp_parameters,basis_tmp)
  end do
  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_write_end(unit,"pp_energies")
  end if

  call ptk_barrier(pp_parameters%comm)

  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_write_begin(unit,"pp_weights")
  end if

  do iq=1,pp_parameters%qmesh%nkibz
    owner = pp_parameters%where_q(1,iq)
    location = pp_parameters%where_q(2,iq)
    if(pp_parameters%rank == pp_parameters%root .and. pp_parameters%rank == owner) then
      call pw_wfc6d_write(pp_parameters%val(location,1),unit,"iotk",name="val"//trim(iotk_index(iq)))
    end if
    if(pp_parameters%rank == pp_parameters%root .and. pp_parameters%rank /= owner) then
      call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq)
      call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
      call pw_wfc6d_recv(val_tmp,owner,iq,pp_parameters%comm)
      call pw_wfc6d_write(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
      call pw_wfc6d_destroy(val_tmp)
    else
      call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq) 
! with this call all proc do the same number of calls to parall routines
! --> agains dead locks 
    end if
    if(pp_parameters%rank /= pp_parameters%root .and. pp_parameters%rank == owner) then
      call pw_wfc6d_send(pp_parameters%val(location,1),pp_parameters%root,iq,pp_parameters%comm)
    end if
    call pw_pp_parameters_giveback_basis(pp_parameters,basis_tmp)
  end do
  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_write_end(unit,"pp_weights")
  end if

  call ptk_barrier(pp_parameters%comm)

  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_write_end(unit,name)
  end if

end subroutine pw_pp_parameters_write_x

subroutine pw_pp_parameters_destroy_x(pp_parameters)
  use pw_pp_parameters_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type (pw_pp_parameters), intent(inout) :: pp_parameters
  nullify(pp_parameters%struct)
  nullify(pp_parameters%symmlist)
  nullify(pp_parameters%qmesh)
  call pw_deallocate(pp_parameters%where_q)
  deallocate(pp_parameters%where_q)
  if(size(pp_parameters%val)>0) call pw_wfc6d_destroy(pp_parameters%val(:,:))
  deallocate(pp_parameters%val)
  call pw_basis_destroy(pp_parameters%basis(:))
  deallocate(pp_parameters%basis)
end subroutine pw_pp_parameters_destroy_x

subroutine pw_pp_parameters_calc_x3(pp_parameters,w,coulomb_div_treatment,ecutvcut)
  use pw_w_type
  use pw_w_interf
  use pw_pp_parameters_type
  use pw_pp_parameters_interf
  use num_module
  use pw_basis_module
  use pw_wfc_module
  use ptk_module, only : ptk_bcast
  use pw_coulomb_module
  use tools_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use coulomb_vcut_module
  use iotk_module
  implicit none
  type (pw_pp_parameters), intent(inout) :: pp_parameters
  type (pw_w), intent(inout) :: w
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
  integer :: iq,location,i,j
  type (pw_basis),pointer :: basis_tmp
  type (pw_wfc)     :: coulomb_wfc
  type (pw_coulomb) :: coulomb
  integer :: iomega,ipw1,ipw2,npw
  complex :: epsilon_0_q, epsilon_wp_q
  complex :: cccccc(3,3), inverse(3,3), identity(3,3)
  real :: lattice(3,3)
  character(len=30) :: coulomb_div_treatment_loc
  real :: ecutvcut_loc
  integer :: system_type
  type(vcut_type) :: vcut

  real :: cocobongo,emin,emax,deltae
  integer :: ie,nbe

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = coulomb_div_treatment

  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut


  identity(:,:)=0.0
  identity(1,1)=1.0
  identity(2,2)=1.0
  identity(3,3)=1.0

  lattice = num_matmul(pp_parameters%struct%b,pp_parameters%qmesh%m_inv)

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    call pw_vcut_init(vcut,w%struct,w%qmesh,ecutvcut_loc)
    call pw_coulomb_init(coulomb,lattice,vcut)
  else
    call pw_coulomb_init(coulomb,lattice)
  endif

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    system_type=1
  elseif(trim(coulomb_div_treatment_loc)=="vcut_spherical") then
    system_type=0
  else
    system_type=3
  endif

!  call pw_coulomb_init(coulomb,lattice)

  do iq=1,pp_parameters%qmesh%nkibz
  if(pp_parameters%where_q(1,iq)/=pp_parameters%rank) cycle
    location = pp_parameters%where_q(2,iq)
    call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq)
    call pw_wfc_init(coulomb_wfc,basis_tmp)
!    call pw_coulomb_get(3,coulomb,coulomb_wfc,linverse=.true.)

    if(system_type/=3) then
      call pw_coulomb_get(system_type,coulomb,coulomb_wfc,linverse=.true.,lhalf=.true.)
    else
      call pw_coulomb_get(system_type,coulomb,coulomb_wfc,linverse=.true.)
    endif

    npw=coulomb_wfc%npw

! calculation of plasmon-pole energies pp_parameters%val(location,0)
! and plasmon pole weights**2 pp_parameters%val(location,1)
! faut verifier la valeur des ailes!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ATENTION il manque un check pour (epsilon_wp_q-epsilon_0_q)<<
! je sais pas s'il suffit un warning ou s'il faut changer qqchose
    if(iq==pp_parameters%iqgamma) then
! -------------------------------------------------------
! the wings
      pp_parameters%val(location,0)%val(1,:)=2.0
      pp_parameters%val(location,0)%val(:,1)=2.0
! wings set to zero to be coherent with W (wings of W set to zero)
! W --> q used in an integral over q integral(W*f(q))=0
      pp_parameters%val(location,1)%val(1,:)=0.0
      pp_parameters%val(location,1)%val(:,1)=0.0

! the heads are set to ZERO
      pp_parameters%val(location,0)%val(1,1)=2.0
      pp_parameters%val(location,1)%val(1,1)=0.0

!---------------------------------------------------------
! the heads original version sax-1.0.0
!----------------------------------------------------------
!       cccccc(:,:)=identity(:,:)
!       call num_inverse_complex_matrix(3,w%macroscopic(:,:,0),cccccc(:,:))
!       inverse = transpose(cccccc)/num_8pi
!       epsilon_0_q=(inverse(1,1)+inverse(2,2)+inverse(3,3))/3.0 - 1.0
!       write(0,*)"epsilon-1-1 for w=0, q-->0 in the (1,0,0) dir", inverse(1,1), epsilon_0_q
!       cccccc(:,:)=identity(:,:)
!       call num_inverse_complex_matrix(3,w%macroscopic(:,:,1),cccccc(:,:))
!       inverse = transpose(cccccc)/num_8pi
!       epsilon_wp_q=(inverse(1,1)+inverse(2,2)+inverse(3,3))/3.0 - 1.0
!       write(0,*)"epsilon-1-1 for w=wp, q-->0 in the (1,0,0) dir", inverse(1,1), epsilon_wp_q

!       if(real((epsilon_wp_q)/(epsilon_0_q-epsilon_wp_q)).lt.0.0) then
!         pp_parameters%val(location,0)%val(1,1)=10.0
!         pp_parameters%val(location,1)%val(1,1)=1.0
!       else
!         pp_parameters%val(location,0)%val(1,1)=pp_parameters%plasmon_energy*sqrt(real((epsilon_wp_q)/(epsilon_0_q-epsilon_wp_q)))
!       pp_parameters%val(location,1)%val(1,1)=pp_parameters%val(location,0)%val(1,1)**2*&
!            (-epsilon_0_q)
!      endif

! -------------------------------------------------------------
!  I choose test new for integratind plasmon-pole with vcut
! -------------------------------------------------------------
        if(system_type==3) then
          epsilon_0_q=real(w%val(location,0)%val(1,1))/num_discontinuity_value(lattice,w%cutoff)
          epsilon_wp_q=real(w%val(location,1)%val(1,1))/num_discontinuity_value(lattice,w%cutoff)
       elseif(system_type==1) then
          epsilon_0_q=real(w%val(location,0)%val(1,1))/vcut%corrected(0,0,0)
          epsilon_wp_q=real(w%val(location,1)%val(1,1))/vcut%corrected(0,0,0)
       elseif(system_type==0) then
          epsilon_0_q=real(w%val(location,0)%val(1,1))/num_v0_vcut_spheric_value(lattice)
          epsilon_wp_q=real(w%val(location,1)%val(1,1))/num_v0_vcut_spheric_value(lattice)
       else
         ERROR("wrong system type")
       endif

       if(real((epsilon_wp_q)/(epsilon_0_q-epsilon_wp_q)).lt.0.0) then
         pp_parameters%val(location,0)%val(1,1)=10.0
!         pp_parameters%val(location,1)%val(1,1)=pp_parameters%val(location,0)%val(1,1)**2*&
!            (-epsilon_0_q)

         pp_parameters%val(location,1)%val(1,1)=0.0
       else
         pp_parameters%val(location,0)%val(1,1)=pp_parameters%plasmon_energy*sqrt(real((epsilon_wp_q)/(epsilon_0_q-epsilon_wp_q)))
       pp_parameters%val(location,1)%val(1,1)=pp_parameters%val(location,0)%val(1,1)**2*&
            (-epsilon_0_q)
      endif
! --------------------------------------------------------------
! end
!--------------------------------------------------------------
      if(system_type/=3) then
      do ipw1=2,npw
         do ipw2=2,npw
            epsilon_0_q=coulomb_wfc%val(ipw1) * real(w%val(location,0)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            epsilon_wp_q=coulomb_wfc%val(ipw1) * real(w%val(location,1)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
! just the positive solutions are taken into account ieta+sqrt()
            if(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)).lt.0.0) then
              pp_parameters%val(location,0)%val(ipw1,ipw2)=10.0

!           pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
!               epsilon_0_q

              pp_parameters%val(location,1)%val(ipw1,ipw2)=0.0
            else 
              pp_parameters%val(location,0)%val(ipw1,ipw2)=pp_parameters%plasmon_energy*&
                 sqrt(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)))
            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
               epsilon_0_q
            endif
         enddo
      enddo
      else
      do ipw1=2,npw
         do ipw2=2,npw
            epsilon_0_q=real(w%val(location,0)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            epsilon_wp_q=real(w%val(location,1)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
! just the positive solutions are taken into account ieta+sqrt()
            if(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)).lt.0.0) then
              pp_parameters%val(location,0)%val(ipw1,ipw2)=10.0

!            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
!               epsilon_0_q

              pp_parameters%val(location,1)%val(ipw1,ipw2)=0.0
            else 
              pp_parameters%val(location,0)%val(ipw1,ipw2)=pp_parameters%plasmon_energy*&
                 sqrt(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)))
            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
               epsilon_0_q
            endif
         enddo
      enddo
      endif ! endif system_type /=3
    else
      if(system_type/=3) then
      do ipw1=1,npw
         do ipw2=1,npw
            epsilon_0_q=coulomb_wfc%val(ipw1) * real(w%val(location,0)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            epsilon_wp_q=coulomb_wfc%val(ipw1) * real(w%val(location,1)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            if(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)).lt.0.0) then
              pp_parameters%val(location,0)%val(ipw1,ipw2)=10.0

!            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
!               epsilon_0_q

              pp_parameters%val(location,1)%val(ipw1,ipw2)=0.0
            else 
              pp_parameters%val(location,0)%val(ipw1,ipw2)=pp_parameters%plasmon_energy*&
                 sqrt(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)))
            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
               epsilon_0_q
            endif
         enddo
      enddo
      else
      do ipw1=1,npw
         do ipw2=1,npw
            epsilon_0_q=real(w%val(location,0)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            epsilon_wp_q=real(w%val(location,1)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            if(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)).lt.0.0) then
              pp_parameters%val(location,0)%val(ipw1,ipw2)=10.0

!            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
!               epsilon_0_q

              pp_parameters%val(location,1)%val(ipw1,ipw2)=0.0
            else 
              pp_parameters%val(location,0)%val(ipw1,ipw2)=pp_parameters%plasmon_energy*&
                 sqrt(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)))
            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
               epsilon_0_q
            endif
         enddo
      enddo
      endif ! endif system_type /= 3
    endif 

    call pw_pp_parameters_giveback_basis(pp_parameters,basis_tmp)
    call pw_wfc_destroy(coulomb_wfc)
  enddo 

  if(system_type==1) call vcut_destroy(vcut)

  call pw_coulomb_destroy(coulomb)

end subroutine pw_pp_parameters_calc_x3

subroutine pw_pp_parameters_calc_x0(pp_parameters,w,coulomb_div_treatment,ecutvcut)
  use pw_w_type
  use pw_w_interf
  use pw_pp_parameters_type
  use pw_pp_parameters_interf
  use num_module
  use pw_basis_module
  use pw_wfc_module
  use ptk_module, only : ptk_bcast
  use pw_coulomb_module
  use tools_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use coulomb_vcut_module
  implicit none
  type (pw_pp_parameters), intent(inout) :: pp_parameters
  type (pw_w), intent(inout) :: w
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
  integer :: iq,location,i,j
  type (pw_basis),pointer :: basis_tmp
  type (pw_wfc)     :: coulomb_wfc
  type (pw_coulomb) :: coulomb
  integer :: iomega,ipw1,ipw2,npw
  complex :: epsilon_0_q, epsilon_wp_q
  complex :: cccccc(3,3), inverse(3,3)
  real :: identity(3,3), lattice(3,3)
  character(len=30) :: coulomb_div_treatment_loc
  real :: ecutvcut_loc
  integer :: system_type
  type(vcut_type) :: vcut

  real :: cocobongo,emin,emax,deltae
  integer :: ie,nbe

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = coulomb_div_treatment
  w%coulomb_div_treatment = coulomb_div_treatment_loc

  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut
  w%ecutvcut = ecutvcut_loc


  identity(:,:)=0.0
  identity(1,1)=1.0
  identity(2,2)=1.0
  identity(3,3)=1.0

  lattice = num_matmul(pp_parameters%struct%b,pp_parameters%qmesh%m_inv)

  if(trim(w%coulomb_div_treatment)=="vcut_ws") then
    call pw_vcut_init(vcut,w%struct,w%qmesh,w%ecutvcut)
    call pw_coulomb_init(coulomb,lattice,vcut)
  else
    call pw_coulomb_init(coulomb,lattice)
  endif

  if(trim(w%coulomb_div_treatment)=="vcut_ws") then
    system_type=1
  elseif(trim(w%coulomb_div_treatment)=="vcut_spherical") then
    system_type=0
  else
    system_type=3
  endif

  do iq=1,pp_parameters%qmesh%nkibz
  if(pp_parameters%where_q(1,iq)/=pp_parameters%rank) cycle
    location = pp_parameters%where_q(2,iq)
    call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq)
    call pw_wfc_init(coulomb_wfc,basis_tmp)
    if(system_type==3) then
      call pw_coulomb_get(system_type,coulomb,coulomb_wfc,linverse=.true.)
    else
      call pw_coulomb_get(system_type,coulomb,coulomb_wfc,linverse=.true.,lhalf=.true.)
    endif
    npw=coulomb_wfc%npw
    
! calculation of plasmon-pole energies pp_parameters%val(location,0)
! and plasmon pole weights**2 pp_parameters%val(location,1)
! faut verifier la valeur des ailes!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ATENTION il manque un check pour (epsilon_wp_q-epsilon_0_q)<< 
! je sais pas s'il suffit un warning ou s'il faut changer qqchose
    if(iq==pp_parameters%iqgamma) then
! ------------------------------------------------------
! the wings
      pp_parameters%val(location,0)%val(1,:)=2.0
      pp_parameters%val(location,0)%val(:,1)=2.0
! wings set to zero to be coherent with W (wings of W set to zero)
! W --> q used in an integral over q integral(W*f(q))=0
      pp_parameters%val(location,1)%val(1,:)=0.0
      pp_parameters%val(location,1)%val(:,1)=0.0
! the heads      
         pp_parameters%val(location,0)%val(1,1)=10.0
         pp_parameters%val(location,1)%val(1,1)=0.0
! -------------------------------------------------------
      if(system_type==3) then
      do ipw1=2,npw
         do ipw2=2,npw
            epsilon_0_q=real(w%val(location,0)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            epsilon_wp_q=real(w%val(location,1)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
! just the positive solutions are taken into account ieta+sqrt() 
            if(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)).lt.0.0) then
              pp_parameters%val(location,0)%val(ipw1,ipw2)=10.0

!            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
!               epsilon_0_q

              pp_parameters%val(location,1)%val(ipw1,ipw2)=0.0
            else  
              pp_parameters%val(location,0)%val(ipw1,ipw2)=pp_parameters%plasmon_energy*&
                 sqrt(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)))
            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
               epsilon_0_q
            endif
         enddo
      enddo
      else 
      do ipw1=2,npw
         do ipw2=2,npw
            epsilon_0_q=coulomb_wfc%val(ipw1)*real(w%val(location,0)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            epsilon_wp_q=coulomb_wfc%val(ipw1)*real(w%val(location,1)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
! just the positive solutions are taken into account ieta+sqrt() 
            if(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)).lt.0.0) then
              pp_parameters%val(location,0)%val(ipw1,ipw2)=10.0

!            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
!               epsilon_0_q


              pp_parameters%val(location,1)%val(ipw1,ipw2)=0.0
            else  
              pp_parameters%val(location,0)%val(ipw1,ipw2)=pp_parameters%plasmon_energy*&
                 sqrt(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)))
            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
               epsilon_0_q
            endif
         enddo
      enddo
      endif ! endif system_type==3
    else
      if(system_type==3) then
      do ipw1=1,npw
         do ipw2=1,npw
            epsilon_0_q=real(w%val(location,0)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            epsilon_wp_q=real(w%val(location,1)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            if(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)).lt.0.0) then
              pp_parameters%val(location,0)%val(ipw1,ipw2)=10.0

!            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
!               epsilon_0_q

              pp_parameters%val(location,1)%val(ipw1,ipw2)=0.0
            else  
              pp_parameters%val(location,0)%val(ipw1,ipw2)=pp_parameters%plasmon_energy*&
                 sqrt(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)))
            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
               epsilon_0_q
            endif
         enddo
      enddo
      else
      do ipw1=1,npw
         do ipw2=1,npw
            epsilon_0_q=coulomb_wfc%val(ipw1)*real(w%val(location,0)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            epsilon_wp_q=coulomb_wfc%val(ipw1)*real(w%val(location,1)%val(ipw1,ipw2))*coulomb_wfc%val(ipw2)
            if(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)).lt.0.0) then
              pp_parameters%val(location,0)%val(ipw1,ipw2)=10.0

!            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
!               epsilon_0_q

              pp_parameters%val(location,1)%val(ipw1,ipw2)=0.0
            else  
              pp_parameters%val(location,0)%val(ipw1,ipw2)=pp_parameters%plasmon_energy*&
                 sqrt(real(-epsilon_wp_q/(epsilon_wp_q-epsilon_0_q)))
            pp_parameters%val(location,1)%val(ipw1,ipw2)=-(pp_parameters%val(location,0)%val(ipw1,ipw2)**2)*&
               epsilon_0_q
            endif
         enddo
      enddo

      endif ! endif system_type==3
    endif  

    call pw_pp_parameters_giveback_basis(pp_parameters,basis_tmp)
    call pw_wfc_destroy(coulomb_wfc)
  enddo  

  if(system_type==1) call vcut_destroy(vcut)
  call pw_coulomb_destroy(coulomb)

end subroutine pw_pp_parameters_calc_x0

subroutine pw_pp_parameters_borrow_wfc6d_x(pp_parameters,wfc6d,ik,iomega)
  use pw_pp_parameters_type
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_allgather
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_pp_parameters),                        intent(in)  :: pp_parameters
  type(pw_wfc6d), pointer, optional :: wfc6d
  integer,                 optional, intent(in)  :: ik
  integer,                 optional, intent(in)  :: iomega
  integer, allocatable :: request(:),request_iomega(:)
  integer :: ip,size,rank,ikk,where_is,ikl
  integer :: iomega_loc
  call ptk_comm_size(pp_parameters%comm,size)
  call ptk_comm_rank(pp_parameters%comm,rank)
  iomega_loc = 0
  if(present(iomega)) iomega_loc = iomega
  ikl = huge(ik)
  if(present(ik)) ikl = ik
  if(present(ik).neqv.present(wfc6d)) ERROR("")
  allocate(request(0:size-1))
  allocate(request_iomega(0:size-1))
  call ptk_allgather(ikl,request,pp_parameters%comm)
  call ptk_allgather(iomega_loc,request_iomega,pp_parameters%comm)
  if(all(request(:)==request(0)) .and. all(request_iomega==request_iomega(0))) then
    where_is = pp_parameters%where_q(1,ikl)
    if(rank==where_is) then
      wfc6d => pp_parameters%val(pp_parameters%where_q(2,ikl),iomega_loc)
    else
        allocate(wfc6d)
        call pw_wfc6d_init(wfc6d,pp_parameters%basis(ik))
    end if
    call pw_wfc6d_bcast(wfc6d,where_is,pp_parameters%comm)
  else
    do ip=0,size-1
      ikk = request(ip)
      where_is = pp_parameters%where_q(1,ikk)
      if(present(ik)) then
        if(rank==ip .and. rank==where_is) then
          if(ikk/=ik) ERROR("")
          wfc6d => pp_parameters%val(pp_parameters%where_q(2,ikk),request_iomega(ip))
        end if
        if(rank==ip .and. rank/=where_is) then
          if(ikk/=ik) ERROR("")
          allocate(wfc6d)
          call pw_wfc6d_init(wfc6d,pp_parameters%basis(ik))
          call pw_wfc6d_recv(wfc6d,where_is,ikk,pp_parameters%comm)
        end if
      end if
      if(rank/=ip .and. rank==where_is) then
        call pw_wfc6d_send(pp_parameters%val(pp_parameters%where_q(2,ikk),request_iomega(ip)),ip,ikk,pp_parameters%comm)
      end if
    end do
  end if
  deallocate(request)
  deallocate(request_iomega)
end subroutine pw_pp_parameters_borrow_wfc6d_x

subroutine pw_pp_parameters_giveback_wfc6d_x(pp_parameters,wfc6d)
  use pw_pp_parameters_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type (pw_pp_parameters),              intent(in)    :: pp_parameters
  type (pw_wfc6d), pointer :: wfc6d
  integer :: ik
  logical :: found
  integer :: iomega
  found = .false.
  do ik=lbound(pp_parameters%val,1),ubound(pp_parameters%val,1)
  do iomega=0,1
    if(associated(wfc6d,pp_parameters%val(ik,iomega))) found = .true.
    if(found) exit
  end do
  end do
  if(found) then
    nullify(wfc6d)
  else
    call pw_wfc6d_destroy(wfc6d)
    deallocate(wfc6d)
  end if
end subroutine pw_pp_parameters_giveback_wfc6d_x

subroutine pw_pp_parameters_read_x(pp_parameters,unit,name)
  use pw_pp_parameters_type
  use pw_pp_parameters_interf
  use ptk_module, only : ptk_bcast, ptk_barrier
  use iotk_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_pp_parameters),  intent(inout) :: pp_parameters
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  type(pw_basis), pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq
  integer :: nq

  if(pp_parameters%rank == pp_parameters%root) then
!    write(0,*) "before begin"
    call iotk_scan_begin(unit,trim(name),attr)
    call iotk_scan_attr(attr,"type",rtype,default="pw_pp_parameters")
    if(rtype/="pw_pp_parameters") ERROR("")
!    write(0,*) "after type"
    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr(attr,"nq",    nq)
    if(nq/=pp_parameters%qmesh%nkbz) ERROR("")
!    write(0,*)"after nq"
  end if

!----------------
! test new new
!----------------
!  if(pp_parameters%rank==pp_parameters%root) then
!    call iotk_scan_dat(unit,"discontinuity_parameters",pp_parameters%discontinuity_parameters(:,:))
!  endif
!  
!  call ptk_bcast(pp_parameters%discontinuity_parameters,pp_parameters%root,pp_parameters%comm)
!----------------------------- 
 
! reading plasmon pole energies
  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_scan_begin(unit,"pp_energies",attr)
  end if

  call ptk_barrier(pp_parameters%comm)

  do iq=1,pp_parameters%qmesh%nkibz
    owner = pp_parameters%where_q(1,iq)
    location = pp_parameters%where_q(2,iq)
    if(pp_parameters%rank == pp_parameters%root .and. pp_parameters%rank == owner) then
      call pw_wfc6d_read(pp_parameters%val(location,0),unit,"iotk",name="val"//trim(iotk_index(iq)))
    end if
    if(pp_parameters%rank == pp_parameters%root .and. pp_parameters%rank /= owner) then
      call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq)
      call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
      call pw_wfc6d_read(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
      call pw_wfc6d_send(val_tmp,owner,iq,pp_parameters%comm)
      call pw_wfc6d_destroy(val_tmp)
    else
      call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq) 
! with this call all proc do the same number of calls to parall routines
! --> agains dead locks 
    end if
    if(pp_parameters%rank /= pp_parameters%root .and. pp_parameters%rank == owner) then
      call pw_wfc6d_recv(pp_parameters%val(location,0),pp_parameters%root,iq,pp_parameters%comm)
    end if
    call pw_pp_parameters_giveback_basis(pp_parameters,basis_tmp)
  end do
  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_scan_end(unit,"pp_energies")
  end if

  call ptk_barrier(pp_parameters%comm)
  
! reading plasmon pole weights
  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_scan_begin(unit,"pp_weights",attr)
  end if

  call ptk_barrier(pp_parameters%comm)

  do iq=1,pp_parameters%qmesh%nkibz
    owner = pp_parameters%where_q(1,iq)
    location = pp_parameters%where_q(2,iq)
    if(pp_parameters%rank == pp_parameters%root .and. pp_parameters%rank == owner) then
      call pw_wfc6d_read(pp_parameters%val(location,1),unit,"iotk",name="val"//trim(iotk_index(iq)))
    end if
    if(pp_parameters%rank == pp_parameters%root .and. pp_parameters%rank /= owner) then
      call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq)
      call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
      call pw_wfc6d_read(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
      call pw_wfc6d_send(val_tmp,owner,iq,pp_parameters%comm)
      call pw_wfc6d_destroy(val_tmp)
    else
      call pw_pp_parameters_borrow_basis(pp_parameters,basis_tmp,iq) 
! with this call all proc do the same number of calls to parall routines
! --> agains dead locks 
    end if
    if(pp_parameters%rank /= pp_parameters%root .and. pp_parameters%rank == owner) then
      call pw_wfc6d_recv(pp_parameters%val(location,1),pp_parameters%root,iq,pp_parameters%comm)
    end if
    call pw_pp_parameters_giveback_basis(pp_parameters,basis_tmp)
  end do
  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_scan_end(unit,"pp_weights")
  end if

  call ptk_barrier(pp_parameters%comm)

  if(pp_parameters%rank == pp_parameters%root) then
    call iotk_scan_end(unit,name)
  end if
end subroutine pw_pp_parameters_read_x
