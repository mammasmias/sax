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

subroutine pw_w_init_x(w,struct,symmlist,qmesh,omegamax,nomega,cutoff,gw_integration_method,root,comm,do_not_alloc)
  use pw_w_type
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
  type (pw_w),   intent(out) :: w
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
!  real,                       intent(in)  :: cutoff,omegamax
  real,                       intent(in)  :: cutoff
  complex,                       intent(in)  :: omegamax
  logical,          optional, intent(in)  :: do_not_alloc
  character(len=*),            intent(in)  :: gw_integration_method
  integer,                    intent(in)  :: root,nomega
  type (ptk_comm),            intent(in)  :: comm
! Costruttore. Nota che e' parallelo, dunque tutti i processori appartenenti
! a comm devono chiamarlo contemporaneamente con gli stessi parametri.
!@ END MANUAL
  integer iq,iomega
  logical :: do_not_alloc_loc

  do_not_alloc_loc = .false.
  if(present(do_not_alloc)) do_not_alloc_loc = do_not_alloc
  w%val_initialized = .false.

  w%struct   => struct
  w%symmlist => symmlist
  w%qmesh    => qmesh
  w%cutoff   =  cutoff
  w%root     =  root
  w%comm     =  comm
  w%nomega   = nomega
  w%omegamax = omegamax


  call ptk_comm_size(comm,w%npe)
  call ptk_comm_rank(comm,w%rank)
  if(w%root >= w%npe) ERROR("")

  allocate(w%where_q(2,w%qmesh%nkibz))
  call pw_allocate(w%where_q)
  w%where_q = 0
  do iq=1,qmesh%nkibz
    w%where_q(1,iq) = modulo(iq-w%root,w%npe)
    w%where_q(2,iq) = count(w%where_q(1,1:iq)==w%where_q(1,iq))
  end do


  allocate(w%basis(qmesh%nkibz))
  call pw_basis_init(w%basis(:),w%struct)
  call pw_basis_create(w%basis,w%qmesh,w%cutoff)

  allocate(w%val(count(w%where_q(1,:)==w%rank),0:nomega))
  if(.not. do_not_alloc_loc) then
    do iomega=0,nomega
      do iq=1,qmesh%nkibz
        if(w%where_q(1,iq)==w%rank) &
         call pw_wfc6d_init(w%val(w%where_q(2,iq),iomega),w%basis(iq))
      end do
    end do
    w%val_initialized = .true.
  end if

  w%iqgamma = pw_kmesh_kibz_index(w%qmesh,(/0.0,0.0,0.0/))

  allocate(w%macroscopic(3,3,0:nomega))
  w%macroscopic = 0.0

  allocate(w%lambda(3,0:nomega))
  allocate(w%rho(3,0:nomega))
  call pw_wfc_init(w%lambda,w%basis(w%iqgamma))
  call pw_wfc_init(w%rho,w%basis(w%iqgamma))

  w%smooth = .false.

  w%v0 = 0.0

  allocate(w%omega(0:nomega),w%weight(0:nomega))
  if(gw_integration_method=="plasmon_pole") then
     w%weight(:)=1.0
     w%omega(0)=0.0
     w%omega(1)=omegamax
  elseif(gw_integration_method=="Farid") then
!    call num_gaussmesh(omegamax,w%omega(0:),w%weight(0:))
  elseif(gw_integration_method=="cohsex".or.gw_integration_method=="sex".or.gw_integration_method=="coh") then
     w%weight(:)=1.0
     w%omega(0)=omegamax
  endif
!!!!!!!!!!!!!! manca conditione per sshf !!!!!!!!!!!!!!!!!!!!!!!

!  if(w%nomega>=1) then
!    do iomega = 0,nomega
!      w%omega(iomega) = omegamax / real(w%nomega) * iomega
!    end do
!  else
!    w%omega(0) = 0.0
!  end if

end subroutine pw_w_init_x

subroutine pw_w_borrow_basis_x(w,basis,iq)
  use pw_w_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_w),                        intent(in)  :: w
  type(pw_basis), optional, pointer :: basis
  integer,        optional,          intent(in)  :: iq
  if(present(basis) .neqv. present(iq)) ERROR("")
  if(present(basis)) basis => w%basis(iq)
end subroutine pw_w_borrow_basis_x

subroutine pw_w_giveback_basis_x(w,basis)
  use pw_w_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_w),              intent(in)    :: w
  type(pw_basis), pointer :: basis
  if(.not.associated(w%basis)) ERROR("")
  nullify(basis)
end  subroutine pw_w_giveback_basis_x

subroutine pw_w_write_x(w,unit,name)
  use pw_w_type
  use pw_w_interf
  use iotk_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_w),  intent(in) :: w
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  type(pw_basis),pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq,iomega
  character(iotk_attlenx) :: attr

  if(w%rank == w%root) then
    call iotk_write_attr(attr,"type","pw_w",first=.true.)
    call iotk_write_begin(unit,trim(name),attr)
    call iotk_write_attr(attr,"nomega",w%nomega,first=.true.)
    call iotk_write_attr(attr,"omegamax",w%omegamax)
    call iotk_write_attr(attr,"smooth",w%smooth)
    call iotk_write_attr(attr,"nq",    w%qmesh%nkbz)
    call iotk_write_empty(unit,"info",attr)
  end if

  if(w%rank==w%root) then
    call iotk_write_dat(unit,"omega",w%omega(:))      
    call iotk_write_dat(unit,"weight",w%weight(:))      
  endif        

  do iomega = 0,w%nomega
    if(w%rank == w%root) then
      call iotk_write_begin(unit,"omega"//trim(iotk_index(iomega)))
      call iotk_write_dat(unit,"macro1",w%macroscopic(1,:,iomega))
      call iotk_write_dat(unit,"macro2",w%macroscopic(2,:,iomega))
      call iotk_write_dat(unit,"macro3",w%macroscopic(3,:,iomega))
      call iotk_write_dat(unit,"v0",w%v0)
      call pw_wfc_write(w%lambda(1,iomega),unit,"iotk",name="lambda1")
      call pw_wfc_write(w%lambda(2,iomega),unit,"iotk",name="lambda2")
      call pw_wfc_write(w%lambda(3,iomega),unit,"iotk",name="lambda3")
      call pw_wfc_write(w%rho(1,iomega),unit,"iotk",name="rho1")
      call pw_wfc_write(w%rho(2,iomega),unit,"iotk",name="rho2")
      call pw_wfc_write(w%rho(3,iomega),unit,"iotk",name="rho3")
    end if

    do iq=1,w%qmesh%nkibz
      owner = w%where_q(1,iq)
      location = w%where_q(2,iq)
      if(w%rank == w%root .and. w%rank == owner) then
        call pw_wfc6d_write(w%val(location,iomega),unit,"iotk",name="val"//trim(iotk_index(iq)))
      end if
      if(w%rank == w%root .and. w%rank /= owner) then
! OCCHIO AL BARRIER
        call pw_w_borrow_basis(w,basis_tmp,iq)
        call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
        call pw_wfc6d_recv(val_tmp,owner,iq,w%comm)
        call pw_wfc6d_write(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
        call pw_wfc6d_destroy(val_tmp)
      else
        call pw_w_borrow_basis(w,basis_tmp,iq) ! serve per il barrier
      end if
      if(w%rank /= w%root .and. w%rank == owner) then
        call pw_wfc6d_send(w%val(location,iomega),w%root,iq,w%comm)
      end if
      call pw_w_giveback_basis(w,basis_tmp)
    end do
    if(w%rank == w%root) then
      call iotk_write_end(unit,"omega"//trim(iotk_index(iomega)))
    end if
  end do

  if(w%rank == w%root) then
    call iotk_write_end(unit,name)
  end if

end subroutine pw_w_write_x

subroutine pw_w_destroy_x(w)
  use pw_w_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type (pw_w), intent(inout) :: w
  nullify(w%struct)
  nullify(w%symmlist)
  nullify(w%qmesh)
  call pw_deallocate(w%where_q)
  deallocate(w%where_q)
  if(size(w%val)>0) call pw_wfc6d_destroy(w%val(:,:))
  deallocate(w%val)
  call pw_wfc_destroy(w%lambda)
  call pw_wfc_destroy(w%rho)
  deallocate(w%lambda)
  deallocate(w%rho)
  deallocate(w%macroscopic)
  call pw_basis_destroy(w%basis(:))
  deallocate(w%basis)
  deallocate(w%omega)
  deallocate(w%weight)
end subroutine pw_w_destroy_x

subroutine pw_w_calc_x3(w,polar,dealloc_polar)
  use pw_w_type
  use pw_w_interf
  use pw_polar_module
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
  use coulomb_vcut_module, only : vcut_type
  implicit none
  type (pw_w), intent(inout) :: w
  type (pw_polar), intent(inout) :: polar
  logical, optional, intent(in) :: dealloc_polar
  real :: identity(3,3)
  integer :: iq,location,i,j,npw,ipw
  type (pw_basis),pointer :: basis_tmp
  type (pw_wfc)     :: coulomb_wfc
  type (pw_coulomb) :: coulomb
  integer :: iomega,ipw1,ipw2
  logical :: dealloc_polar_loc
  logical :: do_not_alloc_loc
  real :: lattice(3,3)

  dealloc_polar_loc = .false.
  if(present(dealloc_polar)) dealloc_polar_loc = dealloc_polar
  if(dealloc_polar_loc) then
    polar%val_initialized = .false.
  end if

  identity(1,1) = 1.0
  identity(2,1) = 0.0
  identity(3,1) = 0.0
  identity(1,2) = 0.0
  identity(2,2) = 1.0
  identity(3,2) = 0.0
  identity(1,3) = 0.0
  identity(2,3) = 0.0
  identity(3,3) = 1.0

  lattice = num_matmul(w%struct%b,w%qmesh%m_inv)

  call pw_coulomb_init(coulomb,lattice)

  do iq=1,w%qmesh%nkibz
    if(w%where_q(1,iq)/=w%rank) cycle
    location = w%where_q(2,iq)
    call pw_w_borrow_basis(w,basis_tmp,iq)
    call pw_wfc_init(coulomb_wfc,basis_tmp)
!
! Loop su omega
!
    do iomega = 0,w%nomega

      call pw_coulomb_get(3,coulomb,coulomb_wfc,linverse=.true.)

!      call pw_coulomb_get(3,coulomb,coulomb_wfc,linverse=.true.)
      if(.not. w%val_initialized) then 
        call pw_wfc6d_init(w%val(location,iomega),w%basis(iq))
      end if
      w%val(location,iomega)%val = - polar%val(location,iomega)%val
      if(dealloc_polar_loc) then
        call pw_wfc6d_destroy(polar%val(location,iomega))
      end if
      npw = coulomb_wfc%npw
      do ipw=1,npw
        w%val(location,iomega)%val(ipw,ipw) = w%val(location,iomega)%val(ipw,ipw) + coulomb_wfc%val(ipw)
      end do
      if(iq==w%iqgamma) then
        w%val(location,iomega)%val(1,:) = 0.0
        w%val(location,iomega)%val(:,1) = 0.0 
! to set the wings of W to 0.0 is not necessary because
! the wings of P have still been set to 0.0
! but I am paranoic
        w%val(location,iomega)%val(1,1) = 1.0
      end if
! NOTE: the polar and w operators are HERMITEAN when calculated for imaginary frequencies
!
! BODY inversion
!
      call num_he_inv(w%val(location,iomega)%val)
      if(iq==w%iqgamma) then
        w%macroscopic(:,:,iomega) = identity / num_8pi - polar%macroscopic(:,:,iomega)
!
! WING BODY WING product
!
        do i = 1,3
          call num_gemv(w%val(location,iomega)%val,polar%gradients_l(i,iomega)%val,&
                        w%lambda(i,iomega)%val)
          call num_gemv(w%val(location,iomega)%val,polar%gradients_r(i,iomega)%val,&
                        w%rho(i,iomega)%val,trans="C")
        end do
        do i = 1,3
          do j = 1,3
            w%macroscopic(i,j,iomega) = w%macroscopic(i,j,iomega) - &
                   pw_wfc_braket(w%rho(i,iomega),polar%gradients_l(j,iomega))
          end do
        end do
      end if ! end if iqgamma
      call pw_coulomb_get(3,coulomb,coulomb_wfc,linverse=.false.)
      npw = coulomb_wfc%npw
      do ipw=1,npw
        w%val(location,iomega)%val(ipw,ipw) = w%val(location,iomega)%val(ipw,ipw) - coulomb_wfc%val(ipw)
      end do
!      write(0,*) iq,iomega,num_spectral_range(polar%val(location,iomega)%val,"H")
!      write(0,*) iq,iomega,num_spectral_range(polar%val(location,iomega)%val,"A")
!      write(0,*) iq,iomega,num_spectral_range(w%val(location,iomega)%val,"H")
!      write(0,*) iq,iomega,num_spectral_range(w%val(location,iomega)%val,"A")

    end do ! enddo iomega

    call pw_wfc_destroy(coulomb_wfc)
    call pw_w_giveback_basis(w,basis_tmp)

  end do ! endo q loop
  call pw_coulomb_destroy(coulomb)

  do iomega=0,w%nomega
    call pw_wfc_bcast(w%lambda(1,iomega),w%where_q(1,w%iqgamma),w%comm)
    call pw_wfc_bcast(w%lambda(2,iomega),w%where_q(1,w%iqgamma),w%comm)
    call pw_wfc_bcast(w%lambda(3,iomega),w%where_q(1,w%iqgamma),w%comm)
    call pw_wfc_bcast(w%rho   (1,iomega),w%where_q(1,w%iqgamma),w%comm)
    call pw_wfc_bcast(w%rho   (2,iomega),w%where_q(1,w%iqgamma),w%comm)
    call pw_wfc_bcast(w%rho   (3,iomega),w%where_q(1,w%iqgamma),w%comm)
    call ptk_bcast(w%macroscopic(:,:,iomega),w%where_q(1,w%iqgamma),w%comm)
  end do
!  w%macroscopic = 0.5 * (w%macroscopic + conjg(transpose(w%macroscopic)))

end subroutine pw_w_calc_x3

subroutine pw_w_calc_x0(w,polar,coulomb_div_treatment,ecutvcut,dealloc_polar)
  use pw_w_type
  use pw_w_interf
  use pw_polar_module
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
  use coulomb_vcut_module, only : vcut_type
  implicit none
  type (pw_w), intent(inout) :: w
  type (pw_polar), intent(inout) :: polar
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut
  logical, optional, intent(in) :: dealloc_polar
  real :: identity(3,3)
  integer :: iq,location,i,j,npw,ipw
  type (pw_basis),pointer :: basis_tmp
  type (pw_wfc)     :: coulomb_wfc
  type (pw_coulomb) :: coulomb
  integer :: iomega,ipw1,ipw2
  logical :: dealloc_polar_loc
  character(len=30) :: coulomb_div_treatment_loc
  real :: ecutvcut_loc
  type(vcut_type) :: vcut
  integer :: system_type
  real :: lattice(3,3)

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = coulomb_div_treatment
  w%coulomb_div_treatment = coulomb_div_treatment_loc

  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut
  w%ecutvcut = ecutvcut_loc


  dealloc_polar_loc = .false.
  if(present(dealloc_polar)) dealloc_polar_loc = dealloc_polar
  if(dealloc_polar_loc) then
    polar%val_initialized = .false.
  end if

  dealloc_polar_loc = .false.
  if(present(dealloc_polar)) dealloc_polar_loc = dealloc_polar
  if(dealloc_polar_loc) then
    polar%val_initialized = .false.
  end if

  identity(1,1) = 1.0
  identity(2,1) = 0.0
  identity(3,1) = 0.0
  identity(1,2) = 0.0
  identity(2,2) = 1.0
  identity(3,2) = 0.0
  identity(1,3) = 0.0
  identity(2,3) = 0.0
  identity(3,3) = 1.0

  lattice = num_matmul(w%struct%b,w%qmesh%m_inv)

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

!  call pw_coulomb_init(coulomb,lattice)
  do iq=1,w%qmesh%nkibz
    if(w%where_q(1,iq)/=w%rank) cycle
    location = w%where_q(2,iq)
    call pw_w_borrow_basis(w,basis_tmp,iq)
    call pw_wfc_init(coulomb_wfc,basis_tmp)
! Loop on omega
    do iomega = 0,w%nomega
      call pw_coulomb_get(system_type,coulomb,coulomb_wfc,linverse=.true.)
      if(.not. w%val_initialized) then 
        call pw_wfc6d_init(w%val(w%where_q(2,iq),iomega),w%basis(iq))
      end if
       npw = ubound(w%val(location,iomega)%val,1)
      do ipw1=1,npw
         do ipw2=1,npw
           w%val(location,iomega)%val(ipw1,ipw2) = -polar%val(location,iomega)%val(ipw1,ipw2)
         enddo
      enddo
      if(dealloc_polar_loc) then
        call pw_wfc6d_destroy(polar%val(location,iomega))
      end if
      npw = coulomb_wfc%npw
      do ipw=1,npw
        w%val(location,iomega)%val(ipw,ipw) = w%val(location,iomega)%val(ipw,ipw) + coulomb_wfc%val(ipw)
      end do
      if(iq==w%iqgamma) then
        w%val(location,iomega)%val(1,:) = 0.0
        w%val(location,iomega)%val(:,1) = 0.0
! to set the wings of W to 0.0 is not necessary because
! the wings of P have still been set to 0.0
! but I am paranoic
        w%val(location,iomega)%val(1,1) = 1.0
      end if
      call num_he_inv(w%val(location,iomega)%val)
      if(iq==w%iqgamma) then
        w%macroscopic(:,:,iomega) = identity / num_8pi
      end if
      call pw_coulomb_get(system_type,coulomb,coulomb_wfc,linverse=.false.)
      npw = coulomb_wfc%npw
      if(iq==w%iqgamma) then
         w%v0=coulomb_wfc%val(1)
         w%val(location,iomega)%val(1,1)=coulomb_wfc%val(1)
      endif
      do ipw=1,npw
        w%val(location,iomega)%val(ipw,ipw) = w%val(location,iomega)%val(ipw,ipw) - coulomb_wfc%val(ipw)
      end do
    end do
    call pw_wfc_destroy(coulomb_wfc)
    call pw_w_giveback_basis(w,basis_tmp)
  end do
  
  call pw_coulomb_destroy(coulomb)
  if(system_type==1) then
    call pw_vcut_destroy(vcut)
  endif
  do iomega=0,w%nomega
    call ptk_bcast(w%macroscopic(:,:,iomega),w%where_q(1,w%iqgamma),w%comm)
    call ptk_bcast(w%v0,w%root,w%comm)
  end do
! for q->0 v(1) has a finite value due to the cutoff 
! products like vP for head and wings od W gives 0
! for q->0 W(1,1)=1 and W(g,1)=0 W(1,g')=0
end subroutine pw_w_calc_x0

subroutine pw_w_smooth_x3(w) 
! the inversion of (1-vP) has been done by blocks
! this routine compute:
!     corrections to this block inversion
!     the head and wings of W
  use pw_w_type
  use pw_w_interf
  use num_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use coulomb_vcut_module
  use pw_coulomb_module
  implicit none
  type (pw_w), intent(inout) :: w

  integer :: iq,location,npw,ipw1,ipw2,iqgamma
  complex :: inverse(3,3),vec(3),cccccc(3,3)
  complex :: tmp2(3),add,tmp1(3)
  real    :: w0,volume,ecut,qsigma
  real    :: lattice(3,3)
  integer :: iomega

  if(w%smooth)return
  w%smooth = .true.
  iqgamma = w%iqgamma
  lattice = num_matmul(w%struct%b,w%qmesh%m_inv)
  w%v0 = num_discontinuity_value(lattice,w%cutoff)

  do iomega=0,w%nomega
    cccccc = num_inverse_tensor(w%macroscopic(:,:,iomega))
    inverse = transpose(cccccc)
    if(w%where_q(1,iqgamma) == w%rank) then
      location = w%where_q(2,iqgamma)

      npw = w%basis(iqgamma)%npw
        do ipw2=2,npw
          tmp2(1) = w%rho(1,iomega)%val(ipw2)
          tmp2(2) = w%rho(2,iomega)%val(ipw2)
          tmp2(3) = w%rho(3,iomega)%val(ipw2)
          do ipw1=2,npw
! NB il conjg e' gia' implicito in dot_product
            tmp1(1) = w%lambda(1,iomega)%val(ipw1)
            tmp1(2) = w%lambda(2,iomega)%val(ipw1)
            tmp1(3) = w%lambda(3,iomega)%val(ipw1)
            add = dot_product(tmp2,num_matmul(inverse,tmp1))
            w%val(location,iomega)%val(ipw1,ipw2) = w%val(location,iomega)%val(ipw1,ipw2) + add
          end do
        end do
        w%val(location,iomega)%val(1,:) = 0.0
        w%val(location,iomega)%val(:,1) = 0.0
! as in pw_w_calc to reset the wings of W to 0.0 is not necessary 
! already done
! but I am paranoic
    end if
  end do

end subroutine pw_w_smooth_x3

subroutine pw_w_cutoff_x3(w,coulomb_div_treatment,ecutvcut) 
! the inversion of (1-vP) has been done by blocks
! this routine compute:
!     corrections to this block inversion
!     the head and wings of W
  use pw_w_type
  use pw_w_interf
  use num_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use coulomb_vcut_module
  use pw_coulomb_module
  implicit none
  type (pw_w), intent(inout) :: w
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut

  integer :: iq,location,npw,ipw1,ipw2,iqgamma
  complex :: inverse(3,3),vec(3),cccccc(3,3)
  complex :: tmp2(3),add,tmp1(3)
  real    :: w0,volume,ecut,qsigma
  real    :: lattice(3,3)
  integer :: iomega
  type(pw_wfc) :: wfc, coulomb_wfc
  type(pw_coulomb) :: coulomb
  type(pw_basis), pointer :: basis_tmp

  character(len=30) :: coulomb_div_treatment_loc
  real :: ecutvcut_loc
  type(vcut_type) :: vcut
  integer :: system_type
  real :: ccccc

  iqgamma = w%iqgamma
  lattice = num_matmul(w%struct%b,w%qmesh%m_inv)
  w%v0 = num_discontinuity_value(lattice,w%cutoff)

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = coulomb_div_treatment
  w%coulomb_div_treatment = coulomb_div_treatment_loc

  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut
  w%ecutvcut = ecutvcut_loc

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


!
! Loop su omega
!
    do iomega = 0,w%nomega
      if(w%where_q(1,iqgamma) == w%rank) then
      location = w%where_q(2,iqgamma)
        if(system_type==3) then
          w%val(location,iomega)%val(1,1) = &
          num_discontinuity_value(lattice,w%cutoff,epsilon=w%macroscopic(:,:,iomega))/num_8pi &
          - num_discontinuity_value(lattice,w%cutoff)
        elseif(system_type==1) then
        ccccc = 0.0
        ccccc = num_trace(num_inverse_tensor(w%macroscopic(:,:,iomega)))/num_8pi - 1.0

           w%val(location,iomega)%val(1,1) = &
!           num_vlr0_plus_vsk0_value(lattice,w%cutoff,epsilon=w%macroscopic(:,:,iomega))/num_8pi &
!                   - num_vlr0_plus_vsk0_value(lattice,w%cutoff)
           ccccc * vcut%corrected(0,0,0)
        elseif(system_type==0) then
          w%val(location,iomega)%val(1,1) = &
          num_v0_vcut_spheric_value(lattice,epsilon=w%macroscopic(:,:,iomega))/num_8pi &
              - num_v0_vcut_spheric_value(lattice)
        else
          ERROR("wrong system type in w")
        endif
      endif
    enddo

! here sym of W .. W=sqrt(v) *[ 1/sqrt(v) * (1-sqrt(v) P sqrt(v)) * 1/sqrt(v) ] * sqrt(v)
if(system_type /= 3 ) then
  do iq=1,w%qmesh%nkibz
    if(w%where_q(1,iq)/=w%rank) cycle
    location = w%where_q(2,iq)
    call pw_w_borrow_basis(w,basis_tmp,iq)
    call pw_wfc_init(coulomb_wfc,basis_tmp)
    npw = w%basis(iq)%npw
!
! Loop su omega
!
    do iomega = 0,w%nomega

      call pw_coulomb_get(3,coulomb,coulomb_wfc,linverse=.true.,lhalf=.true.)

     if(iq==w%iqgamma) then
       do ipw2=2,npw
         do ipw1=2,npw
              w%val(location,iomega)%val(ipw1,ipw2) =  &
                   coulomb_wfc%val(ipw1) * w%val(location,iomega)%val(ipw1,ipw2) * coulomb_wfc%val(ipw2)
         enddo
       enddo
     else
       do ipw2=1,npw
         do ipw1=1,npw
              w%val(location,iomega)%val(ipw1,ipw2) =  &
                   coulomb_wfc%val(ipw1) * w%val(location,iomega)%val(ipw1,ipw2) * coulomb_wfc%val(ipw2)
         enddo
       enddo
     endif

     call pw_coulomb_get(system_type,coulomb,coulomb_wfc,linverse=.false.,lhalf=.true.)

     if(iq==w%iqgamma) then
       do ipw2=2,npw
         do ipw1=2,npw
            w%val(location,iomega)%val(ipw1,ipw2) =  &
                 coulomb_wfc%val(ipw1) * w%val(location,iomega)%val(ipw1,ipw2) * coulomb_wfc%val(ipw2) 
         enddo
       enddo
     else
       do ipw2=1,npw
         do ipw1=1,npw
            w%val(location,iomega)%val(ipw1,ipw2) =  &
                 coulomb_wfc%val(ipw1) * w%val(location,iomega)%val(ipw1,ipw2) * coulomb_wfc%val(ipw2)
         enddo
       enddo
     endif

    enddo ! end do iomega
    call pw_wfc_destroy(coulomb_wfc)
    call pw_w_giveback_basis(w,basis_tmp)
  enddo !end do iq
endif !end if not system_type=3

  if(system_type==1) call pw_vcut_destroy(vcut)
  call pw_coulomb_destroy(coulomb)

end subroutine pw_w_cutoff_x3

subroutine pw_w_borrow_wfc6d_x(w,wfc6d,ik,iomega)
  use pw_w_type
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_allgather
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_w),                        intent(in)  :: w
  type(pw_wfc6d), pointer, optional :: wfc6d
  integer,                 optional, intent(in)  :: ik
  integer,                 optional, intent(in)  :: iomega
  integer, allocatable :: request(:),request_iomega(:)
  integer :: ip,size,rank,ikk,where_is,ikl
  integer :: iomega_loc
  call ptk_comm_size(w%comm,size)
  call ptk_comm_rank(w%comm,rank)
  iomega_loc = 0
  if(present(iomega)) iomega_loc = iomega
  ikl = huge(ik)
  if(present(ik)) ikl = ik
  if(present(ik).neqv.present(wfc6d)) ERROR("")
  allocate(request(0:size-1))
  allocate(request_iomega(0:size-1))
  call ptk_allgather(ikl,request,w%comm)
  call ptk_allgather(iomega_loc,request_iomega,w%comm)
  if(all(request(:)==request(0)) .and. all(request_iomega==request_iomega(0))) then
    where_is = w%where_q(1,ikl)
    if(rank==where_is) then
      wfc6d => w%val(w%where_q(2,ikl),iomega_loc)
    else
        allocate(wfc6d)
        call pw_wfc6d_init(wfc6d,w%basis(ik))
    end if
    call pw_wfc6d_bcast(wfc6d,where_is,w%comm)
  else
    do ip=0,size-1
      ikk = request(ip)
      where_is = w%where_q(1,ikk)
      if(present(ik)) then
        if(rank==ip .and. rank==where_is) then
          if(ikk/=ik) ERROR("")
          wfc6d => w%val(w%where_q(2,ikk),request_iomega(ip))
        end if
        if(rank==ip .and. rank/=where_is) then
          if(ikk/=ik) ERROR("")
          allocate(wfc6d)
          call pw_wfc6d_init(wfc6d,w%basis(ik))
          call pw_wfc6d_recv(wfc6d,where_is,ikk,w%comm)
        end if
      end if
      if(rank/=ip .and. rank==where_is) then
        call pw_wfc6d_send(w%val(w%where_q(2,ikk),request_iomega(ip)),ip,ikk,w%comm)
      end if
    end do
  end if
  deallocate(request)
  deallocate(request_iomega)
end subroutine pw_w_borrow_wfc6d_x

subroutine pw_w_giveback_wfc6d_x(w,wfc6d)
  use pw_w_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type (pw_w),              intent(in)    :: w
  type (pw_wfc6d), pointer :: wfc6d
  integer :: ik
  logical :: found
  integer :: iomega
  found = .false.
  do ik=lbound(w%val,1),ubound(w%val,1)
  do iomega=lbound(w%val,2),ubound(w%val,2)
    if(associated(wfc6d,w%val(ik,iomega))) found = .true.
    if(found) exit
  end do
  end do
  if(found) then
    nullify(wfc6d)
  else
    call pw_wfc6d_destroy(wfc6d)
    deallocate(wfc6d)
  end if
end subroutine pw_w_giveback_wfc6d_x

subroutine pw_w_read_x(w,unit,name)
  use pw_w_type
  use pw_w_interf
  use ptk_module, only : ptk_bcast
  use iotk_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_w),  intent(inout) :: w
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  type(pw_basis), pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq
  integer :: iomega,nomega,nq
!  real :: omegamax
  complex :: omegamax

  if(w%rank == w%root) then
    call iotk_scan_begin(unit,trim(name),attr)
    call iotk_scan_attr(attr,"type",rtype,default="pw_w")
    if(rtype/="pw_w") ERROR("")

    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr(attr,"nomega",nomega,default=w%nomega)
    if(nomega/=w%nomega) ERROR("")
    call iotk_scan_attr(attr,"omegamax",omegamax,default=w%omegamax)
    if(omegamax/=w%omegamax) ERROR(" ")
    call iotk_scan_attr(attr,"nq",    nq)
    if(nq/=w%qmesh%nkbz) ERROR("")
    call iotk_scan_attr(attr,"smooth",w%smooth,default=.true.) ! OCCHIO
  end if

  if(w%rank==w%root) then
    call iotk_scan_dat(unit,"omega",w%omega(:))
    call iotk_scan_dat(unit,"weight",w%weight(:))    
  endif        
  
  do iomega = 0,w%nomega
    if(w%rank == w%root) then
      call iotk_scan_begin(unit,"omega"//trim(iotk_index(iomega)),attr)
      call iotk_scan_dat(unit,"macro1",w%macroscopic(1,:,iomega))
      call iotk_scan_dat(unit,"macro2",w%macroscopic(2,:,iomega))
      call iotk_scan_dat(unit,"macro3",w%macroscopic(3,:,iomega))
      call iotk_scan_dat(unit,"v0",w%v0)
      call pw_wfc_read(w%lambda(1,iomega),unit,"iotk",name="lambda1")
      call pw_wfc_read(w%lambda(2,iomega),unit,"iotk",name="lambda2")
      call pw_wfc_read(w%lambda(3,iomega),unit,"iotk",name="lambda3")
      call pw_wfc_read(w%rho(1,iomega),unit,"iotk",name="rho1")
      call pw_wfc_read(w%rho(2,iomega),unit,"iotk",name="rho2")
      call pw_wfc_read(w%rho(3,iomega),unit,"iotk",name="rho3")
    end if

    call ptk_bcast(w%macroscopic(:,:,iomega),w%root,w%comm)
    call pw_wfc_bcast(w%lambda(1,iomega),w%root,w%comm)
    call pw_wfc_bcast(w%lambda(2,iomega),w%root,w%comm)
    call pw_wfc_bcast(w%lambda(3,iomega),w%root,w%comm)
    call pw_wfc_bcast(w%rho(1,iomega),w%root,w%comm)
    call pw_wfc_bcast(w%rho(2,iomega),w%root,w%comm)
    call pw_wfc_bcast(w%rho(3,iomega),w%root,w%comm)
    call ptk_bcast(w%v0,w%root,w%comm)

    do iq=1,w%qmesh%nkibz
      owner = w%where_q(1,iq)
      location = w%where_q(2,iq)
      if(w%rank == w%root .and. w%rank == owner) then
        call pw_wfc6d_read(w%val(location,iomega),unit,"iotk",name="val"//trim(iotk_index(iq)))
      end if
      if(w%rank == w%root .and. w%rank /= owner) then
! OCCHIO AL BARRIER
        call pw_w_borrow_basis(w,basis_tmp,iq)
        call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
        call pw_wfc6d_read(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
        call pw_wfc6d_send(val_tmp,owner,iq,w%comm)
        call pw_wfc6d_destroy(val_tmp)
      else
        call pw_w_borrow_basis(w,basis_tmp,iq) !per il barrier
      end if
      if(w%rank /= w%root .and. w%rank == owner) then
        call pw_wfc6d_recv(w%val(location,iomega),w%root,iq,w%comm)
      end if
      call pw_w_giveback_basis(w,basis_tmp)
    end do
    if(w%rank == w%root) then
      call iotk_scan_end(unit,"omega"//trim(iotk_index(iomega)))
    end if
  end do
  if(w%rank == w%root) then
    call iotk_scan_end(unit,name)
  end if
end subroutine pw_w_read_x

subroutine pw_w_init_tail_x(w,struct,symmlist,qmesh,iomega_tail,cutoff,root,comm,do_not_alloc)
  use pw_w_type
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
  type (pw_w),   intent(out) :: w
  type (pw_struct),   target, intent(in)  :: struct
  type (pw_symmlist), target, intent(in)  :: symmlist
  type (pw_kmesh),    target, intent(in)  :: qmesh
  integer                   , intent(in)  :: iomega_tail
  real,                       intent(in)  :: cutoff
  logical,          optional, intent(in)  :: do_not_alloc
  integer,                    intent(in)  :: root
  type (ptk_comm),            intent(in)  :: comm
! Costruttore. Nota che e' parallelo, dunque tutti i processori appartenenti
! a comm devono chiamarlo contemporaneamente con gli stessi parametri.
!@ END MANUAL
  integer iq,iomega
  logical :: do_not_alloc_loc

  do_not_alloc_loc = .false.
  if(present(do_not_alloc)) do_not_alloc_loc = do_not_alloc
  w%val_initialized = .false.

  w%struct   => struct
  w%symmlist => symmlist
  w%qmesh    => qmesh
  w%cutoff   =  cutoff
  w%root     =  root
  w%comm     =  comm
  w%nomega   = 0


  call ptk_comm_size(comm,w%npe)
  call ptk_comm_rank(comm,w%rank)
  if(w%root >= w%npe) ERROR("")

  allocate(w%where_q(2,w%qmesh%nkibz))
  call pw_allocate(w%where_q)
  w%where_q = 0
  do iq=1,qmesh%nkibz
    w%where_q(1,iq) = modulo(iq-w%root,w%npe)
    w%where_q(2,iq) = count(w%where_q(1,1:iq)==w%where_q(1,iq))
  end do


  allocate(w%basis(qmesh%nkibz))
  call pw_basis_init(w%basis(:),w%struct)
  call pw_basis_create(w%basis,w%qmesh,w%cutoff)

  allocate(w%val(count(w%where_q(1,:)==w%rank),iomega_tail:iomega_tail))
  if(.not. do_not_alloc_loc) then
    do iq=1,qmesh%nkibz
      if(w%where_q(1,iq)==w%rank) &
       call pw_wfc6d_init(w%val(w%where_q(2,iq),iomega_tail),w%basis(iq))
    end do
    w%val_initialized = .true.
  end if

  w%iqgamma = pw_kmesh_kibz_index(w%qmesh,(/0.0,0.0,0.0/))

  allocate(w%macroscopic(3,3,iomega_tail:iomega_tail))
  w%macroscopic = 0.0

  allocate(w%lambda(3,iomega_tail:iomega_tail))
  allocate(w%rho(3,iomega_tail:iomega_tail))
  call pw_wfc_init(w%lambda,w%basis(w%iqgamma))
  call pw_wfc_init(w%rho,w%basis(w%iqgamma))

  w%smooth = .false.

  w%v0 = 0.0

  allocate(w%omega(iomega_tail:iomega_tail),w%weight(iomega_tail:iomega_tail))
end subroutine pw_w_init_tail_x

subroutine pw_w_read_tail_x(w,iomega_tail,unit,name)
  use pw_w_type
  use pw_w_interf
  use ptk_module, only : ptk_bcast
  use iotk_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_w),  intent(inout) :: w
  integer, intent(in) :: iomega_tail
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name

  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  type(pw_basis), pointer :: basis_tmp
  type(pw_wfc6d) :: val_tmp
  integer        :: owner, location, iq
  integer :: iomega,nomega,nq

  if(w%rank == w%root) then
    call iotk_scan_begin(unit,trim(name),attr)
    call iotk_scan_attr(attr,"type",rtype,default="pw_w")
    if(rtype/="pw_w") ERROR("")

    call iotk_scan_empty(unit,"info",attr)
    call iotk_scan_attr(attr,"nomega",nomega,default=w%nomega)
    call iotk_scan_attr(attr,"nq",    nq)
    if(nq/=w%qmesh%nkbz) ERROR("")
    call iotk_scan_attr(attr,"smooth",w%smooth,default=.true.) ! OCCHIO
  end if

  do iomega = 0,w%nomega
    if(iomega/=iomega_tail) cycle
    if(w%rank == w%root) then
      call iotk_scan_begin(unit,"omega"//trim(iotk_index(iomega_tail)),attr)
      call iotk_scan_dat(unit,"macro1",w%macroscopic(1,:,iomega_tail))
      call iotk_scan_dat(unit,"macro2",w%macroscopic(2,:,iomega_tail))
      call iotk_scan_dat(unit,"macro3",w%macroscopic(3,:,iomega_tail))
      call iotk_scan_dat(unit,"v0",w%v0)
      call pw_wfc_read(w%lambda(1,iomega_tail),unit,"iotk",name="lambda1")
      call pw_wfc_read(w%lambda(2,iomega_tail),unit,"iotk",name="lambda2")
      call pw_wfc_read(w%lambda(3,iomega_tail),unit,"iotk",name="lambda3")
      call pw_wfc_read(w%rho(1,iomega_tail),unit,"iotk",name="rho1")
      call pw_wfc_read(w%rho(2,iomega_tail),unit,"iotk",name="rho2")
      call pw_wfc_read(w%rho(3,iomega_tail),unit,"iotk",name="rho3")
    end if

    call ptk_bcast(w%macroscopic(:,:,iomega_tail),w%root,w%comm)
    call pw_wfc_bcast(w%lambda(1,iomega_tail),w%root,w%comm)
    call pw_wfc_bcast(w%lambda(2,iomega_tail),w%root,w%comm)
    call pw_wfc_bcast(w%lambda(3,iomega_tail),w%root,w%comm)
    call pw_wfc_bcast(w%rho(1,iomega_tail),w%root,w%comm)
    call pw_wfc_bcast(w%rho(2,iomega_tail),w%root,w%comm)
    call pw_wfc_bcast(w%rho(3,iomega_tail),w%root,w%comm)
    call ptk_bcast(w%v0,w%root,w%comm)

    do iq=1,w%qmesh%nkibz
      owner = w%where_q(1,iq)
      location = w%where_q(2,iq)
      if(w%rank == w%root .and. w%rank == owner) then
        call pw_wfc6d_read(w%val(location,iomega_tail),unit,"iotk",name="val"//trim(iotk_index(iq)))
      end if
      if(w%rank == w%root .and. w%rank /= owner) then
! OCCHIO AL BARRIER
        call pw_w_borrow_basis(w,basis_tmp,iq)
        call pw_wfc6d_init(val_tmp,basis_tmp,basis_tmp)
        call pw_wfc6d_read(val_tmp,unit,"iotk",name="val"//trim(iotk_index(iq)))
        call pw_wfc6d_send(val_tmp,owner,iq,w%comm)
        call pw_wfc6d_destroy(val_tmp)
      else
        call pw_w_borrow_basis(w,basis_tmp,iq) !per il barrier
      end if
      if(w%rank /= w%root .and. w%rank == owner) then
        call pw_wfc6d_recv(w%val(location,iomega_tail),w%root,iq,w%comm)
      end if
      call pw_w_giveback_basis(w,basis_tmp)
    end do
    if(w%rank == w%root) then
      call iotk_scan_end(unit,"omega"//trim(iotk_index(iomega_tail)))
    end if
  end do
  if(w%rank == w%root) then
    call iotk_scan_end(unit,name)
  end if
end subroutine pw_w_read_tail_x
