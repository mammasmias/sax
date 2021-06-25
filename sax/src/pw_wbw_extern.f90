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

subroutine pw_wbw_borrow_basis_x(polar,basis,iq)
  use pw_epsilon_macroLF_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_epsilon_macroLF),       intent(in)  :: polar 
  type(pw_basis), optional,           pointer :: basis
  integer,        optional,       intent(in)  :: iq

  if(present(basis) .neqv. present(iq)) ERROR("")
  if(present(basis)) basis => polar%basis(iq)
end subroutine pw_wbw_borrow_basis_x

subroutine pw_wbw_giveback_basis_x(polar,basis)
  use pw_epsilon_macroLF_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_epsilon_macroLF),              intent(in)    :: polar 
  type(pw_basis), pointer :: basis
  if(.not.associated(polar%basis)) ERROR("")
  nullify(basis)
end  subroutine pw_wbw_giveback_basis_x

subroutine pw_wbw_write_x(polar,kind,unit,name)
  use pw_epsilon_macroLF_type
  use pw_epsilon_macroLF_interf
  use iotk_module
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  use num_module
  implicit none
  type(pw_epsilon_macroLF),     intent(in) :: polar 
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: name,kind

  type(pw_basis), pointer      :: basis_tmp
  type(pw_wfc6d)               :: val_tmp
  integer                      :: owner, location, iq,iomega
  character(iotk_attlenx)      :: attr

 if(polar%where_q(1,polar%iqgamma)==polar%rank) then 
  OPEN(unit, file=name)

   if(kind=="real")then
     do iomega = 0,polar%nomega
         write(unit,"(4f15.6)") polar%omega(iomega), REAL(polar%macroscopic(1,1,iomega)*num_8pi),&
                       REAL(polar%macroscopic(2,2,iomega)*num_8pi),&
                       REAL(polar%macroscopic(3,3,iomega)*num_8pi)
     enddo
   else if(kind=="imag")then
     do iomega = 0,polar%nomega
         write(unit,"(4f15.6)") polar%omega(iomega), AIMAG(polar%macroscopic(1,1,iomega)*num_8pi),&
                       AIMAG(polar%macroscopic(2,2,iomega)*num_8pi),&
                       AIMAG(polar%macroscopic(3,3,iomega)*num_8pi)
     enddo
 endif
 
   CLOSE(unit)
 endif

end subroutine pw_wbw_write_x

subroutine pw_wbw_calc_x3(polar,coulomb_div_treatment,ecutvcut)
  use pw_epsilon_macroLF_module
  use pw_wbw_module
  use num_module
  use pw_basis_module
  use pw_wfc_module
  use ptk_module, only           : ptk_bcast
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
  type (pw_epsilon_macroLF), intent(inout) :: polar
  character(len=*), optional, intent(in)   :: coulomb_div_treatment
  real, optional, intent(in)               :: ecutvcut


  real                                     :: identity(3,3)
  integer                                  :: location,i,j,npw,ipw
  type (pw_basis),pointer                  :: basis_tmp
  type (pw_wfc)                            :: coulomb_wfc
  type (pw_coulomb)                        :: coulomb
  integer                                  :: iomega,ipw1,ipw2
  logical                                  :: dealloc_polar_loc
  real                                     :: lattice(3,3)
  type (vcut_type)                         :: vcut
  character(len=30)                        :: coulomb_div_treatment_loc
  real                                     :: ecutvcut_loc
  integer                                  :: system_type

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = coulomb_div_treatment

  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut

  lattice = num_matmul(polar%struct%b,polar%qmesh%m_inv)

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    call pw_vcut_init(vcut,polar%struct,polar%qmesh,ecutvcut_loc)
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


  if(polar%where_q(1,polar%iqgamma)==polar%rank) then 
    identity(1,1) = 1.0
    identity(2,1) = 0.0
    identity(3,1) = 0.0
    identity(1,2) = 0.0
    identity(2,2) = 1.0
    identity(3,2) = 0.0
    identity(1,3) = 0.0
    identity(2,3) = 0.0
    identity(3,3) = 1.0

  
    location = polar%where_q(2,polar%iqgamma)
    call pw_wbw_borrow_basis(polar,basis_tmp,polar%iqgamma )
    call pw_wfc_init(coulomb_wfc,basis_tmp)
!
! Loop over frequency  
!
    do iomega = 0,polar%nomega
!
! calculate inverse of coulomb potential matrix 
!
      call pw_coulomb_get(system_type,coulomb,coulomb_wfc,linverse=.true.)

!      call pw_wfc6d_init(polar%val(location,iomega),polar%basis(polar%iqgamma))
!
! calculate W as (v^-1-P)^-1 (BODY calculation) 
!

      polar%val(location,iomega)%val = - polar%val(location,iomega)%val

!      call pw_wfc6d_destroy(polar%val(location,iomega))

      npw = coulomb_wfc%npw
! coulomb potential is only for diagonal elements
      do ipw=1,npw
        polar%val(location,iomega)%val(ipw,ipw) = polar%val(location,iomega)%val(ipw,ipw) &
                                              + coulomb_wfc%val(ipw)
      end do

      polar%val(location,iomega)%val(1,:) = 0.0
      polar%val(location,iomega)%val(:,1) = 0.0
      polar%val(location,iomega)%val(1,1) = 1.0
!
! NOTE: the polar operator is HERMITEAN when calculated for imaginary frequencies
!
! BODY inversion: in the imaginary axis WBW is hermitian, on the real axis no!!! We need different
!                 inversion routine
!
      if(polar%imaginary_axis) then
         call num_he_inv(polar%val(location,iomega)%val)
      else
         call num_ge_inv(polar%val(location,iomega)%val)
      endif
!
! again the HEAD
!
      polar%macroscopic(:,:,iomega) = identity(:,:)/ num_8pi - polar%macroscopic(:,:,iomega)
!
! WING BODY WING product
!
      do i = 1,3

        call num_gemv(polar%val(location,iomega)%val,polar%gradients_r(i,iomega)%val,&
                      polar%rho(i,iomega)%val)
      end do
      do i = 1,3
        do j = 1,3
            polar%macroscopic(i,j,iomega) = polar%macroscopic(i,j,iomega) - &
!                 pw_wfc_braket(polar%rho(i,iomega),polar%gradients_r(j,iomega))
                  num_dotp(polar%rho(i,iomega)%val,polar%gradients_l(j,iomega)%val)                  
        end do
      end do
  enddo
      call pw_wfc_destroy(coulomb_wfc)
      call pw_wbw_giveback_basis(polar,basis_tmp)

  endif

  call pw_coulomb_destroy(coulomb)

end subroutine pw_wbw_calc_x3

subroutine pw_wbw_borrow_wfc6d_x(polar,wfc6d,ik,iomega)
  use pw_epsilon_macroLF_type
  use ptk_module, only : ptk_comm_size,ptk_comm_rank,ptk_allgather
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type(pw_epsilon_macroLF),          intent(in)  :: polar 
  type(pw_wfc6d), pointer,              optional :: wfc6d
  integer,                 optional, intent(in)  :: ik
  integer,                 optional, intent(in)  :: iomega
  integer, allocatable                           :: request(:),request_iomega(:)
  integer                                        :: ip,size,rank,ikk,where_is,ikl
  integer                                        :: iomega_loc
  call ptk_comm_size(polar%comm,size)
  call ptk_comm_rank(polar%comm,rank)
  iomega_loc = 0
  if(present(iomega)) iomega_loc = iomega
  ikl = huge(ik)
  if(present(ik)) ikl = ik
  if(present(ik).neqv.present(wfc6d)) ERROR("")
  allocate(request(0:size-1))
  allocate(request_iomega(0:size-1))
  call ptk_allgather(ikl,request,polar%comm)
  call ptk_allgather(iomega_loc,request_iomega,polar%comm)
  if(all(request(:)==request(0)) .and. all(request_iomega==request_iomega(0))) then
    where_is = polar%where_q(1,ikl)
    if(rank==where_is) then
      wfc6d => polar%val(polar%where_q(2,ikl),iomega_loc)
    else
        allocate(wfc6d)
        call pw_wfc6d_init(wfc6d,polar%basis(ik))
    end if
    call pw_wfc6d_bcast(wfc6d,where_is,polar%comm)
  else
    do ip=0,size-1
      ikk = request(ip)
      where_is = polar%where_q(1,ikk)
      if(present(ik)) then
        if(rank==ip .and. rank==where_is) then
          if(ikk/=ik) ERROR("")
          wfc6d => polar%val(polar%where_q(2,ikk),request_iomega(ip))
        end if
        if(rank==ip .and. rank/=where_is) then
          if(ikk/=ik) ERROR("")
          allocate(wfc6d)
          call pw_wfc6d_init(wfc6d,polar%basis(ik))
          call pw_wfc6d_recv(wfc6d,where_is,ikk,polar%comm)
        end if
      end if
      if(rank/=ip .and. rank==where_is) then
        call pw_wfc6d_send(polar%val(polar%where_q(2,ikk),request_iomega(ip)),ip,ikk,polar%comm)
      end if
    end do
  end if
  deallocate(request)
  deallocate(request_iomega)
end subroutine pw_wbw_borrow_wfc6d_x

subroutine pw_wbw_giveback_wfc6d_x(polar,wfc6d)
  use pw_epsilon_macroLF_type
  use pw_struct_module
  use pw_symmlist_module
  use pw_kmesh_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_common_module
  implicit none
  type (pw_epsilon_macroLF),              intent(in)    :: polar 
  type (pw_wfc6d), pointer :: wfc6d
  integer :: ik
  logical :: found
  integer :: iomega
  found = .false.
  do ik=lbound(polar%val,1),ubound(polar%val,1)
  do iomega=lbound(polar%val,2),ubound(polar%val,2)
    if(associated(wfc6d,polar%val(ik,iomega))) found = .true.
    if(found) exit
  end do
  end do
  if(found) then
    nullify(wfc6d)
  else
    call pw_wfc6d_destroy(wfc6d)
    deallocate(wfc6d)
  end if
end subroutine pw_wbw_giveback_wfc6d_x

