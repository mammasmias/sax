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

!@ MANUAL
module pw_coulomb_module

use coulomb_vcut_module, only : vcut_type, vcut_get, vcut_spheric_get

implicit none
! This moudle contains the object pw_coulomb and its methods
private
public :: pw_coulomb,         &
          pw_coulomb_init,    &
          pw_coulomb_destroy, &
          pw_coulomb_get,     &
          pw_coulomb_contact_value, &
          pw_coulomb_total_integral, &
          pw_vcut_init, &
          pw_vcut_destroy
!@ END MANUAL

!@ MANUAL
type pw_coulomb
  logical :: lepsilon     ! If true, the potential is screened
  real    :: epsilon(3,3) ! The screenening matrix: qq = q*epsilon*q
  real    :: lattice(3,3) ! The supercell inverse b*qmesh%m_inv
  real    :: inverse_mean_epsilon
                          ! The spherical mean value of q^2/(qq)
  integer :: iqsigma      ! iqsigma =  0 => nothing (=> iqsigma=-1 and qsigma = +INF)
                          ! iqsigma = +1 => multiply by exp(-q^2/qsigma^2)
                          ! iqsigma = -1 => multiply by (1-exp(-q^2/qsigma^2))
  real    :: qsigma        ! (a.u.)^-1
  type (vcut_type), pointer :: vcut
end type pw_coulomb
! It describes a coulomb potential
! defined as an operator diagonal in q space
! and analytical.
! Once initialized, it can be taken to a wfc object.
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_coulomb_init(coulomb,lattice,vcut,iqsigma,qsigma,epsilon)
  use num_module
  type (pw_coulomb), intent(out) :: coulomb
  real, intent(in) :: lattice(3,3)
  type (vcut_type), optional, target, intent(in) :: vcut
  integer, optional, intent(in)  :: iqsigma
  real,    optional, intent(in)  :: qsigma
  complex,    optional, intent(in)  :: epsilon(3,3)
  complex :: inverse(3,3)
!@ END MANUAL

  coulomb%lattice(:,:) = lattice(:,:)

  if(present(vcut)) then
    coulomb%vcut => vcut
  endif
  if(present(iqsigma)) then
    coulomb%iqsigma = iqsigma
    if(.not.present(qsigma)) ERROR("")
    coulomb%qsigma   = qsigma
  else
    coulomb%iqsigma = 0.0
    coulomb%qsigma  = huge(coulomb%qsigma)
  end if

!!!! dovrei cambiare le unita de epsilon per non dovere dividere per 8pi
!!!! num_dicontinuity_value in coulomb_get

  if(present(epsilon)) then
    coulomb%lepsilon = .true.
    coulomb%epsilon  = epsilon
    inverse = num_inverse_tensor(epsilon)
    coulomb%inverse_mean_epsilon = num_trace(inverse)
  else
    coulomb%lepsilon = .false.
    coulomb%epsilon  = reshape((/1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0/),(/3,3/))
    coulomb%inverse_mean_epsilon = 1.0
  end if
end subroutine pw_coulomb_init

!@ MANUAL
subroutine pw_coulomb_destroy(coulomb)
! This subroutine is provided only for future inclusion of dynamic data
  use coulomb_vcut_module, only : vcut_destroy
  type (pw_coulomb), intent(inout) :: coulomb
  if(associated(coulomb%vcut)) then
    nullify(coulomb%vcut)
  endif
!@ END MANUAL
end subroutine pw_coulomb_destroy

!@ MANUAL
subroutine pw_coulomb_get(system_type,coulomb,wfc,lhalf,linverse)
  use num_module
  use pw_wfc_module
  type (pw_coulomb), intent(in)    :: coulomb
  type (pw_wfc),     intent(inout) :: wfc
  logical, optional, intent(in)    :: lhalf    ! If true, the square root has to be taken
  logical, optional, intent(in)    :: linverse ! If true, the inverse has to be taken
  integer :: system_type
!@ END MANUAL
  logical :: lhalf_loc,linverse_loc
  logical :: lepsilon
  integer :: iqsigma
  real    :: qsigma
  integer :: ipw,npw
  real    :: b(3,3),kg(3),kg2,kg2_eps,v,epsilon(3,3)
  logical :: limit
  real :: Rcut, a(3,3)
  real :: lattice(3,3), inverse_lattice(3,3)
  lhalf_loc    = .false.
  linverse_loc = .false.
  if(present(lhalf))    lhalf_loc = lhalf
  if(present(linverse)) linverse_loc = linverse

  if((.not.(associated(coulomb%vcut))).and.(system_type.eq.1))  & 
      ERROR("vcut not associated system")

  lepsilon = coulomb%lepsilon
  epsilon  = coulomb%epsilon
  iqsigma  = coulomb%iqsigma
  qsigma   = coulomb%qsigma
  npw = wfc%npw
  b = wfc%basis%struct%b
  a = wfc%basis%struct%a

!  write(0,*) "system type ", system_type

!  write(0,*) "lattice ", coulomb%lattice(1,1), coulomb%lattice(2,2), coulomb%lattice(3,3)

  inverse_lattice = num_inverse(coulomb%lattice) * num_pi * 2.0

!  write(0,*) "inverse lattice ", inverse_lattice(1,1), inverse_lattice(2,2), inverse_lattice(3,3)

  if(system_type==3) then
     do ipw=1,npw
        limit = .false.
        kg   = num_matmul(b,wfc%basis%g(:,ipw)+wfc%basis%k) ! Ora kg e' in a.u.^-1
        kg2  = sum(kg**2)
        if(lepsilon) then
           kg2_eps = dot_product(kg,num_matmul(epsilon,kg))
        else
           kg2_eps = kg2
        end if
        if(kg2_eps<0.0000001) then
           limit = .true.
        end if
        if(.not.limit) then
           v = num_8pi/kg2_eps
           select case(iqsigma)
           case(1:)
              v = v * exp(-kg2/(2.0*qsigma**2))
           case(:-1)
              v = v * (1.0-exp(-kg2/(2.0*qsigma**2)))
           case(0)
           end select
        else
           if(iqsigma<0) then
              v = num_8pi/(2.0*qsigma**2) * coulomb%inverse_mean_epsilon
           else
! ???????
              v = num_discontinuity_value_old(coulomb%lattice,epsilon=epsilon)
!               v = 0.0
! ?????         
          end if
        end if
        if(linverse_loc) then
           if(v>0.0000001) then
              v = 1.0/v
           else
              v = 0.0
! ???
!              v = 1.0/num_discontinuity_value_old(coulomb%lattice,epsilon=epsilon)
!????
           end if
        end if
        if(lhalf_loc) then
           v = sqrt(v)
        end if
        wfc%val(ipw) = v
     end do
  elseif(system_type==1) then
    do ipw=1,npw
        kg   = num_matmul(b,wfc%basis%g(:,ipw)+wfc%basis%k) ! Ora kg e' in a.u.^-1
        if(lepsilon) then
        v = vcut_get(coulomb%vcut,kg) * 1.0/3.0 * coulomb%inverse_mean_epsilon
        else
          v = vcut_get(coulomb%vcut,kg)
        end if

        if(linverse_loc) then
          v = 1.0/v
        end if

        if(lhalf_loc) then
           v = sqrt(v)
        end if
        wfc%val(ipw) = v
     enddo

  elseif(system_type==0) then
!     Rcut=0.5*minval(sqrt(sum(a**2,1)))
!     Rcut=Rcut-Rcut/50.0
     do ipw=1,npw
!        limit=.false.
        kg=num_matmul(b,wfc%basis%g(:,ipw)+wfc%basis%k)
!        kg2=sum(kg**2)
!        if(kg2<0.000000001) then
!           limit=.true.
!        endif
!        if(.not.limit) then
!           v=num_8pi/kg2*(1.0-cos(Rcut*sqrt(kg2)))
!        else
!           v=num_8pi*Rcut**2/2.0
!        endif
        if(lepsilon) then
          v = vcut_spheric_get(inverse_lattice,kg) * 1.0/3.0 * coulomb%inverse_mean_epsilon
        else
          v = vcut_spheric_get(inverse_lattice,kg)
        endif
        if(linverse_loc) then
!           if(v>0.0000001) then
              v=1/v
!           else
!              v=2.0/(num_8pi*Rcut**2)
!           endif
        endif
        if(lhalf_loc) then
           v=sqrt(v)
        endif
        wfc%val(ipw)=v
     enddo
  endif

end subroutine pw_coulomb_get

function pw_coulomb_contact_value(coulomb) ! Restituisce v(r=0) solo se iqc=+1
  use num_module
  type (pw_coulomb), intent(in) :: coulomb
  real :: pw_coulomb_contact_value
  if(coulomb%iqsigma /= +1 ) ERROR("")
  pw_coulomb_contact_value = 2.0*num_sqrt2*coulomb%qsigma/num_sqrtpi * coulomb%inverse_mean_epsilon
end function pw_coulomb_contact_value

function pw_coulomb_total_integral(coulomb) ! Restituisce v(q=0) solo se iqc=-1
  use num_module
  type (pw_coulomb), intent(in) :: coulomb
  real :: pw_coulomb_total_integral
  if(coulomb%iqsigma /= -1 ) ERROR("")
  pw_coulomb_total_integral = num_4pi / coulomb%qsigma**2 * coulomb%inverse_mean_epsilon
end function pw_coulomb_total_integral

subroutine pw_vcut_init(vcut,struct,qmesh,cutoff)
  use num_module
  use coulomb_vcut_module, only : vcut_type, vcut_init, vcut_init_fft
  use pw_struct_module, only : pw_struct
  use pw_kmesh_module, only : pw_kmesh
  type(vcut_type), intent(out) :: vcut
  type(pw_struct), intent(in) :: struct
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff

  real :: lattice(3,3), inverse_lattice(3,3)

  lattice = num_matmul(struct%b,qmesh%m_inv)
  inverse_lattice = num_inverse(lattice) * 2.0 * num_pi

  call vcut_init_fft(vcut,inverse_lattice,cutoff)

end subroutine pw_vcut_init

subroutine pw_vcut_destroy(vcut)
  use coulomb_vcut_module, only : vcut_type
  type(vcut_type), intent(inout) :: vcut
  
  if(associated(vcut%corrected)) deallocate(vcut%corrected)

end subroutine pw_vcut_destroy


end module pw_coulomb_module
