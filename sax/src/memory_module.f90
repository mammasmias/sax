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

module memory_module
use memory_tools_interf
use pw_module
use num_module
use memory_tools_module
use ffti_module
implicit none
private

public :: memory_sigma, memory_QP, memory_diag, memory_basis
public :: memory_w, memory_w_notalloc, memory_wfc6d, memory_wfc
public :: memory_polar, memory_states, memory_density, memory_pp
public :: memory_bse, memory_field, memory_calc_sigma

contains

subroutine memory_sigma(nbmin,nbmax,mem)
  integer, intent(in) :: nbmin, nbmax
  real, intent(out) :: mem
  integer :: dim
  complex :: a

  dim = 0
  mem = 0.0
  
  dim = nbmax-nbmin+1
  
  call memory_matrix(a,dim,dim,mem)
end subroutine memory_sigma

subroutine memory_QP(nbmin,nbmax,nk,mem)
  integer, intent(in) :: nbmin, nbmax, nk
  real, intent(out) :: mem
  integer :: dim
  real :: mem_energies, mem_eigenvec
  real :: a
  complex :: b

  dim = 0
  mem_energies = 0.0
  mem_eigenvec = 0.0
  mem = 0.0
  
  dim = nbmax-nbmin+1

  call memory_matrix(a,dim,nk,mem_energies)
  call memory_matrix(b,dim,dim,nk,mem_eigenvec)
  
  mem = mem_energies + mem_eigenvec
end subroutine memory_QP

subroutine memory_diag(nbmin,nbmax,mem,evonly)
  integer, intent(in) :: nbmin, nbmax
  logical, intent(in) :: evonly
  real, intent(out) :: mem
  integer :: dim, cc
  real :: mem_w, mem_rw, mem_a_tmp
  real :: a
  complex :: b
  
  dim = 0
  mem_w = 0.0
  mem_rw = 0.0
  mem_a_tmp = 0.0
  mem = 0.0
  
  dim = nbmax-nbmin+1
  
  cc = 2*dim-1
  call memory_matrix(b,cc,dim,mem_w)
  
  cc = max(1,3*dim-2)
  call memory_matrix(a,cc,mem_rw)
  
  if(evonly) then
    call memory_matrix(b,nbmax,nbmax,mem_a_tmp)
    else 
      mem_a_tmp = 0.0
  end if
  mem = mem_w + mem_rw + mem_a_tmp
end subroutine memory_diag

subroutine memory_basis(a,b,k,cutoff,mem)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: k(3)
  real, intent(in) :: cutoff
  real, intent(out) :: mem
  integer :: i, npw
  integer :: ii

  npw = 0
  mem = 0.0
  
  call calc_npw(a,b,k,cutoff,npw)
  i = 3
  call memory_matrix(ii,npw,i,mem)
end subroutine memory_basis

subroutine memory_wfc6d(a,b,k1,k2,cutoff,mem)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: k1(3), k2(3)
  real, intent(in) :: cutoff
  real, intent(out) :: mem
  integer :: npw1, npw2
  complex :: cc

  npw1 = 0
  npw2 = 0
  mem = 0.0

  call calc_npw(a,b,k1,cutoff,npw1)
  call calc_npw(a,b,k2,cutoff,npw2)
  call memory_matrix(cc,npw1,npw2,mem)
end subroutine memory_wfc6d

subroutine memory_wfc(a,b,k,cutoff,mem)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: k(3)
  real, intent(in) :: cutoff
  real, intent(out) :: mem
  integer :: npw
  complex :: cc

  npw = 0
  mem = 0.0

  call calc_npw(a,b,k,cutoff,npw)
  call memory_matrix(cc,npw,mem)
end subroutine memory_wfc
                                                
subroutine memory_w(qmesh,a,b,nomega,cutoff,mem_tot,mem_wfc6d_unit,mem_wfc6d_tot)
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: cutoff
  integer, intent(in) :: nomega
  real, intent(out) :: mem_tot
  real, intent(out) :: mem_wfc6d_tot, mem_wfc6d_unit
  real :: mem_basis, mem_basis_tot, mem_wfc6d
  real :: mem_wfc, mem_wfc_tot
  real :: k(3)
  integer :: i

  mem_basis = 0.0
  mem_basis_tot = 0.0
  mem_wfc6d = 0.0
  mem_wfc6d_tot = 0.0
  mem_wfc6d_unit = 0.0
  mem_wfc = 0.0
  mem_wfc_tot = 0.0
  mem_tot = 0.0
  
  do i = 1, qmesh%nkbz
   k(:) = qmesh%kbz(:,i)
   call memory_basis(a,b,k,cutoff,mem_basis)
   mem_basis_tot = mem_basis_tot + mem_basis*qmesh%nkbz
   call memory_wfc6d(a,b,k,k,cutoff,mem_wfc6d)
   mem_wfc6d_tot = mem_wfc6d_tot + mem_wfc6d
   call memory_wfc(a,b,k,cutoff,mem_wfc)
   mem_wfc_tot = mem_wfc_tot + mem_wfc
  end do
  mem_wfc6d_tot = mem_wfc6d_tot*(nomega+1)
  mem_wfc6d_unit = mem_wfc6d_tot / qmesh%nkbz
  mem_wfc_tot = mem_wfc_tot*3*(nomega+1)
  mem_tot = mem_basis_tot + mem_wfc_tot + mem_wfc_tot
end subroutine memory_w

subroutine memory_w_notalloc(qmesh,a,b,nomega,cutoff,mem_tot)
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: cutoff
  integer, intent(in) :: nomega
  real, intent(out) :: mem_tot
  real :: mem_basis, mem_basis_tot, mem_wfc6d
  real :: mem_wfc, mem_wfc_tot
  real :: k(3)
  integer :: i

  mem_basis = 0.0
  mem_basis_tot = 0.0
  mem_wfc6d = 0.0
  mem_wfc = 0.0
  mem_wfc_tot = 0.0
  mem_tot = 0.0
  k = (/0.0,0.0,0.0/)
  
  call memory_wfc6d(a,b,k,k,cutoff,mem_wfc6d)
 
  do i = 1, qmesh%nkbz
   k(:) = qmesh%kbz(:,i)
   call memory_basis(a,b,k,cutoff,mem_basis)
   mem_basis_tot = mem_basis_tot + mem_basis*qmesh%nkbz
   call memory_wfc(a,b,k,cutoff,mem_wfc)
   mem_wfc_tot = mem_wfc_tot + mem_wfc
  end do

  mem_wfc_tot = mem_wfc_tot*3*(nomega+1)
  mem_tot = mem_basis_tot + mem_wfc_tot + mem_wfc_tot + mem_wfc6d
end subroutine memory_w_notalloc

subroutine memory_pp(qmesh,a,b,cutoff,mem_tot,mem_wfc6d_unit,mem_wfc6d_tot)
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: cutoff
  real, intent(out) :: mem_tot
  real, intent(out) :: mem_wfc6d_tot, mem_wfc6d_unit
  real :: mem_basis, mem_basis_tot, mem_wfc6d
  real :: k(3)
  integer :: i

  mem_basis = 0.0
  mem_basis_tot = 0.0
  mem_wfc6d = 0.0
  mem_wfc6d_tot = 0.0
  mem_wfc6d_unit = 0.0
  mem_tot = 0.0
  
  do i = 1, qmesh%nkbz
   k(:) = qmesh%kbz(:,i)
   call memory_basis(a,b,k,cutoff,mem_basis)
   mem_basis_tot = mem_basis_tot + mem_basis*qmesh%nkbz
   call memory_wfc6d(a,b,k,k,cutoff,mem_wfc6d)
   mem_wfc6d_tot = mem_wfc6d_tot + mem_wfc6d
  end do

  mem_wfc6d_tot = mem_wfc6d_tot*2
  mem_wfc6d_unit = mem_wfc6d_tot / qmesh%nkbz
  mem_tot = mem_basis_tot
end subroutine memory_pp

subroutine memory_polar(qmesh,a,b,nomega,cutoff,mem_tot,mem_wfc6d_unit,mem_wfc6d_tot)
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: cutoff
  integer, intent(in) :: nomega
  real, intent(out) :: mem_tot
  real, intent(out) :: mem_wfc6d_tot, mem_wfc6d_unit
  real :: mem_basis, mem_basis_tot, mem_wfc6d
  real :: mem_wfc, mem_wfc_tot
  real :: k(3)
  integer :: i

  mem_basis = 0.0
  mem_basis_tot = 0.0
  mem_wfc6d = 0.0
  mem_wfc6d_tot = 0.0
  mem_wfc6d_unit = 0.0
  mem_wfc = 0.0
  mem_wfc_tot = 0.0
  mem_tot = 0.0

  do i = 1, qmesh%nkbz
    k(:) = qmesh%kbz(:,i)
    call memory_basis(a,b,k,cutoff,mem_basis)
    mem_basis_tot = mem_basis_tot + mem_basis*qmesh%nkbz
    call memory_wfc6d(a,b,k,k,cutoff,mem_wfc6d)
    mem_wfc6d_tot = mem_wfc6d_tot + mem_wfc6d
    call memory_wfc(a,b,k,cutoff,mem_wfc)
    mem_wfc_tot = mem_wfc_tot + mem_wfc
  end do
    
  mem_wfc6d_tot = mem_wfc6d_tot*(nomega+1)
  mem_wfc6d_unit = mem_wfc6d_tot / qmesh%nkbz  
  mem_wfc_tot = mem_wfc_tot*3*(nomega+1)
  mem_tot = mem_basis_tot + mem_wfc_tot + mem_wfc_tot
end subroutine memory_polar


subroutine memory_states(kmesh,a,b,nomega,nbmin,nbmax,cutoff,mem_tot,mem_wfc_unit,mem_wfc_tot)
  type(pw_kmesh), intent(in) :: kmesh
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: cutoff
  integer, intent(in) :: nomega, nbmin, nbmax
  real, intent(out) :: mem_tot
  real, intent(out) :: mem_wfc_tot, mem_wfc_unit
  real :: mem_basis, mem_basis_tot, mem_wfc, mem_e
  real :: k(3), rr
  integer :: i, dim

  mem_basis = 0.0
  mem_basis_tot = 0.0
  mem_wfc = 0.0
  mem_wfc_unit = 0.0
  mem_wfc_tot = 0.0
  mem_tot = 0.0
  dim = (nbmax-nbmin)+1
  
  do i = 1, kmesh%nkbz
   k(:) = kmesh%kbz(:,i)
   call memory_basis(a,b,k,cutoff,mem_basis)
   mem_basis_tot = mem_basis_tot + mem_basis
   call memory_wfc(a,b,k,cutoff,mem_wfc)
   mem_wfc_tot = mem_wfc_tot + mem_wfc
  end do

  mem_wfc_tot = mem_wfc_tot*dim
  mem_wfc_unit = mem_wfc_tot / dim
  call memory_matrix(rr,dim,kmesh%nkbz,mem_e)
  mem_tot = mem_basis_tot + mem_e + mem_e + mem_e
end subroutine memory_states

subroutine memory_density(a,b,cutoff,mem_tot)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: cutoff
  real, intent(out) :: mem_tot
  real :: mem_basis, mem_wfc
  real :: k(3), rr
  integer :: i

  mem_basis = 0.0
  mem_wfc = 0.0
  mem_tot = 0.0


  k = (/0.0,0.0,0.0/)
  call memory_basis(a,b,k,cutoff,mem_basis)
  call memory_wfc(a,b,k,cutoff,mem_wfc)
  mem_tot = mem_basis + mem_wfc                                                   
end subroutine memory_density

subroutine memory_parall_matrix(states,emin,emax,mem_pm_unit,mem_pm_tot,ntrans)
  type(pw_states), intent(in) :: states
  real, intent(in) :: emin, emax
  real, intent(out) :: mem_pm_unit, mem_pm_tot
  integer :: nk, ik1, ik2, ib1, ib2
  integer, intent(out) :: ntrans
  complex :: cc

  mem_pm_unit = 0.0
  mem_pm_tot = 0.0
  ntrans = 0
  
  nk = ubound(states%wfc,2)
  do ik1 = 1,nk
    do ik2 = 1,nk
      do ib1 = states%nbmin,states%nbmax
        do ib2 = states%nbmin , states%nbmax
          if(check(ib1,ib2,ik1,ik2,states)) ntrans = ntrans + 1
        end do
      end do
    end do
  end do
  call memory_matrix(cc,ntrans,mem_pm_unit)
  mem_pm_tot = mem_pm_unit*ntrans

  contains
  function check(ib1,ib2,ik1,ik2,states)
    logical :: check
    integer, intent(in) :: ib1,ib2,ik1,ik2
    type(pw_states), intent(in) :: states
    check = .true.
    check = ( (abs(states%e(ib2,ik2)-states%e(ib1,ik1)) < emax).and. &
        &   (abs(states%e(ib2,ik2)-states%e(ib1,ik1)) > emin).and. &
        &   ((states%occupation(ib1,ik1)-1.0) < 0.0).and. &
        &   ((states%occupation(ib2,ik2)-1.0) > 0.0).and.(ik1 == ik2) )
  end function check
end subroutine memory_parall_matrix

subroutine memory_bse(states,emin,emax,mem_bse,mem_bse_unit,mem_bse_tot)
  type(pw_states), intent(in) :: states
  real, intent(in) :: emin, emax
  real, intent(out) :: mem_bse, mem_bse_unit, mem_bse_tot
  integer :: ntrans
  real :: rr

  call memory_parall_matrix(states,emin,emax,mem_bse_unit,mem_bse_tot,ntrans)
  call memory_matrix(rr,ntrans,mem_bse)
  
end subroutine memory_bse 

subroutine memory_calc_sigma(a,b,k1,k2,cutoff,nbmin,nbmax,mem)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: k1(3),k2(3)
  real, intent(in) :: cutoff
  integer, intent(in) :: nbmin, nbmax
  real, intent(out) :: mem
  real :: q(3)
  real :: mem_wfc, mem_field
  integer :: dim
  
  dim = nbmax - nbmin + 1
  q(:) = k2(:) - k1(:)
  call memory_wfc(a,b,q,cutoff,mem_wfc)
  call memory_field(a,b,k1,k2,cutoff,mem_field)

  mem = (mem_wfc + mem_field)*dim
end subroutine memory_calc_sigma

subroutine memory_field(a,b,k1,k2,cutoff,mem)
  real, intent(in) :: a(3,3), b(3,3)
  real, intent(in) :: k1(3), k2(3)
  real, intent(in) :: cutoff
  real, intent(out) :: mem
  integer :: gmax1(3), gmax2(3), gmax_dip(3)
  integer :: gmin1(3), gmin2(3), gmin_dip(3)
  integer :: dim(3)
  real :: q(3)
  integer :: n
  complex :: cc

  mem = 0.0
  q(:) = k2(:) - k1(:)
  
  call calc_gmax(a,b,k1,cutoff,gmax1)
  call calc_gmax(a,b,k2,cutoff,gmax2)
  call calc_gmax(a,b,q,cutoff,gmax_dip)
  call calc_gmin(a,b,k1,cutoff,gmin1)
  call calc_gmin(a,b,k2,cutoff,gmin2)
  call calc_gmin(a,b,q,cutoff,gmin_dip)
  
  dim(1) = max(gmax1(1)-gmin2(1)-gmin_dip(1),-gmin1(1)+gmax2(1)+gmax_dip(1))
  dim(2) = max(gmax1(2)-gmin2(2)-gmin_dip(2),-gmin1(2)+gmax2(2)+gmax_dip(2))
  dim(3) = max(gmax1(3)-gmin2(3)-gmin_dip(3),-gmin1(3)+gmax2(3)+gmax_dip(3))
  
  dim(1) = ffti_good_order(dim(1))
  dim(2) = ffti_good_order(dim(2))
  dim(3) = ffti_good_order(dim(3))
  
  n = (dim(1)-1) + (dim(2)-1)*dim(1) + (dim(3)-1)*dim(1)*dim(2) + 1

  call memory_matrix(cc,n,mem)
end subroutine memory_field
 
end module memory_module
