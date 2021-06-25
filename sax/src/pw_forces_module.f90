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

module pw_forces_module
use pw_states_module
use pw_atoms_module

implicit none
private

public :: pw_forces
public :: pw_forces_loc, pw_forces_nloc, pw_forces_ew
public :: pw_dft_forces
public :: pw_forces_init, pw_forces_destroy
public :: pw_wfc_expansion

type pw_forces
   complex, pointer :: val(:,:)
   type(pw_atoms), pointer :: atoms
   type(pw_states), pointer :: states
end type pw_forces


contains

subroutine pw_forces_init(forces,atoms,states)
  use tools_module
  use num_module
  use pw_atoms_module
  use pw_states_module
  
  implicit none

  type(pw_forces), intent(inout) :: forces
  type(pw_atoms), target, intent(in) :: atoms
  type(pw_states), target, intent(in) :: states

  forces%states => states
  forces%atoms => atoms

  allocate(forces%val(1:3,1:atoms%natoms))
  

  forces%val(:,:)=(0.0,0.0)
end subroutine pw_forces_init

subroutine pw_forces_destroy(forces)
 use tools_module
 use num_module

 implicit none

 type(pw_forces), intent(inout) :: forces

 deallocate(forces%val)
 nullify(forces%states)
 nullify(forces%atoms)
end subroutine pw_forces_destroy

subroutine pw_wfc_expansion(wfc_expansion,states,atoms,ib1,ik1)
  use tools_module
  use num_module
  use pw_module
  use pw_bse_type
  use pw_states_module

! calculation of sum_n <G/nk'> <nk'/deltaH/ck>/(e_ck-e_nk')

  implicit none

  type(pw_states), intent(in) :: states
  type(pw_atoms), intent(in) :: atoms
  integer :: ib1,ik1
  type(pw_wfc),  intent(inout) :: wfc_expansion(3,atoms%natoms)

  type(pw_wfc), pointer :: wfc_tmp
  complex :: coefficient(3,atoms%natoms)

  integer :: ik, nbmax, nbmin, nkmax, nkmin
  integer :: iatom, i, ib

! just for calculation at gamma by now
  	
  nbmax=ubound(states%wfc,1)
  nbmin=lbound(states%wfc,1)
  
  coefficient(:,:)=(0.0,0.0)
  ik=ik1
  do ib=nbmin,nbmax
     if(ib==ib1) cycle
     call pw_wfc_expansion_coefficient(coefficient,states,atoms,ib1,ik1,ib,ik)
     call pw_states_borrow_wfc(states,wfc_tmp) 
     do iatom=1,atoms%natoms   
        do i=1,3
           wfc_expansion(i,iatom)%val(:)=wfc_expansion(i,iatom)%val(:)+wfc_tmp%val(:)*coefficient(i,iatom)
        enddo
     enddo
  enddo

  call pw_states_giveback_wfc(states,wfc_tmp)

end subroutine pw_wfc_expansion

subroutine pw_wfc_expansion_coefficient(coefficient,states,atoms,ib1,ik1,ibn,ikn)
  use tools_module
  use num_module
  use pw_module
  use pw_bse_type
  use pw_states_module

! calculation of <nk'/deltaH/ck>/(e_ck-e_nk')

  implicit none

  type(pw_states), intent(in) :: states
  type(pw_atoms), intent(in) :: atoms
  integer, intent(in) :: ib1,ik1,ibn,ikn
  complex, intent(inout) :: coefficient(3,atoms%natoms)

  type(pw_wfcb) :: vloc
  type(pw_vnloc) :: vnloc
  
  type(pw_wfc), pointer :: wfc_tmp1, wfc_tmp
  type(pw_basis), pointer :: basis_tmp

  real :: k(3), k1(3), k2(3)
  integer :: ik
  integer :: iatom, i

  complex :: f_loc(3), f_nloc(3)
  real :: cutoff_vloc, cutoff_states

  if(ik1.ne.ikn) ERROR(" ik1/=ikn, forces conserve translational symmetry")

  cutoff_states=pw_states_get_cutoff(states)

  cutoff_vloc=atoms%cutoff_vloc*cutoff_states

  ik=pw_kmesh_kbz_index(states%kmesh,(/0.0,0.0,0.0/))
  k=(/0.0,0.0,0.0/)


  call pw_states_borrow_wfc(states,wfc_tmp1,ib1,ik1)
  call pw_states_borrow_wfc(states,wfc_tmp,ibn,ikn) 
  
  call pw_wfcb_init(vloc,atoms%struct,(/0.0,0.0,0.0/),cutoff_vloc)
  call pw_states_borrow_basis(states,basis_tmp,ik=ik)

  do iatom=1,atoms%natoms
     call pw_vloc_calc(vloc%wfc,atoms,iatom)
     call pw_vnloc_init(vnloc,basis_tmp,atoms,iatom)
        
     coefficient(:,:)=(0.0,0.0)
        
     f_loc(:)=(0.0,0.0)
     f_nloc(:)=(0.0,0.0)
        
     call pw_forces_loc(f_loc,wfc_tmp,vloc,wfc_tmp1,atoms,iatom)
     call pw_forces_nloc(f_nloc,wfc_tmp,vnloc,wfc_tmp1,atoms,iatom)
     do i=1,3
        coefficient(i,iatom)=(f_loc(i)+f_nloc(i))/(states%e(ib1,ik1)-states%e(ibn,ikn))
     enddo
     call pw_vnloc_destroy(vnloc)
  enddo

  call pw_states_giveback_basis(states,basis_tmp)
  call pw_states_giveback_wfc(states,wfc_tmp1)
  call pw_states_giveback_wfc(states,wfc_tmp)

  call pw_wfcb_destroy(vloc)

end subroutine pw_wfc_expansion_coefficient

subroutine pw_bse_forces(bse,w,cutoff,calc_delta_dipole_elements)
  use pw_bse_type
  use pw_bse_interf
  use iotk_module
  use pw_states_module
  use pw_field_module
  use pw_coulomb_module
  use pw_w_type
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_kmesh_module
  use num_module
  use pw_fft_module
  use pw_w_module
  use tools_module
  use pw_states_module
  
  implicit none

  type(pw_bse), intent(inout) :: bse
  type(pw_w), intent(in) :: w
  real, intent(in) :: cutoff
  logical, intent(in) :: calc_delta_dipole_elements

  type(pw_coulomb) :: coulomb
  type(pw_basis) :: basis_coulomb
  type(pw_wfc) :: wfc_coulomb

  type(pw_basis) :: basis_field
  type(pw_field) :: F_field1, F_field2
  type(pw_field) :: wfc_field1, wfc_field2
  type(pw_wfc), pointer :: wfc1, wfc2
  type(pw_wfc) :: F_wfc1, F_wfc2
  type(pw_wfc) :: tmp_wfc, tmp_wfc2

  type wfc6dpointer
     type(pw_wfc6d), pointer :: val
  end type wfc6dpointer

  Type(wfc6dpointer) :: w0

  type(pw_wfc) :: wfc_expansion(3,bse%atoms%natoms)

  integer :: ik1,ik2,idk,nk
  integer :: id0
  real, dimension(3) :: k0
  integer :: ib1, ib2, ib3, ib4
  real, dimension(3) :: k1, k2, k
  integer :: itrans1,itrans2
  integer, dimension(3) :: dim
  integer :: npw_small

  integer :: nbmax

  real :: scale

  integer :: unit, unit2

  ! forces just for calculations at gamma

  unit=600
  unit2=601

  nk=ubound(bse%states%wfc,2)
  nbmax=ubound(bse%states%wfc,1)

  if(bse%spin>1) ERROR("Wrong spin state on BSE, bse_spin=0 or bse_spin=1")

  scale=1.0/bse%states%struct%a_omega

  write(0,*) "from bse_calc, ntrans=",bse%ntrans

  if(calc_delta_dipole_elements) then

     call tools_log("Writing delta dipole elements on directory delta_dipole_elements")
     call tools_log("Pvv'...",advance=.false.)

     do ib1=bse%nbvmin,bse%nbvmax
        do ib2=bse%nbvmin,bse%nbvmax

           call iotk_open_write(unit,file="delta_dipole_elements/field"//tools_char(ib1,3)//tools_char(ib2,3), &
                binary=.true.)

           k1(:)=0.0

           k0(:)=0.0

           ik1=pw_kmesh_kbz_index(w%qmesh,(k1))

           id0=pw_kmesh_kbz_index(w%qmesh,(k0))

           call pw_field_init(wfc_field1,bse%states%struct)
           call pw_field_init(wfc_field2,bse%states%struct)
           call pw_field_init(F_field1,bse%states%struct)

           call pw_basis_init(basis_field,bse%states%struct)
           call pw_basis_create(basis_field,w%qmesh%kbz(:,id0),cutoff)
           dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik1),bse%states%basis(ik1))
           call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(wfc_field2,dim,k=k1,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

           call pw_wfc_init(F_wfc1,basis_field)

!           call pw_states_borrow_wfc(bse%states,wfc1,ib1,ik1)
           call pw_wfc_expansion(wfc_expansion,bse%states,bse%atoms,ib1,ik1)
           call pw_states_borrow_wfc(bse%states,wfc2,ib2,ik1)
           call pw_wfc2field(wfc_field1,wfc1)
           call pw_wfc2field(wfc_field2,wfc2)
           call pw_states_giveback_wfc(bse%states,wfc1)
           call pw_states_giveback_wfc(bse%states,wfc2)

           call pw_field_mul(F_field1,wfc_field2,wfc_field1)
           call pw_field2wfc(F_wfc1,F_field1)

           call iotk_write_dat(unit,"field"//tools_char(ib1,3)//tools_char(ib2,3),F_wfc1%val)

           call pw_field_destroy(wfc_field1)
           call pw_field_destroy(wfc_field2)
           call pw_field_destroy(F_field1)
           call pw_basis_destroy(basis_field)
           call pw_wfc_destroy(F_wfc1)

           call iotk_close_write(unit)
        enddo
     enddo

     call tools_log("done")

     call tools_log("FcvV...",advance=.false.)

     do itrans1=1,bse%ntrans

        ik1=bse%iktrans(itrans1)
        ib1=bse%ib1trans(itrans1)
        ib2=bse%ib2trans(itrans1)

        call iotk_open_write(unit,"dipole_elements/field"//tools_char(ib1,3)//tools_char(ib2,3), &
                binary=.true.)

        k0(:)=0.0

        k1=bse%states%kmesh%kbz(:,ik1)

        id0=pw_kmesh_kbz_index(w%qmesh,(k0))

        call pw_field_init(wfc_field1,bse%states%struct)
        call pw_field_init(wfc_field2,bse%states%struct)
        call pw_field_init(F_field1,bse%states%struct)

        call pw_basis_init(basis_field,bse%states%struct)
        call pw_basis_create(basis_field,w%qmesh%kbz(:,id0),cutoff)
        dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik1),bse%states%basis(ik1))
        call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(wfc_field2,dim,k=k1,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

        call pw_wfc_init(F_wfc1,basis_field)

        call pw_states_borrow_wfc(bse%states,wfc1,ib1,ik1)
        call pw_states_borrow_wfc(bse%states,wfc2,ib2,ik1)
        call pw_wfc2field(wfc_field1,wfc1)
        call pw_wfc2field(wfc_field2,wfc2)
        call pw_states_giveback_wfc(bse%states,wfc1)
        call pw_states_giveback_wfc(bse%states,wfc2)

        call pw_field_mul(F_field1,wfc_field2,wfc_field1)
        call pw_field2wfc(F_wfc1,F_field1)

        call pw_wfc_init(tmp_wfc,basis_field)

        call pw_basis_init(basis_coulomb,bse%states%struct)
        call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,id0),cutoff)
        
!        call pw_coulomb_init(coulomb)
        call pw_wfc_init(wfc_coulomb,basis_coulomb)
        call pw_coulomb_get(0,coulomb,wfc_coulomb,lhalf=.true.)
        call pw_coulomb_destroy(coulomb)

        wfc_coulomb%val(1)=0.0

        tmp_wfc%val=0.0

        npw_small=ubound(tmp_wfc%val,1)

        call num_vemul(tmp_wfc%val,wfc_coulomb%val,F_wfc1%val)

        call iotk_write_dat(unit,"field"//tools_char(ib1,3)//tools_char(ib2,3),tmp_wfc%val)

        call pw_field_destroy(wfc_field1)
        call pw_field_destroy(wfc_field2)
        call pw_field_destroy(F_field1)
        call pw_basis_destroy(basis_field)
        call pw_wfc_destroy(F_wfc1)

        call pw_wfc_destroy(tmp_wfc)
        call pw_wfc_destroy(wfc_coulomb)
        call pw_basis_destroy(basis_coulomb)

        call iotk_close_write(unit)

     enddo

     call tools_log("done")

     call tools_log("Fcc'W...",advance=.false.)

     do ib1=bse%nbcmin,bse%nbcmax
        do ib2=bse%nbcmin,bse%nbcmax

           call iotk_open_write(unit,"dipole_elements/field"//tools_char(ib1,3)//tools_char(ib2,3), &
                binary=.true.)

           k1(:)=0.0

           k0(:)=0.0

           k2(:)=0.0

           ik1=pw_kmesh_kbz_index(w%qmesh,(k1))

           id0=pw_kmesh_kbz_index(w%qmesh,(k0))

           call pw_basis_init(basis_coulomb,bse%states%struct)
           call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,id0),cutoff)
!           call pw_coulomb_init(coulomb)
           call pw_wfc_init(wfc_coulomb,basis_coulomb)
           call pw_coulomb_get(0,coulomb,wfc_coulomb)
           if(id0==w%iqgamma) wfc_coulomb%val(1)=w%v0
           call pw_coulomb_destroy(coulomb)

           call pw_field_init(wfc_field1,bse%states%struct)
           call pw_field_init(wfc_field2,bse%states%struct)
           call pw_field_init(F_field1,bse%states%struct)

           call pw_basis_init(basis_field,bse%states%struct)
           call pw_basis_create(basis_field,w%qmesh%kbz(:,id0),cutoff)
           dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik1),bse%states%basis(ik1))
           call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

           call pw_wfc_init(F_wfc1,basis_field)
           call pw_wfc_init(tmp_wfc,basis_field)

           call pw_states_borrow_wfc(bse%states,wfc1,ib1,ik1)
           call pw_states_borrow_wfc(bse%states,wfc2,ib2,ik1)
           call pw_wfc2field(wfc_field1,wfc1)
           call pw_wfc2field(wfc_field2,wfc2)
           call pw_states_giveback_wfc(bse%states,wfc1)
           call pw_states_giveback_wfc(bse%states,wfc2)

           call pw_w_borrow_wfc6d(w,w0%val,id0,0)

           npw_small=ubound(w0%val%val,1)
           tmp_wfc%val=0.0

           call pw_field_mul(F_field1,wfc_field2,wfc_field1)
           call pw_field2wfc(F_wfc1,F_field1)

           call num_vemul(tmp_wfc%val,wfc_coulomb%val,F_wfc1%val)
           tmp_wfc%val(1:npw_small)=tmp_wfc%val(1:npw_small)+num_matmul(w0%val%val,F_wfc1%val)
           call iotk_write_dat(unit,"field"//tools_char(ib1,3)//tools_char(ib2,3),tmp_wfc%val)

           call pw_field_destroy(wfc_field1)
           call pw_field_destroy(wfc_field2)
           call pw_field_destroy(F_field1)
           call pw_basis_destroy(basis_field)
           call pw_wfc_destroy(F_wfc1)

           call pw_basis_destroy(basis_coulomb)
           call pw_wfc_destroy(wfc_coulomb)
          
           call pw_wfc_destroy(tmp_wfc)

           call pw_w_giveback_wfc6d(w,w0%val)

           call iotk_close_write(unit)
        enddo
     enddo

     call tools_log("done")

  endif
! -------------------------------------------------------------------------

  if(bse%spin==0) then

     call tools_log("Begin fill BSE exchange kernel")

     do itrans1=1,bse%ntrans

        ik1=bse%iktrans(itrans1)
        ib1=bse%ib1trans(itrans1)
        ib2=bse%ib2trans(itrans1)

        call iotk_open_read(unit,file="dipole_elements/field"//tools_char(ib1,3)//tools_char(ib2,3), &
                binary=.true.)
        
        k0(:)=0.0

        k1=bse%states%kmesh%kbz(:,ik1)

        id0=pw_kmesh_kbz_index(w%qmesh,(k0))
        
        call pw_basis_init(basis_field,bse%states%struct)
        call pw_basis_create(basis_field,w%qmesh%kbz(:,id0),cutoff)
        dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik1),bse%states%basis(ik1))

        call pw_wfc_init(tmp_wfc,basis_field)
        
        call pw_wfc_init(F_wfc1,basis_field)

        call iotk_scan_dat(unit,"field"//tools_char(ib1,3)//tools_char(ib2,3),F_wfc1%val)

        call iotk_close_read(unit)

        do itrans2=1,bse%ntrans

           if(itrans2>itrans1) cycle

           ik2=bse%iktrans(itrans2)
           ib3=bse%ib1trans(itrans2)
           ib4=bse%ib2trans(itrans2)

           call iotk_open_read(unit,"dipole_elements/field"//tools_char(ib3,3)//tools_char(ib4,3), &
                binary=.true.)

           k2=bse%states%kmesh%kbz(:,ik2)
           
           call pw_wfc_init(F_wfc2,basis_field)

           call iotk_scan_dat(unit,"field"//tools_char(ib3,3)//tools_char(ib4,3),F_wfc2%val)

           call iotk_close_read(unit)

!           idk=pw_kmesh_kbz_index(w%qmesh,(k1-k2))
!           call pw_basis_init(basis_coulomb,bse%states%struct)
!           call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
!           call pw_coulomb_init(coulomb)
!           call pw_wfc_init(wfc_coulomb,basis_coulomb)
!           call pw_coulomb_get(3,coulomb,wfc_coulomb)
!           call pw_coulomb_destroy(coulomb)

! (g->0)=0 for local fields effects

!           wfc_coulomb%val(1)=0.0

!           tmp_wfc%val=0.0
 
!           npw_small=ubound(tmp_wfc%val,1)
!           call pw_wfc_init(tmp_wfc2,basis_field)
!           tmp_wfc2%val(1:npw_small)=(0.0,0.0)
!           tmp_wfc2%val(1:npw_small)=wfc_coulomb%val(1:npw_small) 
!           call num_vemul(tmp_wfc%val,tmp_wfc2%val,F_wfc1%val)
!           call pw_wfc_destroy(tmp_wfc2)

           bse%matrix%val(itrans1,itrans2)=2.0 * scale*pw_wfc_braket(F_wfc2,F_wfc1)* &
		bse%states%weight(ib3,ik2)*bse%states%weight(ib4,ik2)
           
!           call pw_basis_destroy(basis_coulomb)
!           call pw_wfc_destroy(wfc_coulomb)
           call pw_wfc_destroy(F_wfc2)

        enddo
        call pw_basis_destroy(basis_field)
        call pw_wfc_destroy(F_wfc1)
        call pw_wfc_destroy(tmp_wfc)
     enddo
  endif

  call tools_log ("end fill BSE exchange kernel")

  call tools_log("begin fill BSE direct kernel")

  do itrans2=1,bse%ntrans
     do itrans1=1,bse%ntrans

        if(itrans2>itrans1) cycle

        ik1=bse%iktrans(itrans1)
        ib1=bse%ib1trans(itrans1)
        ib2=bse%ib2trans(itrans1)
        ik2=bse%iktrans(itrans2)
        ib3=bse%ib1trans(itrans2)
        ib4=bse%ib2trans(itrans2)

        call iotk_open_read(unit,"dipole_elements/field"//tools_char(ib1,3)//tools_char(ib3,3), &
                binary=.true.)
        call iotk_open_read(unit2,"dipole_elements/field"//tools_char(ib2,3)//tools_char(ib4,3), &
                binary=.true.)
        
        k1(:)=bse%states%kmesh%kbz(:,ik1)
        k2(:)=bse%states%kmesh%kbz(:,ik2)
        
        idk=pw_kmesh_kbz_index(w%qmesh,(k1-k2))

        call pw_basis_init(basis_field,bse%states%struct)
        call pw_basis_create(basis_field,w%qmesh%kbz(:,idk),cutoff)

        call pw_wfc_init(F_wfc1,basis_field)
        call pw_wfc_init(F_wfc2,basis_field)
        call pw_wfc_init(tmp_wfc,basis_field)

        call iotk_scan_dat(unit,"field"//tools_char(ib1,3)//tools_char(ib3,3),tmp_wfc%val)
        call iotk_scan_dat(unit2,"field"//tools_char(ib2,3)//tools_char(ib4,3),F_wfc2%val)

        call iotk_close_read(unit)
        call iotk_close_read(unit2)


        bse%matrix%val(itrans1,itrans2)=bse%matrix%val(itrans1,itrans2)-scale*pw_wfc_braket(F_wfc2,tmp_wfc) * &
		bse%states%weight(ib3,ik2)*bse%states%weight(ib4,ik2)

        call pw_basis_destroy(basis_field)
        call pw_wfc_destroy(F_wfc1)
        call pw_wfc_destroy(F_wfc2)
        call pw_wfc_destroy(tmp_wfc)

        if(ik1==ik2.and.ib1==ib3.and.ib2==ib4) then
          bse%matrix%val(itrans1,itrans2)=bse%matrix%val(itrans1,itrans2) + &
            (bse%states%e(ib1,ik1)-bse%states%e(ib2,ik2))
        endif

     enddo
  enddo

end subroutine pw_bse_forces

subroutine pw_dft_forces(states,atoms,forces)
  use tools_module
  use num_module
  use pw_module
  use ptk_module
  use pw_states_module

  implicit none

  type(pw_atoms), intent(in) :: atoms
  type(pw_states), intent(in) :: states
  type(pw_forces), intent(inout) :: forces

  integer :: nkmin,nkmax,nbmin,nbmax  
  integer :: ik,ib1,iatom

  type(pw_wfc), pointer :: wfc1
  type(pw_wfcb) :: vloc
  type(pw_basis), pointer :: basis_tmp
  type(pw_vnloc) :: vnloc
  complex :: f_loc(3)
  complex :: f_nloc(3)
  complex :: f_ewald(3)
  complex :: sum(1:3)
  complex :: f_loctot(3),f_nloctot(3)
  real :: weight(1,2)
  real :: cutoff_vloc, cutoff_states

  cutoff_states = pw_states_get_cutoff(states)
  cutoff_vloc = atoms%cutoff_vloc*cutoff_states
  
  
  nkmin=lbound(states%wfc,2)
  nkmax=ubound(states%wfc,2)
  nbmin=lbound(states%wfc,1)
  nbmax=ubound(states%wfc,1)

  forces%val(:,:)=(0.0,0.0)

  call pw_wfcb_init(vloc,atoms%struct,(/0.0,0.0,0.0/),cutoff_vloc)

  do iatom=1,atoms%natoms
     write(0,*)"iatom", iatom
     call pw_vloc_calc(vloc%wfc,atoms,iatom)
     f_ewald(:)=(0.0,0.0)
     f_nloctot(:)=(0.0,0.0)
     f_loctot(:)=(0.0,0.0)

     do ik=nkmin,nkmax
        call pw_states_borrow_basis(states,basis_tmp,ik=ik)
        call pw_vnloc_init(vnloc,basis_tmp,atoms,iatom)
        do ib1=nbmin,nbmax
           if(states%occupation(ib1,ik)<1.0) cycle
           if(pw_states_is_local(states,ib1,ik)) then
              f_loc(:)=(0.0,0.0)
              f_nloc(:)=(0.0,0.0)
              weight(1,1)=sqrt(states%occupation(ib1,ik)*states%weight(ib1,ik))
              weight(1,2)=sqrt(states%occupation(ib1,ik)*states%weight(ib1,ik))
              call pw_states_borrow_wfc(states,wfc1,ib1,ik)
              call pw_forces_loc(f_loc,wfc1,vloc,wfc1,atoms,iatom)
              f_loctot(:)=f_loctot(:)+f_loc(:)*weight(1,1)*weight(1,2)
              call pw_forces_nloc(f_nloc,wfc1,vnloc,wfc1,atoms,iatom)
              f_nloctot(:)=f_nloctot(:)+f_nloc(:)*weight(1,1)*weight(1,2)
              call pw_states_giveback_wfc(states,wfc1)
              forces%val(:,iatom)=forces%val(:,iatom)+f_loc(:)*weight(1,1)*weight(1,2)+ &
		   f_nloc(:)*weight(1,1)*weight(1,2)
          endif
        enddo
        call pw_vnloc_destroy(vnloc)
        call pw_states_giveback_basis(states,basis_tmp)
     enddo
        call pw_forces_ew(f_ewald,atoms,iatom)
        write(0,*)"ew contrib", real(f_ewald(1)), real(f_ewald(2)), real(f_ewald(3))
        write(0,*)"lc contrib", real(f_loctot(1)), real(f_loctot(2)), real(f_loctot(3))
        write(0,*)"nlc contrib", real(f_nloctot(1)), real(f_nloctot(2)), real(f_nloctot(3))
        forces%val(:,iatom)=forces%val(:,iatom)+f_ewald(:)
  enddo

  call pw_wfcb_destroy(vloc)
! no net force
  sum(:)=(0.0,0.0)
  do iatom=1,atoms%natoms
     sum(:)=sum(:)+forces%val(:,iatom)
  enddo
  do iatom=1,atoms%natoms	
     forces%val(:,iatom)=forces%val(:,iatom)-sum(:)/atoms%natoms
  enddo

  call ptk_allreduce_inplace(forces%val,comm=states%comm,op=ptk_sum)
end subroutine pw_dft_forces

subroutine pw_forces_loc(f_loc,wfc2,vloc,wfc1,atoms,iatom)
  use tools_module
  use num_module
  use pw_module

  type (pw_wfc), intent(in) :: wfc1,wfc2
  type (pw_wfcb), intent(in) :: vloc
  type(pw_atoms), intent(in) :: atoms
  integer, intent(in) :: iatom
  complex, intent(inout) :: f_loc(3)

  type(pw_field) :: wfc_field1, wfc_field2, dip_field
  type(pw_wfc) :: dip_wfc
  real :: b(3,3)
  real :: g_au(3)
  real :: k1(3),k2(3), k(3)
  real :: modk2
  integer :: npw, ipw,i
  integer :: dim(3)

  complex :: v_loc
  complex :: vlocbis
  type(pw_wfc) :: wfc_tmp
  complex :: flocbis(3)
  type(pw_wfc) :: g_vloc(3)

  f_loc(:)=(0.0,0.0)
  v_loc=(0.0,0.0)
  vlocbis=(0.0,0.0)
  flocbis(:)=(0.0,0.0)

  npw = vloc%basis%npw
  b = vloc%basis%struct%b

  k1(:)=wfc1%basis%k(:)
  k2(:)=wfc2%basis%k(:)
  k(:)=k1(:)-k2(:)

  modk2=k(1)**2+k(2)**2+k(3)**2

  if(modk2>(0.00000001)) ERROR(" ")

!  call pw_field_init(dip_field,vloc%basis%struct)
!  call pw_field_init(wfc_field1,vloc%basis%struct)
!  call pw_field_init(wfc_field2,vloc%basis%struct)
!  dim=pw_field_dim_from_dipole(vloc%basis,wfc1%basis,wfc2%basis)
!  call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
!  call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
!  call pw_field_set_dim(dip_field,dim,k=k2-k1,r0=(/0.0,0.0,0.0/))

!  call pw_wfc_init(dip_wfc,vloc%basis)

!  call pw_wfc2field(wfc_field1,wfc1)
!  call pw_wfc2field(wfc_field2,wfc2)

!  call pw_field_mul(dip_field,wfc_field2,wfc_field1)

!  call pw_field2wfc(dip_wfc,dip_field)

!  do ipw=1,npw
!     g_au(:)=num_matmul(b,real(vloc%basis%g(:,ipw)))
!     f_loc(:)=f_loc(:)-(0.0,-1.0)*g_au(:)* &
!		dip_wfc%val(ipw)*vloc%wfc%val(ipw)
!     v_loc=v_loc+dip_wfc%val(ipw)*vloc%wfc%val(ipw)
!  enddo

  call pw_wfc_init(wfc_tmp,wfc1%basis)
  do i=1,3
     call pw_wfc_init(g_vloc(i),vloc%basis)
  enddo
  do ipw=1,vloc%basis%npw
     g_au(:)=num_matmul(b,real(vloc%basis%g(:,ipw)))
     do i=1,3
        g_vloc(i)%val(ipw)=(0.0,-1.0)*g_au(i)*vloc%wfc%val(ipw)
     enddo
  enddo
  do i=1,3
     wfc_tmp%val(:)=(0.0,0.0)
     call pw_vloc_apply(wfc_tmp,g_vloc(i),wfc1)
     f_loc(i)=-pw_wfc_braket(wfc2,wfc_tmp)
  enddo

!  call pw_vloc_apply(wfc_tmp,vloc%wfc,wfc1)
!  vlocbis=pw_wfc_braket(wfc2,wfc_tmp)


  call pw_wfc_destroy(wfc_tmp)

!  write(0,*)'vloc',v_loc
!  write(0,*)'vlocbis',vlocbis
!  write(0,*)'flocbis', real(flocbis(1)), real(flocbis(2)), real(flocbis(3))

  call pw_wfc_destroy(g_vloc(:))

!  call pw_field_destroy(dip_field)
!  call pw_field_destroy(wfc_field1)
!  call pw_field_destroy(wfc_field2)
!  call pw_wfc_destroy(dip_wfc)

end subroutine pw_forces_loc

subroutine pw_forces_nloc(f_nloc,wfc2,vnloc,wfc1,atoms,iatom)
  use pw_module
  use tools_module
  use num_module

  implicit none

  complex, intent(inout) :: f_nloc(3)
  type(pw_wfc), intent(in) :: wfc1,wfc2
  type(pw_atoms), intent(in) :: atoms
  integer, intent(in) :: iatom
  type(pw_vnloc), intent(in) :: vnloc

  real :: b(3,3)
  real :: pos(3),q_au(3)
  complex :: q(3)
  real :: a_omega 
  integer :: ik 
  integer :: npw, ipw, jpw
  integer :: iproj
  integer :: i
  type(pw_wfc) :: vkb1(1:3,1:vnloc%nproj)
  complex :: sum1, sum2, sum3, sum4
  type(pw_wfc) :: wfc_tmp(1:vnloc%nproj), wfc_new
  complex :: dprojection(1:3,1:vnloc%nproj)


  f_nloc(:)=(0.0,0.0)

  write(0,*) "from pw_forces_nloc, npw", vnloc%basis%npw

  npw = vnloc%basis%npw
  b = vnloc%basis%struct%b
  a_omega=vnloc%basis%struct%a_omega
  pos(:) = atoms%positions(:,iatom)

  call pw_wfc_init(vkb1(:,:),vnloc%basis)
  do iproj=1,vnloc%nproj
     do ipw=1,npw
        q_au(:)=num_matmul(b,real(vnloc%basis%g(:,ipw)))
!         q_au(:)=vnloc%basis%g(:,ipw)
        do i=1,3
           vkb1(i,iproj)%val(ipw)=(0.0,-1.0)*q_au(i)* &
              vnloc%proj(iproj)%val(ipw) 
        enddo 
     enddo
     do i=1,3
        dprojection(i,iproj)=pw_wfc_braket(vkb1(i,iproj),wfc1)
        dprojection(i,iproj)=dprojection(i,iproj)*vnloc%d(iproj)/a_omega
     enddo
  enddo

  do i=1,3
     call pw_wfc_init(wfc_tmp(:),vnloc%basis)
     wfc_tmp = vnloc%proj
     call pw_wfc_init(wfc_new,vnloc%basis)
     do iproj=1,vnloc%nproj
        call pw_wfc_scale(wfc_tmp(iproj),dprojection(i,iproj))
     enddo
     call pw_wfc_sum(wfc_new,wfc_tmp)
     f_nloc(i)=-pw_wfc_braket(wfc2,wfc_new)
     call pw_wfc_destroy(wfc_tmp(:))
     call pw_wfc_destroy(wfc_new)
  enddo
  
  call pw_wfc_destroy(vkb1(:,:))

end subroutine pw_forces_nloc

subroutine pw_forces_ew(f_ew,atoms,iatom)
  use num_module
  use pw_coulomb_module
  use pw_basis_module
  use pw_wfc_module
  use pw_atoms_module

  type(pw_atoms), intent(in) :: atoms
  complex, intent(out) :: f_ew(3)
  integer, intent(in) :: iatom

  real, allocatable :: z(:)
  type(pw_coulomb) :: coulomb_l
  integer :: iatom1,iatom2
  type(pw_basis) :: basis
  type(pw_wfc) :: vl
  real :: sigma, cutoff,rmax,dr(3),moddr,dr_a(3)
  integer :: r1max,r2max,r3max,ir1,ir2,ir3,ipw
  complex :: g_au(3)

  allocate(z(atoms%natoms))
  do iatom1=1,atoms%natoms
     z(iatom1) = atoms%pseudo(atoms%type_map(iatom1))%z
  enddo

  f_ew(:) = (0.0,0.0)

  sigma = exp(1.0/3.0*log(atoms%struct%b_omega))
  cutoff = (16.0*sigma)**2 ! cutoff on G^2
  rmax = 20.0/sigma ! cutoff on R
  r1max = rmax*sqrt(sum(atoms%struct%b(:,1)**2))/num_2pi
  r2max = rmax*sqrt(sum(atoms%struct%b(:,2)**2))/num_2pi
  r3max = rmax*sqrt(sum(atoms%struct%b(:,3)**2))/num_2pi

!  call pw_coulomb_init(coulomb_l,iqsigma=+1,qsigma=sigma)

  call pw_basis_init(basis,atoms%struct)
  call pw_basis_create(basis,(/0.0,0.0,0.0/),cutoff)
  call pw_wfc_init(vl,basis)
  call pw_coulomb_get(3,coulomb_l,vl)
 
 
  do iatom2 = 1 , atoms%natoms
     dr(:) = atoms%positions(:,iatom) - atoms%positions(:,iatom2)
     do ipw=1,basis%npw
        g_au(:)=num_matmul(atoms%struct%b,real(basis%g(:,ipw)))
        f_ew(:) = f_ew(:) + (0.0,1.0)*g_au(:)*z(iatom) * z(iatom2) * &
             vl%val(ipw) * exp(-num_2pi_i * dot_product(real(basis%g(:,ipw)),dr))  / atoms%struct%a_omega
     end do
  end do

  call pw_wfc_destroy(vl)
  call pw_basis_destroy(basis)
 
  do iatom2 = 1 , atoms%natoms
     if(iatom2==iatom) cycle
     do ir1=-r1max,+r1max
        do ir2=-r2max,+r2max
           do ir3=-r3max,+r3max
              dr = atoms%positions(:,iatom) + (/ir1,ir2,ir3/) - atoms%positions(:,iatom2)
              moddr = sqrt(sum(num_matmul(atoms%struct%a,dr)**2))
              dr_a(:) = num_matmul(atoms%struct%a,dr)
!              if(moddr < 0.0000001) cycle
              f_ew(:) = f_ew(:) + (dr_a(:)*2.0/moddr**2)*z(iatom)*z(iatom2)*(num_erfc(sigma*moddr/num_sqrt2)/moddr+ &
                   sigma/num_sqrt2*sqrt(8.0/num_2pi)*exp(-(sigma**2/2.0*moddr**2)))
           end do
        end do
     end do
  end do

  call pw_coulomb_destroy(coulomb_l)
  deallocate(z)  
end subroutine pw_forces_ew

end module pw_forces_module
