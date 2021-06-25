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

module pw_dipole_module
implicit none

private

public :: pw_dipole, pw_vxc_calc, pw_vloc_apply
public :: pw_calc_dipole_elements

interface pw_dipole
  module procedure pw_dipole000
  module procedure pw_dipole211
end interface

contains

subroutine pw_vxc_calc(vxc,density,opt)
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  type (pw_wfc), intent(inout) :: vxc
  type (pw_wfc), intent(in)    :: density
  character(*),  intent(in)    :: opt
  type(pw_field) :: rdensity,rvxc
  integer :: dim(3)

  dim = max(pw_field_dim_from_basis(vxc%basis), &
            pw_field_dim_from_basis(density%basis))

  call pw_field_init(rdensity,density%basis%struct)
  call pw_field_set_dim(rdensity,dim, &
                        k=(/0.0,0.0,0.0/),r0=(/0.0,0.0,0.0/))
  call pw_field_init(rvxc,density%basis%struct)
  call pw_field_set_dim(rvxc,dim, &
                        k=(/0.0,0.0,0.0/),r0=(/0.0,0.0,0.0/))

  call pw_wfc2field(rdensity,density)

  call pw_field_calc_vxc(rvxc,rdensity,trim(opt))

  call pw_field2wfc(vxc,rvxc)

  call pw_field_destroy(rdensity)
  call pw_field_destroy(rvxc)

end subroutine pw_vxc_calc

subroutine pw_vloc_apply(wfc_new,vloc,wfc)
  use pw_wfc_module
  type (pw_wfc), intent(inout) :: wfc_new
  type (pw_wfc), intent(in)    :: vloc
  type (pw_wfc), intent(in)    :: wfc

  call pw_dipole(wfc_new,vloc,wfc)

end subroutine pw_vloc_apply



#if 0
subroutine pw_dipole000(wfc_dip,wfc1,wfc2)
  use pw_wfc_module
  type (pw_wfc), intent(inout) :: wfc_dip
  type (pw_wfc), intent(in)    :: wfc1,wfc2

  type (pw_wfc) :: wfc1_tmp(1),wfc2_tmp(1),wfc_dip_tmp(1,1)

  call pw_wfc_init(wfc1_tmp(1),wfc1%basis)
  call pw_wfc_init(wfc2_tmp(1),wfc2%basis)
  call pw_wfc_init(wfc_dip_tmp(1,1),wfc_dip%basis)

  wfc1_tmp(1) = wfc1
  wfc2_tmp(1) = wfc2
  call pw_dipole(wfc_dip_tmp,wfc1_tmp,wfc2_tmp)
  wfc_dip = wfc_dip_tmp(1,1)

  call pw_wfc_destroy(wfc1_tmp(1))
  call pw_wfc_destroy(wfc2_tmp(1))
  call pw_wfc_destroy(wfc_dip_tmp(1,1))
end subroutine pw_dipole000
#endif


subroutine pw_dipole000(wfc_dip,wfc1,wfc2)
  use ffti_module, only : ffti_good_order
  use num_module
  use pw_fft_module
  use pw_struct_module
  use pw_basis_module
  use pw_wfc_module
  use pw_field_module
  type (pw_wfc), intent(inout) :: wfc_dip
  type (pw_wfc), intent(in)    :: wfc1,wfc2

  type (pw_field) :: field_dip
  type (pw_field) :: field_1, field_2

  type (pw_struct), pointer :: struct

  integer :: n1,n2,dim(3),i1,i2
  integer :: gmin1(3),gmax1(3),gmin2(3),gmax2(3),gmin_dip(3),gmax_dip(3)
  real :: q(3)

  q = wfc2%basis%k - wfc1%basis%k

  struct => wfc_dip%basis%struct

  call pw_field_init(field_1,struct)
  call pw_field_init(field_2,struct)
  call pw_field_init(field_dip,struct)

  gmin1 = wfc1%basis%gmin
  gmin2 = wfc2%basis%gmin
  gmax1 = wfc1%basis%gmax
  gmax2 = wfc2%basis%gmax
  gmin_dip = wfc_dip%basis%gmin
  gmax_dip = wfc_dip%basis%gmax

  dim(1) = max(gmax1(1)-gmin2(1)-gmin_dip(1),-gmin1(1)+gmax2(1)+gmax_dip(1))
  dim(2) = max(gmax1(2)-gmin2(2)-gmin_dip(2),-gmin1(2)+gmax2(2)+gmax_dip(2))
  dim(3) = max(gmax1(3)-gmin2(3)-gmin_dip(3),-gmin1(3)+gmax2(3)+gmax_dip(3))

!  dim = num_fft_next_allowed(dim)
  dim(1) = ffti_good_order(dim(1))
  dim(2) = ffti_good_order(dim(2))
  dim(3) = ffti_good_order(dim(3))
  call pw_field_set_dim(field_1,dim,k=wfc1%basis%k,r0=(/0.0,0.0,0.0/))
  call pw_field_set_dim(field_2,dim,k=wfc2%basis%k,r0=(/0.0,0.0,0.0/))
  call pw_field_set_dim(field_dip,dim,k=q,r0=(/0.0,0.0,0.0/))

  call pw_wfc2field(field_1,wfc1)
  call pw_wfc2field(field_2,wfc2)
  call pw_field_mul(field_dip,field_1,field_2)
  call pw_field2wfc(wfc_dip,field_dip)

  call pw_field_destroy(field_dip)
  call pw_field_destroy(field_2)
  call pw_field_destroy(field_1)


end subroutine pw_dipole000



subroutine pw_dipole211(wfc_dip,wfc1,wfc2)
  use ffti_module, only : ffti_good_order
  use num_module
  use pw_fft_module
  use pw_struct_module
  use pw_basis_module
  use pw_wfc_module
  use pw_field_module
  type (pw_wfc), intent(inout) :: wfc_dip(:,:)
  type (pw_wfc), intent(in)    :: wfc1(:),wfc2(:)

  type (pw_field) :: field_dip
  type (pw_field) :: field_1(size(wfc1)), field_2(size(wfc2))

  type (pw_struct), pointer :: struct

  integer :: n1,n2,dim(3),i1,i2
  integer :: gmin1(3),gmax1(3),gmin2(3),gmax2(3),gmin_dip(3),gmax_dip(3)
  real :: q(3)

  n1 = size(wfc1)
  n2 = size(wfc2)

  if(n1/=ubound(wfc_dip,1)) ERROR("")
  if(n2/=ubound(wfc_dip,2)) ERROR("")

  q = wfc2(1)%basis%k - wfc1(1)%basis%k

  if(n1==0 .or. n2==0) return

  struct => wfc_dip(1,1)%basis%struct


  call pw_field_init(field_1(:),struct)
  call pw_field_init(field_2(:),struct)
  call pw_field_init(field_dip,struct)

  gmin1 = wfc1(1)%basis%gmin
  gmin2 = wfc2(1)%basis%gmin
  gmax1 = wfc1(1)%basis%gmax
  gmax2 = wfc2(1)%basis%gmax
  gmin_dip = wfc_dip(1,1)%basis%gmin
  gmax_dip = wfc_dip(1,1)%basis%gmax

  dim(1) = max(gmax1(1)-gmin2(1)-gmin_dip(1),-gmin1(1)+gmax2(1)+gmax_dip(1))
  dim(2) = max(gmax1(2)-gmin2(2)-gmin_dip(2),-gmin1(2)+gmax2(2)+gmax_dip(2))
  dim(3) = max(gmax1(3)-gmin2(3)-gmin_dip(3),-gmin1(3)+gmax2(3)+gmax_dip(3))

!  dim = num_fft_next_allowed(dim)
  dim(1) = ffti_good_order(dim(1))
  dim(2) = ffti_good_order(dim(2))
  dim(3) = ffti_good_order(dim(3))
  call pw_field_set_dim(field_1,dim,wfc1(1)%basis%k,(/0.0,0.0,0.0/))
  call pw_field_set_dim(field_2,dim,wfc2(1)%basis%k,(/0.0,0.0,0.0/))
  call pw_field_set_dim(field_dip,dim,k=q,r0=(/0.0,0.0,0.0/))

  do i1=1,n1
    call pw_wfc2field(field_1(i1),wfc1(i1))
  end do
  do i2=1,n2
    call pw_wfc2field(field_2(i2),wfc2(i2))
  end do
  do i2=1,n2
    do i1=1,n1
      call pw_field_mul(field_dip,field_1(i1),field_2(i2))
      call pw_field2wfc(wfc_dip(i1,i2),field_dip)
    end do
  end do

  call pw_field_destroy(field_dip)
  call pw_field_destroy(field_2)
  call pw_field_destroy(field_1)


end subroutine pw_dipole211


subroutine pw_calc_dipole_elements(nbmin,nbmax,qmesh,struct,states,cutoff,lrestart)
  use iotk_module
  use pw_states_module
  use pw_field_module
  use pw_wfc_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_struct_module
  use num_module
  use pw_fft_module
  use tools_module

! This routine compute and write on file the dipole elements Fnn'
! just for gamma point calculations 
 
  implicit none

  integer,            intent(in) :: nbmin, nbmax
  type(pw_kmesh),     intent(in) :: qmesh
  type(pw_struct),    intent(in) :: struct
  type(pw_states), intent(in) :: states
  real,               intent(in) :: cutoff
  logical,            intent(in) :: lrestart

  type(pw_wfc), pointer :: wfc1, wfc2
  type(pw_wfc) :: F_wfc1
  type(pw_field) :: wfc_field1, wfc_field2, F_field1
  type(pw_basis) :: basis_field

  integer :: ib1restart, ib2restart
  integer :: nbmin_restart, nbmax_restart
  integer :: unit

  integer :: ib1,ik1,ib2,ik2, id0 
  integer :: dim(3)
  
  real:: k1(3), k0(3)


  call tools_log("Dipole elements")

  call tools_log("Writing dipole elements on directory dipole_elements ...",advance=.false.)

  unit=603

  ib1restart=0
  ib2restart=0

  if(lrestart) then
     open(unit, file="nbnd.restart", status="old")
     read(unit,*) nbmin_restart, nbmax_restart
     close(unit)
  endif

  open(unit, file="nbnd.restart", status="unknown")
  write(unit,*) nbmin, nbmax

  do ib1=nbmin,nbmax
     do ib2=nbmin,nbmax

        if(ib1<ib1restart.and.ib2<ib2restart.and.ib1>nbmin_restart.and.ib2>nbmin_restart) cycle

           call iotk_open_write(unit,file="dipole_elements/field"//tools_char(ib1,3)//tools_char(ib2,3), &
                binary=.true.)

           k1(:)=0.0

           k0(:)=0.0

           ik1=pw_kmesh_kbz_index(qmesh,(k1))

           id0=pw_kmesh_kbz_index(qmesh,(k0))

           call pw_field_init(wfc_field1,struct)
           call pw_field_init(wfc_field2,struct)
           call pw_field_init(F_field1,struct)

           call pw_basis_init(basis_field,struct)
           call pw_basis_create(basis_field,qmesh%kbz(:,id0),cutoff)
           dim=pw_field_dim_from_dipole(basis_field,states%basis(ik1),states%basis(ik1))
           call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(wfc_field2,dim,k=k1,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

           call pw_wfc_init(F_wfc1,basis_field)

           call pw_states_borrow_wfc(states,wfc1,ib1,ik1)
           call pw_states_borrow_wfc(states,wfc2,ib2,ik1)
           call pw_wfc2field(wfc_field1,wfc1)
           call pw_wfc2field(wfc_field2,wfc2)
           call pw_states_giveback_wfc(states,wfc1)
           call pw_states_giveback_wfc(states,wfc2)

           call pw_field_mul(F_field1,wfc_field2,wfc_field1)
           call pw_field2wfc(F_wfc1,F_field1)

           call iotk_write_dat(unit,"field"//tools_char(ib1,3)//tools_char(ib2,3),F_wfc1%val)

           call pw_field_destroy(wfc_field1)
           call pw_field_destroy(wfc_field2)
           call pw_field_destroy(F_field1)
           call pw_basis_destroy(basis_field)
           call pw_wfc_destroy(F_wfc1)

           call iotk_close_write(unit)
           open(unit,file="dipole.restart",status="unknown")
           write(unit,*) ib1, ib2
           close(unit)
        enddo
     enddo

     call tools_log("done")

end subroutine pw_calc_dipole_elements

end module pw_dipole_module
