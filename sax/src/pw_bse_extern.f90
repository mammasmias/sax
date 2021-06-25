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

subroutine pw_bse_init_x(bse,states,atoms,emin,emax,spin,root,comm,find_trans)
  use pw_bse_type
  use pw_states_type
  use pw_atoms_module
  use ptk_module, only : ptk_comm, ptk_comm_size, ptk_comm_rank
  use pw_parall_matrix_module, only : pw_parall_matrix_init 
  implicit none
  type (pw_bse),   intent(out) :: bse
  type(pw_states), intent(in), target :: states
  type(pw_atoms), intent(in), target :: atoms
  real,            intent(in)  :: emin,emax
  integer, intent(in) :: spin
  integer, intent(in) :: root
  type(ptk_comm), intent(in) :: comm
  logical, optional, intent(in) :: find_trans
  integer :: itrans,ntrans,ib1,ib2,ik1,ik2,nb,nk
  integer :: ib1_old, ib2_old, ik1_old
  logical :: find_trans_loc

  find_trans_loc = .true.
  if(present(find_trans)) find_trans_loc = find_trans

  bse%states => states
  bse%atoms => atoms
  bse%spin=spin
  bse%emax = emax
  bse%emin = emin

  if (find_trans_loc) then
    ntrans = 0
    nb = ubound(bse%states%wfc,1)
    nk = ubound(bse%states%wfc,2)
    do ik1 = 1 , nk
      do ib1 = bse%states%nbmin , bse%states%nbmax
        do ib2 = bse%states%nbmin , bse%states%nbmax
          if(check(ib1,ib2,ik1,ik1,bse)) ntrans = ntrans + 1
        end do
      end do
    end do
    bse%ntrans = ntrans
  end if

  allocate(bse%iktrans(bse%ntrans))
  allocate(bse%ib1trans(bse%ntrans))
  allocate(bse%ib2trans(bse%ntrans))

  if(find_trans_loc) then
    itrans = 0
    do ik1 = 1 , nk
      do ib1 = bse%states%nbmin , bse%states%nbmax
        do ib2 = bse%states%nbmin , bse%states%nbmax
          if(check(ib1,ib2,ik1,ik1,bse)) then
             itrans = itrans + 1
             if(itrans>ntrans) ERROR("")
             bse%iktrans(itrans) = ik1
             bse%ib1trans(itrans) = ib1
             bse%ib2trans(itrans) = ib2
          end if
        end do
      end do
    end do
    if(itrans/=ntrans) ERROR("")
  endif

if(find_trans_loc) then
! determination of bse nbvmax nbvmin nbcmin nbcmax
    ib2_old=0
    ib1_old=0
    ik1_old=0
    do itrans=1,ntrans
       ib1=bse%ib1trans(itrans)
       if(ib1>ib1_old) ib1_old=ib1
       ib2=bse%ib2trans(itrans)
       if(ib2>ib2_old) ib2_old=ib2
       ik1=bse%iktrans(itrans)
       if(ik1>ik1_old) ik1_old=ik1
    enddo
  
    bse%nbcmax=ib1_old
    bse%nbvmax=ib2_old
    bse%nkmax=ik1_old

    ib1_old=100000
    ib2_old=100000
    ik1_old=10000000
    do itrans=1,ntrans
       ib1=bse%ib1trans(itrans)
       if(ib1<ib1_old) ib1_old=ib1
       ib2=bse%ib2trans(itrans)
       if(ib2<ib2_old) ib2_old=ib2
       ik1=bse%iktrans(itrans)
       if(ik1<ik1_old) ik1_old=ik1
    enddo

    bse%nbcmin=ib1_old
    bse%nbvmin=ib2_old
    bse%nkmin=ik1_old
    write(0,*)"minimum and maximum valence band index: ",bse%nbvmin, bse%nbvmax
    write(0,*)"minimum and maximum conduction band index: ", bse%nbcmin, bse%nbcmax
endif
    write(0,*)"number of transitions taken into account: ", bse%ntrans
    write(0,*) "Spin: ", bse%spin
! ------------------------------------------  
! call to the distribution of bse matrix on proc
    call pw_parall_matrix_init(bse%matrix,1,bse%ntrans,root,comm)
! -----------------------------------------

contains 
function check(ib1,ib2,ik1,ik2,bse)
  logical :: check
  integer, intent(in) :: ib1,ib2,ik1,ik2
  type(pw_bse), intent(in) :: bse
  check = (  abs(bse%states%e(ib2,ik2)-bse%states%e(ib1,ik1)) < emax .and. &
             abs(bse%states%e(ib2,ik2)-bse%states%e(ib1,ik1)) > emin .and. &
             (bse%states%occupation(ib1,ik1)-1.0) < 0.0.and.(bse%states%occupation(ib2,ik2)-1.0) > 0.0 .and. &
             ik1 == ik2 ) ! THIS IS FOR Q = 0 !!
! just the resonant positive definite part of the two particle H is taken into account for bse%matrix
! bse%ib1trans(i) is for empty states and bse%ib2trans(i) is for occupied states
end function check
end subroutine pw_bse_init_x

subroutine pw_bse_destroy_x(bse)
  use pw_bse_type
  use pw_parall_matrix_module
  implicit none
  type (pw_bse), intent(inout) :: bse

  call pw_parall_matrix_destroy(bse%matrix)
  deallocate(bse%iktrans)
  deallocate(bse%ib1trans)
  deallocate(bse%ib2trans)
  nullify(bse%states)
  nullify(bse%atoms)
end subroutine pw_bse_destroy_x

subroutine pw_bse_write_x(bse,unit,name)
  use pw_bse_type
  use iotk_module
  use ptk_module, only : ptk_send, ptk_recv
  use pw_parall_matrix_module
  use ptk_module, only : ptk_barrier
  implicit none
  type (pw_bse), intent(in) :: bse
  integer,       intent(in) :: unit
  character(*),  intent(in) :: name
  character(len=iotk_attlenx) :: attr

! variables controling parallel flux
  integer :: location, owner

  integer :: itrans1

  if(bse%matrix%rank==bse%matrix%root) then
    call iotk_write_attr (attr,"type","pw_bse",first=.true.)
    call iotk_write_attr (attr,"ntrans",bse%ntrans)
    call iotk_write_begin(unit,trim(name),attr)
 
    call iotk_write_dat (unit,"spin",bse%spin)
    call iotk_write_dat  (unit,"iktrans",bse%iktrans)
    call iotk_write_dat  (unit,"ib1trans",bse%ib1trans)
    call iotk_write_dat  (unit,"ib2trans",bse%ib2trans)
  endif
  call ptk_barrier(bse%matrix%comm) 
  call ptk_barrier(bse%matrix%comm)
  call pw_parall_matrix_write(bse%matrix,unit,"matrix")
  call ptk_barrier(bse%matrix%comm)
  if(bse%matrix%rank==bse%matrix%root) then
    call iotk_write_end(unit,trim(name))
  endif
  call ptk_barrier(bse%matrix%comm)
End subroutine pw_bse_write_x

subroutine pw_bse_readbcast_x(bse,unit,name,root,comm,not_allocated)
  use pw_bse_type
  use ptk_module, only : ptk_comm, ptk_bcast, ptk_comm_rank
  use pw_parall_matrix_module
  use iotk_module
  implicit none
  type (pw_bse), intent(inout) :: bse
  integer,       intent(in)    :: unit
  character(*),  intent(in)    :: name
  integer, intent(in) :: root
  type (ptk_comm), intent(in) :: comm
  logical, optional, intent(in) :: not_allocated

! variables controling parallel flux
  integer :: location, owner
  integer :: myrank

  integer :: itrans1
  character(len=iotk_attlenx) :: attr
  character(len=iotk_vallenx) :: rtype
  integer :: ntrans

  logical :: not_allocated_loc

  not_allocated_loc = .false.

  if(present(not_allocated)) not_allocated_loc = not_allocated
  call ptk_comm_rank(comm,myrank)

  if(myrank==root) then
!	write(0,*) "before scan begin", trim(name)
    call iotk_scan_begin(unit,trim(name),attr)
!	write(0,*) "scan begin"
    call iotk_scan_attr(attr,"type",rtype,default="pw_bse")
!	write(0,*) "pw_bse"
    if(rtype/="pw_bse") ERROR("Wrong type in bse read")
    call iotk_scan_attr(attr,"ntrans",ntrans)
!	write(0,*) "ntrans"
    if((ntrans/=bse%ntrans).and.(.not.not_allocated_loc)) then
      ERROR ("ntrans in bse read not coherent with bse init")
    else
      bse%ntrans = ntrans
    endif
    if(.not.not_allocated_loc) then
      call iotk_scan_dat (unit,"spin",bse%spin)
!	write(0,*) "spin"
      call iotk_scan_dat  (unit,"iktrans",bse%iktrans)
!	write(0,*) "iktrans"
      call iotk_scan_dat  (unit,"ib1trans",bse%ib1trans)
!	write(0,*) "ib1trans"
      call iotk_scan_dat  (unit,"ib2trans",bse%ib2trans)
!	write(0,*) "ib2trans"
    endif
  endif
 
  call ptk_bcast(bse%ntrans,root,comm)
  call ptk_bcast(bse%spin,root,comm)
  call ptk_bcast(bse%iktrans,root,comm)
  call ptk_bcast(bse%ib1trans,root,comm)
  call ptk_bcast(bse%ib2trans,root,comm)

  if(.not.not_allocated_loc) call pw_parall_matrix_read(bse%matrix,unit,"matrix")
  
  if(myrank==root) call iotk_scan_end(unit,trim(name))

end subroutine pw_bse_readbcast_x

subroutine pw_bse_allreduce_inplace(bse,comm)
  use pw_bse_type
  use ptk_module, only : ptk_allreduce_inplace,ptk_sum,ptk_comm
  implicit none
  type (pw_bse),   intent(inout) :: bse
  type (ptk_comm), intent(in)    :: comm
  call ptk_allreduce_inplace(bse%matrix%val,ptk_sum,comm)
end subroutine pw_bse_allreduce_inplace

subroutine pw_bse_calc_x3(bse,w,energy_shift,cutoff,coulomb_div_treatment,ecutvcut)
  use ptk_module, only : ptk_barrier
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
  use num_la_parall_module
  use coulomb_vcut_module, only :vcut_type
  implicit none

  type(pw_bse), intent(inout) :: bse
  type(pw_w), intent(in) :: w
  real, intent(in) :: energy_shift
  real, intent(in) :: cutoff
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut

  type(pw_coulomb) :: coulomb
  type(pw_basis) :: basis_coulomb
  type(pw_wfc) :: wfc_coulomb

  real :: ecutvcut_loc
  character(len=30) :: coulomb_div_treatment_loc
  integer :: system_type
  type(vcut_type) :: vcut

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

  integer :: ik1,ik2,idk,nk
  integer :: id0
  real, dimension(3) :: k0
  integer :: ib1, ib2, ib3, ib4
  real, dimension(3) :: k1, k2, k
  integer :: itrans1,itrans2
  integer, dimension(3) :: dim
  integer :: npw_small
  integer :: npw(3)

  real :: scale
  real :: lattice(3,3)

! ---------------------------------------------------------
! variables controling part of parallel flux
  integer :: iboh
  integer :: location
! --------------------------------------------------------

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = coulomb_div_treatment
write(0,*) "from calc bse coulomb_div treatment: ", trim(coulomb_div_treatment_loc)
  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut


  nk=ubound(bse%states%wfc,2)

  lattice = num_matmul(bse%states%struct%b,w%qmesh%m_inv)

  if(bse%spin>1) ERROR("Wrong spin state on BSE, bse_spin=0 or bse_spin=1")

  scale=1.0/bse%states%struct%a_omega

  write(0,*)"Transition number taken into account in BSE calculation:", bse%ntrans

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    call pw_vcut_init(vcut,bse%states%struct,w%qmesh,ecutvcut_loc)
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

  if(bse%spin==0) then

     write(0,*)"Fill BSE matrix exchange kernel"
     iboh=1

     do itrans1=1,bse%ntrans

        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then

        location=bse%matrix%where_line(2,itrans1)

        ik1=bse%iktrans(itrans1)
        ib1=bse%ib1trans(itrans1)
        ib2=bse%ib2trans(itrans1)
        
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

        call pw_field_destroy(wfc_field1)
        call pw_field_destroy(wfc_field2)
        call pw_field_destroy(F_field1)

        do itrans2=1,bse%ntrans

           ik2=bse%iktrans(itrans2)
           ib3=bse%ib1trans(itrans2)
           ib4=bse%ib2trans(itrans2)

           k2=bse%states%kmesh%kbz(:,ik2)
           
           call pw_field_init(wfc_field1,bse%states%struct)
           call pw_field_init(wfc_field2,bse%states%struct)
           call pw_field_init(F_field1,bse%states%struct)
           
           call pw_wfc_init(F_wfc2,basis_field)
           call pw_wfc_init(tmp_wfc,basis_field)

           dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik2))
           call pw_field_set_dim(wfc_field1,dim,k=k2,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

           call pw_states_borrow_wfc(bse%states,wfc1,ib3,ik2)
           call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
           call pw_wfc2field(wfc_field1,wfc1)
           call pw_wfc2field(wfc_field2,wfc2)
           call pw_states_giveback_wfc(bse%states,wfc1)
           call pw_states_giveback_wfc(bse%states,wfc2)

           call pw_field_mul(F_field1,wfc_field2,wfc_field1)
           call pw_field2wfc(F_wfc2,F_field1)

           idk=pw_kmesh_kbz_index(w%qmesh,(k1-k2))
           call pw_basis_init(basis_coulomb,bse%states%struct)
           call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
!           call pw_coulomb_init(coulomb,lattice)
           call pw_wfc_init(wfc_coulomb,basis_coulomb)
           call pw_coulomb_get(system_type,coulomb,wfc_coulomb)
!           call pw_coulomb_destroy(coulomb)

! ----------------------------------------------------------
! for inclusion of non local field effects
           wfc_coulomb%val(basis_coulomb%index_Gzero)=0.0
! ----------------------------------------------------------

           tmp_wfc%val=0.0
           npw(1)=ubound(wfc_coulomb%val,1)
           npw(2)=ubound(F_wfc1%val,1)
           npw(3)=ubound(tmp_wfc%val,1)
           npw_small = minval(npw)
           call num_vemul(tmp_wfc%val(1:npw_small),wfc_coulomb%val(1:npw_small),F_wfc1%val(1:npw_small))

           if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) &
              bse%matrix%val(location,itrans2)=2.0 * scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
            sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))
           call pw_field_destroy(wfc_field1)
           call pw_field_destroy(wfc_field2)
           call pw_field_destroy(F_field1)
           call pw_basis_destroy(basis_coulomb)
           call pw_wfc_destroy(wfc_coulomb)
           call pw_wfc_destroy(F_wfc2)
           call pw_wfc_destroy(tmp_wfc)


!        if(ik1==ik2.and.ib1==ib3.and.ib2==ib4) then
!          if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
!                 bse%matrix%val(location,itrans2) + &
!                 (bse%states%e(ib1,ik1)-bse%states%e(ib2,ik2)+energy_shift)
!        endif

        enddo
        call pw_basis_destroy(basis_field)
        call pw_wfc_destroy(F_wfc1)
        if(bse%matrix%incomplete) iboh=iboh+1
        endif
     enddo
  endif

  write(0,*) "Fill BSE matrix direct kernel"

  iboh=1
  do itrans1=1,bse%ntrans
     if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then
     location=bse%matrix%where_line(2,itrans1)
     do itrans2=1,bse%ntrans

        ik1=bse%iktrans(itrans1)
        ib1=bse%ib1trans(itrans1)
        ib2=bse%ib2trans(itrans1)
        ik2=bse%iktrans(itrans2)
        ib3=bse%ib1trans(itrans2)
        ib4=bse%ib2trans(itrans2)

        k1(:)=bse%states%kmesh%kbz(:,ik1)
        k2(:)=bse%states%kmesh%kbz(:,ik2)
        
        idk=pw_kmesh_kbz_index(w%qmesh,(k1-k2))

        call pw_basis_init(basis_coulomb,bse%states%struct)
        call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
!        call pw_coulomb_init(coulomb,lattice)
        call pw_wfc_init(wfc_coulomb,basis_coulomb)
        call pw_coulomb_get(system_type,coulomb,wfc_coulomb)
!        if(idk==w%iqgamma) wfc_coulomb%val(1)=w%v0
!        call pw_coulomb_destroy(coulomb)

        call pw_basis_init(basis_field,bse%states%struct)
        call pw_basis_create(basis_field,w%qmesh%kbz(:,idk),cutoff)

        call pw_field_init(wfc_field1,bse%states%struct)
        call pw_field_init(wfc_field2,bse%states%struct)
        call pw_field_init(F_field1,bse%states%struct)
        call pw_field_init(F_field2,bse%states%struct)

        call pw_wfc_init(F_wfc1,basis_field)
        call pw_wfc_init(F_wfc2,basis_field)
        call pw_wfc_init(tmp_wfc,basis_field)

        dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik1))
        call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(F_field1,dim,k=k1-k2,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(F_field2,dim,k=k1-k2,r0=(/0.0,0.0,0.0/))

        call pw_states_borrow_wfc(bse%states,wfc1,ib1,ik1)
        call pw_states_borrow_wfc(bse%states,wfc2,ib3,ik2)
        call pw_wfc2field(wfc_field1,wfc1)
        call pw_wfc2field(wfc_field2,wfc2)
        call pw_states_giveback_wfc(bse%states,wfc1)
        call pw_states_giveback_wfc(bse%states,wfc2)

        call pw_field_mul(F_field1,wfc_field2,wfc_field1)
        call pw_field2wfc(F_wfc1,F_field1)

        call pw_field_destroy(wfc_field1)
        call pw_field_destroy(wfc_field2)

        call pw_field_init(wfc_field1,bse%states%struct)
        call pw_field_init(wfc_field2,bse%states%struct)
        call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))

        call pw_states_borrow_wfc(bse%states,wfc1,ib2,ik1)
        call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
        call pw_wfc2field(wfc_field1,wfc1)
        call pw_wfc2field(wfc_field2,wfc2)
        call pw_states_giveback_wfc(bse%states,wfc1)
        call pw_states_giveback_wfc(bse%states,wfc2)

        call pw_field_mul(F_field2,wfc_field2,wfc_field1)
        call pw_field2wfc(F_wfc2,F_field2) 

        call pw_w_borrow_wfc6d(w,w0%val,idk,0)
! iomega on pw_w_borrow_wfc6d not needed here 
! iomega defined as optional, default=0

        npw_small=ubound(w0%val%val,1)

        tmp_wfc%val=0.0

        tmp_wfc%val(1:npw_small)=num_matmul(w0%val%val,F_wfc1%val)

        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                bse%matrix%val(location,itrans2)-scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
             sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))

        tmp_wfc%val=0.0
        
        call num_vemul(tmp_wfc%val,wfc_coulomb%val,F_wfc1%val)
        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                bse%matrix%val(location,itrans2)-scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
            sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))

        call pw_basis_destroy(basis_field)
        call pw_w_giveback_wfc6d(w,w0%val)
        call pw_wfc_destroy(F_wfc1)
        call pw_wfc_destroy(F_wfc2)
        call pw_field_destroy(wfc_field1)
        call pw_field_destroy(wfc_field2)
        call pw_field_destroy(F_field1)
        call pw_field_destroy(F_field2)
        call pw_wfc_destroy(tmp_wfc)
        call pw_basis_destroy(basis_coulomb)
        call pw_wfc_destroy(wfc_coulomb)

        if(ik1==ik2.and.ib1==ib3.and.ib2==ib4) then
          if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                 bse%matrix%val(location,itrans2) + &
                 (bse%states%e(ib1,ik1)-bse%states%e(ib2,ik2)+energy_shift)
        endif

     enddo
     if(bse%matrix%incomplete) iboh=iboh+1
     endif
  enddo

! diagonalisation

!  write(0,*) "Diagonalization BSE matrix"
  
  call num_parall_he_diag(bse%matrix,bse%matrix%root,bse%matrix%comm)

  if(system_type==1) then
    call pw_vcut_destroy(vcut)
  endif
  call pw_coulomb_destroy(coulomb)

!  if(bse%matrix%rank==bse%matrix%root) write(0,*) bse%matrix%eigenval

end subroutine pw_bse_calc_x3

subroutine pw_bse_calc_x0(bse,w,energy_shift,cutoff)
  use ptk_module, only : ptk_barrier
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
  use num_la_parall_module
  implicit none

  type(pw_bse), intent(inout) :: bse
  type(pw_w), intent(in) :: w
  real, intent(in) :: energy_shift
  real, intent(in) :: cutoff

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

  integer :: ik1,ik2,idk,nk
  integer :: id0
  real, dimension(3) :: k0
  integer :: ib1, ib2, ib3, ib4
  real, dimension(3) :: k1, k2, k
  integer :: itrans1,itrans2
  integer, dimension(3) :: dim
  integer :: npw_small
  integer :: npw(3)

  real :: scale
  real :: lattice(3,3)

! ---------------------------------------------------------
! variables controling part of parallel flux
  integer :: iboh
  integer :: location
! --------------------------------------------------------

  nk=ubound(bse%states%wfc,2)

  lattice = num_matmul(bse%states%struct%b,w%qmesh%m_inv)

  if(bse%spin>1) ERROR("Wrong spin state on BSE, bse_spin=0 or bse_spin=1")

  scale=1.0/bse%states%struct%a_omega

  write(0,*)"Transition number taken into account in BSE calculation:", bse%ntrans

  if(bse%spin==0) then

     write(0,*)"Fill BSE matrix exchange kernel"
     iboh=1

     do itrans1=1,bse%ntrans

        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then

        location=bse%matrix%where_line(2,itrans1)

        ik1=bse%iktrans(itrans1)
        ib1=bse%ib1trans(itrans1)
        ib2=bse%ib2trans(itrans1)
        
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

        call pw_field_destroy(wfc_field1)
        call pw_field_destroy(wfc_field2)
        call pw_field_destroy(F_field1)

        do itrans2=1,bse%ntrans

           ik2=bse%iktrans(itrans2)
           ib3=bse%ib1trans(itrans2)
           ib4=bse%ib2trans(itrans2)

           k2=bse%states%kmesh%kbz(:,ik2)
           
           call pw_field_init(wfc_field1,bse%states%struct)
           call pw_field_init(wfc_field2,bse%states%struct)
           call pw_field_init(F_field1,bse%states%struct)
           
           call pw_wfc_init(F_wfc2,basis_field)
           call pw_wfc_init(tmp_wfc,basis_field)

           dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik2))
           call pw_field_set_dim(wfc_field1,dim,k=k2,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
           call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

           call pw_states_borrow_wfc(bse%states,wfc1,ib3,ik2)
           call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
           call pw_wfc2field(wfc_field1,wfc1)
           call pw_wfc2field(wfc_field2,wfc2)
           call pw_states_giveback_wfc(bse%states,wfc1)
           call pw_states_giveback_wfc(bse%states,wfc2)

           call pw_field_mul(F_field1,wfc_field2,wfc_field1)
           call pw_field2wfc(F_wfc2,F_field1)

           idk=pw_kmesh_kbz_index(w%qmesh,(k1-k2))
           call pw_basis_init(basis_coulomb,bse%states%struct)
           call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
           call pw_coulomb_init(coulomb,lattice)
           call pw_wfc_init(wfc_coulomb,basis_coulomb)
           call pw_coulomb_get(0,coulomb,wfc_coulomb)
           call pw_coulomb_destroy(coulomb)

! ----------------------------------------------------------
! for inclusion of local field effects
           wfc_coulomb%val(basis_coulomb%index_Gzero)=0.0
! ----------------------------------------------------------

           tmp_wfc%val=0.0
           npw(1)=ubound(wfc_coulomb%val,1)
           npw(2)=ubound(F_wfc1%val,1)
           npw(3)=ubound(tmp_wfc%val,1)
           npw_small = minval(npw)
           call num_vemul(tmp_wfc%val(1:npw_small),wfc_coulomb%val(1:npw_small),F_wfc1%val(1:npw_small))

           if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) &
        bse%matrix%val(location,itrans2)=2.0 * scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
		sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))
           call pw_field_destroy(wfc_field1)
           call pw_field_destroy(wfc_field2)
           call pw_field_destroy(F_field1)
           call pw_basis_destroy(basis_coulomb)
           call pw_wfc_destroy(wfc_coulomb)
           call pw_wfc_destroy(F_wfc2)
           call pw_wfc_destroy(tmp_wfc)

        enddo
        call pw_basis_destroy(basis_field)
        call pw_wfc_destroy(F_wfc1)
        if(bse%matrix%incomplete) iboh=iboh+1
        endif
     enddo
  endif

  write(0,*) "Fill BSE matrix direct kernel"

  iboh=1
  do itrans1=1,bse%ntrans
     if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then
     location=bse%matrix%where_line(2,itrans1)
     do itrans2=1,bse%ntrans

        ik1=bse%iktrans(itrans1)
        ib1=bse%ib1trans(itrans1)
        ib2=bse%ib2trans(itrans1)
        ik2=bse%iktrans(itrans2)
        ib3=bse%ib1trans(itrans2)
        ib4=bse%ib2trans(itrans2)

        k1(:)=bse%states%kmesh%kbz(:,ik1)
        k2(:)=bse%states%kmesh%kbz(:,ik2)
        
        idk=pw_kmesh_kbz_index(w%qmesh,(k1-k2))

        call pw_basis_init(basis_coulomb,bse%states%struct)
        call pw_basis_create(basis_coulomb,w%qmesh%kbz(:,idk),cutoff)
        call pw_coulomb_init(coulomb,lattice)
        call pw_wfc_init(wfc_coulomb,basis_coulomb)
        call pw_coulomb_get(0,coulomb,wfc_coulomb)
        call pw_coulomb_destroy(coulomb)

        call pw_basis_init(basis_field,bse%states%struct)
        call pw_basis_create(basis_field,w%qmesh%kbz(:,idk),cutoff)

        call pw_field_init(wfc_field1,bse%states%struct)
        call pw_field_init(wfc_field2,bse%states%struct)
        call pw_field_init(F_field1,bse%states%struct)
        call pw_field_init(F_field2,bse%states%struct)

        call pw_wfc_init(F_wfc1,basis_field)
        call pw_wfc_init(F_wfc2,basis_field)
        call pw_wfc_init(tmp_wfc,basis_field)

        dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik1))
        call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(F_field1,dim,k=k1-k2,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(F_field2,dim,k=k1-k2,r0=(/0.0,0.0,0.0/))

        call pw_states_borrow_wfc(bse%states,wfc1,ib1,ik1)
        call pw_states_borrow_wfc(bse%states,wfc2,ib3,ik2)
        call pw_wfc2field(wfc_field1,wfc1)
        call pw_wfc2field(wfc_field2,wfc2)
        call pw_states_giveback_wfc(bse%states,wfc1)
        call pw_states_giveback_wfc(bse%states,wfc2)

        call pw_field_mul(F_field1,wfc_field2,wfc_field1)
        call pw_field2wfc(F_wfc1,F_field1)

        call pw_field_destroy(wfc_field1)
        call pw_field_destroy(wfc_field2)

        call pw_field_init(wfc_field1,bse%states%struct)
        call pw_field_init(wfc_field2,bse%states%struct)
        call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))

        call pw_states_borrow_wfc(bse%states,wfc1,ib2,ik1)
        call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
        call pw_wfc2field(wfc_field1,wfc1)
        call pw_wfc2field(wfc_field2,wfc2)
        call pw_states_giveback_wfc(bse%states,wfc1)
        call pw_states_giveback_wfc(bse%states,wfc2)

        call pw_field_mul(F_field2,wfc_field2,wfc_field1)
        call pw_field2wfc(F_wfc2,F_field2) 

        call pw_w_borrow_wfc6d(w,w0%val,idk,0)
! iomega on pw_w_borrow_wfc6d not needed here 
! iomega defined as optional, default=0

        npw_small=ubound(w0%val%val,1)

        tmp_wfc%val=0.0

        tmp_wfc%val(1:npw_small)=num_matmul(w0%val%val,F_wfc1%val)

        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                bse%matrix%val(location,itrans2)-scale*pw_wfc_braket(F_wfc2,tmp_wfc) * &
		sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))

        tmp_wfc%val=0.0
        
        call num_vemul(tmp_wfc%val,wfc_coulomb%val,F_wfc1%val)
        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                bse%matrix%val(location,itrans2)-scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
		sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))

        call pw_basis_destroy(basis_field)
        call pw_w_giveback_wfc6d(w,w0%val)
        call pw_wfc_destroy(F_wfc1)
        call pw_wfc_destroy(F_wfc2)
        call pw_field_destroy(wfc_field1)
        call pw_field_destroy(wfc_field2)
        call pw_field_destroy(F_field1)
        call pw_field_destroy(F_field2)
        call pw_wfc_destroy(tmp_wfc)
        call pw_basis_destroy(basis_coulomb)
        call pw_wfc_destroy(wfc_coulomb)

        if(ik1==ik2.and.ib1==ib3.and.ib2==ib4) then
          if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                 bse%matrix%val(location,itrans2) + &
                 (bse%states%e(ib1,ik1)-bse%states%e(ib2,ik2)+energy_shift)
        endif

     enddo
     if(bse%matrix%incomplete) iboh=iboh+1
     endif
  enddo

! diagonalisation

  write(0,*)"Diagonalization BSE matrix"

  call num_parall_he_diag(bse%matrix,bse%matrix%root,bse%matrix%comm)

end subroutine pw_bse_calc_x0

subroutine pw_bse_macro_x3(bse,epsilon,energy_shift,qmesh,cutoff1,cutoff2, &
	coulomb_div_treatment,ecutvcut)
  use pw_bse_type
  use pw_bse_interf
  use iotk_module
  use pw_states_module
  use pw_field_module
  use pw_coulomb_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_kmesh_module
  use num_module
  use pw_fft_module
  use tools_module
  use pw_epsilon_macro_module
  use num_la_parall_module
  use ptk_module, only : ptk_barrier
  use coulomb_vcut_module, only : vcut_type
  implicit none

  type(pw_bse), intent(inout) :: bse
  type(pw_epsilon_macro), intent(in) :: epsilon
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff1,cutoff2
  real, intent(in) :: energy_shift
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut

  type(pw_coulomb) :: coulomb
  type(pw_basis) :: basis_coulomb
  type(pw_wfc) :: wfc_coulomb

  type(pw_basis) :: basis_field
  type(pw_field) :: F_field1, F_field2
  type(pw_field) :: wfc_field1, wfc_field2
  type(pw_wfc), pointer :: wfc1, wfc2
  type(pw_wfc) :: F_wfc1, F_wfc2
  type(pw_wfc) :: tmp_wfc, tmp_wfc2


  integer :: ik1,ik2,idk,nk
  integer :: id0
  real, dimension(3) :: k0
  integer :: ib1, ib2, ib3, ib4
  real, dimension(3) :: k1, k2, k
  integer :: itrans1,itrans2
  integer, dimension(3) :: dim
  integer :: npw_small
  integer :: npw(3)

  real :: scale

  real :: lattice(3,3)
  complex :: macro(3,3)

  character(len=30) :: coulomb_div_treatment_loc
  real :: ecutvcut_loc
  integer :: system_type
  type(vcut_type) :: vcut

! ------------------------------------------------------------------------
! variable controling part of parallel flux
  integer :: iboh
  integer :: location
! -----------------------------------------------------------------------  

! epsilon is writen as epsilon*8pi in gw
! to got right results :

  macro(:,:)=epsilon%macroscopic(:,:,0)*num_8pi

  lattice=num_matmul(bse%states%struct%b,qmesh%m_inv)

  nk=ubound(bse%states%wfc,2)

  if(bse%spin>1) ERROR("Wrong spin state on BSE, bse_spin=0 or bse_spin=1")

  scale=1.0/bse%states%struct%a_omega

  write(0,*) "Transition number taken in to account in BSE calculation:",bse%ntrans

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = coulomb_div_treatment

  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    call pw_vcut_init(vcut,bse%states%struct,qmesh,ecutvcut_loc)
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

!  write(0,*) "coulomb_div_treatment: ", coulomb_div_treatment_loc
!  write(0,*) "energy_shift: ", energy_shift

! calculation starts
  if(bse%spin==0) then

     write(0,*)"Fill BSE matrix exchange kernel"
     iboh=1

     do itrans1=1,bse%ntrans

        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then
          location=bse%matrix%where_line(2,itrans1)        
          ik1=bse%iktrans(itrans1)
          ib1=bse%ib1trans(itrans1)
          ib2=bse%ib2trans(itrans1)
        
          k0(:)=0.0

          k1=bse%states%kmesh%kbz(:,ik1)

          id0=pw_kmesh_kbz_index(qmesh,(k0))

          call pw_field_init(wfc_field1,bse%states%struct)
          call pw_field_init(wfc_field2,bse%states%struct)
          call pw_field_init(F_field1,bse%states%struct)
        
          call pw_basis_init(basis_field,bse%states%struct)
          call pw_basis_create(basis_field,qmesh%kbz(:,id0),cutoff2)
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

          call pw_field_destroy(wfc_field1)
          call pw_field_destroy(wfc_field2)
          call pw_field_destroy(F_field1)

          do itrans2=1,bse%ntrans

             ik2=bse%iktrans(itrans2)
             ib3=bse%ib1trans(itrans2)
             ib4=bse%ib2trans(itrans2)

             k2=bse%states%kmesh%kbz(:,ik2)
           
             call pw_field_init(wfc_field1,bse%states%struct)
             call pw_field_init(wfc_field2,bse%states%struct)
             call pw_field_init(F_field1,bse%states%struct)
           
             call pw_wfc_init(F_wfc2,basis_field)
             call pw_wfc_init(tmp_wfc,basis_field)

             dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik2))
             call pw_field_set_dim(wfc_field1,dim,k=k2,r0=(/0.0,0.0,0.0/))
             call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
             call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

             call pw_states_borrow_wfc(bse%states,wfc1,ib3,ik2)
             call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
             call pw_wfc2field(wfc_field1,wfc1)
             call pw_wfc2field(wfc_field2,wfc2)
             call pw_states_giveback_wfc(bse%states,wfc1)
             call pw_states_giveback_wfc(bse%states,wfc2)

             call pw_field_mul(F_field1,wfc_field2,wfc_field1)
             call pw_field2wfc(F_wfc2,F_field1)

             idk=pw_kmesh_kbz_index(qmesh,(k1-k2))
             call pw_basis_init(basis_coulomb,bse%states%struct)
             call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff2)
             call pw_wfc_init(wfc_coulomb,basis_coulomb)
             call pw_coulomb_get(system_type,coulomb,wfc_coulomb)

! -----------------------------------------------------------------
! for inclusion of local field effects
             wfc_coulomb%val(basis_coulomb%index_Gzero)=0.0
! -----------------------------------------------------------------             
             
             tmp_wfc%val=0.0
             npw(1)=ubound(wfc_coulomb%val,1)
             npw(2)=ubound(F_wfc1%val,1)
             npw(3)=ubound(tmp_wfc%val,1)
             npw_small = minval(npw)
             call num_vemul(tmp_wfc%val(1:npw_small),wfc_coulomb%val(1:npw_small),F_wfc1%val(1:npw_small))
             
             if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                2.0 * scale*pw_wfc_braket(F_wfc2,tmp_wfc) * &
		sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))
                
             call pw_field_destroy(wfc_field1)
             call pw_field_destroy(wfc_field2)
             call pw_field_destroy(F_field1)
             call pw_basis_destroy(basis_coulomb)
             call pw_wfc_destroy(wfc_coulomb)
             call pw_wfc_destroy(F_wfc2)
             call pw_wfc_destroy(tmp_wfc)

          enddo
          call pw_basis_destroy(basis_field)
          call pw_wfc_destroy(F_wfc1)
          if(bse%matrix%incomplete) iboh=iboh+1
        endif  
     enddo
  endif

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    call pw_vcut_destroy(vcut)
  endif

  call pw_coulomb_destroy(coulomb)

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    call pw_vcut_init(vcut,bse%states%struct,qmesh,ecutvcut_loc)
    call pw_coulomb_init(coulomb,lattice,vcut=vcut)
  else
    call pw_coulomb_init(coulomb,lattice,epsilon=macro)
  endif


  write(0,*) "Fill BSE matrix direct kernel"

  iboh=1
  do itrans1=1,bse%ntrans
     if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then
       location=bse%matrix%where_line(2,itrans1)      
       do itrans2=1,bse%ntrans

          ik1=bse%iktrans(itrans1)
          ib1=bse%ib1trans(itrans1)
          ib2=bse%ib2trans(itrans1)
          ik2=bse%iktrans(itrans2)
          ib3=bse%ib1trans(itrans2)
          ib4=bse%ib2trans(itrans2)

          k1(:)=bse%states%kmesh%kbz(:,ik1)
          k2(:)=bse%states%kmesh%kbz(:,ik2)
        
          idk=pw_kmesh_kbz_index(qmesh,(k1-k2))

          call pw_basis_init(basis_coulomb,bse%states%struct)
          call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff1)
          call pw_wfc_init(wfc_coulomb,basis_coulomb)
          call pw_coulomb_get(system_type,coulomb,wfc_coulomb)

          call pw_basis_init(basis_field,bse%states%struct)
          call pw_basis_create(basis_field,qmesh%kbz(:,idk),cutoff1)

          call pw_field_init(wfc_field1,bse%states%struct)
          call pw_field_init(wfc_field2,bse%states%struct)
          call pw_field_init(F_field1,bse%states%struct)
          call pw_field_init(F_field2,bse%states%struct)

          call pw_wfc_init(F_wfc1,basis_field)
          call pw_wfc_init(F_wfc2,basis_field)
          call pw_wfc_init(tmp_wfc,basis_field)

          dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik1))
          call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
          call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
          call pw_field_set_dim(F_field1,dim,k=k1-k2,r0=(/0.0,0.0,0.0/))
          call pw_field_set_dim(F_field2,dim,k=k1-k2,r0=(/0.0,0.0,0.0/))

          call pw_states_borrow_wfc(bse%states,wfc1,ib1,ik1)
          call pw_states_borrow_wfc(bse%states,wfc2,ib3,ik2)
          call pw_wfc2field(wfc_field1,wfc1)
          call pw_wfc2field(wfc_field2,wfc2)
          call pw_states_giveback_wfc(bse%states,wfc1)
          call pw_states_giveback_wfc(bse%states,wfc2)

          call pw_field_mul(F_field1,wfc_field2,wfc_field1)
          call pw_field2wfc(F_wfc1,F_field1)

          call pw_field_destroy(wfc_field1)
          call pw_field_destroy(wfc_field2)

          call pw_field_init(wfc_field1,bse%states%struct)
          call pw_field_init(wfc_field2,bse%states%struct)
          call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
          call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))

          call pw_states_borrow_wfc(bse%states,wfc1,ib2,ik1)
          call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
          call pw_wfc2field(wfc_field1,wfc1)
          call pw_wfc2field(wfc_field2,wfc2)
          call pw_states_giveback_wfc(bse%states,wfc1)
          call pw_states_giveback_wfc(bse%states,wfc2)

          call pw_field_mul(F_field2,wfc_field2,wfc_field1)
          call pw_field2wfc(F_wfc2,F_field2) 


          tmp_wfc%val=0.0
        
          call num_vemul(tmp_wfc%val,wfc_coulomb%val,F_wfc1%val)
          
          if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
            bse%matrix%val(location,itrans2)-scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
            sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))

          call pw_basis_destroy(basis_field)
          call pw_wfc_destroy(F_wfc1)
          call pw_wfc_destroy(F_wfc2)
          call pw_field_destroy(wfc_field1)
          call pw_field_destroy(wfc_field2)
          call pw_field_destroy(F_field1)
          call pw_field_destroy(F_field2)
          call pw_wfc_destroy(tmp_wfc)
          call pw_basis_destroy(basis_coulomb)
          call pw_wfc_destroy(wfc_coulomb)

          if(ik1==ik2.and.ib1==ib3.and.ib2==ib4) then
             if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                  bse%matrix%val(location,itrans2) + &
                  (bse%states%e(ib1,ik1)-bse%states%e(ib2,ik2)+energy_shift)
!             if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                 
!                  (bse%states%e(ib1,ik1)-bse%states%e(ib2,ik2)+energy_shift)

          endif
       enddo
       if(bse%matrix%incomplete) iboh=iboh+1
     endif     
  enddo

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    call pw_vcut_destroy(vcut)
  endif

  call pw_coulomb_destroy(coulomb)


! diagonalisation

!  write(0,*)"Diagonalization BSE matrix"

!ib1= 81+bse%matrix%rank
!write(0,*) bse%matrix%eigenval(:)
!  call ptk_barrier(bse%matrix%comm) 
!if(bse%matrix%rank==0) write(0,*) "I am rank 0 here is matrix"
!do ib1 = 1, bse%matrix%m_dim_loc
!  do ib2=1,bse%matrix%m_dim
!    if(real(bse%matrix%val(ib1,ib2))<=0.0.or.real(bse%matrix%val(ib1,ib2))>100.0) cycle
!    if(bse%matrix%rank==0) write(0,*) bse%matrix%rank, bse%matrix%val(:,:)
!  enddo
!enddo
  call num_parall_he_diag(bse%matrix,bse%matrix%root,bse%matrix%comm)
  call ptk_barrier(bse%matrix%comm)
!write(0,*) "after: ", bse%matrix%eigenval(1), bse%matrix%eigenval(2)
!ib1= 11+bse%matrix%rank
!do ib2=1,bse%ntrans
!write(ib1,*) bse%matrix%eigenval(ib2)
!enddo

!ib1= 51+bse%matrix%rank
!write(ib1,*) bse%matrix%eigenval(:)

end subroutine pw_bse_macro_x3

subroutine pw_bse_macro_x0(bse,epsilon,energy_shift,qmesh,cutoff1,cutoff2)
  use pw_bse_type
  use pw_bse_interf
  use iotk_module
  use pw_states_module
  use pw_field_module
  use pw_coulomb_module
  use pw_wfc_module
  use pw_wfc6d_module
  use pw_basis_module
  use pw_kmesh_module
  use num_module
  use pw_fft_module
  use tools_module
  use pw_epsilon_macro_module
  use num_la_parall_module
  implicit none

  type(pw_bse), intent(inout) :: bse
  type(pw_epsilon_macro), intent(in) :: epsilon
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff1, cutoff2
  real, intent(in) :: energy_shift

  type(pw_coulomb) :: coulomb
  type(pw_basis) :: basis_coulomb
  type(pw_wfc) :: wfc_coulomb

  type(pw_basis) :: basis_field
  type(pw_field) :: F_field1, F_field2
  type(pw_field) :: wfc_field1, wfc_field2
  type(pw_wfc), pointer :: wfc1, wfc2
  type(pw_wfc) :: F_wfc1, F_wfc2
  type(pw_wfc) :: tmp_wfc, tmp_wfc2


  integer :: ik1,ik2,idk,nk
  integer :: id0
  real, dimension(3) :: k0
  integer :: ib1, ib2, ib3, ib4
  real, dimension(3) :: k1, k2, k
  integer :: itrans1,itrans2
  integer, dimension(3) :: dim
  integer :: npw_small
  integer :: npw(3)
  complex :: macro(3,3)

  real :: scale
  real :: lattice(3,3)

! ------------------------------------------------------------------------
! variable controling part of parallel flux
  integer :: iboh
  integer :: location
! -----------------------------------------------------------------------  

! epsilon is writen as epsilon*8pi in gw
! to got right result :

  macro(:,:)=epsilon%macroscopic(:,:,0)*num_8pi

  lattice = num_matmul(bse%states%struct%b,qmesh%m_inv)

  nk=ubound(bse%states%wfc,2)

  if(bse%spin>1) ERROR("Wrong spin state on BSE, bse_spin=0 or bse_spin=1")

  scale=1.0/bse%states%struct%a_omega

  write(0,*) "Transition number taken in to account in BSE calculation:",bse%ntrans

  if(bse%spin==0) then

     write(0,*)"Fill BSE matrix exchange kernel"
     iboh=1

     do itrans1=1,bse%ntrans

        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then
          location=bse%matrix%where_line(2,itrans1)        
          ik1=bse%iktrans(itrans1)
          ib1=bse%ib1trans(itrans1)
          ib2=bse%ib2trans(itrans1)
        
          k0(:)=0.0

          k1=bse%states%kmesh%kbz(:,ik1)

          id0=pw_kmesh_kbz_index(qmesh,(k0))

          call pw_field_init(wfc_field1,bse%states%struct)
          call pw_field_init(wfc_field2,bse%states%struct)
          call pw_field_init(F_field1,bse%states%struct)
        
          call pw_basis_init(basis_field,bse%states%struct)
          call pw_basis_create(basis_field,qmesh%kbz(:,id0),cutoff2)
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

          call pw_field_destroy(wfc_field1)
          call pw_field_destroy(wfc_field2)
          call pw_field_destroy(F_field1)

          do itrans2=1,bse%ntrans

             ik2=bse%iktrans(itrans2)
             ib3=bse%ib1trans(itrans2)
             ib4=bse%ib2trans(itrans2)

             k2=bse%states%kmesh%kbz(:,ik2)
           
             call pw_field_init(wfc_field1,bse%states%struct)
             call pw_field_init(wfc_field2,bse%states%struct)
             call pw_field_init(F_field1,bse%states%struct)
           
             call pw_wfc_init(F_wfc2,basis_field)
             call pw_wfc_init(tmp_wfc,basis_field)

             dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik2))
             call pw_field_set_dim(wfc_field1,dim,k=k2,r0=(/0.0,0.0,0.0/))
             call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
             call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

             call pw_states_borrow_wfc(bse%states,wfc1,ib3,ik2)
             call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
             call pw_wfc2field(wfc_field1,wfc1)
             call pw_wfc2field(wfc_field2,wfc2)
             call pw_states_giveback_wfc(bse%states,wfc1)
             call pw_states_giveback_wfc(bse%states,wfc2)

             call pw_field_mul(F_field1,wfc_field2,wfc_field1)
             call pw_field2wfc(F_wfc2,F_field1)

             idk=pw_kmesh_kbz_index(qmesh,(k1-k2))
             call pw_basis_init(basis_coulomb,bse%states%struct)
             call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff2)
             call pw_coulomb_init(coulomb,lattice)
             call pw_wfc_init(wfc_coulomb,basis_coulomb)
             call pw_coulomb_get(0,coulomb,wfc_coulomb)
             call pw_coulomb_destroy(coulomb)

! -----------------------------------------------------------------
! for inclusion of local field effects
             wfc_coulomb%val(basis_coulomb%index_Gzero)=0.0
! -----------------------------------------------------------------             

             tmp_wfc%val=0.0
             npw(1)=ubound(wfc_coulomb%val,1)
             npw(2)=ubound(F_wfc1%val,1)
             npw(3)=ubound(tmp_wfc%val,1)
             npw_small = minval(npw)
             call num_vemul(tmp_wfc%val(1:npw_small),wfc_coulomb%val(1:npw_small),F_wfc1%val(1:npw_small))

             if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                2.0 * scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
		sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))
                
             call pw_field_destroy(wfc_field1)
             call pw_field_destroy(wfc_field2)
             call pw_field_destroy(F_field1)
             call pw_basis_destroy(basis_coulomb)
             call pw_wfc_destroy(wfc_coulomb)
             call pw_wfc_destroy(F_wfc2)
             call pw_wfc_destroy(tmp_wfc)

          enddo
          call pw_basis_destroy(basis_field)
          call pw_wfc_destroy(F_wfc1)
          if(bse%matrix%incomplete) iboh=iboh+1
        endif  
     enddo
  endif

  write(0,*) "Fill BSE matrix direct kernel"

  iboh=1
  do itrans1=1,bse%ntrans

     if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then
       location=bse%matrix%where_line(2,itrans1)      
       do itrans2=1,bse%ntrans

          ik1=bse%iktrans(itrans1)
          ib1=bse%ib1trans(itrans1)
          ib2=bse%ib2trans(itrans1)
          ik2=bse%iktrans(itrans2)
          ib3=bse%ib1trans(itrans2)
          ib4=bse%ib2trans(itrans2)

          k1(:)=bse%states%kmesh%kbz(:,ik1)
          k2(:)=bse%states%kmesh%kbz(:,ik2)
        
          idk=pw_kmesh_kbz_index(qmesh,(k1-k2))

          call pw_basis_init(basis_coulomb,bse%states%struct)
          call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff1)
          call pw_coulomb_init(coulomb,lattice,epsilon=macro)
          call pw_wfc_init(wfc_coulomb,basis_coulomb)
          call pw_coulomb_get(0,coulomb,wfc_coulomb)
          call pw_coulomb_destroy(coulomb)

          call pw_basis_init(basis_field,bse%states%struct)
          call pw_basis_create(basis_field,qmesh%kbz(:,idk),cutoff1)

          call pw_field_init(wfc_field1,bse%states%struct)
          call pw_field_init(wfc_field2,bse%states%struct)
          call pw_field_init(F_field1,bse%states%struct)
          call pw_field_init(F_field2,bse%states%struct)

          call pw_wfc_init(F_wfc1,basis_field)
          call pw_wfc_init(F_wfc2,basis_field)
          call pw_wfc_init(tmp_wfc,basis_field)

          dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik1))
          call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
          call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
          call pw_field_set_dim(F_field1,dim,k=k1-k2,r0=(/0.0,0.0,0.0/))
          call pw_field_set_dim(F_field2,dim,k=k1-k2,r0=(/0.0,0.0,0.0/))

          call pw_states_borrow_wfc(bse%states,wfc1,ib1,ik1)
          call pw_states_borrow_wfc(bse%states,wfc2,ib3,ik2)
          call pw_wfc2field(wfc_field1,wfc1)
          call pw_wfc2field(wfc_field2,wfc2)
          call pw_states_giveback_wfc(bse%states,wfc1)
          call pw_states_giveback_wfc(bse%states,wfc2)

          call pw_field_mul(F_field1,wfc_field2,wfc_field1)
          call pw_field2wfc(F_wfc1,F_field1)

          call pw_field_destroy(wfc_field1)
          call pw_field_destroy(wfc_field2)

          call pw_field_init(wfc_field1,bse%states%struct)
          call pw_field_init(wfc_field2,bse%states%struct)
          call pw_field_set_dim(wfc_field1,dim,k=k1,r0=(/0.0,0.0,0.0/))
          call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))

          call pw_states_borrow_wfc(bse%states,wfc1,ib2,ik1)
          call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
          call pw_wfc2field(wfc_field1,wfc1)
          call pw_wfc2field(wfc_field2,wfc2)
          call pw_states_giveback_wfc(bse%states,wfc1)
          call pw_states_giveback_wfc(bse%states,wfc2)

          call pw_field_mul(F_field2,wfc_field2,wfc_field1)
          call pw_field2wfc(F_wfc2,F_field2) 


          tmp_wfc%val=0.0
        
          call num_vemul(tmp_wfc%val,wfc_coulomb%val,F_wfc1%val)
          
          if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
            bse%matrix%val(location,itrans2)-scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
            sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))

          call pw_basis_destroy(basis_field)
          call pw_wfc_destroy(F_wfc1)
          call pw_wfc_destroy(F_wfc2)
          call pw_field_destroy(wfc_field1)
          call pw_field_destroy(wfc_field2)
          call pw_field_destroy(F_field1)
          call pw_field_destroy(F_field2)
          call pw_wfc_destroy(tmp_wfc)
          call pw_basis_destroy(basis_coulomb)
          call pw_wfc_destroy(wfc_coulomb)

          if(ik1==ik2.and.ib1==ib3.and.ib2==ib4) then
             if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                  bse%matrix%val(location,itrans2) + &
                  (bse%states%e(ib1,ik1)-bse%states%e(ib2,ik2)+energy_shift)
          endif
       enddo
       if(bse%matrix%incomplete) iboh=iboh+1
     endif     
  enddo


! diagonalisation

  write(0,*)"Diagonalization BSE matrix"

  call num_parall_he_diag(bse%matrix,bse%matrix%root,bse%matrix%comm)

end subroutine pw_bse_macro_x0

subroutine pw_bse_x_calc_x(bse,qmesh,cutoff,coulomb_div_treatment,ecutvcut)
  use ptk_module, only : ptk_barrier
  use pw_bse_type
  use pw_bse_interf
  use iotk_module
  use pw_states_module
  use pw_field_module
  use pw_coulomb_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc_module
  use num_module
  use pw_fft_module
  use tools_module
  use num_la_parall_module
  use coulomb_vcut_module, only :vcut_type

  implicit none

  type(pw_bse), intent(inout) :: bse
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
  character(len=*), optional, intent(in) :: coulomb_div_treatment
  real, optional, intent(in) :: ecutvcut

  type(pw_coulomb) :: coulomb
  type(pw_basis) :: basis_coulomb
  type(pw_wfc) :: wfc_coulomb

  real :: ecutvcut_loc
  character(len=30) :: coulomb_div_treatment_loc
  integer :: system_type
  type(vcut_type) :: vcut

  type(pw_basis) :: basis_field
  type(pw_field) :: F_field1, F_field2
  type(pw_field) :: wfc_field1, wfc_field2
  type(pw_wfc), pointer :: wfc1, wfc2
  type(pw_wfc) :: F_wfc1, F_wfc2
  type(pw_wfc) :: tmp_wfc, tmp_wfc2

  integer :: ik1,ik2,idk,nk
  integer :: id0
  real, dimension(3) :: k0
  integer :: ib1, ib2, ib3, ib4
  real, dimension(3) :: k1, k2, k
  integer :: itrans1,itrans2
  integer, dimension(3) :: dim
  integer :: npw_small
  integer :: npw(3)

  real :: scale
  real :: lattice(3,3)

! ---------------------------------------------------------
! variables controling part of parallel flux
  integer :: iboh
  integer :: location
! --------------------------------------------------------

  coulomb_div_treatment_loc = "gigi-baldereschi"
  if(present(coulomb_div_treatment)) coulomb_div_treatment_loc = coulomb_div_treatment

  ecutvcut_loc=0.1
  if(present(ecutvcut)) ecutvcut_loc = ecutvcut

  nk=ubound(bse%states%wfc,2)

  scale=1.0/bse%states%struct%a_omega

  lattice = num_matmul(bse%states%struct%b,qmesh%m_inv)

  if(trim(coulomb_div_treatment_loc)=="vcut_ws") then
    call pw_vcut_init(vcut,bse%states%struct,qmesh,ecutvcut_loc)
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


  iboh=1

  do itrans1=1,bse%ntrans

     if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank.or.iboh==(bse%matrix%m_dim_loc+1)) then

     location=bse%matrix%where_line(2,itrans1)

     ik1=bse%iktrans(itrans1)
     ib1=bse%ib1trans(itrans1)
     ib2=bse%ib2trans(itrans1)
     
     k0(:)=0.0

     k1=bse%states%kmesh%kbz(:,ik1)

     id0=pw_kmesh_kbz_index(qmesh,(k0))

     call pw_field_init(wfc_field1,bse%states%struct)
     call pw_field_init(wfc_field2,bse%states%struct)
     call pw_field_init(F_field1,bse%states%struct)
     
     call pw_basis_init(basis_field,bse%states%struct)
     call pw_basis_create(basis_field,qmesh%kbz(:,id0),cutoff)
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

     call pw_field_destroy(wfc_field1)
     call pw_field_destroy(wfc_field2)
     call pw_field_destroy(F_field1)

     do itrans2=1,bse%ntrans

        ik2=bse%iktrans(itrans2)
        ib3=bse%ib1trans(itrans2)
        ib4=bse%ib2trans(itrans2)

        k2=bse%states%kmesh%kbz(:,ik2)
        
        call pw_field_init(wfc_field1,bse%states%struct)
        call pw_field_init(wfc_field2,bse%states%struct)
        call pw_field_init(F_field1,bse%states%struct)
        
        call pw_wfc_init(F_wfc2,basis_field)
        call pw_wfc_init(tmp_wfc,basis_field)

        dim=pw_field_dim_from_dipole(basis_field,bse%states%basis(ik2),bse%states%basis(ik2))
        call pw_field_set_dim(wfc_field1,dim,k=k2,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(wfc_field2,dim,k=k2,r0=(/0.0,0.0,0.0/))
        call pw_field_set_dim(F_field1,dim,k=k0,r0=(/0.0,0.0,0.0/))

        call pw_states_borrow_wfc(bse%states,wfc1,ib3,ik2)
        call pw_states_borrow_wfc(bse%states,wfc2,ib4,ik2)
        call pw_wfc2field(wfc_field1,wfc1)
        call pw_wfc2field(wfc_field2,wfc2)
        call pw_states_giveback_wfc(bse%states,wfc1)
        call pw_states_giveback_wfc(bse%states,wfc2)

        call pw_field_mul(F_field1,wfc_field2,wfc_field1)
        call pw_field2wfc(F_wfc2,F_field1)

        idk=pw_kmesh_kbz_index(qmesh,(k1-k2))
        call pw_basis_init(basis_coulomb,bse%states%struct)
        call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
        call pw_wfc_init(wfc_coulomb,basis_coulomb)
        call pw_coulomb_get(system_type,coulomb,wfc_coulomb)

!----------------------------------------------------------
! for inclusion of non local field effects
        wfc_coulomb%val(basis_coulomb%index_Gzero)=0.0
! ----------------------------------------------------------

        tmp_wfc%val=0.0
        npw(1)=ubound(wfc_coulomb%val,1)
        npw(2)=ubound(F_wfc1%val,1)
        npw(3)=ubound(tmp_wfc%val,1)
        npw_small = minval(npw)
        call num_vemul(tmp_wfc%val(1:npw_small),wfc_coulomb%val(1:npw_small),F_wfc1%val(1:npw_small))

        if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) &
          bse%matrix%val(location,itrans2)=2.0 * scale*pw_wfc_braket(F_wfc2,tmp_wfc)* &
            sqrt(bse%states%weight(ib3,ik2))*sqrt(bse%states%weight(ib4,ik2))
        call pw_field_destroy(wfc_field1)
        call pw_field_destroy(wfc_field2)
        call pw_field_destroy(F_field1)
        call pw_basis_destroy(basis_coulomb)
        call pw_wfc_destroy(wfc_coulomb)
        call pw_wfc_destroy(F_wfc2)
        call pw_wfc_destroy(tmp_wfc)

        if(ik1==ik2.and.ib1==ib3.and.ib2==ib4) then
          if(bse%matrix%where_line(1,itrans1)==bse%matrix%rank) bse%matrix%val(location,itrans2)= &
                 bse%matrix%val(location,itrans2) + &
                 (bse%states%e(ib1,ik1)-bse%states%e(ib2,ik2))
        endif

     enddo
     call pw_basis_destroy(basis_field)
     call pw_wfc_destroy(F_wfc1)
     if(bse%matrix%incomplete) iboh=iboh+1
    endif
  enddo

  call pw_coulomb_destroy(coulomb)

! diagonalisation

  call num_parall_he_diag(bse%matrix,bse%matrix%root,bse%matrix%comm)

end subroutine pw_bse_x_calc_x

subroutine pw_bse_transpose_x(bse,root,comm)
  use pw_bse_type
  use num_la_parall_module, only : num_parall_c_transpose
  use ptk_module, only : ptk_comm
  implicit none
  type (pw_bse),   intent(inout) :: bse
  integer, intent(in) :: root
  type (ptk_comm), intent(in)    :: comm
  call num_parall_c_transpose(bse%matrix,root,comm)
end subroutine pw_bse_transpose_x
