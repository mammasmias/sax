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

subroutine pw_selfinteraction3_x(states,nbmin,nbmax,ik,qmesh,cutoff,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank 
  use num_module
  use tools_module
  use iotk_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_states_module
  
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
  character(*), intent(in) :: outdir

  integer :: ib, ibp, ibx
  integer :: ikx, idk
  integer :: dim(3)
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb)     :: coulomb
  type (pw_wfc)         :: wfc_coulomb
  type (pw_field) :: wfc_field
  type (pw_wfc) :: dip_wfc
  type (pw_wfc) :: dip_wfc2
  real    :: scale
  real :: lattice(3,3)
  real :: a_omega
  real :: b(3,3)
  integer :: myrank
  integer :: iboh

!  real :: m_inv(3,3)

  complex :: selfint(nbmin:nbmax)

!  m_inv(:,:) = 0.0

!  m_inv(1,1) = 1.0
!  m_inv(2,2) = 1.0
!  m_inv(3,3) = 1.0

  call ptk_comm_rank(states%comm,myrank)

  selfint(:) =0.0

  iboh = 1

  lattice=num_matmul(states%struct%b,qmesh%m_inv)

!  lattice=num_matmul(states%struct%b,m_inv)
!  write(0,*)"num_discontinuity_value: ", num_discontinuity_value(lattice,cutoff)/ &
!     states%struct%a_omega

  ikx = ik
  k = states%kmesh%kbz(:,ik)
  kx = states%kmesh%kbz(:,ikx)
  idk = pw_kmesh_kbz_index(qmesh,(k-kx))


  call pw_field_init(dip_field,states%struct)
  call pw_field_init(exc_field,states%struct)
  call pw_field_init(wfc_field,states%struct)
  call pw_basis_init(basis_coulomb,states%struct)
  call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
  call pw_coulomb_init(coulomb,lattice)
  call pw_wfc_init(wfc_coulomb,basis_coulomb)
  call pw_coulomb_get(3,coulomb,wfc_coulomb)
  if(idk==pw_kmesh_kibz_index(qmesh,(/0.0,0.0,0.0/))) wfc_coulomb%val(1) = num_discontinuity_value(lattice,cutoff)
  call pw_coulomb_destroy(coulomb)
  dim = pw_field_dim_from_dipole(basis_coulomb,states%basis(ikx), &
   states%basis(ik))
  call pw_field_set_dim(wfc_field, dim, k=k,r0=(/0.0,0.0,0.0/))
  call pw_field_set_dim(exc_field, dim, k=kx,r0=(/0.0,0.0,0.0/))
  call pw_field_set_dim(dip_field, dim, k=k-kx,r0=(/0.0,0.0,0.0/))
  call pw_wfc_init(dip_wfc,basis_coulomb)
  call pw_wfc_init(dip_wfc2,basis_coulomb)

  do ibx= nbmin,nbmax
     if(pw_states_is_local(states,ib=ibx,ik=ikx).or.iboh==(states%nband_loc+1)) then
       call pw_states_borrow_wfc(states,wfc,ibx,ikx)
       call pw_wfc2field(wfc_field,wfc)
       call pw_states_giveback_wfc(states,wfc)
       scale=1.0/states%struct%a_omega*states%weight(ibx,ikx)
       call pw_states_borrow_wfc(states,exc_wfc,ibx,ikx)
       call pw_wfc2field(exc_field,exc_wfc)
       call pw_states_giveback_wfc(states,exc_wfc)
       call pw_field_mul(dip_field,exc_field,wfc_field)
       call pw_field2wfc(dip_wfc,dip_field)

       call num_vemul(dip_wfc2%val,wfc_coulomb%val,dip_wfc%val)
         
       if(pw_states_is_local(states,ib=ibx,ik=ikx)) then
                selfint(ibx)=-pw_wfc_braket(dip_wfc,dip_wfc2) * &
                 scale 
       endif
       if(states%incomplete) iboh=iboh+1
     endif
  enddo

  call pw_wfc_destroy(dip_wfc)
  call pw_wfc_destroy(dip_wfc2)
  call pw_wfc_destroy(wfc_coulomb)
  call pw_basis_destroy(basis_coulomb)
   
  
  call pw_field_destroy(wfc_field)
  
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)


! just reduce selfint    
  call ptk_allreduce_inplace(selfint,ptk_sum,states%comm)
  
  selfint(:)=abs(selfint(:))/num_discontinuity_value(lattice,cutoff)* &
     states%struct%a_omega

  if(myrank==states%root) then
    call iotk_open_write(unit=65,binary=.false.,file=trim(outdir)//"self-interaction"//trim(iotk_index(ik)))
       call iotk_write_dat(65,"selfinteraction"//trim(iotk_index(ik)),selfint)
    call iotk_close_write(65)
  endif 

end subroutine pw_selfinteraction3_x

subroutine pw_selfinteraction0_x(states,nbmin,nbmax,ik,qmesh,cutoff,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank 
  use num_module
  use tools_module
  use iotk_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_states_module
  
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  type(pw_kmesh), intent(in) :: qmesh
  real, intent(in) :: cutoff
  character(*), intent(in) :: outdir

  integer :: ib, ibp, ibx
  integer :: ikx, idk
  integer :: dim(3)
  integer :: npw_small
  real    :: k(3),kx(3)
  type (pw_field)       :: exc_field,dip_field
  type (pw_wfc),   pointer :: exc_wfc,wfc
  type (pw_basis)       :: basis_coulomb
  type (pw_coulomb)     :: coulomb
  type (pw_wfc)         :: wfc_coulomb
  type (pw_field) :: wfc_field
  type (pw_wfc) :: dip_wfc
  type (pw_wfc) :: dip_wfc2
  real    :: scale
  real :: lattice(3,3)
  real :: a_omega
  real :: b(3,3)
  integer :: myrank
  integer :: iboh
  real :: a(3,3)
  complex :: v0

  complex :: selfint(nbmin:nbmax)

  call ptk_comm_rank(states%comm,myrank)

  selfint(:) =0.0
  a(:,:) = 0.0

  iboh=1

  lattice=num_matmul(states%struct%b,qmesh%m_inv)

! for V(0)
! maybe with k points rcut should be minval(sqrt(sum(lattice**2,1)))?
! da guardare 

  a = states%struct%a
  Rcut=0.5*minval(sqrt(sum(a**2,1)))
  Rcut=Rcut-Rcut/50.0
  v0=num_8pi*Rcut**2/2.0
!

  ikx = ik
  k = states%kmesh%kbz(:,ik)
  kx = states%kmesh%kbz(:,ikx)
  idk = pw_kmesh_kbz_index(qmesh,(k-kx))


  call pw_field_init(dip_field,states%struct)
  call pw_field_init(exc_field,states%struct)
  call pw_field_init(wfc_field,states%struct)
  call pw_basis_init(basis_coulomb,states%struct)
  call pw_basis_create(basis_coulomb,qmesh%kbz(:,idk),cutoff)
  call pw_coulomb_init(coulomb,states%struct%b)
  call pw_wfc_init(wfc_coulomb,basis_coulomb)
  call pw_coulomb_get(0,coulomb,wfc_coulomb)
  call pw_coulomb_destroy(coulomb)
  dim = pw_field_dim_from_dipole(basis_coulomb,states%basis(ikx), &
   states%basis(ik))
  call pw_field_set_dim(wfc_field, dim, k=k,r0=(/0.0,0.0,0.0/))
  call pw_field_set_dim(exc_field, dim, k=kx,r0=(/0.0,0.0,0.0/))
  call pw_field_set_dim(dip_field, dim, k=k-kx,r0=(/0.0,0.0,0.0/))
  call pw_wfc_init(dip_wfc,basis_coulomb)
  call pw_wfc_init(dip_wfc2,basis_coulomb)


  do ibx=nbmin,nbmax
     if(pw_states_is_local(states,ib=ibx,ik=ikx).or.iboh==(states%nband_loc+1)) then
       call pw_states_borrow_wfc(states,wfc,ibx,ikx)
       call pw_wfc2field(wfc_field,wfc)
       call pw_states_giveback_wfc(states,wfc)
       scale=1.0/states%struct%a_omega*states%weight(ibx,ikx)
       call pw_states_borrow_wfc(states,exc_wfc,ibx,ikx)
       call pw_wfc2field(exc_field,exc_wfc)
       call pw_states_giveback_wfc(states,exc_wfc)
       call pw_field_mul(dip_field,exc_field,wfc_field)
       call pw_field2wfc(dip_wfc,dip_field)

        
       call num_vemul(dip_wfc2%val,wfc_coulomb%val,dip_wfc%val)
         
       if(pw_states_is_local(states,ib=ibx,ik=ikx)) then
         selfint(ibx)=-pw_wfc_braket(dip_wfc,dip_wfc2) * &
                 scale 
       endif
       if(states%incomplete) iboh=iboh+1 
     endif    
  enddo

   
  call pw_wfc_destroy(dip_wfc)
  call pw_wfc_destroy(dip_wfc2)
  call pw_wfc_destroy(wfc_coulomb)
  call pw_basis_destroy(basis_coulomb)
   
  
  call pw_field_destroy(wfc_field)
  
  call pw_field_destroy(dip_field)
  call pw_field_destroy(exc_field)


! just reduce selfint    
  call ptk_allreduce_inplace(selfint,ptk_sum,states%comm)

  selfint(:)=selfint(:)/v0/ &
     states%struct%a_omega

  if(myrank==states%root) then
    call iotk_open_write(unit=65,binary=.false.,file=trim(outdir)//"self-interaction"//trim(iotk_index(ik)))
       call iotk_write_dat(65,"selfinteraction"//trim(iotk_index(ik)),selfint)
    call iotk_close_write(65)

  endif 

end subroutine pw_selfinteraction0_x

subroutine pw_participationratio_x(states,nbmin,nbmax,ik,cutoff,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank 
  use num_module
  use tools_module
  use iotk_module
  use pw_wfc_module
  use pw_field_module
  use pw_fft_module
  use pw_basis_module
  use pw_kmesh_module
  use pw_wfc6d_module
  use pw_coulomb_module
  use pw_states_module
  
  type(pw_states), intent(in) :: states
  integer, intent(in) :: nbmin, nbmax
  integer, intent(in) :: ik
  real, intent(in) :: cutoff
  character(*), intent(in) :: outdir

  integer :: ibx
  integer :: dim(3)
  real    :: k(3)
  type (pw_field)       :: field
  type (pw_basis)       :: basis
  type (pw_field) :: rdensity
  type (pw_wfc) :: rdensity_wfc
  type(pw_wfc), pointer :: wfc
  real    :: scale
  real :: a_omega
  integer :: myrank

  complex :: partratio(nbmin:nbmax)

  call ptk_comm_rank(states%comm,myrank)

  partratio(:) =0.0

  k = states%kmesh%kbz(:,ik)

  call pw_basis_init(basis,states%struct)
  call pw_basis_create(basis,(/0.0,0.0,0.0/),cutoff)

  call pw_field_init(rdensity,states%struct)
  call pw_field_init(field,states%struct)
  dim = pw_field_dim_from_dipole(basis,states%basis(ik),states%basis(ik))
  call pw_field_set_dim(field, dim, k=k, r0=(/0.0,0.0,0.0/))
  call pw_field_set_dim(rdensity,dim ,k=(/0.0,0.0,0.0/),r0=(/0.0,0.0,0.0/))

  call pw_wfc_init(rdensity_wfc,basis)
  write(0,*) "from pptools ext, dim", dim(1), dim(2), dim(3)
  write(0,*) "from pptools ibx, dens2, sum_dens/states%struct%a_omega"
  do ibx=nbmin,nbmax
     field%val(:)=0.0
     rdensity%val(:)=0.0
     rdensity_wfc%val(:)=0.0

     if(.not.pw_states_is_local(states,ib=ibx,ik=ik)) cycle

     call pw_states_borrow_wfc(states,wfc,ibx,ik)
     write(0,*)"wfc braket", pw_wfc_braket(wfc,wfc)

     call pw_wfc2field(field,wfc)
     call pw_states_giveback_wfc(states,wfc)
     call pw_field_mul(rdensity,field,field)

!     call pw_field2wfc(rdensity_wfc,rdensity)
     
     sum_dens=0.0
     dens2=0.0 
     do ix=lbound(rdensity%val,1),ubound(rdensity%val,1)
       dens2=dens2+rdensity%val(ix)**2 
       sum_dens=sum_dens+rdensity%val(ix)
     enddo
     write(0,*) ibx, sum_dens, dens2

     partratio(ibx)=dens2/sum_dens
  enddo

   
  call pw_basis_destroy(basis)
  call pw_field_destroy(field)
  call pw_wfc_destroy(rdensity_wfc)
  call pw_field_Destroy(rdensity)


! just reduce selfint    
  call ptk_allreduce_inplace(partratio,ptk_sum,states%comm)
  

  if(myrank==states%root) then
    call iotk_open_write(unit=65,binary=.false.,file=trim(outdir)//"participation-ratio"//trim(iotk_index(ik)))
       call iotk_write_dat(65,"participationratio"//trim(iotk_index(ik)),partratio)
    call iotk_close_write(65)

  endif 

end subroutine pw_participationratio_x

subroutine pw_dos_x(nomega,omegamin,omegamax,degauss,nkbz,nbmin,nbmax, &
  diagonal,start_from,ionode,outp,outdir)
  use ptk_module, only : ptk_allreduce_inplace, ptk_sum, ptk_allreduce, &
      ptk_barrier, ptk_comm_rank, ptk_comm_world 
  use num_module
  use tools_module
  use iotk_module
  use pw_QP_module

  integer, intent(in)         :: nomega, nbmin, nbmax, nkbz
  real, intent(in)            :: omegamin,omegamax,degauss
  character(10), intent(in)   :: start_from
  logical, intent(in)         :: diagonal, ionode
  character(256), intent(in)  :: outp
  character(*), intent(in)    :: outdir
  real, allocatable           :: energies(:,:), dos(:,:)
  integer                     :: i,j,k, nbnd
  real                        :: sumdos, pisqrt, deltae
  type(pw_QP)                 :: QP

  if(start_from=="DFT") then
!      call tools_log("Start from a DFT calculation")
     ERROR("Not implemented yet")
  elseif(start_from=="HF") then
     call tools_log("Start from a HF calculation")
  elseif(start_from=="GW") then
    call tools_log("Start from a GW calculation")
  else
      ERROR("Wrong start_from")
  endif

  call pw_QP_init(QP,nkbz,nbmin,nbmax,diagonal)
  call tools_log("Reading QP...",advance=.false.)

  select case(start_from)
  case("HF")

  if(ionode) call iotk_open_read(10,file=trim(outdir)//"HF_QP",binary=.false.)
  call pw_QP_readbcast(QP,nkbz,10,"HF_QP",0,ptk_comm_world)

!  case("DFT")  
!  if(ionode) call iotk_open_read(10,file="DFT_QP",binary=.false.)
!  call pw_QP_readbcast(QP,nkbz,10,"DFT_QP",0,ptk_comm_world)
  
  case("GW")
  if(ionode) call iotk_open_read(10,file=trim(outdir)//"GW_QP",binary=.false.)
  call pw_QP_readbcast(QP,nkbz,10,"GW_QP",0,ptk_comm_world)
 
  end select
  if(ionode) call iotk_close_read(10)
  call tools_log("done")

  deltae=(omegamax-omegamin)/nomega
  nbnd = nbmax - nbmin + 1
  allocate(energies(nbnd,nkbz))
  do i=1, nbnd
     energies(i,:) = QP%energies(QP%nbmin+i-1,:)
  enddo
  call pw_QP_destroy(QP)
  
  allocate(dos(nomega,2))
  dos(:,:)=0.0
  pisqr=sqrt(2/num_pi)
 
  call tools_log("Calculating DoS...",advance=.false.)
  do k=1, nomega
     dos(k,2)=omegamin+deltae*(k-1)
     do j=1, nbnd
        do i=1, nkbz
        dos(k,1)=dos(k,1)+degauss*pisqr*exp(-(dos(k,2)-energies(j,i))**2/(2*degauss**2))
        enddo
     enddo
  enddo
  sumdos=0.0
  do i=1,nomega
     sumdos=sumdos+dos(i,1)
  enddo
  dos(:,1)=dos(:,1)/sumdos
  
  call tools_log(" done")

  open(10,file=trim(outp),status="unknown")
  do i=1,nomega
    write(10,*) dos(i,2), dos(i,1)
  enddo
  close(10)
  
  deallocate(energies)
  deallocate(dos)
end subroutine pw_dos_x

subroutine pw_band_1_x(states,kmesh,k_read,num_k_read,atoms,nbmin,nbmax,file_out,outdir)
  use tools_module
  use pw_states_module
  use num_module
  use pw_kmesh_module
  use ptk_module
  use pw_pseudovelocity_module
  use pw_basis_module
  use pw_atoms_module
  use pw_wfc_module
 
  type (pw_states),intent(in) :: states
  integer, intent(in)         :: num_k_read, nbmin, nbmax
  real, intent(in)            :: k_read(num_k_read,3)
  type (pw_kmesh),intent(in)  :: kmesh
  type(pw_atoms), intent(in)  :: atoms
  character(256), intent(in)  :: file_out
  character(*), intent(in)    :: outdir

  type(pw_basis), pointer :: basis
  type (pw_wfc),  pointer :: wfc
  type(pw_pseudovelocity) :: pseudovelocity
  complex                 :: scaled_velocity(3)
  integer                 :: ik_nn, ik_nn_x, ik_nn_y, ik_nn_z
  complex                 :: qxyz(3)

  real                    :: k_r(3)
  real                    :: kxyz(3), shift(3), kbase(3,3)
  integer                 :: iboh, ib, ik
  real, allocatable       :: band(:,:)
  
  allocate(band(num_k_read,(nbmax-nbmin+1)))
  shift = kmesh%kbz(:,1)
  kbase = num_matmul(kmesh%m_inv,states%struct%b)
  call tools_log("Calculating Band Structure...")
  call tools_log("Nearest Neighbor interpolation")
  ik_l=0
  do ik=1,num_k_read
     call tools_log("kpoint:",advance=.false.)
     write(tools_log_unit(),"(i5)") ik
     k_r(:) = MODULO(k_read(ik,:),1.0) ! goes to [0,1[ range
     kxyz = num_matmul(real(kmesh%m),(shift + k_r(:)))
     ik_nn_x = floor(kxyz(1)+0.5)
     ik_nn_y = floor(kxyz(2)+0.5)
     ik_nn_z = floor(kxyz(3)+0.5)
     ik_nn = 1 + ik_nn_x + ik_nn_y*kmesh%m(1,1) + ik_nn_z*kmesh%m(1,1)*kmesh%m(2,2)
     qxyz = kmesh%kbz(:,ik_nn) - k_r(:)
     call pw_states_borrow_basis(states,basis,ik_nn)
     call pw_pseudovelocity_init(pseudovelocity,basis,atoms)
     iboh = 1
     do ib=nbmin, nbmax
        if(pw_states_is_local(states,ib=ib,ik=ik_nn).or.iboh==(states%nband_loc+1)) then
          call pw_states_borrow_wfc(states,wfc,ib,ik_nn)
          scaled_velocity = (pw_wfc_p_braket(wfc,wfc) &
                         +pw_pseudovelocity_braket(pseudovelocity,wfc,wfc))
          band(ik,ib) = states%e(ib,ik_nn) - num_dotp(scaled_velocity, qxyz)
          call pw_states_giveback_wfc(states,wfc)
        endif
     enddo
     call pw_pseudovelocity_destroy(pseudovelocity)
     call pw_states_giveback_basis(states,basis)
  enddo
  
  open(10,file=trim(outdir)//trim(file_out),status="unknown")
  do ib=nbmin, nbmax
     do ik=1, num_k_read
        write (10,*) ik, band(ik,ib)
     enddo
     write (10,*)
  enddo
  close(10)
  deallocate(band)
  call tools_log(" done")
  
end subroutine pw_band_1_x


subroutine pw_band_2_x(states,kmesh,k_read,num_k_read,nbmin,nbmax,m,n,p,q, &
  file_out,outdir)
  use tools_module
  use pw_states_module
  use num_module
  use pw_kmesh_module
  use ptk_module
  
  type (pw_states),intent(in) :: states
  integer, intent(in)         :: num_k_read, nbmin, nbmax
  real, intent(in)            :: k_read(num_k_read,3)
  type (pw_kmesh),intent(in)  :: kmesh
  character(256), intent(in)  :: file_out
  real, intent(in)            :: m, n, p, q
  character(*), intent(in)    :: outdir
  
  real                    :: k_r(3)
  real                    :: kxyz(3), shift(3), kbase(3,3)
  integer                 :: iboh, ib, ik
  real, allocatable       :: band(:,:)
  
  integer                 :: igrid(4,2,2,2), j1,j2,j3, ix
  real                    :: delta(3)
  real                    :: et
  
  allocate(band(num_k_read,(nbmax-nbmin+1)))
  shift = kmesh%kbz(:,1)
  kbase = num_matmul(kmesh%m_inv,states%struct%b)
  call tools_log("Calculating Band Structure...")
  call tools_log("Trilinear interpolation")
  ik_l=0
  do ik=1,num_k_read
     call tools_log("kpoint:",advance=.false.)
     write(tools_log_unit(),"(i5)") ik
     k_r(:) = MODULO(k_read(ik,:),1.0) ! goes to [0,1[ range
     kxyz = num_matmul(real(kmesh%m),(shift + k_r))

     igrid(1,1,1,1) = floor(kxyz(1))
     igrid(2,1,1,1) = floor(kxyz(2))
     igrid(3,1,1,1) = floor(kxyz(3))
     do j3=1,2
        do j2=1,2
           do j1=1,2
              igrid(1,j1,j2,j3) = MOD(igrid(1,1,1,1) + (j1-1), kmesh%m(1,1))
              igrid(2,j1,j2,j3) = MOD(igrid(2,1,1,1) + (j2-1), kmesh%m(2,2))
              igrid(3,j1,j2,j3) = MOD(igrid(3,1,1,1) + (j3-1), kmesh%m(3,3))
              igrid(4,j1,j2,j3) = 1+igrid(1,j1,j2,j3)+igrid(2,j1,j2,j3)*kmesh%m(1,1) &
                           + igrid(3,j1,j2,j3)*kmesh%m(1,1)*kmesh%m(2,2)
           enddo
        enddo
     enddo
     do j1=1,3
        delta(j1) = real(igrid(j1,1,1,1)) - kxyz(j1)
     enddo
     iboh = 1
     do ib=nbmin,nbmax
        if(pw_states_is_local(states,ib=ib,ik=ik_nn).or.iboh==(states%nband_loc+1)) then
          band(ik,ib)=0.0
          do j3=1,2
             do j2=1,2
                do j1=1,2
                   et = states%e(ib,igrid(4,j1,j2,j3))*(-1)**(j1+j2+j3-1)    &
                  *(1.0*real(2-j1)+delta(1))*(1.0*real(2-j2)+delta(2))*(1.0*real(2-j3)+delta(3))
                   if(states%occupation(ib,igrid(4,j1,j2,j3))<1.99) then
                      et=et*n+q/8
                   else
                      et=et*m+p/8
                   endif
                   band(ik,ib)=band(ik,ib)+et
                enddo
             enddo
          enddo
        endif
     enddo
  enddo
  
  
  open(10,file=trim(outdir)//trim(file_out),status="unknown")
  do ib=nbmin, nbmax
     do ik=1, num_k_read
        write (10,*) ik, band(ik,ib)
     enddo
     write (10,*)
  enddo
  close(10)
  deallocate(band)
  call tools_log(" done")
  
end subroutine pw_band_2_x

