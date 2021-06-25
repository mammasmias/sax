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
#include "sax_version.h"
subroutine gw_program()
!
use ptk_module
use iotk_module
use mp_global
use gw_options_module
use pw_module
use tools_module
use num_module
use pw_QP_module
use pw_QP_states_module
use pw_epsilon_macro_module
use pw_pp_parameters_module
use pw_smearing_module
use sax_to_want_module
use coulomb_vcut_module
use pw_coh_module

implicit none

integer :: rank,ierr,npe

character(256) :: args(256)
integer        :: nargs

type(pw_sigma) :: sigma
type(pw_sigma) :: dsigma

type(pw_hartree) :: vcoh
type(pw_pp_parameters) :: pp_parameters

type(sax_options)            :: options         ! Contiene le opzioni globali del codice
type(pw_struct)     :: struct          ! Contiene i dati di struttura (dim. cella) del sistema in analisi
type(pw_atoms)              :: atoms           ! Struttura atomica e pseudopotenziali
type(pw_symmlist)   :: symmlist        ! Contiene una lista delle simmetrie del sistema
type(pw_kmesh)      :: kmesh           ! Contiene la mesh per le funzioni d'onda
type(pw_kmesh)      :: qmesh           ! Contiene la mesh per gli elementi di dipolo
                                               ! (le differenze in kmesh)
type(pw_states)             :: states          ! Contiene tutti gli stati di singola particella e
                                               !   le energie
type(pw_density)            :: density        ! Contiene la density

type(pw_QP)                 :: QP
type(pw_states)             :: QP_states
type(pw_epsilon_macro)            :: epsilon
type(pw_smearing)           :: smearing
type(vcut_type)             :: vcut

integer :: ib1,ib2,ik1,ik2,ibmin,ibmax
integer :: log_unit

integer :: iostat

type(pw_basis), pointer     :: basis_tmp

type(pw_polar)     :: polar ! Polarizzabilita'
type(pw_w)                  :: w            ! Contiene W

integer :: ip,ik,ib,ibp,ibb,ibc,ibv,iq,ikv,ikc ! indici temporanei
real :: energy_ewald
real    :: efermi
logical :: ionode
real :: states_cutoff
logical :: first_QPcall

complex, allocatable :: field_tmp(:,:,:)
type(pw_field) :: coh_field
real, allocatable :: grid(:,:)
integer :: i1,i2,i3, i
integer :: dim1, dim2, dim3
integer :: str2, str3
integer :: dim(1:3)

character(256) :: infile,logfile

integer :: ipool, my_pool_id

call ptk_comm_rank(ptk_comm_world,rank=rank)
call ptk_comm_size(ptk_comm_world,size=npe)
call init_pool(npool,ptk_comm_world)

ionode = .false.
if(rank==0) ionode = .true.

if(ionode) then
  call iotk_readcmdline(args,nargs)
  if(nargs/=2) ERROR("")
  infile  = args(1)
  logfile = args(2)
end if

call ptk_bcast(infile,0,ptk_comm_world)
call ptk_bcast(logfile,0,ptk_comm_world)

call tools_log_set_file(trim(logfile))
call tools_artwork_print(tools_log_unit(),"sax")
call tools_log("www.sax-project.org, version "//__SAX_VERSION)
call tools_log("Program gw.x starts")
call tools_log("This is log from pe "//tools_char(rank,3)//" of "//tools_char(npe,3))

call tools_log("Writing here initial mallinfo ...")
call tools_mallinfo_print(tools_log_unit(),msg="MALLINFO")
call tools_log("... done")
call tools_log("Writing here initial PW statistics ...")
call pw_print_statistics(tools_log_unit())
call tools_log("... done")

call tools_log("Pasting input file "//trim(infile)//" ...")
call iotk_copyfile(dest_unit=tools_log_unit(),source=trim(infile))
call tools_log("... done")

call tools_log("Opening input file "//trim(infile)//" ...",advance=.false.)
if(ionode) call iotk_open_read(unit=15,file=trim(infile))
call tools_log(" done")

call tools_log("Reading options ...",advance=.false.)
call gw_options_readbcast(options,15,0,ptk_comm_world,fmt="iotk",name="sax_options")
call tools_log(" done")

!----------------------------------------------------
! Kind of calculation and system initialisation

if(options%calculation_kind=="HF") then
   call tools_log("Self-Energy calculated within HF approximation")
elseif(options%calculation_kind=="GW") then
   call tools_log("Self_Energy calculated within GW approximation")
else
   ERROR("Wrong calculation_kind")
endif

if(options%system_kind=="0D") then
   call tools_log("0D system")
elseif(options%system_kind=="1D") then
   ERROR("1D system kind has to be treated as 3D with coulomb_div_treatment=vcut_ws. See example4")
elseif(options%system_kind=="2D") then
   call tools_log("2D system kind not implemented yet, assuming 3D")
   options%system_kind="3D"
elseif(options%system_kind=="3D") then
   call tools_log("3D system kind")
else
   ERROR("Wrong system_kind")
endif

!----------------------------------------------------
call tools_log("Reading structure ...",advance=.false.)
call pw_struct_readbcast(struct,15,0,ptk_comm_world,name="structure")
call tools_log(" done")

call pw_atoms_init(atoms,struct,options%cutoff_vloc)
call tools_log("Reading atoms ...",advance=.false.)
call pw_atoms_readbcast(atoms,15,0,ptk_comm_world,fmt="iotk",name="atoms")
call tools_log(" done")

call tools_log("Symmetries are not implemented")
call pw_symmlist_init(symmlist,struct)
call pw_symmlist_only_identity(symmlist)

call pw_kmesh_init(kmesh,symmlist)
call pw_kmesh_init(qmesh,symmlist)

call tools_log("Reading kmesh ...",advance=.false.)
call pw_kmesh_readbcast(kmesh,15,0,ptk_comm_world,fmt="iotk",name="kmesh")
call pw_kmesh_set_dim(qmesh,kmesh%m,(/0.0,0.0,0.0/))
call tools_log(" done")

call tools_log("Closing input file "//trim(infile)//" ...",advance=.false.)
if(ionode) call iotk_close_read(unit=15)
call tools_log(" done")

call tools_log("Writing here the kmesh for PWSCF code ...")
call pw_kmesh_write(kmesh,tools_log_unit(),"pwscf")
call tools_log("... done")


if(options%convert_from_pwscf) then
  call tools_log("Initializing smearing for fermi energy and occupations...",advance=.false.)
  call pw_smearing_init(smearing,options%smearing,options%smearing_kind)
  call tools_log(" done")

  call tools_log("Smearing parameters should be coherent with the previous calculation")

  if(options%smearing_kind=="none") then
    call tools_log("The system is treated as a semi-conductor/insulator")
  elseif((options%smearing_kind=="gauss").or.(options%smearing_kind=="gaussian")) then
    call tools_log("Occupations and fermi energy calculated with a gaussian smearing")
  elseif((options%smearing_kind=="methfessel-paxton").or.(options%smearing_kind=="mp").or. &
      (options%smearing_kind=="m-p")) then
    call tools_log("Occupations and fermi energy calculated with a methfessel-paxton smearing")
  elseif((options%smearing_kind=="marzari-vanderbilt").or.(options%smearing_kind=="cold").or. &
     (options%smearing_kind=="mv").or.(options%smearing_kind=="m-v")) then
    call tools_log("Occupations and fermi energy calculated with a marzari-vanderbilt smearing")
  elseif((options%smearing_kind=="fermi-dirac").or.(options%smearing_kind=="f-d").or. &
     (options%smearing_kind=="fd")) then
    call tools_log("Occupations and fermi energy calculated with a fermi-dirac smearing")
  else
    ERROR("Wrong smearing kind")
  endif

  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init(states,smearing,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,ptk_comm_world)
  call tools_log(" done")
  call tools_log("Converting states from pwscf format to file 'states' ...",advance=.false.)
  if(ionode) call iotk_open_write(10,file=trim(options%outdir)//"states",binary=.true.)
  call pw_states_convert_from_newp(states,trim(options%convert_from_pwscf_file),10,options%nelec,"states")
  if(ionode) call iotk_close_write(10)
  call tools_log(" done")
  call pw_states_destroy(states)
end if


if(options%start_from=="DFT") then
 call tools_log("Start from a DFT calculation")
elseif(options%start_from=="HF") then
  call tools_log("Start from a HF calculation")
elseif(options%start_from=="GW") then
  call tools_log("Start from a GW calculation")
else
  ERROR("Wrong start_from")
endif

! QUI INIZIA IL LOOP AUTOCONSISTENTE (?????)
select case(options%start_from)
case("DFT")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm)
  call tools_log(" done")
  call tools_log("Reading states from file 'states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.false.)
  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  do ipool=0,npool-1
     if(my_pool_id==ipool) then
       if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.true.) 
       call pw_states_read(states,10,"states")
       if(states%rank==states%root) call iotk_close_read(10)
     endif
  enddo
!  if(ionode) call iotk_close_read(10)
  call tools_log(" done")
case("HF")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm)
  call tools_log(" done")
  call tools_log("Reading states from file 'HF_states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.)
  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  do ipool=0,npool-1
     if(my_pool_id==ipool) then
       if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.) 
       call pw_states_read(states,10,"HF_states")
       if(states%rank==states%root) call iotk_close_read(10)
     endif
  enddo
!  if(ionode) call iotk_close_read(10)
  call tools_log(" done")
case("GW")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm)
  call tools_log(" done")
  call tools_log("Reading states from file 'GW_states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.)
  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  do ipool=0,npool-1
     if(my_pool_id==ipool) then
       if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.) 
       call pw_states_read(states,10,"GW_states")
       if(states%rank==states%root) call iotk_close_read(10)
     endif
  enddo
!  if(ionode) call iotk_close_read(10)
  call tools_log(" done")
end select

states_cutoff = pw_states_get_cutoff(states)
call tools_log("The cutoff for single particle wfc is: ",advance=.false.)
write(tools_log_unit(),"(f15.9)",advance='no') states_cutoff
call tools_log(" Ry")

!if(modulo(options%nbandmax-options%nbandmin+1,npe)/=0) &
!   ERROR("nb bands in states should be a multiple of nb proc")

if(options%calc_expectation_values) then

  call tools_log("Initializing density ...",advance=.false.)
  call pw_density_init(density,struct,options%cutoff_density*states_cutoff)
  call tools_log(" done")

  call tools_log("The cutoff for density is: ",advance=.false.)
  write(tools_log_unit(),"(f15.9)",advance='no') pw_basis_get_cutoff(density%basis)
  call tools_log(" Ry")

  call tools_log("Calculating the total density ...",advance=.false.)
  call pw_states_density(states,density)
  call tools_log(" done")

  call tools_log("Calculating DFT expectation values ...",advance=.false.)
  call pw_states_calc_expectation(states,options%sigma_nbmin, &
       options%sigma_nbmax,options%diagonal,atoms,density)
  call tools_log("done")
  call tools_log("DFT expectation values writen on files exp_dft.ik",advance=.true.)
  call tools_log("done")
  call pw_density_destroy(density)
endif 

if(options%calc_sp_hmatrix) then
  call tools_log("Initializing density...",advance=.false.)
  call pw_density_init(density,struct,options%cutoff_density*states_cutoff)
  call tools_log("done")
  call tools_log("The cutoff for density is:",advance=.false.)
  write(tools_log_unit(),"(f15.9)",advance='no') pw_basis_get_cutoff(density%basis)
  call tools_log(" Ry")
  call tools_log("Calculating the total density...",advance=.false.)
  call pw_states_density(states,density)
  call tools_log("done")

  call tools_log("Calculating single particle hamiltonian matrix...",advance=.false.)
  if(options%system_kind=="0D") then
    call pw_states_calc_sp_hmatrix(0,states,options%sigma_nbmin,options%sigma_nbmax, &
      options%diagonal,atoms,density,options%outdir)
  else
    call pw_states_calc_sp_hmatrix(3,states,options%sigma_nbmin,options%sigma_nbmax, &
      options%diagonal,atoms,density,options%outdir)
  endif
  call tools_log("done")
  call pw_density_destroy(density)
  call tools_log("Single-particle expectation values writen on files exp_sp.ik",advance=.true.)
endif



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
select case(options%calculation_kind)
case("HF")

  call pw_QP_init(QP,states%kmesh%nkbz,options%sigma_nbmin,options%sigma_nbmax, &
     options%diagonal)

  ! AF
  !call tools_log("Calculating HF energies ...",advance=.false.)
  call tools_log("Calculating HF energies ...",advance=.true.)
  !
  do ik=1,states%kmesh%nkbz
       ! AF
       call tools_log("Computing ik("//TRIM(tools_char(ik,5))//") ..." ,advance=.false.)
       !
       if(trim(options%coulomb_div_treatment).ne."vcut_ws") then

!     if(options%system_kind=="0D") then
!       call pw_hf_calc0(QP,states,options%sigma_nbmin,options%sigma_nbmax, &
!          ik,options%diagonal,qmesh,options%cutoff_fock)
!     else
         call pw_hf_calc3(QP,states,options%sigma_nbmin,options%sigma_nbmax, &
            ik,options%diagonal,qmesh,options%cutoff_fock,options%coulomb_div_treatment, &
            outdir=options%outdir)
       else
         call pw_hf_calc3(QP,states,options%sigma_nbmin,options%sigma_nbmax, &
            ik,options%diagonal,qmesh,options%cutoff_fock,options%coulomb_div_treatment, &
            options%ecutvcut, options%outdir)
       endif
!     endif

       !
       ! AF
       call tools_log("done")
       !
  enddo

  call tools_log("done")

  if(ionode) then
    call tools_log("Writing HF energies ...",advance=.false.)
    call iotk_open_write(10,file=trim(options%outdir)//"HF_QP",binary=.false.)
    call pw_QP_write(QP,states%kmesh%nkbz,10,"HF_QP")
    call iotk_close_write(10)
    call tools_log("done")
  endif

  call pw_QP_destroy(QP)

! Conversion of SAX format for HF QP Hamiltonian to WanT format
  if(options%sax_to_want) then
    call tools_log("Reading QP...",advance=.false.)
    call pw_QP_init(QP,states%kmesh%nkbz,options%sigma_nbmin,options%sigma_nbmax,options%diagonal)
    if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"HF_QP",binary=.false.)
    call pw_QP_readbcast(QP,states%kmesh%nkbz,10,"HF_QP",0,ptk_comm_world)
    if(ionode) call iotk_close_read(10)
    call tools_log("done")

    if(ionode) then
      call tools_log("Writing HF QP hamiltonian for transport calculation with WanT on file sax_to_want...", &
        advance=.false.)
      call iotk_open_write(10,file=trim(options%outdir)//trim(options%sax_to_want_output),binary=.false.)
      call sax_to_want(10,QP,kmesh,states)
      call iotk_close_write(10)
      call tools_log(" done")
    endif
    call pw_QP_destroy(QP)
  endif

  if(options%calc_QP_states) then
    call tools_log("Reading QP...",advance=.false.)
    call pw_QP_init(QP,states%kmesh%nkbz,options%sigma_nbmin,options%sigma_nbmax,options%diagonal)
    if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"HF_QP",binary=.false.)
    call pw_QP_readbcast(QP,states%kmesh%nkbz,10,"HF_QP",0,ptk_comm_world)
    if(ionode) call iotk_close_read(10)
    call tools_log("done")
    call tools_log("Initializing HF states...",advance=.false.)
    call pw_QP_states_init(QP_states,states,QP,0,ptk_comm_world)
    call tools_log("done")
    call tools_log("Writing HF states on file HF_states...",advance=.false.)
    if(ionode) call iotk_open_write(10,file=trim(options%outdir)//"HF_states",binary=.true.)
    call pw_states_write(QP_states,10,"HF_states")
    if(ionode) call iotk_close_write(10)
    call tools_log("done")
    call pw_QP_states_destroy(QP_states)
    call pw_QP_destroy(QP)

  endif

case("GW")
  if(options%gw_integration_method=="plasmon_pole") then
     call tools_log("Integration method is plasmon pole") 
     call tools_log("Plasmon energy:",advance=.false.)
     write(tools_log_unit(),"(f15.9)",advance="no") options%plasmon_energy*num_ry2ev
     call tools_log(" eV")
! for plasmon pole just two frequencies are needed 0 and plasmon frequency
! P is allocate from 0 to options%nomega
  elseif(options%gw_integration_method=="Farid") then
     call tools_log("Integration method is Farid PRB 38 7530 (1988)")
     call tools_log("DANGER!!!")
  elseif(options%gw_integration_method=="cohsex") then
     call tools_log("Integration method is COulomb Hole and Screened EXchange") 
  elseif(options%gw_integration_method=="cohsex_c") then
     call tools_log("Integration method is COulomb Hole and Screened EXchange with conduction bands")
  elseif(options%gw_integration_method=="coh") then
     call tools_log("Integration method is COulomb Hole")
  elseif(options%gw_integration_method=="sex") then
     call tools_log("Integration method is Screened EXchange")
  elseif(options%gw_integration_method=="sshf") then
     call tools_log("Integration method is Statistically Screened Hartree-Fock")
  else  
     ERROR("Wrong gw_integration_method")      
  endif  

  if(options%calc_polarizability) then
! ----------------------------------
! begin superparall
! ----------------------------------
    if(options%superparall) then 
      call pw_states_destroy(states)
      select case(options%start_from)
      case("DFT")
      call tools_log("RE-Initializing states with new parall ...",advance=.false.)
      call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm,&
           do_not_alloc=.true.,superparall=options%superparall)
      call tools_log(" done")
      call tools_log("Reading e, weights and occupations from file 'states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.false.)
      call ptk_comm_rank(inter_pool_comm,my_pool_id)
      do ipool=0,npool-1
        if(my_pool_id==ipool) then
          if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.true.) 
          call pw_states_read(states,10,"states")
          if(states%rank==states%root) call iotk_close_read(10)
        endif
      enddo
      call tools_log("RE-Initializing states wfc ...",advance=.false.)
      call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm,&
           do_not_alloc=.false.,superparall=options%superparall)
      call tools_log(" done")
      call tools_log("Reading wfc states from file 'states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.false.)
      call ptk_comm_rank(inter_pool_comm,my_pool_id)
      do ipool=0,npool-1
        if(my_pool_id==ipool) then
!          if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.true.) 
          call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.true.)
          call pw_states_read(states,10,"states")
!          if(states%rank==states%root) call iotk_close_read(10)
          call iotk_close_read(10)
        endif
      enddo
!  if(ionode) call iotk_close_read(10)
      call tools_log(" done")
      case("HF")
      call tools_log("RE-Initializing states with new parall ...",advance=.false.)
      call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm, &
            do_not_alloc=.true.,superparall=options%superparall)
      call tools_log(" done")
      call tools_log("Reading e, weights, occupations from file 'HF_states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.)
      call ptk_comm_rank(inter_pool_comm,my_pool_id)
     do ipool=0,npool-1
       if(my_pool_id==ipool) then
         if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.) 
         call pw_states_read(states,10,"HF_states")
         if(states%rank==states%root) call iotk_close_read(10)
       endif
     enddo
!  if(ionode) call iotk_close_read(10)
     call tools_log(" done")

      call tools_log("RE-Initializing states wfc ...",advance=.false.)
      call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm, &
            do_not_alloc=.false.,superparall=options%superparall)
      call tools_log(" done")
      call tools_log("Reading states wf from file 'HF_states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.)
      call ptk_comm_rank(inter_pool_comm,my_pool_id)
     do ipool=0,npool-1
       if(my_pool_id==ipool) then
         if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.) 
         call pw_states_read(states,10,"HF_states")
         if(states%rank==states%root) call iotk_close_read(10)
       endif
     enddo
!  if(ionode) call iotk_close_read(10)
     call tools_log(" done")

     case("GW")
     call tools_log("RE-Initializing states with new parall ...",advance=.false.)
     call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm, &
          do_not_alloc=.true.,superparall=options%superparall)
     call tools_log(" done")
     call tools_log("Reading e, weights and occupations from file 'GW_states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.)
     call ptk_comm_rank(inter_pool_comm,my_pool_id)
     do ipool=0,npool-1
       if(my_pool_id==ipool) then
          if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.) 
          call pw_states_read(states,10,"GW_states")
          if(states%rank==states%root) call iotk_close_read(10)
       endif
     enddo
!  if(ionode) call iotk_close_read(10)
     call tools_log(" done")

     call tools_log("RE-Initializing states wfc ...",advance=.false.)
     call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm, &
          do_not_alloc=.false.,superparall=options%superparall)
     call tools_log(" done")
     call tools_log("Reading states wf from file 'GW_states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.)
     call ptk_comm_rank(inter_pool_comm,my_pool_id)
     do ipool=0,npool-1
       if(my_pool_id==ipool) then
          if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.) 
          call pw_states_read(states,10,"GW_states")
          if(states%rank==states%root) call iotk_close_read(10)
       endif
     enddo
!  if(ionode) call iotk_close_read(10)
     call tools_log(" done")


     end select
  endif 
!------------------------------
! end superparall
!--------------------------
    call tools_log("Initializing polarizability...",advance=.false.)
    call pw_polar_init(polar,struct,symmlist,qmesh,options%omegamax,options%nomega,options%cutoff_polarizability, &
         options%emax_polarizability,options%degauss_polarizability,options%gw_integration_method,root=0,comm=ptk_comm_world)
    call tools_log(" done")
    ! AF
    !call tools_log("Calculating polarizability...",advance=.false.)
    call tools_log("Calculating polarizability...")
    call pw_polar_calc(polar,states,atoms)
    !call tools_log(" done")
    !
    call tools_log("Writing polar on file 'polar'...")
    if(ionode) call iotk_open_write(file=trim(options%outdir)//"polar",unit=200000,binary=.true.)
    call pw_polar_write(polar,200000,"polar")
    if(ionode) call iotk_close_write(200000)
    call tools_log(" done")
  end if

! Better for memory, states is not needed in the calculation of w or pp_parameters
  if(options%calc_w.or.options%calc_plasmon_pole_parameters) call pw_states_destroy_basis_wfc(states)


  if(options%calc_w) then
    if(.not.options%calc_polarizability) then
      call pw_polar_init(polar,struct,symmlist,qmesh,options%omegamax,options%nomega,options%cutoff_polarizability, &
           options%emax_polarizability,options%degauss_polarizability,options%gw_integration_method,root=0,comm=ptk_comm_world) 
      if(ionode) call iotk_open_read(file=trim(options%outdir)//"polar",unit=200000,binary=.true.)
      call pw_polar_read(polar,200000,"polar")
      if(ionode) call iotk_close_read(200000)
      call tools_log("... done")
    end if
  endif 


  if(options%calc_w) then
    call tools_log("Initializing w ...",advance=.false.)
    call pw_w_init(w,struct,symmlist,qmesh,options%omegamax,options%nomega,options%cutoff_polarizability, &
                   options%gw_integration_method,root=0,comm=ptk_comm_world,do_not_alloc=.true.)
    call tools_log("done")
    call tools_log("Calculating w ...",advance=.false.)

    if(trim(options%system_kind)=="0D") then
      call pw_w_calc_0(w,polar,coulomb_div_treatment="vcut_ws", &
	      ecutvcut=options%ecutvcut,dealloc_polar=.true.)
    else
!      if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
!         call pw_w_calc_3(w,polar,options%coulomb_div_treatment, &
!              dealloc_polar=.true.)
!      else       
!         call pw_w_calc_3(w,polar,options%coulomb_div_treatment, &
!              options%ecutvcut, dealloc_polar=.true.)
      call pw_w_calc_3(w,polar, dealloc_polar=.true.)

!      endif
    endif

    call tools_log("done")

    call tools_log("Initilazing static macroscopic dielectric constant...", &
      advance=.false.)
    call pw_epsilon_macro_init(epsilon,struct,symmlist,qmesh,0.0,0,0.0, &
        0.0,0.0,0.0,0.0,.true.,root=0,comm=ptk_comm_world)
    epsilon%macroscopic(:,:,0)=w%macroscopic(:,:,0)
    call tools_log("done")
    call tools_log("Writing static macroscopic dielectric constant...", &
      advance=.false.)
    if(ionode) call pw_epsilon_macro_write(epsilon,200002,"epsilon.static")  
    call tools_log("done")
    call pw_epsilon_macro_destroy(epsilon) 

  end if

  if(options%calc_w.or.options%calc_polarizability) then
    call pw_polar_destroy(polar)
  end if

  if(options%calc_w) then
    call tools_log("Smoothing w ...")
    call pw_w_smooth_3(w)
    if(options%system_kind=="3D") then
       if(trim(options%coulomb_div_treatment)/="vcut_ws") then
         call pw_w_cutoff_3(w,options%coulomb_div_treatment)
       else
         call pw_w_cutoff_3(w,options%coulomb_div_treatment,options%ecutvcut)
       endif
    endif
    call tools_log("... done")
    call tools_log("Writing w on file '.smooth' ...")
    if(ionode) call iotk_open_write(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
    call pw_w_write(w,200001,"w")
    if(ionode) call iotk_close_write(200001)
    call tools_log("... done")
  end if

  if(options%gw_integration_method=="plasmon_pole") then
     if(options%calc_plasmon_pole_parameters) then
        if(.not.options%calc_w) then
           call pw_w_init(w,struct,symmlist,qmesh,options%omegamax,options%nomega,options%cutoff_polarizability, &
                options%gw_integration_method,0,ptk_comm_world)
           call tools_log("Reading w.smooth ...",advance=.false.)
           if(ionode) call iotk_open_read(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
           call pw_w_read(w,200001,"w")
           if(ionode) call iotk_close_read(200001)
           call tools_log(" done")

        endif
! Calculation of plasmon pole parameters
        call tools_log("Initializing plasmon pole parameters ...",advance=.false.)
        call pw_pp_parameters_init(pp_parameters,options%plasmon_energy,options%lorentzian_broadening,struct, &
          symmlist,qmesh,options%cutoff_polarizability,0,ptk_comm_world)
          call tools_log("done") 
        call tools_log("Calculating plasmon pole parameters ...",advance=.false.)

        if(trim(options%system_kind)=="0D") then
          call pw_pp_parameters_calc_0(pp_parameters,w,coulomb_div_treatment="vcut_ws", &
               ecutvcut=options%ecutvcut)
        else
          if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
             call pw_pp_parameters_calc_3(pp_parameters,w,options%coulomb_div_treatment)
          else 
             call pw_pp_parameters_calc_3(pp_parameters,w,options%coulomb_div_treatment,options%ecutvcut)      
          endif
        endif
        call tools_log("done")

! destruction of W        
        if(.not.options%calc_w) call pw_w_destroy(w)
! Writing of plasmon pole parameters        
        call tools_log("Writing pp_parameters on file 'pp_parameters' ...")
        if(ionode) call iotk_open_write(unit=10,file=trim(options%outdir)//"pp_parameters",binary=.true.)
        call pw_pp_parameters_write(pp_parameters,10,"pp_parameters")
        if(ionode) call iotk_close_write(10)
        call tools_log("... done")
     endif
! destruction of W 
  if(options%calc_w) call pw_w_destroy(w)

  endif        


if((options%calc_sigma_c.or.options%calc_sigma_x.or.options%calc_QP_states).and.&
   (options%calc_w.or.options%calc_plasmon_pole_parameters)) then
select case(options%start_from)
case("DFT")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init_basis_wfc(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm)
  call tools_log(" done")
  call tools_log("Reading states from file 'states' ...",advance=.false.)
  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  do ipool=0,npool-1
     if(my_pool_id==ipool) then
       if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.true.) 
       write(0,*) "ny_pool_id ", ipool
       call pw_states_read(states,10,"states")
       if(states%rank==states%root) call iotk_close_read(10)
     endif
  enddo
  call tools_log(" done")
case("HF")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init_basis_wfc(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm)
  call tools_log(" done")
  call tools_log("Reading states from file 'HF_states' ...",advance=.false.)
!  if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.)
  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  do ipool=0,npool-1
     if(my_pool_id==ipool) then
       if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.) 
       write(0,*) "ny_pool_id ", ipool
       call pw_states_read(states,10,"HF_states")
       if(states%rank==states%root) call iotk_close_read(10)
     endif
  enddo
!  if(ionode) call iotk_close_read(10)
  call tools_log(" done")
case("GW")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init_basis_wfc(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,intra_pool_comm)
  call tools_log(" done")
  call tools_log("Reading states from file 'GW_states' ...",advance=.false.)
  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  do ipool=0,npool-1
     if(my_pool_id==ipool) then
       if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.) 
       write(0,*) "ny_pool_id ", ipool
       call pw_states_read(states,10,"GW_states")
       if(states%rank==states%root) call iotk_close_read(10)
     endif
  enddo
  call tools_log(" done")
end select

states_cutoff = pw_states_get_cutoff(states)
call tools_log("The cutoff for single particle wfc is: ",advance=.false.)
write(tools_log_unit(),"(f15.9)",advance='no') states_cutoff
call tools_log(" Ry")

endif


! AF
! very very bad
if ( options%ikmax == 0 ) options%ikmax = states%kmesh%nkbz
! AF
  
! Calulation of the correlation part of the self-energy operator projected on
! states  
! PLASMON-POLE
  if(options%gw_integration_method=="plasmon_pole") then
    if(options%calc_sigma_c) then
      call tools_log("Correlation part of the self-energy operator projected on states")  
      if(.not.options%calc_plasmon_pole_parameters) then
! Init and reading plasmon pole parameters
        call tools_log("Initializing plasmon pole parameters ...")
        call pw_pp_parameters_init(pp_parameters,options%plasmon_energy,options%lorentzian_broadening,struct, &
          symmlist,qmesh,options%cutoff_polarizability,0,ptk_comm_world)
        call tools_log("done")
        call tools_log("Reading plasmon pole parameters ...")
        if(ionode) call iotk_open_read(unit=10,file=trim(options%outdir)//"pp_parameters",binary=.true.)
        call pw_pp_parameters_read(pp_parameters,10,"pp_parameters")
        if(ionode) call iotk_close_read(10)
        call tools_log("done")
      endif 
      call ptk_comm_rank(inter_pool_comm,my_pool_id)       
      do ik=options%ikmin,options%ikmax
        if(mod(ik,npool).ne.my_pool_id) cycle
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
        call tools_log("Initializing sigma ...", advance=.false.)

!        call pw_sigma_init(sigma,states,ik, &
!           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
!           diagonal=options%diagonal,do_not_alloc=.false.)

        if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
          call pw_sigma_init(sigma,states,ik, &
             nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
             diagonal=options%diagonal,coulomb_div_treatment=trim(options%coulomb_div_treatment))
        else
          call pw_sigma_init(sigma,states,ik, &
             nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
             diagonal=options%diagonal,coulomb_div_treatment=trim(options%coulomb_div_treatment), &
             ecutvcut=options%ecutvcut)
        endif
        call tools_log("done")

! initialization of first derivative of self-energy operator        
        if(options%sigma_first_order) then
        call tools_log("Initializing dsigma ...", advance=.false.)
        call pw_sigma_init(dsigma,states,ik, &
           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
           diagonal=options%diagonal)
        call tools_log("done")
        else 
        call pw_sigma_init(dsigma,states,ik, &
           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
           diagonal=options%diagonal,do_not_alloc=.true.)
        endif

        call tools_log("Calculating sigma and dsigma if sigma_first_order requested ...",advance=.false.)   
!        if(options%system_kind=="0D") then
!           call pw_sigma_c_calc0(sigma,dsigma,pp_parameters,cutoff=options%cutoff_polarizability, &
!           comm=intra_pool_comm)
!        else
           call pw_sigma_c_calc3(sigma,dsigma,pp_parameters,cutoff=options%cutoff_polarizability, &
	  comm=intra_pool_comm)
!        endif
        call tools_log("done")
        if(states%rank==states%root) then
           call tools_log("Writing sigma on file sigma_c.ik ...",advance=.false.)
           call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_c"//trim(iotk_index(ik)))
           call pw_sigma_write(sigma,34,"sigma_c")
           call iotk_close_write(unit=34)
           call tools_log("done")
        endif
        call pw_sigma_destroy(sigma) 
        if(options%sigma_first_order.and.(states%rank==states%root)) then
          call tools_log("Writing dsigma on file dsigma_c.ik ...",advance=.false.)
          call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"dsigma_c"//trim(iotk_index(ik)))
          call pw_sigma_write(dsigma,34,"dsigma_c")
          call iotk_close_write(unit=34)
          call tools_log("done")
        endif  
        call pw_sigma_destroy(dsigma)
      enddo
! destruction of plasmon pole parameters
      if(.not.options%calc_plasmon_pole_parameters) call pw_pp_parameters_destroy(pp_parameters)
    endif
    if(options%calc_plasmon_pole_parameters) call pw_pp_parameters_destroy(pp_parameters) 

!FARID
   
  elseif(options%gw_integration_method=="Farid") then        
    if(options%calc_sigma_c) then
      call tools_log("Correlation part of the self-energy operator projected on states")      
      if(.not.options%calc_w) then 
        call tools_log("Initializing W ...",advance=.false.)              
        call pw_w_init(w,struct,symmlist,qmesh,options%omegamax,options%nomega,options%cutoff_polarizability, &
             options%gw_integration_method,0,ptk_comm_world)
             call tools_log("done")     
        call tools_log("Reading w.smooth ...",advance=.false.)
        if(ionode) call iotk_open_read(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
        call pw_w_read(w,200001,"w")
        if(ionode) call iotk_close_read(200001)
        call tools_log(" done")
      endif  

      call tools_log("Calculating Fermi energy...",advance=.false.)
      call pw_sigma_efermi(efermi,options%nelec,states)
      call tools_log("done")
      call tools_log("Fermi energy:",advance=.false.)
      write(tools_log_unit(),"(f15.9)",advance="no") efermi*num_ry2ev
      call tools_log(" eV")

      call tools_log("Sigma is calculated at the fermi energy")
      call ptk_comm_rank(inter_pool_comm,my_pool_id)       
      ! AF
      do ik=options%ikmin,options%ikmax
        if(mod(ik,npool).ne.my_pool_id) cycle
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
        call tools_log("Initializing sigma ...", advance=.false.)
        call pw_sigma_init(sigma,states,ik, &
           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
           diagonal=options%diagonal)
           call tools_log("done") 
        call tools_log("Calculating sigma ...",advance=.false.)   
        call pw_sigma_c_calc(sigma,w,cmplx(efermi),cutoff=options%cutoff_polarizability, &
	  comm=intra_pool_comm)
        call tools_log("done")
        if(states%rank==states%root) then
           call tools_log("Writing sigma on file sigma_c.ik ...",advance=.false.)
           call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_c"//trim(iotk_index(ik)))
           call pw_sigma_write(sigma,34,"sigma_c")
           call iotk_close_write(unit=34)
           call tools_log("done")
        endif
        call pw_sigma_destroy(sigma) 
      enddo
      if(.not.options%calc_w) call pw_w_destroy(w)
    endif
    if(options%calc_w) call pw_w_destroy(w)

!COHSEX

  elseif(options%gw_integration_method=="cohsex_c") then        
    if(options%calc_sigma_c) then
      call tools_log("Correlation part of the self-energy operator projected on states")      
      if(.not.options%calc_w) then 
        call tools_log("Initializing W ...",advance=.false.)              
        call pw_w_init(w,struct,symmlist,qmesh,0,options%cutoff_polarizability, &
             0,ptk_comm_world)
             call tools_log("done")     
        call tools_log("Reading w.smooth ...",advance=.false.)
        if(ionode) call iotk_open_read(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
        call pw_w_read(w,0,200001,"w")
        if(ionode) call iotk_close_read(200001)
        call tools_log(" done")
      endif  

      call ptk_comm_rank(inter_pool_comm,my_pool_id)       
      ! AF
      do ik=options%ikmin,options%ikmax
        if(mod(ik,npool).ne.my_pool_id) cycle
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
        call tools_log("Initializing sigma ...", advance=.false.)

!        call pw_sigma_init(sigma,states,ik, &
!           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
!           diagonal=options%diagonal)

        if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
          call pw_sigma_init(sigma,states,ik, &
             nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
             diagonal=options%diagonal,coulomb_div_treatment=trim(options%coulomb_div_treatment))
        else
          call pw_sigma_init(sigma,states,ik, &
             nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
             diagonal=options%diagonal,coulomb_div_treatment=trim(options%coulomb_div_treatment), &
             ecutvcut=options%ecutvcut)
        endif
        call tools_log("done") 
        call tools_log("Calculating sigma ...",advance=.false.)   
        call pw_sigma_c_calc("cohsex",sigma,w,cutoff=options%cutoff_polarizability, &
	  comm=intra_pool_comm)
        call tools_log("done")
        if(states%rank==states%root) then
           call tools_log("Writing sigma on file sigma_c.ik ...",advance=.false.)
           call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_c"//trim(iotk_index(ik)))
           call pw_sigma_write(sigma,34,"sigma_c")
           call iotk_close_write(unit=34)
           call tools_log("done")
        endif
        call pw_sigma_destroy(sigma) 
      enddo
      if(.not.options%calc_w) call pw_w_destroy(w)
    endif
    if(options%calc_w) call pw_w_destroy(w)

! COHSEX without conduction states
  elseif(options%gw_integration_method=="cohsex") then
    if(options%calc_sigma_c) then
      call tools_log("Correlation part of the self-energy operator projected on states")
      if(.not.options%calc_w) then
        call tools_log("Initializing W ...",advance=.false.)
        call pw_w_init(w,struct,symmlist,qmesh,0,options%cutoff_polarizability, &
             0,ptk_comm_world)
             call tools_log("done")
        call tools_log("Reading w.smooth ...",advance=.false.)
        if(ionode) call iotk_open_read(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
        call pw_w_read(w,0,200001,"w")
        if(ionode) call iotk_close_read(200001)
        call tools_log(" done")
      endif

      call pw_hartree_init(vcoh,struct,4.0*options%cutoff_polarizability)
      call ptk_barrier(ptk_comm_world)
      call pw_vcoh_calc(w,vcoh,ptk_comm_world)

!----------------------------------------------
    call ptk_barrier(ptk_comm_world)
!---------------------------------------------------------

      call ptk_comm_rank(inter_pool_comm,my_pool_id)
      ! AF
      do ik=options%ikmin,options%ikmax
        if(mod(ik,npool).ne.my_pool_id) cycle
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), &
            kmesh%kbz(2,ik), kmesh%kbz(3,ik)
        call tools_log("Initializing sigma ...", advance=.false.)
        call pw_sigma_init(sigma,states,ik, &
           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
           diagonal=options%diagonal)
           call tools_log("done")
        call tools_log("Calculating coh ...",advance=.false.)
        call pw_sigmacoh_calc(sigma,vcoh,cutoff=options%cutoff_polarizability, &
          comm=intra_pool_comm)
        call tools_log("done")

! sex part
        call tools_log("Calculating sex ...",advance=.false.)
        call pw_sigmasex_calc(sigma,w,options%cutoff_polarizability, &
          intra_pool_comm)
        call tools_log("done")
        if(states%rank==states%root) then
           call tools_log("Writing sigma on file sigma_c.ik ...",advance=.false.)
           call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_c"//trim(iotk_index(ik)))
           call pw_sigma_write(sigma,34,"sigma_c")
           call iotk_close_write(unit=34)
           call tools_log("done")
        endif                         ! end rank==root
        call pw_sigma_destroy(sigma)
      enddo
      if(.not.options%calc_w) call pw_w_destroy(w)
      call pw_hartree_destroy(vcoh)
    endif       ! if calc sigma_c
    if(options%calc_w) call pw_w_destroy(w)
!COH
  elseif(options%gw_integration_method=="coh") then
    if(options%calc_sigma_c) then
      call tools_log("Correlation part of the self-energy operator projected on states")
      if(.not.options%calc_w) then
        call tools_log("Initializing W ...",advance=.false.)
        call pw_w_init(w,struct,symmlist,qmesh,0,options%cutoff_polarizability, &
             0,ptk_comm_world)
             call tools_log("done")
        call tools_log("Reading w.smooth ...",advance=.false.)
        if(ionode) call iotk_open_read(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
        call pw_w_read(w,0,200001,"w")
        if(ionode) call iotk_close_read(200001)
        call tools_log(" done")
      endif 

      call pw_hartree_init(vcoh,struct,4.0*options%cutoff_polarizability)
      call ptk_barrier(ptk_comm_world)
      call pw_vcoh_calc(w,vcoh,ptk_comm_world)
!      call ptk_barrier(ptk_comm_world)
!      write(0,*) "vcoh"
!      write(0,*) vcoh%wfc%val
!      write(0,*)"---------------------"
      call pw_w_destroy(w)

!----------------------------------------------

!      call tools_log("Writing coh in file coh...", advance=.false.)
!      if(ionode) then
!        call iotk_open_write(200002,file=trim(options%outdir)//"coh", binary=.false.)
!        call pw_hartree_write(vcoh,200002,"coh")
!        call iotk_close_write(200002)
!      endif
!      call tools_log("done")
!      if(ionode) then
!        call tools_log("Converting to xsf format...",advance=.false.)
!        call pw_field_init(coh_field,struct)
!        dim = pw_field_dim_from_dipole(vcoh%basis,vcoh%basis,vcoh%basis)
!        call pw_field_set_dim(coh_field, dim ,k=(/0.0,0.0,0.0/),r0=(/0.5,0.5,0.5/))
!        dim1=coh_field%dim(1)
!        dim2=coh_field%dim(2)
!        dim3=coh_field%dim(3)
!        allocate(field_tmp(1:dim1,1:dim2,1:dim3))
!        call pw_wfc2field(coh_field,vcoh%wfc)
!        open(10,file="coh.xsf",form="formatted",status="unknown")
!        call xsf_struct(struct%a,atoms%natoms, atoms%positions, atoms%names, atoms%type_map, 10)
!        write(0,*) "after xsf struc"
!        str2 = coh_field%str2
!        str3 = coh_field%str3
!        do i3=1,coh_field%dim(3)
!          do i2=1,coh_field%dim(2)
!            do i1=1,coh_field%dim(1)
!              i  = (i1-1) + (i2-1)*str2 + (i3-1)*str3 + 1
!              field_tmp(i1,i2,i3) = coh_field%val(i)
!            end do
!          end do
!        end do
!
!        call xsf_fast_datagrid_3d(field_tmp,coh_field%dim(1),coh_field%dim(2), &
!           coh_field%dim(3), coh_field%str2, coh_field%str3,1,struct%a,10)
!        close(10)
!
!        deallocate(field_tmp)
!        call pw_field_destroy(coh_field)
!
!      call tools_log("done")
!    endif 

    call ptk_barrier(ptk_comm_world)
!---------------------------------------------------------

      call ptk_comm_rank(inter_pool_comm,my_pool_id)       
      ! AF
      do ik=options%ikmin,options%ikmax
        if(mod(ik,npool).ne.my_pool_id) cycle
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
        call tools_log("Initializing sigma ...", advance=.false.)
        call pw_sigma_init(sigma,states,ik, &
           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
           diagonal=options%diagonal)
           call tools_log("done")
        call tools_log("Calculating sigma ...",advance=.false.)
        call pw_sigmacoh_calc(sigma,vcoh,cutoff=options%cutoff_polarizability, &
          comm=intra_pool_comm)
        call tools_log("done")
        if(states%rank==states%root) then
           call tools_log("Writing sigma on file sigma_coh.ik ...",advance=.false.)
           call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_coh"//trim(iotk_index(ik)))
           call pw_sigma_write(sigma,34,"sigma_c")
           call iotk_close_write(unit=34)
           call tools_log("done")
        endif
        call pw_sigma_destroy(sigma)
      enddo
     call pw_hartree_destroy(vcoh)
   endif
! SEX

  elseif(options%gw_integration_method=="sex") then
    if(options%calc_sigma_c) then
      call tools_log("Correlation part of the self-energy operator projected on states")
      if(.not.options%calc_w) then
        call tools_log("Initializing W ...",advance=.false.)
        call pw_w_init(w,struct,symmlist,qmesh,0,options%cutoff_polarizability, &
             0,ptk_comm_world)
             call tools_log("done")
        call tools_log("Reading w.smooth ...",advance=.false.)
        if(ionode) call iotk_open_read(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
        call pw_w_read(w,0,200001,"w")
        if(ionode) call iotk_close_read(200001)
        call tools_log(" done")
      endif 

      call ptk_comm_rank(inter_pool_comm,my_pool_id)       
      ! AF
      do ik=options%ikmin,options%ikmax
        if(mod(ik,npool).ne.my_pool_id) cycle
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
        call tools_log("Initializing sigma ...", advance=.false.)
        call pw_sigma_init(sigma,states,ik, &
           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
           diagonal=options%diagonal)
           call tools_log("done")
        call tools_log("Calculating sigma ...",advance=.false.)
        call pw_sigmasex_calc(sigma,w,options%cutoff_polarizability, &
          intra_pool_comm)
        call tools_log("done")
        if(states%rank==states%root) then
           call tools_log("Writing sigma on file sigma_sex.ik ...",advance=.false.)
           call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_sex"//trim(iotk_index(ik)))
           call pw_sigma_write(sigma,34,"sigma_c")
           call iotk_close_write(unit=34)
           call tools_log("done")
        endif
        call pw_sigma_destroy(sigma)
      enddo
        if(.not.options%calc_w) call pw_w_destroy(w)
    endif
    if(options%calc_w) call pw_w_destroy(w)

!SSHF

  elseif(options%gw_integration_method=="sshf") then        
    if(options%calc_sigma_c) then
      call tools_log("Correlation part of the self-energy operator projected on states")   
      if(.not.options%calc_w) then 
        call tools_log("Initializing W ...",advance=.false.)              
        call pw_w_init(w,struct,symmlist,qmesh,0,options%cutoff_polarizability, &
             0,ptk_comm_world)
             call tools_log("done")     
        call tools_log("Reading w.smooth ...",advance=.false.)
        if(ionode) call iotk_open_read(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
        call pw_w_read(w,0,200001,"w")
        if(ionode) call iotk_close_read(200001)
        call tools_log(" done")
      endif  

      call ptk_comm_rank(inter_pool_comm,my_pool_id)       
      !
      do ik=options%ikmin,options%ikmax
        if(mod(ik,npool).ne.my_pool_id) cycle
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
        call tools_log("Initializing sigma ...", advance=.false.)
!        call pw_sigma_init(sigma,states,ik, &
!           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
!           diagonal=options%diagonal)

        if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
          call pw_sigma_init(sigma,states,ik, &
             nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
             diagonal=options%diagonal,coulomb_div_treatment=trim(options%coulomb_div_treatment))
        else
          call pw_sigma_init(sigma,states,ik, &
             nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
             diagonal=options%diagonal,coulomb_div_treatment=trim(options%coulomb_div_treatment), &
             ecutvcut=options%ecutvcut)
        endif
        call tools_log("done") 
        call tools_log("Calculating sigma ...",advance=.false.)   
        call pw_sigma_c_calc("sshf",sigma,w,cutoff=options%cutoff_polarizability, &
	  comm=intra_pool_comm)
        call tools_log("done")
        if(states%rank==states%root) then
           call tools_log("Writing sigma on file sigma_c.ik ...",advance=.false.)
           call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_c"//trim(iotk_index(ik)))
           call pw_sigma_write(sigma,34,"sigma_c")
           call iotk_close_write(unit=34)
           call tools_log("done")
        endif
        call pw_sigma_destroy(sigma) 
      enddo
      if(.not.options%calc_w) call pw_w_destroy(w)
    endif

    if(options%calc_w) call pw_w_destroy(w)
  endif ! end if options%gw_integration_method 

! calculation of the exchange part of the self-energy operator projected on
! states


  if(options%calc_sigma_x) then
      call tools_log("Exchange part of the self-energy operator projected on states") 
      call ptk_comm_rank(inter_pool_comm,my_pool_id) 
      ! AF
      do ik=options%ikmin,options%ikmax
        if(mod(ik,npool).ne.my_pool_id) cycle
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
        call tools_log("Initializing sigma ...", advance=.false.)
        if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
          call pw_sigma_init(sigma,states,ik, &
             nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
             diagonal=options%diagonal,coulomb_div_treatment=trim(options%coulomb_div_treatment))
        else
          call pw_sigma_init(sigma,states,ik, &
             nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
             diagonal=options%diagonal,coulomb_div_treatment=trim(options%coulomb_div_treatment), &
             ecutvcut=options%ecutvcut)
        endif
        call tools_log("done")
        call tools_log("Calculating sigma ...",advance=.false.)   
!        if(options%system_kind=="0D") then
!           call pw_sigma_x_calc0(sigma,qmesh,cutoff=options%cutoff_fock, &
!           comm=intra_pool_comm)
!        else
           call pw_sigma_x_calc3(sigma,qmesh,cutoff=options%cutoff_fock, &
                comm=intra_pool_comm)
!        endif
        call tools_log("done")

        if(states%rank==states%root) then
           call tools_log("Writing sigma on file sigma_x.ik ...",advance=.false.)
           call iotk_open_write(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_x"//trim(iotk_index(ik)))
           call pw_sigma_write(sigma,34,"sigma_x")
           call iotk_close_write(unit=34)
           call tools_log("done")
        endif

        call pw_sigma_destroy(sigma) 
      enddo
  endif

  
  if(options%calc_energies) then
    energy_ewald = pw_atoms_ewald(atoms)
    call tools_log("Ewald energy is: ",advance=.false.)
    write(tools_log_unit(),"(f15.9)",advance="no") energy_ewald
    call tools_log(" Ry")
    call pw_QP_init(QP,kmesh%nkbz,options%sigma_nbmin,options%sigma_nbmax,options%diagonal)
! add of sigma exchange
    call tools_log("Adding to QP single-particle and the exchange part of self_energy operator")
    first_QPcall=.true.
    do ik=1,kmesh%nkbz
      call tools_log("kpoint:",advance=.false.)
      write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
      call tools_log("Initializing sigma ...",advance=.false.)
      call pw_sigma_init(sigma,states,ik, &
         nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax, &
         diagonal=options%diagonal)
         call tools_log("done")   
      call tools_log("Reading sigma ...",advance=.false.)
      if(ionode) call iotk_open_read(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_x"//trim(iotk_index(ik)))
      call pw_sigma_readbcast(sigma,34,"sigma_x",0,ptk_comm_world)
      if(ionode) call iotk_close_read(unit=34)
      call tools_log("done")

      call tools_log("Collecting ...",advance=.false.)
!-----------------------------------------------------------
! calculation of QP energies

      call pw_sigma_QPenergies_collect(sigma,QP,first_QPcall,options%outdir)
!-----------------------------------------------------------
      call tools_log("done")

      call pw_sigma_destroy(sigma)
    end do
    first_QPcall=.false.
! add of sigmacorrelation 
    call tools_log("Adding to QP the correlation part of self_energy operator")
    do ik=1,kmesh%nkbz
      call tools_log("kpoint:",advance=.false.)
      write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
      call tools_log("Initializing sigma ...",advance=.false.)
      call pw_sigma_init(sigma,states,ik, &
         nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax, &
         diagonal=options%diagonal)
         call tools_log("done")   
      call tools_log("Reading sigma ...",advance=.false.)
      if(ionode) call iotk_open_read(unit=34,binary=.false.,file=trim(options%outdir)//"sigma_c"//trim(iotk_index(ik)))
      call pw_sigma_readbcast(sigma,34,"sigma_c",0,ptk_comm_world)
      if(ionode) call iotk_close_read(unit=34)
      call tools_log("done")

      call tools_log("Collecting ...",advance=.false.)
!-----------------------------------------------------------
! calculation of QP energies

      call pw_sigma_QPenergies_collect(sigma,QP,first_QPcall,options%outdir)
!-----------------------------------------------------------
      call tools_log("done")

      call pw_sigma_destroy(sigma)
    enddo 

    if(options%sigma_first_order) then
    call tools_log("Adding to QP the first_order correction to the QP hamiltonian")
      do ik=1,kmesh%nkbz
        call tools_log("kpoint:",advance=.false.)
        write(tools_log_unit(),"(f12.6,2x,f12.6,2x,f12.6)") kmesh%kbz(1,ik), kmesh%kbz(2,ik), kmesh%kbz(3,ik)
! initialization of first derivative of self-energy operator        
        call tools_log("Initializing dsigma ...", advance=.false.)
        call pw_sigma_init(dsigma,states,ik, &
           nbmin=options%sigma_nbmin,nbmax=options%sigma_nbmax,&
           diagonal=options%diagonal)
        call tools_log("done")
! reading first-derivative of self-energy operator       
        call tools_log("Reading dsigma ...",advance=.false.)
        if(ionode) call iotk_open_read(unit=34,binary=.false.,file=trim(options%outdir)//"dsigma_c"//trim(iotk_index(ik)))
        call pw_sigma_readbcast(dsigma,34,"dsigma_c",0,ptk_comm_world)
        if(ionode) call iotk_close_read(unit=34)
        call tools_log("done")

        call tools_log("first-order corrections ...",advance=.false.)
!-----------------------------------------------------------
! calculation of QP energies

        call pw_sigma_QPenergies_firstorder_corr(dsigma,QP,options%diagonal)
!-----------------------------------------------------------
        call tools_log("done")
        call pw_sigma_destroy(dsigma)
      end do
    endif 

    call tools_log("Diagonalization of QP Hamiltonian if requested",advance=.false.)   
    do ik=1,kmesh%nkbz
      call pw_sigma_QPdiag(QP,ik,options%diagonal)
    enddo 
    call tools_log("done")

    if(ionode) then
      call tools_log("Writing GW QP on file GW_QP...",advance=.false.)
      call iotk_open_write(346,file=trim(options%outdir)//"GW_QP",binary=.false.)
      call pw_QP_write(QP,kmesh%nkbz,346,"GW_QP")
      call iotk_close_write(346)
      call tools_log("done")
    endif

    call pw_QP_destroy(QP)
  endif

! Conversion of SAX format for GW QP Hamiltonian to WanT format
  if(options%sax_to_want) then
    call tools_log("Reading QP...",advance=.false.)
    call pw_QP_init(QP,kmesh%nkbz,options%sigma_nbmin,options%sigma_nbmax,options%diagonal)
    if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"GW_QP",binary=.false.)
    call pw_QP_readbcast(QP,kmesh%nkbz,10,"GW_QP",0,ptk_comm_world)
    if(ionode) call iotk_close_read(10)
    call tools_log("done")

    if(ionode) then
      call tools_log("Writing QP hamiltonian for transport calculation with WanT on file sax_to_want...", &
        advance=.false.)
      call iotk_open_write(10,file=trim(options%outdir)//trim(options%sax_to_want_output),binary=.false.)
      call sax_to_want(10,QP,kmesh,states)
      call iotk_close_write(10)
      call tools_log(" done")
    endif
    call pw_QP_destroy(QP)
  endif

  if(options%calc_QP_states) then
    call tools_log("Reading QP...",advance=.false.)
    call pw_QP_init(QP,kmesh%nkbz,options%sigma_nbmin,options%sigma_nbmax,options%diagonal)
    if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"GW_QP",binary=.false.)
    call pw_QP_readbcast(QP,kmesh%nkbz,10,"GW_QP",0,ptk_comm_world)
    if(ionode) call iotk_close_read(10)
    call tools_log("done")
    call tools_log("Initializing QP_states...",advance=.false.)
    call pw_QP_states_init(QP_states,states,QP,0,ptk_comm_world)
    call tools_log("done")
    call tools_log("Writing QP_states on file GW_states...",advance=.false.)
    if(ionode) call iotk_open_write(10,file=trim(options%outdir)//"GW_states",binary=.true.)
    call pw_states_write(QP_states,10,"GW_states")
    if(ionode) call iotk_close_write(10)
    call tools_log("done")
   
    call pw_QP_states_destroy(QP_states)
    call pw_QP_destroy(QP)
  endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end select


call pw_atoms_destroy(atoms)
call pw_kmesh_destroy(qmesh)
call pw_kmesh_destroy(kmesh)
call pw_symmlist_destroy(symmlist)
call pw_states_destroy(states)

call tools_log("Writing here final mallinfo ...")
call tools_mallinfo_print(tools_log_unit(),msg="MALLINFO")
call tools_log("... done")
call tools_log("Writing here final PW statistics ...")
call pw_print_statistics(tools_log_unit())
call tools_log("... done")

call tools_log("Program GW ends")

end subroutine gw_program
