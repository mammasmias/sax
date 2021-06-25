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
subroutine bse_program()
!
use ptk_module
use iotk_module
use bse_options_module
use tools_module
use num_module
use pw_epsilon_macro_module
use pw_module
use pw_bse_interf
use pw_bse_type
use pw_smearing_module
use num_la_parall_module
use mp_global

implicit none

integer :: rank,ierr,npe

character(256) :: args(256)
integer        :: nargs

type(sax_options)            :: options         ! Contiene le opzioni globali del codice
type(pw_struct)      :: struct          ! Contiene i dati di struttura (dim. cella) del sistema in analisi
type(pw_atoms)      :: atoms           ! Struttura atomica e pseudopotenziali
type(pw_symmlist)   :: symmlist        ! Contiene una lista delle simmetrie del sistema
type(pw_kmesh)      :: kmesh           ! Contiene la mesh per le funzioni d'onda
type(pw_kmesh)   :: qmesh           ! Contiene la mesh per gli elementi di dipolo
                                               ! (le differenze in kmesh)
type(pw_bse)                :: bse
type(pw_states)     :: states
type(pw_epsilon_macro)            :: epsilon 
type(pw_smearing)           :: smearing

integer :: log_unit

integer :: iostat

type(pw_w)                  :: w            ! Contiene W
logical :: ionode
real :: states_cutoff
integer :: ib                      ! temp index
character(256) :: infile,logfile
character(256) :: crank

integer :: itrans1, itrans2
integer :: my_pool_id, ipool, npool_loc

call ptk_comm_rank(ptk_comm_world,rank=rank)
call ptk_comm_size(ptk_comm_world,size=npe)
call init_pool(npool,ptk_comm_world)

ionode = .false.
if(rank==0) ionode = .true.

if(ionode) then
  call iotk_readcmdline(args,nargs)
  if(nargs/=2) ERROR(" ")
  infile  = args(1)
  logfile = args(2)
end if

call ptk_bcast(infile,0,ptk_comm_world)
call ptk_bcast(logfile,0,ptk_comm_world)

call tools_log_set_file(trim(logfile))
call tools_artwork_print(tools_log_unit(),"sax")
call tools_log("www.sax-project.org, version "//__SAX_VERSION)
call tools_log("Program bse starts")
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
call bse_options_readbcast(options,15,0,ptk_comm_world,fmt="iotk",name="sax_options")
call tools_log(" done")

if(options%calculation_kind=="MACRO") then
  call tools_log("BSE calculated with a macroscopic dielectric constant")      
elseif(options%calculation_kind=="FULL") then
  call tools_log("BSE calculated with the full W within the static approximation")
else
  ERROR("Wrong calculation_kind")      
endif        

if(options%system_kind=="0D") then
  call tools_log("0D system")      
elseif(options%system_kind=="1D") then
  call tools_log("1D system kind not implemented yet, assuming 3D")
  options%system_kind="3D"
elseif(options%system_kind=="2D") then
  call tools_log("2D system kind not implemented yet, assuming 3D")
  options%system_kind="3D"
elseif(options%system_kind=="3D") then
  call tools_log("3D system")
else
  ERROR("Wrong system_kind")      
endif        

call tools_log("Reading structure ...",advance=.false.)
call pw_struct_readbcast(struct,15,0,ptk_comm_world,name="structure")
call tools_log(" done")

call pw_atoms_init(atoms,struct,4.0)
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

  call pw_smearing_init(smearing,options%smearing,options%smearing_kind)

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

select case(options%start_from)
case("DFT")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,ptk_comm_world)
  call tools_log(" done")
  call tools_log("Reading states from file 'states' ...",advance=.false.)
  if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.true.)
  call pw_states_read(states,10,"states")
  if(states%rank==states%root) call iotk_close_read(10)
  call tools_log(" done")
case("HF")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,ptk_comm_world)
  call tools_log(" done")
  call tools_log("Reading states from file 'HF_states' ...",advance=.false.)
  if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.)
  call pw_states_read(states,10,"HF_states")
  if(states%rank==states%root) call iotk_close_read(10)
  call tools_log("done")
case("GW")
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,ptk_comm_world)
  call tools_log(" done")
  call tools_log("Reading states from file 'GW_states'...",advance=.false.)
  if(states%rank==states%root) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.)
  call pw_states_read(states,10,"GW_states")
  if(states%rank==states%root) call iotk_close_read(10)
  call tools_log("done")
case DEFAULT
  ERROR("Wrong start_from")
end select

if(options%calc_bse) then
call tools_log("Initializing bse ...",advance=.false.)
call pw_bse_init(bse,states,atoms,options%bse_emin,options%bse_emax, &
     options%bse_spin,0,ptk_comm_world)
call tools_log("done")

select case(options%calculation_kind)
case("MACRO")

   call tools_log("Initializing static macroscopic dielectric constant...", &
   advance=.false.) 
   call pw_epsilon_macro_init(epsilon,struct,symmlist,qmesh,0.0,0,0.0, &
        0.0,0.0,0.0,0.0,.false.,0,ptk_comm_world)
   call tools_log("done")   
   call tools_log("Reading macroscopic dielectric constant...",advance=.false.)
   call pw_epsilon_macro_readbcast(epsilon,200002,trim(options%outdir)//"epsilon.static",0,ptk_comm_world)
   call tools_log("done")

! calculating single particle wfc cutoff for coulomb
   states_cutoff=pw_states_get_cutoff(states)
   call tools_log("The cutoff for single particle wfc is: ", advance=.false.)
   write(tools_log_unit(),"(f15.9)",advance='no') states_cutoff
   call tools_log(" Ry")

   call tools_log("Solving Bethe-Salpeter Equation...",advance=.false.)

   if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
      call pw_bse_macro3(bse,epsilon,options%energy_shift,qmesh,options%cutoff_fock,&
      options%cutoff_fock,coulomb_div_treatment=trim(options%coulomb_div_treatment))
   else
      call pw_bse_macro3(bse,epsilon,options%energy_shift,qmesh,options%cutoff_fock, &
      options%cutoff_fock,coulomb_div_treatment=trim(options%coulomb_div_treatment), &
      ecutvcut=options%ecutvcut)   
   endif

   call tools_log("done")
   call pw_epsilon_macro_destroy(epsilon)

! Transposition of eigenvectors
   call tools_log("Calculating transpose ...",advance=.false.)
   call pw_bse_transpose(bse,0,ptk_comm_world)
   call tools_log(" done")

! Write of bse
   call tools_log("Writing eigenvalues and eigenstates on file bse ...",advance=.false.)
   if(bse%matrix%rank==bse%matrix%root) call iotk_open_write(66,file=trim(options%outdir)//"bse")
   call pw_bse_write(bse,66,"bse")
   if(bse%matrix%rank==bse%matrix%root) call iotk_close_write(66)
   call tools_log("done")

case("FULL") 
   if(options%start_from/="GW") then
     call tools_log("!!!! asking for a calculation with W from GW, but using other QP states")
   endif       
   call tools_log("Initializing w ...",advance=.false.)
   call pw_w_init(w,struct,symmlist,qmesh,0,options%cutoff_polarizability,0,ptk_comm_world)
   call tools_log("done")
   call tools_log("Reading w.smooth ...",advance=.false.)
   if(ionode) call iotk_open_read(unit=200001,file=trim(options%outdir)//"w.smooth",binary=.true.)
   call pw_w_read(w,0,200001,"w")
   if(ionode) call iotk_close_read(200001)
   call tools_log(" done")

! calculating single particle wfc cutoff for coulomb
   states_cutoff=pw_states_get_cutoff(states)
   call tools_log("The cutoff for single particle wfc is: ",advance=.false.)
   write(tools_log_unit(),"(f15.9)",advance='no') states_cutoff
   call tools_log(" Ry") 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   call tools_log("Solving Bethe-Salpeter Equation ...",advance=.false.)
! for 0D system the macroscopic screening is set to 1.0 this is done at w level
! so no need for special routine at bse level
   if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
      call pw_bse_calc3(bse,w,options%energy_shift,options%cutoff_fock,&
      coulomb_div_treatment=trim(options%coulomb_div_treatment))
   else
      call pw_bse_calc3(bse,w,options%energy_shift,options%cutoff_fock, &
      coulomb_div_treatment=trim(options%coulomb_div_treatment), &
      ecutvcut=options%ecutvcut)   
   endif
   call tools_log("done")

! Destruction of w
   call pw_w_destroy(w)

! Transposition of eigenvectors
   call tools_log("Calculating transpose ...",advance=.false.)
   call pw_bse_transpose(bse,0,ptk_comm_world)
   call tools_log(" done")

! Write of bse
   call tools_log("Writing eigenvalues and eigenstates on file bse ...",advance=.false.)
   if(bse%matrix%rank==bse%matrix%root) call iotk_open_write(66,file=trim(options%outdir)//"bse")
   call pw_bse_write(bse,66,"bse")
   if(bse%matrix%rank==bse%matrix%root) call iotk_close_write(66)
   call tools_log("done")

case DEFAULT
   ERROR("Wrong calculation_kind")
end select 
call pw_bse_destroy(bse)
endif ! end if options%calc_bse

if(options%calc_oscillators) then
npool_loc = npe
call init_pool(npool_loc, ptk_comm_world)
  call tools_log("Initializing bse ...",advance=.false.)
  call pw_bse_init(bse,states,atoms,options%bse_emin,options%bse_emax, &
       options%bse_spin,0,intra_pool_comm)
  call tools_log("done")
  call tools_log("Reading bse from file bse...",advance=.false.)
  call ptk_comm_rank(inter_pool_comm,my_pool_id)
  do ipool=0,npool-1     
  if(my_pool_id==ipool) then
    if(bse%matrix%rank==bse%matrix%root) call iotk_open_read(66,file=trim(options%outdir)//"bse")
    call pw_bse_readbcast(bse,66,"bse",0,intra_pool_comm)
    if(bse%matrix%rank==bse%matrix%root) call iotk_close_read(66)
  endif
  enddo
  call tools_log(" done")
!   do ipool=0,npool-1     
!  if(my_pool_id==ipool) then
!    if(bse%matrix%rank==bse%matrix%root) call iotk_open_write(66,file="bse_pippo")
!    call pw_bse_write(bse,66,"bse")
!    if(bse%matrix%rank==bse%matrix%root) call iotk_close_write(66)
!  endif
!  enddo
  call init_pool(npool,ptk_comm_world)
  write(crank,*) states%rank
  if(ionode) open(25,file=trim(options%outdir)//"rpaexc_oscillators",form="formatted", status="unknown")
  call tools_log("Calculating oscillators strengh ...",advance=.false.)
      call pw_oscillator_strengh_bis_calc(options%nstatemin,options%nstatemax,bse,states,atoms)
  call tools_log(" done")
  if(ionode) close(25)
  call pw_bse_destroy(bse)
endif

! Destruction of all general structures
call pw_states_destroy(states)
call pw_atoms_destroy(atoms)
call pw_kmesh_destroy(qmesh)
call pw_kmesh_destroy(kmesh)
call pw_symmlist_destroy(symmlist)

call tools_log("Writing here final mallinfo ...")
call tools_mallinfo_print(tools_log_unit(),msg="MALLINFO")
call tools_log("... done")
call tools_log("Writing here final PW statistics ...")
call pw_print_statistics(tools_log_unit())
call tools_log("... done")

call tools_log("Program BSE ends")

end subroutine bse_program
