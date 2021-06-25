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
subroutine pptools_program(npool)
!
use ptk_module
use iotk_module
use pptools_options_module
use tools_module
use num_module
use pw_module
use pw_pptools_module

implicit none

integer, intent(in) :: npool

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
type(pw_states)     :: states

integer :: log_unit

integer :: iostat

logical :: ionode
real :: states_cutoff
integer :: ib, ik                      ! temp index
character(256) :: infile,logfile

real, allocatable   :: k_read(:,:)     !i punti k letti per la atruttura a bande
integer             :: num_k_read      !Numero di punti k letti
character(len=iotk_attlenx) :: attr
real                :: pos(3)

call ptk_comm_rank(ptk_comm_world,rank=rank)
call ptk_comm_size(ptk_comm_world,size=npe)

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
call tools_log("Program pptools.x starts")
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
call pptools_options_readbcast(options,15,0,ptk_comm_world,fmt="iotk",name="sax_options")
call tools_log(" done")

if(options%calculation_kind=="SI") then
  call tools_log("Self-Interaction")      
elseif(options%calculation_kind=="PR") then
  call tools_log("Participation Ratio")
elseif(options%calculation_kind=="DOS") then
  call tools_log("Density of States")
elseif(options%calculation_kind=="BAND") then
  call tools_log("Band Structure")
else
  ERROR("Wrong calculation_kind")      
endif        

if(options%system_kind=="0D") then
  call tools_log("0D system")
elseif(options%system_kind=="1D") then
  call tools_log("1D system kind not yet implemented, assuming 3D")
  options%system_kind="3D" 
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

if(options%calculation_kind=="BAND") then
  call tools_log("Reading k-points for band structure...",advance=.false.)
  if(ionode) then
    call iotk_scan_begin(15,"kband",attr)
    call iotk_scan_attr (attr,"num_k",num_k_read)
    allocate(k_read(num_k_read,3))
    do ik=1,num_k_read
      read(15,*,iostat=iostat) pos
      IOCHECK(15,iostat)
      k_read(ik,:)=pos(:)
    enddo
    call iotk_scan_end  (15,"kband")
    call ptk_bcast(num_k_read,0,ptk_comm_world)
    call ptk_bcast(k_read,0,ptk_comm_world)
  endif   
  call tools_log(" done")
endif

call tools_log("Closing input file "//trim(infile)//" ...",advance=.false.)
if(ionode) call iotk_close_read(unit=15)
call tools_log(" done")

call tools_log("Writing here the kmesh for PWSCF code ...")
call pw_kmesh_write(kmesh,tools_log_unit(),"pwscf")
call tools_log("... done")

if(options%calculation_kind/="DOS") then
  call tools_log("Initializing states ...",advance=.false.)
  call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,ptk_comm_world)
  call tools_log(" done")

  if(options%convert_from_pwscf) then
    call tools_log("Converting states from pwscf format to file 'states' ...",advance=.false.)
    if(ionode) call iotk_open_write(10,file=trim(options%outdir)//"states",binary=.true.)
    call pw_states_convert_from_newp(states,trim(options%convert_from_pwscf_file),10,options%nelec,"states")
    if(ionode) call iotk_close_write(10)
    call tools_log(" done")
  end if

  select case(options%start_from)
  case("DFT")
    call tools_log("Reading states from file 'states' ...",advance=.false.)
    if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"states",binary=.true.)
    call pw_states_read(states,10,"states")
    if(ionode) call iotk_close_read(10)
    call tools_log(" done")
  case("HF")
    call tools_log("Reading states from file 'HF_states'...",advance=.false.)
    if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"HF_states",binary=.true.)
    call pw_states_read(states,10,"HF_states")
    if(ionode) call iotk_close_read(10)
    call tools_log("done")
  case("GW")
    call tools_log("Reading states from file 'GW_states'...",advance=.false.)
    if(ionode) call iotk_open_read(10,file=trim(options%outdir)//"GW_states",binary=.true.)
    call pw_states_read(states,10,"GW_states")
    if(ionode) call iotk_close_read(10)
    call tools_log("done")
  case DEFAULT
    ERROR("Wrong start_from")
  end select
end if
! calculating single particle wfc cutoff for coulomb
states_cutoff=pw_states_get_cutoff(states)
call tools_log("The cutoff for single particle wfc is: ", advance=.false.)
write(tools_log_unit(),"(f15.9)",advance='no') states_cutoff
call tools_log(" Ry")

select case(options%calculation_kind)
case("SI")
  if(options%system_kind=="0D") then
    do ik=1,states%kmesh%nkbz
       call pw_selfinteraction0(states,options%nbandmin,options%nbandmax, &
         ik,qmesh,options%cutoff_fock,options%outdir)
    enddo

  else
    do ik=1,states%kmesh%nkbz
       call pw_selfinteraction3(states,options%nbandmin,options%nbandmax, &
         ik,qmesh,options%cutoff_fock,options%outdir)
    enddo
  endif

case("PR")

   ERROR("PR not implemented yet") 
!  do ik=1,states%kmesh%nkbz
!     call pw_participationratio(states,options%nbandmin,options%nbandmax, &
!       ik,qmesh,options%cutoff_density*states_cutoff,options%outdir)
!  enddo

case("DOS")
   call pw_dos(options%nomega,options%omegamin,options%omegamax,   &
     options%degauss,kmesh%nkbz,options%nbandmin,options%nbandmax,    &
     options%diagonal,options%start_from,ionode,options%file_out,options%outdir)

case("BAND")
   if(options%interp_kind==1) then
     call pw_band_1(states,kmesh,k_read,num_k_read,atoms,options%nbandmin, &
         options%nbandmax,options%file_out,options%outdir)
   elseif(options%interp_kind==2) then
     call pw_band_2(states,kmesh,k_read,num_k_read,options%nbandmin, &
         options%nbandmax,options%stretch_val,      &
         options%stretch_cond, options%shift_val, options%shift_cond, &
         options%file_out, options%outdir)
   else 
      ERROR("Wrong interp_kind")
   endif
   deallocate(k_read)
case DEFAULT
   ERROR("Wrong calculation_kind")
end select 

! Destruction of all general structures
if(options%calculation_kind/="DOS") call pw_states_destroy(states)
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

call tools_log("Program pptools ends")

end subroutine pptools_program
