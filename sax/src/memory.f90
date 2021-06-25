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
subroutine memory_program(npool)

use ptk_module
use iotk_module
use mem_options_module
use pw_module
use tools_module
use num_module
use pw_QP_module
use pw_QP_states_module
use pw_epsilon_macro_module
use pw_pp_parameters_module
use memory_module
use pw_smearing_module
use memory_tools_module

implicit none

integer, intent(in) :: npool

integer :: rank,ierr,npe

character(256) :: args(256)
integer        :: nargs

type(sax_options)            :: options         ! Contiene le opzioni globali del codice
type(pw_struct)     :: struct          ! Contiene i dati di struttura (dim. cella) del sistema in analisi
type(pw_atoms)              :: atoms           ! Struttura atomica e pseudopotenziali
type(pw_symmlist)   :: symmlist        ! Contiene una lista delle simmetrie del sistema
type(pw_kmesh)      :: kmesh           ! Contiene la mesh per le funzioni d'onda
type(pw_kmesh)      :: qmesh           ! Contiene la mesh per gli elementi di dipolo
type(pw_states)     :: states
type(pw_QP)         :: QP

integer :: log_unit

integer :: iostat

integer :: ik

real :: mem_sigma, mem_QP, mem_diag, nbmin, nbmax, nk, mem_density
real :: mem_w, mem_w_unit, mem_w_tot, mem_w_scalable
real :: mem_polar, mem_polar_unit, mem_polar_tot, mem_polar_scalable
real :: mem_states, mem_states_unit, mem_states_tot, mem_states_scalable
real :: mem_QP_states, mem_QP_states_unit, mem_QP_states_tot, mem_QP_states_scalable
real :: mem_pp, mem_pp_tot, mem_pp_unit, mem_pp_scalable
real :: mem_bse, mem_bse_tot, mem_bse_unit, mem_bse_scalable
real :: mem_calc_sigma, mem_calc_QP
real, pointer :: total_ns_memory(:)
real, pointer :: total_s_memory(:,:)
real, pointer :: memory_max(:)
integer, pointer :: test_npe(:)
integer :: nb_test_npe, nMaxactions
integer :: im, kk, i
logical :: ionode
type(pw_smearing)           :: smearing
character(256) :: infile,logfile,outfile

call ptk_comm_rank(ptk_comm_world,rank=rank)
call ptk_comm_size(ptk_comm_world,size=npe)
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
call tools_log("Program memory.x starts")
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
call tools_log("... done")

call tools_log("Reading options ...",advance=.false.)
call mem_options_readbcast(options,15,0,ptk_comm_world,fmt="iotk",name="sax_options")
call tools_log("... done")
!----------------------------------------------------
!---CHECK THE NUMBER OF PROCESSOR---------
if(options%calculation_kind=="GW") then
  if(modulo(options%nbandmax-options%nbandmin+1,options%guessed_nb_proc)/=0) then
    ERROR("GW calculation need number of processor multiple of number of bands")
  end if
  nb_test_npe = options%nb_memory_test_nproc
  else
    nb_test_npe = 4
end if

nMaxactions = 50
call test_npe_init(nb_test_npe+1,test_npe)

!------FIND THE NUMBER OF TEST PROCESSORS------------
if(options%calculation_kind=="GW") then
  call calc_test_processors(nb_test_npe,options%nbandmin,options%nbandmax,test_npe)
  else
    test_npe(2) = 1
    test_npe(3) = 4
    test_npe(4) = 16
    test_npe(5) = 32
end if

nb_test_npe = nb_test_npe + 1 
test_npe(1) = options%guessed_nb_proc
call total_memory_init(nMaxactions,nb_test_npe,total_s_memory, total_ns_memory)

call tools_log("Reading structure ...",advance=.false.)
call pw_struct_readbcast(struct,15,0,ptk_comm_world,name="structure")
call tools_log("... done")

call pw_atoms_init(atoms,struct,options%cutoff_vloc)
call tools_log("Reading atoms ...",advance=.false.)
call pw_atoms_readbcast(atoms,15,0,ptk_comm_world,fmt="iotk",name="atoms")
call tools_log("... done")

call tools_log("Symmetries are not implemented")
call pw_symmlist_init(symmlist,struct)
call pw_symmlist_only_identity(symmlist)

call pw_kmesh_init(kmesh,symmlist)
call pw_kmesh_init(qmesh,symmlist)

call tools_log("Reading kmesh ...",advance=.false.)
call pw_kmesh_readbcast(kmesh,15,0,ptk_comm_world,fmt="iotk",name="kmesh")
call pw_kmesh_set_dim(qmesh,kmesh%m,(/0.0,0.0,0.0/))
call tools_log("... done")

call tools_log("Closing input file "//trim(infile)//" ...",advance=.false.)
if(ionode) call iotk_close_read(unit=15)
call tools_log("... done",advance=.true.)

call pw_smearing_init(smearing,options%smearing,options%smearing_kind)

call pw_states_init(states, smearing, struct,symmlist,kmesh,options%nbandmin, & 
                   options%nbandmax,0,ptk_comm_world)

!-------INIZIO PROGRAMMA DI CALCOLO DELLA MEMORIA--------
total_ns_memory(:) = 0.0
im = 1

!-----INIZIALIZZAZIONE DI TUTTE LE VARIABILI DI MEMORIA------
mem_states = 0.0
mem_states_unit = 0.0
mem_states_tot = 0.0
mem_states_scalable = 0.0
mem_sigma = 0.0
mem_QP = 0.0
mem_diag = 0.0
mem_density = 0.0
mem_calc_sigma = 0.0
mem_calc_QP = 0.0
mem_polar = 0.0
mem_polar_unit = 0.0
mem_polar_tot = 0.0
mem_polar_scalable = 0.0
mem_w = 0.0
mem_w_unit = 0.0
mem_w_tot = 0.0
mem_w_scalable = 0.0
mem_QP_states = 0.0
mem_QP_states_unit = 0.0
mem_QP_states_tot = 0.0
mem_QP_states_scalable = 0.0
mem_pp = 0.0
mem_pp_unit = 0.0
mem_pp_tot = 0.0
mem_pp_scalable = 0.0
mem_bse = 0.0
mem_bse_unit = 0.0
mem_bse_tot = 0.0
mem_bse_scalable = 0.0

!--------STATES_INIT-----------
call tools_log("Calculation of the memory required by the object 'states'..",advance=.true.)
call tools_log("Memory in bytes required by 'states': ",advance=.false.)
call memory_states(kmesh,struct%a,struct%b,options%nomega,options%nbandmin, &
   options%nbandmax,atoms%pseudo(1)%cutoff,mem_states,mem_states_unit,mem_states_tot)
write(tools_log_unit(),"(f15.3)",advance='no') mem_states + mem_states_tot
call tools_log(" bytes",advance=.true.)
call tools_log("Divided in :",advance=.true.)
call tools_log("Total scalable memory: ",advance=.false.)
write(tools_log_unit(),"(f15.3)",advance='no') mem_states_tot
call tools_log(" bytes",advance=.true.)
call tools_log("Non-scalable memory :",advance=.false.)
write(tools_log_unit(),"(f15.3)",advance='no') mem_states
call tools_log(" bytes",advance=.true.)
call tools_log("... done")

!-------MEM_TOT----------
do kk = 1, nb_test_npe
  call calc_scalable_memory(mem_states_unit,mem_states_tot,test_npe(kk), &
     mem_states_scalable)
  total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_states_scalable
end do
total_ns_memory(im+1) = total_ns_memory(im) + mem_states 
im = im+1
call tools_log("For guessed processors, maximum memory required by a processor here: ", &
  advance=.false.)
write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
call tools_log(" bytes",advance=.true.)

!--------DENSITY_INIT--------
if(options%calc_expectation_values) then
  call tools_log("Calculation of the memory required by the object 'density'..",advance=.true.)
  call tools_log("Memory in bytes required by 'density': ",advance=.false.)
  call memory_density(struct%a,struct%b,options%cutoff_density*atoms%pseudo(1)%cutoff,&
      mem_density)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_density
  call tools_log(" bytes",advance=.true.)
  call tools_log("... done")
  
  !-------MEM_TOT----------
  do kk = 1, nb_test_npe
    total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) + mem_density
  im = im+1
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)
  
  !------------DENSITY_DESTROY---------
  call tools_log("The object 'density' has been destroyed",advance=.true.)
  do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) - mem_density
  im = im+1
  mem_density = 0.0
  call tools_log ("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)
end if   

!--------DENSITY_INIT--------
if(options%calc_sp_hmatrix) then
  call tools_log("Calculation of the memory required by the object 'density'..",advance=.true.)
  call tools_log("Memory in bytes required by 'density': ",advance=.false.)
  call memory_density(struct%a,struct%b,options%cutoff_density*atoms%pseudo(1)%cutoff, &
     mem_density)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_density
  call tools_log(" bytes",advance=.true.)
  call tools_log("... done")

  !-------MEM_TOT----------
  do kk = 1, nb_test_npe
     total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) + mem_density
  im = im+1
  call tools_log("For guessed processors, maxium memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

  !------------DENSITY_DESTROY---------
  call tools_log("The object 'density' has been destroyed",advance=.true.)
  do kk = 1, nb_test_npe
     total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) - mem_density
  im = im+1
  mem_density = 0.0
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)
end if

select case(options%calculation_kind)
case("HF")
  !--------QP_INIT---------------
  call tools_log("Calculation of the memory required by the object 'QP'..",advance=.true.)
  call tools_log("Memory in bytes required by 'QP': ",advance=.false.)
  call memory_QP(options%sigma_nbmin,options%sigma_nbmax,states%kmesh%nkbz,mem_QP)
  call memory_calc_sigma(struct%a,struct%b,(/0.0,0.0,0.0/),(/0.0,0.0,0.0/), &
         options%cutoff_fock,options%sigma_nbmin, &
         options%sigma_nbmax,mem_calc_QP)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_QP + mem_calc_QP
  call tools_log(" bytes")
  call tools_log("... done")
  
  !-------MEM_TOT---------------
  do kk = 1, nb_test_npe
    total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) + mem_QP + mem_calc_QP
  im = im+1
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

  !------END CALC_QP-------
  call tools_log("The calculation of QP is finished",advance=.true.)
  do kk = 1, nb_test_npe
    total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) - mem_calc_QP
  im = im+1
  mem_calc_QP = 0.0
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)
  
  !----DIAGONALISATION------
  call tools_log("Calculation of the additional memory due to diagonalisation",advance=.true.)
  call tools_log("Additional memory in bytes: ",advance=.false.)
  call memory_diag(options%sigma_nbmin,options%sigma_nbmax,mem_diag,evonly=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_diag
  call tools_log(" bytes")
  call tools_log("... done")
  
  !-------MEM_TOT---------------
  do kk = 1, nb_test_npe
    total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) + mem_diag
  im = im+1
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)
              
 !------END DIAGONALISATION-------
  call tools_log("The diagonalisation is finished",advance=.true.)
  do kk = 1, nb_test_npe
    total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) - mem_diag
  im = im+1
  mem_diag = 0.0
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

 !--------QP_DESTROY-------------
  call tools_log("The object 'QP' has been destroyed",advance=.true.)
  do kk = 1, nb_test_npe
    total_s_memory(kk,im+1) = total_s_memory(kk,im)
  end do
  total_ns_memory(im+1) = total_ns_memory(im) - mem_QP 
  im = im+1
  mem_QP = 0.0
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

  if(options%calc_QP_states) then 
    !--------------QP_INIT---------------
    call tools_log("Calculation of the memory required by the object 'QP'..",advance=.true.)
    call tools_log("Memory in bytes required by 'QP': ",advance=.false.)
    call memory_QP(options%sigma_nbmin,options%sigma_nbmax,states%kmesh%nkbz,mem_QP)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP
    call tools_log(" bytes")
    call tools_log("... done")
      
    !-----------MEM_TOT---------------
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) + mem_QP
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)

   !----------QP_STATES_INIT--------------
    call tools_log("Calculation of the memory required by the object 'QP_states'..", &
       advance=.true.)
    call tools_log("Memory in bytes required by 'QP_states': ",advance=.false.)
    call memory_states(kmesh,struct%a,struct%b,options%nomega,options%nbandmin, &
       options%nbandmax,options%cutoff_polarizability,mem_QP_states,mem_QP_states_unit, &
       mem_QP_states_tot)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP_states + mem_QP_states_tot
    call tools_log(" bytes",advance=.true.)
    call tools_log("Divided in :",advance=.true.)
    call tools_log("Total scalable memory: ",advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP_states_tot
    call tools_log(" bytes",advance=.true.)
    call tools_log("Non-scalable memory :",advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP_states
    call tools_log(" bytes",advance=.true.)
    call tools_log("... done")

   !-------MEM_TOT------------------
    do kk = 1, nb_test_npe
       call calc_scalable_memory(mem_QP_states_unit,mem_QP_states_tot,test_npe(kk), &
          mem_QP_states_scalable)
       total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_QP_states_scalable
    end do

    total_ns_memory(im+1) = total_ns_memory(im) + mem_QP_states
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)

   !-----QP_STATES_DESTROY--------
    call tools_log("The object 'QP_states' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      call calc_scalable_memory(mem_QP_states_unit,mem_QP_states_tot,test_npe(kk), &
         mem_QP_states_scalable)
      total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_QP_states_scalable
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_QP_states
    im = im+1
    mem_QP_states = 0.0
    mem_QP_states_scalable = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  
   !-----QP_DESTROY-----------
    call tools_log("The object 'QP' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_QP
    im = im+1
    mem_QP = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  end if

case("GW")
  if(options%calc_polarizability) then
   !------POLAR_INIT-------------
    call tools_log("Calculation of the memory required by the object 'polar'..",advance=.true.)
    call tools_log("Memory in bytes required by 'polar': ",advance=.false.)
    call memory_polar(qmesh,struct%a,struct%b,options%nomega,options%cutoff_polarizability, &
       mem_polar,mem_polar_unit,mem_polar_tot)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_polar + mem_polar_tot
    call tools_log(" bytes",advance=.true.)
    call tools_log("Divided in :",advance=.true.)
    call tools_log("Total scalable memory: ",advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_polar_tot
    call tools_log(" bytes",advance=.true.)
    call tools_log("Non-scalable memory :",advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_polar
    call tools_log(" bytes",advance=.true.)
    call tools_log("... done")

    !-------MEM_TOT------------
    do kk = 1, nb_test_npe
      call calc_scalable_memory(mem_polar_unit,mem_polar_tot,test_npe(kk), &
          mem_polar_scalable)
      total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_polar_scalable
    end do

    total_ns_memory(im+1) = total_ns_memory(im) + mem_polar 
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  end if

  if(options%calc_w) then
    if(.not.options%calc_polarizability) then
      !---------POLAR_INIT----------
      call tools_log("Calculation of the memory required by the object 'polar'..", &
         advance=.true.)
      call tools_log("Memory in bytes required by 'polar': ",advance=.false.)
      call memory_polar(qmesh,struct%a,struct%b,options%nomega,options%cutoff_polarizability, &
         mem_polar,mem_polar_unit,mem_polar_tot)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_polar + mem_polar_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Divided in :",advance=.true.)
      call tools_log("Total scalable memory: ",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_polar_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Non-scalable memory :",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_polar
      call tools_log(" bytes",advance=.true.)
      call tools_log("...done")

      call calc_scalable_memory(mem_polar_unit,mem_polar_tot,options%guessed_nb_proc, &
         mem_polar_scalable)
              
      !-------MEM_TOT------------
      do kk = 1, nb_test_npe
        call calc_scalable_memory(mem_polar_unit,mem_polar_tot,test_npe(kk), &
          mem_polar_scalable)
        total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_polar_scalable
      end do
      total_ns_memory(im+1) = total_ns_memory(im) + mem_polar
      im = im+1
      call tools_log("For guessed processors, maximim memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
      call tools_log("bytes",advance=.true.)
    end if
    
    !-----------W_INIT-------------
    call tools_log("Calculation of the additional memory required by calc and write of 'w'..",advance=.true.)
    call tools_log("Memory in bytes required : ",advance=.false.)
    call memory_w_notalloc(qmesh,struct%a,struct%b,options%nomega,&
       options%cutoff_polarizability,mem_w)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_w 
    call tools_log(" bytes",advance=.true.)
    call tools_log("... done")
    
    !----------MEM_TOT-----------
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) + mem_w
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)  

   !---------DESTRUCTION OF ADD MEMORY-----
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_w
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)

   !---------------------------------------

  end if
   
  if(options%calc_w.OR.options%calc_polarizability) then
   !----------POLAR_DESTROY--------
    call tools_log("The object 'polar' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      call calc_scalable_memory(mem_polar_unit,mem_polar_tot,test_npe(kk), &
         mem_polar_scalable)
      total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_polar_scalable
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_polar
    im = im+1
    mem_polar = 0.0
    mem_polar_scalable = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  end if

  call tools_log("End of the first block of GW")

  if(options%gw_integration_method=="plasmon_pole") then
    if(options%calc_plasmon_pole_parameters) then
     !----------W_INIT------------
      call tools_log("Calculation of the memory required by the object 'w'..",advance=.true.)
      call tools_log("Memory in bytes required by 'w': ",advance=.false.)
      call memory_w(qmesh,struct%a,struct%b,options%nomega,options%cutoff_polarizability, &
         mem_w,mem_w_unit,mem_w_tot)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w + mem_w_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Divided in :",advance=.true.)
      call tools_log("Total scalable memory: ",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Non-scalable memory :",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w
      call tools_log(" bytes",advance=.true.)
      call tools_log("... done")
    !--------MEM_TOT-----------
      do kk = 1, nb_test_npe
        call calc_scalable_memory(mem_w_unit,mem_w_tot,test_npe(kk),mem_w_scalable)
        total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_w_scalable
      end do
      total_ns_memory(im+1)= total_ns_memory(im) + mem_w
      im = im + 1
      call tools_log("For guessed processors, maximum memory required by a processor here :", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)

    !---------PP_PARAMETERS_INIT----------------
      call tools_log("Calculation of the memory required by the object 'pp_parameters'..", &
         advance=.true.)
      call tools_log("Memory in bytes required by 'pp_parameters': ",advance=.false.)
      call memory_pp(qmesh,struct%a,struct%b,options%cutoff_polarizability, &
         mem_pp,mem_pp_unit,mem_pp_tot)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_pp + mem_pp_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Divided in :",advance=.true.)
      call tools_log("Total scalable memory: ",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_pp_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Non-scalable memory :",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_pp
      call tools_log(" bytes",advance=.true.)
      call tools_log("... done")

      !--------MEM_TOT-----------
      do kk = 1, nb_test_npe
        call calc_scalable_memory(mem_pp_unit,mem_pp_tot,test_npe(kk),mem_pp_scalable)
        total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_pp_scalable
      end do
      total_ns_memory(im+1)= total_ns_memory(im) + mem_pp
      im = im + 1
      call tools_log("For guessed processors, maximum memory required by a processor here :", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
      
      !---------W_DESTROY----------
        call tools_log("The object 'w' has been destroyed",advance=.true.)
        do kk = 1, nb_test_npe
          call calc_scalable_memory(mem_w_unit,mem_w_tot,test_npe(kk),mem_w_scalable)
          total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_w_scalable
        end do
        total_ns_memory(im+1) = total_ns_memory(im) - mem_w
        im = im+1
        mem_w = 0.0
        mem_w_scalable = 0.0
        call tools_log("For guessed processors, maximum memory required by a processor here: ", &
          advance=.false.)
        write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im) 
        call tools_log(" bytes",advance=.true.)
    end if
    
    if(options%calc_sigma_c) then
      if(.not.options%calc_plasmon_pole_parameters) then
      !---------PP_PARAMETERS_INIT----------------
        call tools_log("Calculation of the memory required by the object 'pp_parameters'..",&
           advance=.true.)
        call tools_log("Memory in bytes required by 'pp_parameters': ",advance=.false.)
        call memory_pp(qmesh,struct%a,struct%b,options%cutoff_polarizability, &
           mem_pp,mem_pp_unit,mem_pp_tot)
        write(tools_log_unit(),"(f15.3)",advance='no') mem_pp + mem_pp_tot
        call tools_log(" bytes",advance=.true.)
        call tools_log("Divided in :",advance=.true.)
        call tools_log("Total scalable memory: ",advance=.false.)
        write(tools_log_unit(),"(f15.3)",advance='no') mem_pp_tot
        call tools_log(" bytes",advance=.true.)
        call tools_log("Non-scalable memory :",advance=.false.)
        write(tools_log_unit(),"(f15.3)",advance='no') mem_pp
        call tools_log(" bytes",advance=.true.)
        call tools_log("... done")
        
        !--------MEM_TOT-----------
        do kk = 1, nb_test_npe
          call calc_scalable_memory(mem_pp_unit,mem_pp_tot,test_npe(kk),mem_pp_scalable)
          total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_pp_scalable
        end do
        total_ns_memory(im+1)= total_ns_memory(im) + mem_pp 
        im = im + 1
        call tools_log("For guessed processors, maximum memory required by a processor here :", &
           advance=.false.)
        write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
           total_s_memory(1,im)
        call tools_log(" bytes",advance=.true.)
      end if
         
         !-------SIGMA_INIT-------------
      call tools_log("Calculation of the memory required by the object 'sigma'..", &
         advance=.true.)
      call tools_log("Memory in bytes required by 'sigma': ",advance=.false.)
      call memory_sigma(options%sigma_nbmin,options%sigma_nbmax,mem_sigma)
      call memory_calc_sigma(struct%a,struct%b,(/0.0,0.0,0.0/),(/0.0,0.0,0.0/), &
         options%cutoff_polarizability,options%sigma_nbmin,options%sigma_nbmax, &
         mem_calc_sigma)
      if(options%sigma_first_order) then
        write(tools_log_unit(),"(f15.3)",advance='no') mem_sigma*2 + mem_calc_sigma
        else
          write(tools_log_unit(),"(f15.3)",advance='no') mem_sigma + mem_calc_sigma
      end if
      call tools_log(" bytes")
      call tools_log("... done",advance=.true.)
       
     !-------MEM_TOT-----------------
      do kk = 1, nb_test_npe
        total_s_memory(kk,im+1) = total_s_memory(kk,im)
      end do
      total_ns_memory(im+1) = total_ns_memory(im) + mem_sigma + mem_calc_sigma
      im = im+1
      call tools_log("For guessed processors, maximum memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
    
     !------SIGMA_DESTROY--------------
      call tools_log("The object 'sigma' has been destroyed",advance=.true.)
      do kk = 1, nb_test_npe
        total_s_memory(kk,im+1) = total_s_memory(kk,im)
      end do
      total_ns_memory(im+1) = total_ns_memory(im) - mem_sigma - mem_calc_sigma
      im = im+1
      mem_sigma = 0.0
      mem_calc_sigma = 0.0
      call tools_log("For guessed processors, maximum memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
    end if
      
    !-------PP_PARAMETERS_DESTROY-------
    call tools_log("The object 'pp_parameters' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      call calc_scalable_memory(mem_pp_unit,mem_pp_tot,test_npe(kk),mem_pp_scalable)
      total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_pp_scalable
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_pp
    im = im+1
    mem_pp = 0.0
    mem_pp_scalable = 0.0
    call tools_log("For guessed processors, maximum memory required by a program here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
   
  elseif(options%gw_integration_method=="Farid") then
    if(options%calc_sigma_c) then
      !----------W_INIT------------
      call tools_log("Calculation of the memory required by the object 'w'..",advance=.true.)
      call tools_log("Memory in bytes required by 'w': ",advance=.false.)
      call memory_w(qmesh,struct%a,struct%b,options%nomega,options%cutoff_polarizability, &
         mem_w,mem_w_unit,mem_w_tot)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w + mem_w_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Divided in :",advance=.true.)
      call tools_log("Total scalable memory: ",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Non-scalable memory :",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w
      call tools_log(" bytes",advance=.true.)
      call tools_log("... done")
      !--------MEM_TOT-----------
      do kk = 1, nb_test_npe
        call calc_scalable_memory(mem_w_unit,mem_w_tot,test_npe(kk),mem_w_scalable)
        total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_w_scalable
      end do
      total_ns_memory(im+1)= total_ns_memory(im) + mem_w 
      im = im + 1
      call tools_log("For guessed processors, maximum memory required by a processor here :", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
     
     !-------SIGMA_INIT-------------
      call tools_log("Calculation of the memory required by the object 'sigma'..", &
         advance=.true.)
      call tools_log("Memory in bytes required by 'sigma': ",advance=.false.)
      call memory_sigma(options%sigma_nbmin,options%sigma_nbmax,mem_sigma)
      call memory_calc_sigma(struct%a,struct%b,(/0.0,0.0,0.0/),(/0.0,0.0,0.0/), &
         options%cutoff_polarizability,options%sigma_nbmin,options%sigma_nbmax, &
         mem_calc_sigma)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_sigma + mem_calc_sigma
      call tools_log(" bytes")
      call tools_log("... done",advance=.true.)
        
     !-------MEM_TOT-----------------
      do kk = 1, nb_test_npe
        total_s_memory(kk,im+1) = total_s_memory(kk,im)
      end do
      total_ns_memory(im+1) = total_ns_memory(im) + mem_sigma + mem_calc_sigma
      im = im+1
      call tools_log("For guessed processors, maximum memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
     
      !------SIGMA_DESTROY--------------
      call tools_log("The object 'sigma' has been destroyed",advance=.true.)
      do kk = 1, nb_test_npe
        total_s_memory(kk,im+1) = total_s_memory(kk,im)
      end do
      total_ns_memory(im+1) = total_ns_memory(im) - mem_sigma - mem_calc_sigma
      im = im+1
      mem_sigma = 0.0
      mem_calc_sigma = 0.0
      call tools_log("For guessed processors, maximum memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
    end if

    !------W_DESTROY------------
    call tools_log("The object 'w' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      call calc_scalable_memory(mem_w_unit,mem_w_tot,test_npe(kk),mem_w_scalable)
      total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_w_scalable
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_w
    im = im+1
    mem_w = 0.0
    mem_w_scalable = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
       
  elseif(options%gw_integration_method=="cohsex") then
    if(options%calc_sigma_c) then
     !----------W_INIT------------
      call tools_log("Calculation of the memory required by the object 'w'..",advance=.true.)
      call tools_log("Memory in bytes required by 'w': ",advance=.false.)
      call memory_w(qmesh,struct%a,struct%b,options%nomega,options%cutoff_polarizability, &
         mem_w,mem_w_unit,mem_w_tot)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w + mem_w_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Divided in :",advance=.true.)
      call tools_log("Total scalable memory: ",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w_tot
      call tools_log(" bytes",advance=.true.)
      call tools_log("Non-scalable memory :",advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_w
      call tools_log(" bytes",advance=.true.)
      call tools_log("... done")

      !--------MEM_TOT-----------
      do kk = 1, nb_test_npe
        call calc_scalable_memory(mem_w_unit,mem_w_tot,test_npe(kk),mem_w_scalable)
        total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_w_scalable
      end do
      total_ns_memory(im+1)= total_ns_memory(im) + mem_w 
      im = im + 1
      call tools_log("For guessed processors, maximum memory required by a processor here :", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
     
     !-------SIGMA_INIT-------------
      call tools_log("Calculation of the memory required by the object 'sigma'..", &
         advance=.true.)
      call tools_log("Memory in bytes required by 'sigma': ",advance=.false.)
      call memory_sigma(options%sigma_nbmin,options%sigma_nbmax,mem_sigma)
      call memory_calc_sigma(struct%a,struct%b,(/0.0,0.0,0.0/),(/0.0,0.0,0.0/), &
         options%cutoff_polarizability,options%sigma_nbmin,options%sigma_nbmax, &
         mem_calc_sigma)
      write(tools_log_unit(),"(f15.3)",advance='no') mem_sigma + mem_calc_sigma
      call tools_log(" bytes")
      call tools_log("... done",advance=.true.)

      !-------MEM_TOT-----------------
      do kk = 1, nb_test_npe
        total_s_memory(kk,im+1) = total_s_memory(kk,im)
      end do
      total_ns_memory(im+1) = total_ns_memory(im) + mem_sigma + mem_calc_sigma
      im = im+1
      call tools_log("For guessed processors, maximum memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
      
      !------SIGMA_DESTROY--------------
      call tools_log("The object 'sigma' has been destroyed",advance=.true.)
      do kk = 1, nb_test_npe
        total_s_memory(kk,im+1) = total_s_memory(kk,im)
      end do
      total_ns_memory(im+1) = total_ns_memory(im) - mem_sigma - mem_calc_sigma
      im = im+1
      mem_sigma = 0.0
      mem_calc_sigma = 0.0
      call tools_log("For guessed processors, maximum memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
    end if

    !------W_DESTROY------------
    call tools_log("The object 'w' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      call calc_scalable_memory(mem_w_unit,mem_w_tot,test_npe(kk),mem_w_scalable)
      total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_w_scalable
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_w
    im = im+1
    mem_w = 0.0
    mem_w_scalable = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  end if

  if(options%calc_sigma_x) then
    !-------SIGMA_INIT----------
    call tools_log("Calculation of the memory required by the object 'sigma'..", &
       advance=.true.)
    call tools_log("Memory in bytes required by 'sigma': ",advance=.false.)
    call memory_sigma(options%sigma_nbmin,options%sigma_nbmax,mem_sigma)
    call memory_calc_sigma(struct%a,struct%b,(/0.0,0.0,0.0/),(/0.0,0.0,0.0/), &
         options%cutoff_fock,options%sigma_nbmin, &
         options%sigma_nbmax,mem_calc_sigma)      
    write(tools_log_unit(),"(f15.3)",advance='no') mem_sigma + mem_calc_sigma
    call tools_log(" bytes")
    call tools_log("... done",advance=.true.)
        
    !-------MEM_TOT-----------------
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) + mem_sigma + mem_calc_sigma
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
     
    !------SIGMA_DESTROY--------------
    call tools_log("The object 'sigma' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_sigma - mem_calc_sigma
    im = im+1
    mem_sigma = 0.0
    mem_calc_sigma = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  end if

  if(options%calc_energies) then
    !--------------QP_INIT---------------
    call tools_log("Calculation of the memory required by the object 'QP'..",advance=.true.)
    call tools_log("Memory in bytes required by 'QP': ",advance=.false.)
    call memory_QP(options%sigma_nbmin,options%sigma_nbmax,states%kmesh%nkbz,mem_QP)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP
    call tools_log(" bytes")
    call tools_log("... done")
    
    !-------------MEM_TOT------------
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) + mem_QP
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a program here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)

    !-------SIGMA_INIT----------
    call tools_log("Calculation of the memory required by the object 'sigma'..", &
       advance=.true.)
    call tools_log("Memory in bytes required by 'sigma': ",advance=.false.)
    call memory_sigma(options%sigma_nbmin,options%sigma_nbmax,mem_sigma)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_sigma
    call tools_log(" bytes")
    call tools_log("... done",advance=.true.)
        
    !-------MEM_TOT-----------------
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) + mem_sigma
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
     
    !------SIGMA_DESTROY--------------
    call tools_log("The object 'sigma' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_sigma
    im = im+1
    mem_sigma = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)

    if(options%sigma_first_order) then
      !-------SIGMA_INIT----------
      call tools_log("Calculation of the memory required by the object 'sigma'..", &
         advance=.true.)
      call tools_log("Memory in bytes required by 'sigma': ",advance=.false.)
      call memory_sigma(options%sigma_nbmin,options%sigma_nbmax,mem_sigma)
      mem_sigma = mem_sigma*2
      write(tools_log_unit(),"(f15.3)",advance='no') mem_sigma
      call tools_log(" bytes")
      call tools_log("... done",advance=.true.)
          
      !-------MEM_TOT-----------------
      do kk = 1, nb_test_npe
        total_s_memory(kk,im+1) = total_s_memory(kk,im)
      end do
      total_ns_memory(im+1) = total_ns_memory(im) + mem_sigma
      im = im+1
      call tools_log("For guessed processors, maximum memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
     
      !------SIGMA_DESTROY--------------
      call tools_log("The object 'sigma' has been destroyed",advance=.true.)
      do kk = 1, nb_test_npe
        total_s_memory(kk,im+1) = total_s_memory(kk,im)
      end do
      total_ns_memory(im+1) = total_ns_memory(im) - mem_sigma
      im = im+1
      mem_sigma = 0.0
      call tools_log("For guessed processors, maximum memory required by a processor here: ", &
         advance=.false.)
      write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
         total_s_memory(1,im)
      call tools_log(" bytes",advance=.true.)
    end if

    !----DIAGONALISATION------
    call tools_log("Calculation of the additional memory due to diagonalisation",advance=.true.)
    call tools_log("Additional memory in bytes: ",advance=.false.)
    call memory_diag(options%sigma_nbmin,options%sigma_nbmax,mem_diag,evonly=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_diag
    call tools_log(" bytes")
    call tools_log("... done")
  
    !-------MEM_TOT---------------
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) + mem_diag
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
              
    !------END DIAGONALISATION-------
    call tools_log("The diagonalisation is finished",advance=.true.)
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_diag
    im = im+1
    mem_diag = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)

    !-----QP_DESTROY-----------
    call tools_log("The object 'QP' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_QP
    im = im+1
    mem_QP = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  end if
  
  if(options%calc_QP_states) then 
    !--------------QP_INIT---------------
    call tools_log("Calculation of the memory required by the object 'QP'..",advance=.true.)
    call tools_log("Memory in bytes required by 'QP': ",advance=.false.)
    call memory_QP(options%sigma_nbmin,options%sigma_nbmax,states%kmesh%nkbz,mem_QP)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP
    call tools_log(" bytes")
    call tools_log("... done")
      
    !-----------MEM_TOT---------------
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) + mem_QP
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)

   !----------QP_STATES_INIT--------------
    call tools_log("Calculation of the memory required by the object 'QP_states'..", &
       advance=.true.)
    call tools_log("Memory in bytes required by 'QP_states': ",advance=.false.)
    call memory_states(kmesh,struct%a,struct%b,options%nomega,options%nbandmin, &
       options%nbandmax,options%cutoff_polarizability,mem_QP_states,mem_QP_states_unit, &
       mem_QP_states_tot)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP_states + mem_QP_states_tot
    call tools_log(" bytes",advance=.true.)
    call tools_log("Divided in :",advance=.true.)
    call tools_log("Total scalable memory: ",advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP_states_tot
    call tools_log(" bytes",advance=.true.)
    call tools_log("Non-scalable memory :",advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') mem_QP_states
    call tools_log(" bytes",advance=.true.)
    call tools_log("... done")

   !-------MEM_TOT------------------
    do kk = 1, nb_test_npe
      call calc_scalable_memory(mem_QP_states_unit,mem_QP_states_tot, &
         test_npe(kk),mem_QP_states_scalable)
      total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_QP_states_scalable
    end do             
    total_ns_memory(im+1) = total_ns_memory(im) + mem_QP_states
    im = im+1
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)

   !-----QP_STATES_DESTROY--------
    call tools_log("The object 'QP_states' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      call calc_scalable_memory(mem_QP_states_unit,mem_QP_states_tot,test_npe(kk), &
         mem_QP_states_scalable)
      total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_QP_states_scalable
    end do             
    total_ns_memory(im+1) = total_ns_memory(im) - mem_QP_states
    im = im+1
    mem_QP_states = 0.0
    mem_QP_states_scalable = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  
   !-----QP_DESTROY-----------
    call tools_log("The object 'QP' has been destroyed",advance=.true.)
    do kk = 1, nb_test_npe
      total_s_memory(kk,im+1) = total_s_memory(kk,im)
    end do
    total_ns_memory(im+1) = total_ns_memory(im) - mem_QP
    im = im+1
    mem_QP = 0.0
    call tools_log("For guessed processors, maximum memory required by a processor here: ", &
       advance=.false.)
    write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
       total_s_memory(1,im)
    call tools_log(" bytes",advance=.true.)
  end if
  
case("MACRO")
!------------READ STATES-----------------------------
  call tools_log("Reading states...",advance=.false.)
  if(options%start_from=="DFT") then
    if(ionode) call iotk_open_read(10,file="states",binary=.true.)
    call pw_states_read(states,10,"states")
    if(ionode) call iotk_close_read(10)
  else if(options%start_from=="HF") then
    if(ionode) call iotk_open_read(10,file="HF_states",binary=.true.)
    call pw_states_read(states,10,"HF_states")
    if(ionode) call iotk_close_read(10)
  else if(options%start_from=="GW") then
    if(ionode) call iotk_open_read(10,file="GW_states",binary=.true.)
    call pw_states_read(states,10,"GW_states")
    if(ionode) call iotk_close_read(10)
  else
    ERROR("wrong option start_from for bse memory")
  end if  
  call tools_log(" done")

!----------BSE_INIT---------------
  call tools_log("Calculation of the memory required by the object 'bse'..", &
     advance=.true.)
  call tools_log("Memory in bytes required by 'bse': ",advance=.false.)
  call memory_bse(states,options%bse_emin,options%bse_emax,mem_bse,mem_bse_unit, &
     mem_bse_tot)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_bse + mem_bse_tot
  call tools_log(" bytes",advance=.true.)
  call tools_log("Divided in :",advance=.true.)
  call tools_log("Total scalable memory: ",advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_bse_tot
  call tools_log(" bytes",advance=.true.)
  call tools_log("Non-scalable memory :",advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_bse
  call tools_log(" bytes",advance=.true.)
  call tools_log("... done")

 !-------MEM_TOT------------------
  do kk = 1, nb_test_npe
    call calc_scalable_memory(mem_bse_unit,mem_bse_tot,test_npe(kk),mem_bse_scalable)
    total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_bse_scalable
  end do
  total_ns_memory(im+1) = total_ns_memory(im) + mem_bse
  im = im+1
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
     total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

!------BSE_DESTROY------------
  call tools_log("The object 'bse' has been destroyed",advance=.true.)
  do kk = 1, nb_test_npe
    call calc_scalable_memory(mem_bse_unit,mem_bse_tot,test_npe(kk),mem_bse_scalable)
    total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_bse_scalable
  end do
  total_ns_memory(im+1) = total_ns_memory(im) - mem_bse
  im = im+1
  mem_bse = 0.0
  mem_bse_scalable = 0.0
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
     total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

case("FULL")
!----------READ STATES-------------------------
  call tools_log("Reading states...",advance=.false.)
  if(options%start_from=="DFT") then
    if(ionode) call iotk_open_read(10,file="states",binary=.true.)
    call pw_states_read(states,10,"states")
    if(ionode) call iotk_close_read(10)
  else if(options%start_from=="HF") then
    if(ionode) call iotk_open_read(10,file="HF_states",binary=.true.)
    call pw_states_read(states,10,"HF_states")
    if(ionode) call iotk_close_read(10)
  else if(options%start_from=="GW") then
    if(ionode) call iotk_open_read(10,file="GW_states",binary=.true.)
    call pw_states_read(states,10,"GW_states")
    if(ionode) call iotk_close_read(10)
  else
    ERROR("wrong option start_from for bse memory")
  end if  
  call tools_log(" done")

!-------BSE_INIT-------------
  call tools_log("Calculation of the memory required by the object 'bse'..", &
     advance=.true.)
  call tools_log("Memory in bytes required by 'bse': ",advance=.false.)
  call memory_bse(states,options%bse_emin,options%bse_emax,mem_bse,mem_bse_unit, &
     mem_bse_tot)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_bse + mem_bse_tot
  call tools_log(" bytes",advance=.true.)
  call tools_log("Divided in :",advance=.true.)
  call tools_log("Total scalable memory: ",advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_bse_tot
  call tools_log(" bytes",advance=.true.)
  call tools_log("Non-scalable memory :",advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_bse
  call tools_log(" bytes",advance=.true.)
  call tools_log("... done")

 !-------MEM_TOT------------------
  do kk = 1, nb_test_npe
    call calc_scalable_memory(mem_bse_unit,mem_bse_tot,test_npe(kk),mem_bse_scalable)
    total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_bse_scalable
  end do
  total_ns_memory(im+1) = total_ns_memory(im) + mem_bse
  im = im+1
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
     total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)
 
 !----------W_INIT------------
  call tools_log("Calculation of the memory required by the object 'w'..",advance=.true.)
  call tools_log("Memory in bytes required by 'w': ",advance=.false.)
  call memory_w(qmesh,struct%a,struct%b,options%nomega,options%cutoff_polarizability, &
     mem_w,mem_w_unit,mem_w_tot)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_w + mem_w_tot
  call tools_log(" bytes",advance=.true.)
  call tools_log("Divided in :",advance=.true.)
  call tools_log("Total scalable memory: ",advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_w_tot
  call tools_log(" bytes",advance=.true.)
  call tools_log("Non-scalable memory :",advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') mem_w
  call tools_log(" bytes",advance=.true.)
  call tools_log("... done")

 !--------MEM_TOT-----------
  do kk = 1, nb_test_npe
    call calc_scalable_memory(mem_w_unit,mem_w_tot,test_npe(kk),mem_w_scalable)
    total_s_memory(kk,im+1) = total_s_memory(kk,im) + mem_w_scalable
  end do
  total_ns_memory(im+1)= total_ns_memory(im) + mem_w
  im = im + 1
  call tools_log("For guessed processors, maximum memory required by a processor here :", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
     total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

 !------W_DESTROY------------
  call tools_log("The object 'w' has been destroyed",advance=.true.)
  do kk = 1, nb_test_npe
    call calc_scalable_memory(mem_w_unit,mem_w_tot,test_npe(kk),mem_w_scalable)
    total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_w_scalable
  end do
  total_ns_memory(im+1) = total_ns_memory(im) - mem_w
  im = im+1
  mem_w = 0.0
  mem_w_scalable = 0.0
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
    total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

 !------BSE_DESTROY------------
  call tools_log("The object 'bse' has been destroyed",advance=.true.)
  do kk = 1, nb_test_npe
    call calc_scalable_memory(mem_bse_unit,mem_bse_tot,test_npe(kk),mem_bse_scalable)
    total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_bse_scalable
  end do
  total_ns_memory(im+1) = total_ns_memory(im) - mem_bse
  im = im+1
  mem_bse = 0.0
  mem_bse_scalable = 0.0
  call tools_log("For guessed processors, maximum memory required by a processor here: ", &
     advance=.false.)
  write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
     total_s_memory(1,im)
  call tools_log(" bytes",advance=.true.)

end select

call pw_atoms_destroy(atoms)
call pw_kmesh_destroy(qmesh)
call pw_kmesh_destroy(kmesh)
call pw_symmlist_destroy(symmlist)
call pw_states_destroy(states)

!-----QP_STATES_DESTROY--------
call tools_log("The object 'states' has been destroyed",advance=.true.)
do kk = 1, nb_test_npe
  call calc_scalable_memory(mem_states_unit,mem_states_tot,test_npe(kk), &
     mem_states_scalable)
  total_s_memory(kk,im+1) = total_s_memory(kk,im) - mem_states_scalable
end do
total_ns_memory(im+1) = total_ns_memory(im) - mem_states
im = im+1
mem_states = 0.0
mem_states_scalable = 0.0
call tools_log("For guessed processors, maximum memory required by a processor here: ", &
   advance=.false.)
write(tools_log_unit(),"(f15.3)",advance='no') total_ns_memory(im) + &
   total_s_memory(1,im)
call tools_log(" bytes",advance=.true.)

call tools_log("Total number of memory-actions: ",advance=.false.)
write(tools_log_unit(),"(i15)",advance='yes') im-1

!-----FINAL STATISTIC--------------
call memory_max_init(nb_test_npe,memory_max)
outfile = options%memory_output_file

if(ionode) then
  call iotk_open_write(unit=111,file=trim(outfile))
  write(111,*) "STATISTICS ABOUT ALLOCATED MEMORY FOR PROCESSOR"
  do kk = 1, nb_test_npe
    do i = 1, im
      total_s_memory(kk,i) = total_s_memory(kk,i) + total_ns_memory(i)
    end do
    memory_max(kk) = maxval(total_s_memory(kk,:))
    write(111,'("Maximum allocated memory for",2x,i3,2x,"processors is",2x)',advance="no") &
       test_npe(kk)
    write(111,"(f15.1)",advance="no") memory_max(kk)
    write(111,*) "bytes"
  end do
  call iotk_close_write(111)
end if

call memory_max_destroy(memory_max)
call test_npe_destroy(test_npe)

!------END PROGRAM-------
call tools_log("Writing here final mallinfo ...")
call tools_mallinfo_print(tools_log_unit(),msg="MALLINFO")
call tools_log("... done")
call tools_log("Writing here final PW statistics ...")
call pw_print_statistics(tools_log_unit())
call tools_log("... done")

call tools_log("Program memory ends")

end subroutine memory_program
