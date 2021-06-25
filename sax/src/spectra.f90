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
subroutine spectra_program()
!
use ptk_module
use iotk_module
use mp_global
use spectra_options_module
use tools_module
use num_module
use pw_epsilon_macro_module
use pw_epsilon_macroLF_module
use pw_wbw_module
use pw_module
use pw_bse_interf
use pw_bse_type
use pw_smearing_module

implicit none

integer                  :: rank,ierr,npe

character(256)           :: args(256)
integer                  :: nargs

type(sax_options)        :: options   ! Contiene le opzioni globali del codice
type(pw_struct)          :: struct    ! Contiene i dati di struttura (dim. cella) del sistema in analisi
type(pw_atoms)           :: atoms     ! Struttura atomica e pseudopotenziali
type(pw_symmlist)        :: symmlist  ! Contiene una lista delle simmetrie del sistema
type(pw_kmesh)           :: kmesh     ! Contiene la mesh per le funzioni d'onda
type(pw_kmesh)           :: qmesh     ! Contiene la mesh per gli elementi di dipolo
                                      ! (le differenze in kmesh)
type(pw_bse)             :: bse
type(pw_states)          :: states
type(pw_epsilon_macro)   :: epsilon 
type(pw_epsilon_macroLF) :: epsilonLF ! Contiene le WINGS 
type(pw_smearing)        :: smearing

integer                  :: log_unit, iostat

type(pw_w)               :: w         ! Contiene W
logical                  :: ionode
real                     :: states_cutoff
integer                  :: ib        ! temp index
character(256)           :: infile,logfile

integer :: ipool, my_pool_id

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
call tools_log("Program bse.x starts")
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
call spectra_options_readbcast(options,15,0,ptk_comm_world,fmt="iotk",name="sax_options")
call tools_log(" done")

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
  call pw_states_init(states,smearing,struct,symmlist,kmesh,options%nbandmin,&
                      options%nbandmax,0,ptk_comm_world)
  call tools_log(" done")

  call tools_log("Converting states from pwscf format to file 'states' ...",advance=.false.)
  if(ionode) call iotk_open_write(10,file=trim(options%outdir)//"states",binary=.true.)
  call pw_states_convert_from_newp(states,trim(options%convert_from_pwscf_file),10,options%nelec,"states")
  if(ionode) call iotk_close_write(10)
  call tools_log(" done")
  call pw_states_destroy(states)
end if

call tools_log("Initializing states ...",advance=.false.)
call pw_states_init(states,struct,symmlist,kmesh,options%nbandmin,options%nbandmax,0,ptk_comm_world)
call tools_log(" done")

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

! calculating single particle wfc cutoff for coulomb

states_cutoff=pw_states_get_cutoff(states)
call tools_log("The cutoff for single particle wfc is: ", advance=.false.)
write(tools_log_unit(),"(f15.9)",advance='no') states_cutoff
call tools_log(" Ry")

! to avoid deadlock...

if(modulo(options%nbandmax-options%nbandmin+1,npe)/=0) &
    ERROR("nb bands in states should be a multiple of nb proc")

select case(options%calculation_kind)

!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
! RPA dielectric function
!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

case("RPA")

!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
! Calculation including local fields
!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

  if(options%lfe) then
    if(.not.options%just_project_on_q_direction) then

!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
! Local-fields via diagonalization of BSE eigen-problem 
!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

      if(options%lfe_kind=='diago') then 
        call tools_log("Initializing bse ...",advance=.false.)
        call pw_bse_init(bse,states,atoms,0.0,options%emax_polarizability, &
             0,0,ptk_comm_world)
        call tools_log("done")
        call tools_log("calculating exchange part of two particle hamiltonian...",advance=.false.)
        if(trim(options%system_kind)=="0D") options%coulomb_div_treatment="vcut_spherical"
        if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
          call pw_bse_x_calc(bse,qmesh,options%cutoff_fock*states_cutoff,&
             coulomb_div_treatment=trim(options%coulomb_div_treatment))
        else
          call pw_bse_x_calc(bse,qmesh,options%cutoff_fock*states_cutoff,&
             coulomb_div_treatment=trim(options%coulomb_div_treatment), &
             ecutvcut=options%ecutvcut)
        endif
        call tools_log(" done")
        call tools_log("Writing eigenvalues and eigenstates of two particle hamiltonian" &
                       //"on file H2p_x ...",advance=.false.)
        if(ionode) call iotk_open_write(66,file=trim(options%outdir)//"H2p_x")
        call pw_bse_write(bse,66,"H2p_x")
        if(ionode) call iotk_close_write(66)
        call tools_log("done")
   
        call tools_log("Initializing epsilon...",advance=.false.)
        call pw_epsilon_macro_init(epsilon,struct,symmlist,qmesh,options%omegamax, &
        options%nomega,options%cutoff_polarizability,options%emax_polarizability, &
        options%degauss_polarizability,options%energy_shift,options%lorentzian_broadening, &
             options%imaginary_axis,0,ptk_comm_world)
        call tools_log(" done")
        call tools_log("Calculating optical absorption spectrum...",advance=.false.)
        call pw_epsilon_macro_calc(epsilon,bse,states,atoms)
        call tools_log(" done")
        call pw_bse_destroy(bse)
        call tools_log("Writing macroscopic dielectric tensor on file epsilon.lf.RPA...",advance=.false.)
        if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"epsilon.lf.RPA")
        call tools_log(" done")
        call pw_epsilon_macro_destroy(epsilon)

!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
! Local-fields via direct inversion (chi Dyson equation) 
!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

      else if (options%lfe_kind=='invert') then

         call tools_log("Initializing HEAD BODY and WINGS...",advance=.false.)
         call pw_epsilon_macroLF_init(epsilonLF,struct,symmlist,qmesh,options%omegamax,&
                 options%nomega,options%cutoff_polarizability, options%emax_polarizability,&
                 options%degauss_polarizability,states%kmesh%nkibz,options%imaginary_axis,&
                 0,comm=ptk_comm_world)
         call tools_log(" done")
         call tools_log("Calculating HEAD BODY and WINGS...",advance=.false.)
         call pw_epsilon_macroLF_calc(epsilonLF,states,atoms,options%lorentzian_broadening)
         call tools_log(" done")

         call tools_log("done")
         call tools_log("Calculating HEAD  - WING * BODY * WING product ...",advance=.false.)
         if(trim(options%system_kind)=="0D") options%coulomb_div_treatment="vcut_spherical"
         if(trim(options%coulomb_div_treatment).ne."vcut_ws") then
           call pw_wbw_calc_3(epsilonLF,coulomb_div_treatment=trim(options%coulomb_div_treatment))
         else
           call pw_wbw_calc_3(epsilonLF,coulomb_div_treatment=trim(options%coulomb_div_treatment), &
               ecutvcut=options%ecutvcut)
         endif
         call tools_log("done")
         
         call tools_log("Writing optical absorption spectrum on file oas.lf.RPA...",advance=.false.)
         call pw_wbw_write(epsilonLF,"imag",41,"oas.lf.RPA")
         call tools_log(" done")
         
         call tools_log("Writing real part of macroscopic dielectric tensor on file repsilon.lf.RPA...",advance=.false.)
         call pw_wbw_write(epsilonLF,"real",41,"repsilon.lf.RPA")
         call tools_log(" done")
         call pw_epsilon_macroLF_destroy(epsilonLF)
         STOP
      else 
         ERROR("Wrong lfe_kind")
      endif   
    endif

!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
! Calculation without local field effects
!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

    call tools_log("Initializing epsilon...",advance=.false.)
    call pw_epsilon_macro_init(epsilon,struct,symmlist,qmesh,options%omegamax, &
      options%nomega,options%cutoff_polarizability,options%emax_polarizability, &
      options%degauss_polarizability,options%energy_shift,options%lorentzian_broadening, &
         options%imaginary_axis,0,ptk_comm_world)
    call tools_log(" done")
    call tools_log("Reading macroscopic dielectric tensor from file epsilon.lf.RPA...",advance=.false.)
    call pw_epsilon_macro_readbcast(epsilon,200002,trim(options%outdir)//"epsilon.lf.RPA",0,ptk_comm_world)
    call tools_log(" done")
    call tools_log("Projecting macroscopic dielectric tensor on  ",advance=.false.)
    write(tools_log_unit(),"(i2,2x,i2,2x)",advance='no') options%q
    call tools_log(" direction")
    call pw_epsilon_macro_project_on_q(epsilon,options%q)
    call tools_log("Writing electron energy loss on file eels.lf.RPA...",advance=.false.)
    if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"eels.lf.RPA", &
                    kindout="eels",fmt=options%output_format)
    call tools_log(" done")
    call tools_log("Writing optical absorption spectrum on file oas.lf.RPA...",advance=.false.)
    if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"oas.lf.RPA", &
                    kindout="imag",fmt=options%output_format)
    call tools_log(" done")
    call tools_log("Writing real part of macroscopic dielectric tensor on file repsilon.lf.RPA...",advance=.false.)
    call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"repsilon.lf.RPA", &
         kindout="real",fmt=options%output_format)
    call pw_epsilon_macro_destroy(epsilon)
  else
    if(.not.options%just_project_on_q_direction) then

!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~* 
! projection on other q direction of previously calculated epsilon
!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

      call tools_log("Initializing epsilon...",advance=.false.)
      call pw_epsilon_macro_init(epsilon,struct,symmlist,qmesh,options%omegamax, &
        options%nomega,options%cutoff_polarizability,options%emax_polarizability, &
        options%degauss_polarizability,options%energy_shift,options%lorentzian_broadening, &
         options%imaginary_axis,0,ptk_comm_world)
      call tools_log(" done")
      call tools_log("Calculating macroscopic dielectric tensor...",advance=.false.)
      call pw_epsilon_macro_calc(epsilon,states,atoms)
      call tools_log(" done")
      call tools_log("Writing macroscopic dielectric tensor on file epsilon.nlf.RPA...",advance=.false.)
      if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"epsilon.nlf.RPA")
      call tools_log(" done")
      call pw_epsilon_macro_destroy(epsilon)
    endif
    call tools_log("Initializing epsilon...",advance=.false.)
    call pw_epsilon_macro_init(epsilon,struct,symmlist,qmesh,options%omegamax, &
      options%nomega,options%cutoff_polarizability,options%emax_polarizability, &
      options%degauss_polarizability,options%energy_shift,options%lorentzian_broadening, &
         options%imaginary_axis,0,ptk_comm_world)
    call tools_log(" done")
    call tools_log("Reading macroscopic dielectric tensor from file epsilon.nlf.RPA...",advance=.false.)
    call pw_epsilon_macro_readbcast(epsilon,200002,trim(options%outdir)//"epsilon.nlf.RPA", &
         0,ptk_comm_world)
    call tools_log(" done")
    call tools_log("Projecting macroscopic dielectric tensor on  ",advance=.false.)
    write(tools_log_unit(),"(i2,2x,i2,2x)",advance='no') options%q
    call tools_log(" direction")
    call pw_epsilon_macro_project_on_q(epsilon,options%q)
    call tools_log("Writing electron energy loss on file eels.nlf.RPA...",advance=.false.)
    if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"eels.nlf.RPA", &
                    kindout="eels",fmt=options%output_format)
    call tools_log(" done")
    call tools_log("Writing optical absorption spectrum on file oas.nlf.RPA...",advance=.false.)
    if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"oas.nlf.RPA", &
                    kindout="imag",fmt=options%output_format)
    call tools_log(" done")
    call tools_log("Writing real part of macroscopic dielectric tensor on file repsilon...",advance=.false.)
    call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"repsilon.nlf.RPA", &
         kindout="real",fmt=options%output_format)
    call pw_epsilon_macro_destroy(epsilon)
  endif

!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~* 
! Dielectric function with excitonic effects
!~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

case("RPAEXC")
  if(.not.options%lfe) then
    call tools_log("RPA + excitonic effects allways calculated including nlfe")
     options%lfe = .true.
     options%lfe_kind ='diago'
  endif
  if(.not.options%just_project_on_q_direction) then
    call tools_log("Initializing bse ...",advance=.false.)
    call pw_bse_init(bse,states,atoms,options%bse_emin,options%bse_emax, &
         options%bse_spin,0,ptk_comm_world)
    call tools_log("done")
    call tools_log("Reading bse from file bse...",advance=.false.)
    if(ionode) call iotk_open_read(66,file=trim(options%outdir)//"bse")
    call pw_bse_readbcast(bse,66,"bse",0,ptk_comm_world)
    if(ionode) call iotk_close_read(66)
    call tools_log(" done")
    call tools_log("Initializing macroscopic dielectric tensor...",advance=.false.)
    call pw_epsilon_macro_init(epsilon,struct,symmlist,qmesh,options%omegamax, &
      options%nomega,options%cutoff_polarizability,options%emax_polarizability, &
      options%degauss_polarizability,options%energy_shift,options%lorentzian_broadening, &
         options%imaginary_axis,0,ptk_comm_world)
    call tools_log(" done")
    call tools_log("Calculating optical absorption spectrum...",advance=.false.)
    call pw_epsilon_macro_calc(epsilon,bse,states,atoms)
    call tools_log(" done")
    call pw_bse_destroy(bse)
    call tools_log("Writing macroscopic dielectric tensor on file epsilon.RPAEXC...",advance=.false.)
    if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"epsilon.RPAEXC")
    call tools_log(" done")
    call pw_epsilon_macro_destroy(epsilon)
  endif
  call tools_log("Initializing macroscopic dielectric tensor...",advance=.false.)
  call pw_epsilon_macro_init(epsilon,struct,symmlist,qmesh,options%omegamax, &
    options%nomega,options%cutoff_polarizability,options%emax_polarizability, &
    options%degauss_polarizability,options%energy_shift,options%lorentzian_broadening, &
       options%imaginary_axis,0,ptk_comm_world)
  call tools_log(" done")
  call tools_log("Reading macroscopic dielectric tensor from file epsilon...",advance=.false.)
  call pw_epsilon_macro_readbcast(epsilon,200002,trim(options%outdir)//"epsilon.RPAEXC", &
       0,ptk_comm_world)
  call tools_log(" done")
  call tools_log("Projecting macroscopic dielectric tensor on : ",advance=.false.)
  write(tools_log_unit(),"(i2,2x,i2,2x)",advance='no') options%q
  call tools_log(" direction")
  call pw_epsilon_macro_project_on_q(epsilon,options%q)
  call tools_log("Writing optical absorption spectrum on file oas.RPAEXC...",advance=.false.)
  if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"oas.RPAEXC", &
                  kindout="imag",fmt=options%output_format)
  call tools_log(" done")
  call tools_log("Writing electron energy loss on file eels.RPAEXC...",advance=.false.)
  if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"eels.RPAEXC", &
                  kindout="eels",fmt=options%output_format)
  call tools_log(" done")
  call tools_log("Writing real part of macroscopic dielectric tensor on file repsilon.RPAEXC...",advance=.false.)
  if(ionode) call pw_epsilon_macro_write(epsilon,200002,trim(options%outdir)//"repsilon.RPAEXC", &
                  kindout="real",fmt=options%output_format)
  call tools_log(" done")

  call pw_epsilon_macro_destroy(epsilon)
case DEFAULT
   ERROR("Wrong calculation_kind")
end select 

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

call tools_log("Program spectra ends")

end subroutine spectra_program
