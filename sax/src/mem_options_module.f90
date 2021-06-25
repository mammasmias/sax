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
!@ MANUAL
module mem_options_module
! Module defining the options type and the relative methods
use tools_module
!@ END MANUAL

! Input options:
! calculation_kind -> 
!	HF: sigma hf
!       GW:  sigma GW
! system_kind ->
!       0D: 0D system spherical cutoff on coulomb interaction
!       1D: 1D system not implemented
!       2D: 2D systen not implemented
!       3D: 3D system 

! start_from ->
!       DFT: the operators are calculated on the basis of a previous DFT
!       calculation
!       HF: the operators are calculated on the basis of a previous HF
!       calculation
!       GW: the operators are calculated on the basis of a previous GW calculation 

! nbandmin,nbandmax : extrema for band index. nbandmax should be less than the number of
!                     bands calculated in the dft calculation.
!                     has to be a multiple of the number of procs.

! elec: number of electrons in the system.!!!! is a real number!

! CONVERSION FROM PWSCF FORMATS
! convert_from_pwscf : if true, the states are first converted from pwscf calculation (default .false.)
! convert_from_pwscf_file : index file obtained from pwscf calculation

! THE CUTOFFS
! cutoff_density : cutoff for the density, in unit of the cutoff in the wavefunctions (default 4.0)
! cutoff_vxc     : cutoff for the grid to be used in vxc potential, same units as above (default 4.0)
! cutoff_vloc    : cutoff for the local part of the pseudo, same units as above (default 4.0)

! cutoff_fock    : cutoff for the bare Coulomb potential, in Ry (default 6.0)
! cutoff_polarizability : cutoff for the polarizability and screened potential, in Ry

! CALCULATION OPTIONS
! Polarizability ->
! calc_polarizability : if .true., the polarizability is calculated
! emax_polarizability : energy scale for the polarizability imaginary grid
! degauss_polarizability : not yet in use
! polar_broadening : not yet in use

! Screened potential W ->
! calc_w : if .true., the screened potential is calculated

! Frequency dependence
! nomega : number of energies in the calculation of P and W
!          -> number of energies for the energy integeration
! omegamax : max value of energy for integration, in Ry 

! velocity_operator : not yet in use
! velocity_operator_pairs : not yet in use

! Self-energy
! calc_sigma_x : if .true. the exchange part of the self-energy operator projected on states is calculated
! calc_sigma_c : if .true. the correlation part of the self_energy operator
! projected on states is calculated
! sigma_nbmin, sigma_nbmax : band extrema for sigma projection
! diagonal : if .false. the full sigma is computed or the full <psi/H/psi> or
! full <psi/H_sp/psi> or <psi/H_hf/psi>

! sigma_first_order : if .true. compute derivative of sigma for first order correction to the QP energies
! just implemented for plasmon_pole model

! QP energies 
! calc_energies : if .true. the QP energies and eigenvectors using previous
! sigma calculation. If combined with sigma_diagonal=.false. --> diagonalizatio
! of H matrix. Computed within [sigma_nbmin,sigma_nbmax] interval.

! QP states
! calc_QP_states : if .true. calculation of QP_states. Depending of options
!                  calculation_kind -> HF_states or GW_states

! gw_integration_method -->
!    plasmon_pole : gw calculated in the plasmon pole approximation
!    Farid : gw calculated in the approximation proposed by Farid PRB 38 7530
!    (1988). This approach is still under investigation. By now the self-energy
!    is calculated at the fermi energy.
!    cohsex : static approximation to the screening

! calc_plasmon_pole_parameters : if .true. compute plasmon pole parameters
! plasmon_energy : (Ry) parameter needed in a plasmon pole calculation. Corresponds
! to the collective excitation energy
! lorentzian_broadening: (Ry) parameter needed in a plasmon pole calculation.
! Used for numerical stability mainly. This parameter is equivalent to the
! broadening used in a GW calculation for real frequencies.
! 
! guessed_nb_proc : number of processors guessed for the calculation, on which 
! the memory will be estimated.
!
! nb_memory_test_nproc : number of number of test processors for the
! calculation of memory
! 
! memory_output_file : name for memory statistics output file

implicit none

private
type sax_options
  integer :: nbandmin,nbandmax
  real    :: nelec
  logical      :: convert_from_pwscf
  character(256) :: convert_from_pwscf_file
  real    :: cutoff_density
  real    :: cutoff_vxc
  real    :: cutoff_vloc
  real    :: cutoff_fock
  logical :: calc_polarizability
  real    :: cutoff_polarizability
  real    :: emax_polarizability
  real    :: degauss_polarizability
  real    :: polar_broadening
  logical :: calc_w
  logical :: velocity_operator
  character(50) :: velocity_operator_pairs
  integer :: nomega
  real    :: omegamax
  logical :: calc_sigma_x
  logical :: calc_sigma_c
  logical :: calc_energies
  logical :: diagonal
  logical :: sigma_first_order
  logical :: calc_expectation_values
  logical :: calc_sp_hmatrix 
  logical :: calc_QP_states

  character(10) :: system_kind
  character(10) :: calculation_kind
  character(10) :: start_from
  character(15) :: gw_integration_method

  real    :: sigma_tail,sigma_emin,sigma_emax,sigma_deltae,sigma_deltaefine,sigma_broadening
  integer :: sigma_nbmin,sigma_nbmax
  real :: plasmon_energy, lorentzian_broadening
  logical :: calc_plasmon_pole_parameters

  real :: energy_shift
  real    :: bse_emax, bse_emin
  integer :: bse_spin
  real    :: smearing
  character(30) :: smearing_kind

  integer :: guessed_nb_proc, nb_memory_test_nproc
  character(256) :: memory_output_file

end type sax_options

public :: sax_options, &
          mem_options_read, &
          mem_options_readbcast, &
          mem_options_bcast

contains

subroutine mem_options_read(options,unit,fmt,name)
  use iotk_module
  type (sax_options), intent(out) :: options
  integer,           intent(in)  :: unit
  character(len=*),  intent(in)  :: fmt,name

  character(iotk_attlenx) :: attr

  select case(fmt)
  case("iotk")
    call iotk_scan_empty(unit,trim(name),attr=attr)
    call iotk_scan_attr (attr,"nbandmin",options%nbandmin)
    call iotk_scan_attr (attr,"nbandmax",options%nbandmax)

    call iotk_scan_attr (attr,"convert_from_pwscf",options%convert_from_pwscf)
    if (options%convert_from_pwscf) then
      call iotk_scan_attr (attr,"convert_from_pwscf_file",options%convert_from_pwscf_file)
      call iotk_scan_attr (attr,"nelec",options%nelec)
    endif
    call iotk_scan_attr (attr,"cutoff_density",options%cutoff_density,default=4.0)
    call iotk_scan_attr (attr,"cutoff_vloc",options%cutoff_vloc,default=4.0)
    call iotk_scan_attr (attr,"cutoff_vxc",options%cutoff_vxc,default=4.0)
    call iotk_scan_attr (attr,"cutoff_fock",options%cutoff_fock,default=6.0)
    call iotk_scan_attr (attr,"velocity_operator",options%velocity_operator,default=.false.)
    call iotk_scan_attr (attr,"smearing",options%smearing, default=0.0)
    call iotk_scan_attr (attr,"smearing_kind",options%smearing_kind, default="gauss" )


    call iotk_scan_attr(attr,"system_kind",options%system_kind,default="3D")
    call iotk_scan_attr(attr,"calculation_kind",options%calculation_kind)
    call iotk_scan_attr(attr,"start_from",options%start_from,default="DFT")
    if(options%calculation_kind=="GW") then
       call iotk_scan_attr(attr,"gw_integration_method",options%gw_integration_method,default="plasmon_pole")
       if(options%gw_integration_method=="plasmon_pole") then 
          call iotk_scan_attr(attr,"plasmon_energy",options%plasmon_energy,default=1.3)
          call iotk_scan_attr(attr,"lorentzian_broadening",options%lorentzian_broadening,default=0.01)
          call iotk_scan_attr(attr,"calc_plasmon_pole_parameters",options%calc_plasmon_pole_parameters,default=.false.)
! for plasmon pole only two frequencies are needed 0 and plasmon frequency          
          options%nomega=1
          options%omegamax=options%plasmon_energy
          call iotk_scan_attr(attr,"sigma_first_order",options%sigma_first_order,default=.false.)

       elseif(options%gw_integration_method=="Farid") then 
          call iotk_scan_attr (attr,"nomega",options%nomega,default=0)
          call iotk_scan_attr (attr,"omegamax",options%omegamax,default=0.0)
       elseif(options%gw_integration_method=="cohsex") then
          options%nomega=0
          options%omegamax=0.0
       endif
       call iotk_scan_attr (attr,"cutoff_polarizability",options%cutoff_polarizability,default=6.0)
       call iotk_scan_attr (attr,"emax_polarizability",options%emax_polarizability,default=1000.0)
       call iotk_scan_attr (attr,"degauss_polarizability",options%degauss_polarizability,default=0.0001)
       call iotk_scan_attr (attr,"polar_broadening",options%polar_broadening,default=0.0001)
       call iotk_scan_attr (attr,"calc_polarizability",options%calc_polarizability,default=.false.)
       call iotk_scan_attr (attr,"calc_w",options%calc_w,default=.false.)
       call iotk_scan_attr (attr,"calc_sigma_x",options%calc_sigma_x,default=.false.)
       call iotk_scan_attr (attr,"calc_sigma_c",options%calc_sigma_c,default=.false.)
       call iotk_scan_attr (attr,"calc_energies",options%calc_energies,default=.false.)
    endif        

!    options%read_sigma = .false.
!

    call iotk_scan_attr (attr,"sigma_tail",options%sigma_tail,default=-10.0)
    call iotk_scan_attr (attr,"sigma_emin",options%sigma_emin,default=-5.0)
    call iotk_scan_attr (attr,"sigma_emax",options%sigma_emax,default=0.0)
    call iotk_scan_attr (attr,"sigma_deltae",options%sigma_deltae,default=0.1)
    call iotk_scan_attr (attr,"sigma_deltaefine",options%sigma_deltaefine,default=0.01)
    call iotk_scan_attr (attr,"sigma_broadening",options%sigma_broadening,default=0.1)
    call iotk_scan_attr (attr,"sigma_nbmin",options%sigma_nbmin,default=1)
    call iotk_scan_attr (attr,"sigma_nbmax",options%sigma_nbmax,default=1)
    call iotk_scan_attr (attr,"diagonal",options%diagonal,default=.true.)
    call iotk_scan_attr (attr,"calc_expectation_values",options%calc_expectation_values,default=.false.)
    call iotk_scan_attr (attr,"calc_sp_hmatrix",options%calc_sp_hmatrix,default=.false.)
    call iotk_scan_attr (attr,"calc_QP_states",options%calc_QP_states,default=.false.)

    if(options%calculation_kind=="FULL".or.options%calculation_kind=="MACRO") then
     call iotk_scan_attr (attr,"energy_shift",options%energy_shift,default=0.0)
     call iotk_scan_attr (attr,"bse_emin",options%bse_emin,default=0.0)
     call iotk_scan_attr (attr,"bse_emax",options%bse_emax,default=1000.0)
     call iotk_scan_attr (attr, "bse_spin",options%bse_spin,default=0)
     if(options%calculation_kind=="FULL") then
       call iotk_scan_attr (attr,"cutoff_polarizability",options%cutoff_polarizability,default=6.0)
       options%nomega=0
     endif
    end if
  
  call iotk_scan_attr (attr,"guessed_nb_proc",options%guessed_nb_proc,default=1)
  if(options%calculation_kind=="GW") then
     call iotk_scan_attr (attr,"nb_memory_test_nproc",options%nb_memory_test_nproc,& 
     default=4)
  end if
  call iotk_scan_attr (attr,"memory_output_file",options%memory_output_file,default="memory.out")
  
  case default
    ERROR("")
  end select
end subroutine mem_options_read

subroutine mem_options_bcast(options,root,comm)
  use ptk_module, only : ptk_bcast,ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,           intent(in)    :: root
  type (ptk_comm),   intent(in)    :: comm

  integer :: rank
  call ptk_bcast(options%nbandmin,root,comm)
  call ptk_bcast(options%nbandmax,root,comm)
  call ptk_bcast(options%nelec,root,comm)
  call ptk_bcast(options%smearing,root,comm)
  call ptk_bcast(options%smearing_kind,root,comm)
  call ptk_bcast(options%convert_from_pwscf,root,comm)
  call ptk_bcast(options%convert_from_pwscf_file,root,comm)
  call ptk_bcast(options%cutoff_density,root,comm)
  call ptk_bcast(options%cutoff_vloc,root,comm)
  call ptk_bcast(options%cutoff_vxc,root,comm)
  call ptk_bcast(options%cutoff_fock,root,comm)
  call ptk_bcast(options%system_kind,root,comm)
  call ptk_bcast(options%calculation_kind,root,comm)
  call ptk_bcast(options%start_from,root,comm)
  call ptk_bcast(options%velocity_operator,root,comm)
  call ptk_bcast(options%velocity_operator_pairs,root,comm)
  call ptk_bcast(options%calc_energies,root,comm)
  call ptk_bcast(options%sigma_tail,root,comm)
  call ptk_bcast(options%sigma_emin,root,comm)
  call ptk_bcast(options%sigma_emax,root,comm)
  call ptk_bcast(options%sigma_deltae,root,comm)
  call ptk_bcast(options%sigma_deltaefine,root,comm)
  call ptk_bcast(options%sigma_broadening,root,comm)
  call ptk_bcast(options%sigma_nbmin,root,comm)
  call ptk_bcast(options%sigma_nbmax,root,comm)
  call ptk_bcast(options%diagonal,root,comm)
  call ptk_comm_rank(comm,rank)
  call ptk_bcast(options%calc_expectation_values,root,comm)
  call ptk_bcast(options%calc_QP_states,root,comm)
  call ptk_bcast(options%calc_sp_hmatrix,root,comm)

  if(options%calculation_kind=="GW") then
     call ptk_bcast(options%gw_integration_method,root,comm)
     if(options%gw_integration_method=="plasmon_pole") then 
        call ptk_bcast(options%plasmon_energy,root,comm)
        call ptk_bcast(options%lorentzian_broadening,root,comm)
        call ptk_bcast(options%calc_plasmon_pole_parameters,root,comm)
        call ptk_bcast(options%sigma_first_order,root,comm)
     endif
     call ptk_bcast(options%calc_polarizability,root,comm)
     call ptk_bcast(options%cutoff_polarizability,root,comm)
     call ptk_bcast(options%emax_polarizability,root,comm)
     call ptk_bcast(options%degauss_polarizability,root,comm)
     call ptk_bcast(options%polar_broadening,root,comm)
     call ptk_bcast(options%calc_w,root,comm)
     call ptk_bcast(options%calc_sigma_x,root,comm)
     call ptk_bcast(options%calc_sigma_c,root,comm)
     call ptk_bcast(options%nomega,root,comm)
     call ptk_bcast(options%omegamax,root,comm)
     call ptk_bcast(options%nb_memory_test_nproc,root,comm)
  endif        
  
  if(options%calculation_kind=="FULL".or.options%calculation_kind=="MACRO") then 
    call ptk_bcast(options%energy_shift,root,comm)
    call ptk_bcast(options%bse_emin,root,comm)
    call ptk_bcast(options%bse_emax,root,comm)
    call ptk_bcast(options%bse_spin,root,comm)
    if(options%calculation_kind=="FULL") then
      call ptk_bcast(options%cutoff_polarizability,root,comm)
    end if
  end if
  
  call ptk_bcast(options%guessed_nb_proc,root,comm)
  call ptk_bcast(options%memory_output_file,root,comm)
end subroutine mem_options_bcast

subroutine mem_options_readbcast(options,unit,root,comm,fmt,name)
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,           intent(in)    :: root,unit
  type (ptk_comm),   intent(in)    :: comm
  character(len=*),  intent(in)    :: fmt,name
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(rank==root) call mem_options_read(options,unit,fmt,name)
  call mem_options_bcast(options,root,comm)
  
end subroutine mem_options_readbcast

end module mem_options_module

