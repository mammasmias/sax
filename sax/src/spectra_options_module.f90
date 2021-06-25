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
module spectra_options_module
! Module defining the options type and the relative methods
use tools_module
!@ END MANUAL

! Input options:
! system_kind --> 0D spherical cutoff on coulomb interaction
!                 3D no cutoff
! calculation_kind --> RPA compute macroscopic epsilon within RPA approx
!                      RPAEXC compute absorption spectra
! start_from --> DFT start with dft states
!                HF start with hf states
!                GW start with gw states

! CONVERSION FROM PWSCF FORMATS
! convert_from_pwscf : if true, the states are first converted from pwscf calculation (default .false.)
! convert_from_pwscf_file : index file obtained from pwscf calculation


! if RPAEXC this parameter has do be coherent with the BSE calculation
! nlfe --> if .true. compute macroscopic epsilon including non local fields effects, 
!                               by now .false. for RPA and .true. for RPAEXC
! in input file writen as non_local_field_effects
! energy_shift : apply an energy shift on energies 
! emax_polarizability : is the energy cutoff for the allowed transitions
!
! cutoff_polarizability : is the cutoff for the dipole elements of epsilon
! cutoff_fock : for RPA and local_fields_effects, is the cutoff on units
! of states_cutoff for the calculation of the two particle exchange kernel
  
! nbandmin : min index of band in states
! nbandmax : max index of band in states

! q(3) : is a vector showing the direction on which the transition vector q tends to 0

! output_format : format for output "iotk" xml or "txt" readable by any plot programm

! coulomb_div_treatment : gigi-baldereschi, vcut_spherical, vcut_ws
! ecutvcut : if coulomb_div_treatment is vcut_ws, ecutvcut gives energy
! cutoff for q+G to be included in the special treatment vcut_ws

implicit none

private
type sax_options
  integer :: nbandmin,nbandmax
  real    :: smearing
  character(30) :: smearing_kind
  real    :: cutoff_polarizability
  real :: cutoff_fock
  character(256) :: outdir

  character(10) :: system_kind
  character(10) :: calculation_kind
  character(10) :: start_from
  character(10) :: lfe_kind

  logical :: lfe

  logical :: just_project_on_q_direction

  real :: energy_shift
  real :: emax_polarizability
  real :: degauss_polarizability
  integer :: q(3)
  real :: lorentzian_broadening

  real :: omegamax
  integer :: nomega
  logical :: imaginary_axis 

  real :: bse_emin,bse_emax
  integer :: bse_spin

  logical      :: convert_from_pwscf
  character(256) :: convert_from_pwscf_file

  real :: nelec

  character(10) :: output_format

  character(30) :: coulomb_div_treatment
  real :: ecutvcut

end type sax_options

public :: sax_options, &
          spectra_options_read, &
          spectra_options_write, &
          spectra_options_readbcast, &
          spectra_options_bcast

contains

subroutine spectra_options_read(options,unit,fmt,name)
  use iotk_module
  type (sax_options), intent(out) :: options
  integer,           intent(in)  :: unit
  character(len=*),  intent(in)  :: fmt,name

  character(iotk_attlenx) :: attr
  integer :: scratch_unit
  logical :: found

  select case(fmt)
  case("iotk")
    call iotk_scan_empty(unit,trim(name),attr=attr)
    call iotk_scan_attr (attr,"nbandmin",options%nbandmin)
    call iotk_scan_attr (attr,"nbandmax",options%nbandmax)
    call iotk_scan_attr (attr,"cutoff_polarizability",options%cutoff_polarizability,default=6.0)
    call iotk_scan_attr (attr,"outdir",options%outdir,default="./")

    call iotk_scan_attr(attr,"system_kind",options%system_kind,default="3D")
    if(options%calculation_kind.ne."0D") then
      call iotk_scan_attr(attr,"coulomb_div_treatment",options%coulomb_div_treatment, &
           default="gigi-baldereschi")
      if(options%coulomb_div_treatment=="vcut_ws") then
        call iotk_scan_attr(attr,"ecutvcut",options%ecutvcut)
      endif
    endif

    call iotk_scan_attr(attr,"calculation_kind",options%calculation_kind, &
    default="RPA")
    call iotk_scan_attr(attr,"energy_shift",options%energy_shift,default=0.0)
    call iotk_scan_attr(attr,"start_from",options%start_from,default="DFT")
    call iotk_scan_attr(attr,"convert_from_pwscf",options%convert_from_pwscf,default=.false.)
    if(options%convert_from_pwscf) then
      call iotk_scan_attr(attr,"convert_from_pwscf_file",options%convert_from_pwscf_file)
      call iotk_scan_attr(attr,"nelec",options%nelec)
      call iotk_scan_attr(attr,"smearing_kind",options%smearing_kind,default="none")
      call iotk_scan_attr(attr,"smearing",options%smearing,default=0.0)
    endif
    call iotk_scan_attr(attr,"emax_polarizability",options%emax_polarizability,default=1000.0)
    call iotk_scan_attr(attr,"degauss_polarizability",options%degauss_polarizability,default=0.0001)

    call iotk_scan_attr(attr,"just_project_on_q_direction", options%just_project_on_q_direction,default=.false.)
    call iotk_scan_attr(attr,"q",options%q,default=(/1 ,0 ,0 /))

    call iotk_scan_attr(attr,"omegamax",options%omegamax,default=0.0)
    call iotk_scan_attr(attr,"nomega",options%nomega,default=0)
    if(options%calculation_kind=="RPAEXC") then
      call iotk_scan_attr(attr,"bse_emax",options%bse_emax,default=1000.0)
      call iotk_scan_attr(attr,"bse_emin",options%bse_emin,default=0.0)
      call iotk_scan_attr(attr,"bse_spin",options%bse_emin,default=0.0)
    else
      call iotk_scan_attr(attr,"cutoff_fock",options%cutoff_fock,default=1.0)
      call iotk_scan_attr(attr,"local_fields_effects",options%lfe,default=.false.)
      call iotk_scan_attr(attr,"local_fields_effects_kind",options%lfe_kind,default='invert')
    endif

    call iotk_scan_attr(attr,"lorentzian_broadening",options%lorentzian_broadening,default=0.01)
    call iotk_scan_attr(attr,"imaginary_axis", options%imaginary_axis,default=.false.)

    call iotk_scan_attr(attr,"output_format",options%output_format,default="txt")
    
  case default
    ERROR("")
  end select
end subroutine spectra_options_read

subroutine spectra_options_write(options,unit,fmt)
  type (sax_options), intent(in)   :: options
  integer,           intent(in)  :: unit
  character(len=*),  intent(in)  :: fmt

  select case(fmt)
  case default
    ERROR("")
  end select
end subroutine spectra_options_write

subroutine spectra_options_bcast(options,root,comm)
  use ptk_module, only : ptk_bcast,ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,            intent(in)    :: root
  type (ptk_comm),    intent(in)    :: comm

  call ptk_bcast(options%nbandmin,root,comm)
  call ptk_bcast(options%nbandmax,root,comm)
  call ptk_bcast(options%cutoff_polarizability,root,comm)
  call ptk_bcast(options%system_kind,root,comm)
  if(options%system_kind.ne."0D") then
    call ptk_bcast(options%coulomb_div_treatment,root,comm)
    if(options%coulomb_div_treatment=="vcut_ws") then
      call ptk_bcast(options%ecutvcut,root,comm)
    endif
  endif
  call ptk_bcast(options%calculation_kind,root,comm)
  call ptk_bcast(options%start_from,root,comm)
  call ptk_bcast(options%outdir,root,comm)
  call ptk_bcast(options%convert_from_pwscf,root,comm)
  if(options%convert_from_pwscf) then
    call ptk_bcast(options%convert_from_pwscf_file,root,comm)
    call ptk_bcast(options%nelec,root,comm)
    call ptk_bcast(options%smearing_kind,root,comm)
    call ptk_bcast(options%smearing,root,comm)
  endif
  call ptk_bcast(options%energy_shift,root,comm)
  call ptk_bcast(options%emax_polarizability,root,comm)
  call ptk_bcast(options%degauss_polarizability,root,comm)
  call ptk_bcast(options%just_project_on_q_direction,root,comm)
  call ptk_bcast(options%q,root,comm)
  call ptk_bcast(options%omegamax,root,comm)
  call ptk_bcast(options%nomega,root,comm)
  if(options%calculation_kind=="RPAEXC") then
    call ptk_bcast(options%bse_emax,root,comm)
    call ptk_bcast(options%bse_emin,root,comm)
    call ptk_bcast(options%bse_spin,root,comm)
  else
    call ptk_bcast(options%cutoff_fock,root,comm)
    call ptk_bcast(options%lfe,root,comm)
    call ptk_bcast(options%lfe_kind,root,comm)
  endif
  call ptk_bcast(options%lorentzian_broadening,root,comm)
  call ptk_bcast(options%imaginary_axis,root,comm)
  call ptk_bcast(options%output_format,root,comm)
end subroutine spectra_options_bcast

subroutine spectra_options_readbcast(options,unit,root,comm,fmt,name)
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,           intent(in)    :: root,unit
  type (ptk_comm),   intent(in)    :: comm
  character(len=*),  intent(in)    :: fmt,name
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(rank==root) call spectra_options_read(options,unit,fmt,name)
  call spectra_options_bcast(options,root,comm)
  
end subroutine spectra_options_readbcast

end module spectra_options_module

