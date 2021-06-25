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
module bse_options_module
! Module defining the options type and the relative methods
use tools_module
!@ END MANUAL

! Input options:
! calculation_kind ->
!       MACRO: screened coulomb interation approximated by an epsilon macroscopic 
!              obtained in a previous calculation
!       FULL: read W from a previous GW calculation
! system_kind ->
!       0D: 0D Martyna-Tuckerman
!       1D: 1D system not implemented
!       2D: 2D systen not implemented
!       3D: 3D system

! start_from ->
!       DFT: the is solved on the basis of a previous DFT
!       calculation
!       HF: the BSE is solved on the basis of a previous HF calculation 
!       calculation
!       GW: the BSE is solved on the basis of a previous GW calculation

! nbandmin,nbandmax : extrema for band index. nbandmax should be less than the number of
!                     bands calculated in the previous calculation (DFT, HF or
!                     GW states).

! cutoff_polarizability : cutoff for the screened Coulomb potential, in Ry (default 6-0 Ry)

! cutoff_fock : cutoff for bare coulomb potential, in Ry (default 6.0 Ry)
!                recommended to use at least wfc cutoff

! BSE

! Spin
! bse_spin: if 0 -> singlet
!           if 1 -> triplet

! energy_shift: shift on energies (scissor shift) for a MACRO calculation

! bse_emin, bse_emax: energy window for the solution of BSE

! convert_from_pwscf : if .true. the code read file_name.export and create the object states
! convert_from_pwscf_file : the path for file_name.export (path/file_name.export)
! nelec : the number of electrons, needed just for the construction of states (states%occupation)

! coulomb_div_treatment : gigi-baldereschi, vcut_spherical, vcut_ws
! ecutvcut : if coulomb_div_treatment is vcut_ws, ecutvcut gives energy
! cutoff for q+G to be included in the special treatment vcut_ws
! for 0D martyna-tuckerman is applied -> coulomb_div_treatment=vct_ws and ecuvcut=2.0

implicit none

private
type sax_options
  integer :: nbandmin,nbandmax
  real    :: cutoff_polarizability
  real    :: cutoff_fock

  character(10) :: system_kind
  character(10) :: calculation_kind
  character(10) :: start_from
  character(256) :: outdir

  real :: energy_shift

  real    :: bse_emax, bse_emin
  integer :: bse_spin
  logical :: calc_oscillators
  logical :: calc_bse
  integer :: nstatemax, nstatemin

  logical :: convert_from_pwscf
  character(256) :: convert_from_pwscf_file
  real :: nelec
  real               :: smearing
  character(30)      :: smearing_kind

  character(30) :: coulomb_div_treatment
  real :: ecutvcut

end type sax_options

public :: sax_options, &
          bse_options_read, &
          bse_options_write, &
          bse_options_readbcast, &
          bse_options_bcast

contains

subroutine bse_options_read(options,unit,fmt,name)
  use iotk_module
  type (sax_options), intent(out) :: options
  integer,            intent(in)  :: unit
  character(len=*),   intent(in)  :: fmt,name

  character(iotk_attlenx) :: attr
  integer :: scratch_unit
  logical :: found

  select case(fmt)
  case("iotk")
    call iotk_scan_empty(unit,trim(name),attr=attr)
    call iotk_scan_attr (attr,"nbandmin",options%nbandmin)
    call iotk_scan_attr (attr,"nbandmax",options%nbandmax)

    call iotk_scan_attr(attr,"system_kind",options%system_kind,default="3D")
    if(options%system_kind.ne."0D") then
      call iotk_scan_attr(attr,"coulomb_div_treatment",options%coulomb_div_treatment, &
           default="gigi-baldereschi")
      if(options%coulomb_div_treatment=="vcut_ws") then
        call iotk_scan_attr(attr,"ecutvcut",options%ecutvcut)
      endif
    else
        call iotk_scan_attr(attr,"ecutvcut",options%ecutvcut,default=2.0)
        options%coulomb_div_treatment="vcut_ws"
    endif

    call iotk_scan_attr(attr,"calculation_kind",options%calculation_kind)
    call iotk_scan_attr(attr,"start_from",options%start_from,default="DFT")
    call iotk_scan_attr(attr,"outdir",options%outdir,default="./")
    call iotk_scan_attr(attr,"calc_oscillators",options%calc_oscillators,default=.true.)
    call iotk_scan_attr(attr,"calc_bse",options%calc_bse,default=.true.)
    call iotk_scan_attr(attr,"nstatemin",options%nstatemin,default=1)
    call iotk_scan_attr(attr,"nstatemax",options%nstatemax,default=10000)
   
    call iotk_scan_attr (attr,"cutoff_fock", options%cutoff_fock,default=6.0)
    if(options%calculation_kind=="FULL") then
      call iotk_scan_attr (attr,"cutoff_polarizability",options%cutoff_polarizability,default=6.0)
    elseif(options%calculation_kind=="MACRO") then
      call iotk_scan_attr (attr,"cutoff_polarizability",options%cutoff_polarizability,default=options%cutoff_fock)
    endif
    
    call iotk_scan_attr (attr,"bse_emin",options%bse_emin,default=0.0)
    call iotk_scan_attr (attr,"bse_emax",options%bse_emax,default=1000.0)
    call iotk_scan_attr (attr, "bse_spin",options%bse_spin,default=0)
    
    call iotk_scan_attr(attr,"energy_shift",options%energy_shift,default=0.0)
 
    call iotk_scan_attr(attr,"convert_from_pwscf",options%convert_from_pwscf,default=.false.)
    if(options%convert_from_pwscf) then
      call iotk_scan_attr(attr,"convert_from_pwscf_file",options%convert_from_pwscf_file)
      call iotk_scan_attr(attr,"nelec",options%nelec)
      call iotk_scan_attr (attr,"smearing",options%smearing, default=0.0)
      call iotk_scan_attr (attr,"smearing_kind",options%smearing_kind, default="none" )
    endif
    
  case default
    ERROR("")
  end select
end subroutine bse_options_read

subroutine bse_options_write(options,unit,fmt)
  type (sax_options), intent(in)   :: options
  integer,           intent(in)  :: unit
  character(len=*),  intent(in)  :: fmt

  select case(fmt)
  case default
    ERROR("")
  end select
end subroutine bse_options_write

subroutine bse_options_bcast(options,root,comm)
  use ptk_module, only : ptk_bcast,ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,            intent(in)    :: root
  type (ptk_comm),    intent(in)    :: comm

  call ptk_bcast(options%nbandmin,root,comm)
  call ptk_bcast(options%nbandmax,root,comm)
  call ptk_bcast(options%nstatemin,root,comm)
  call ptk_bcast(options%nstatemax,root,comm)
  call ptk_bcast(options%calc_bse,root,comm)
  call ptk_bcast(options%calc_oscillators,root,comm)
  call ptk_bcast(options%cutoff_polarizability,root,comm)
  call ptk_bcast(options%cutoff_fock,root,comm)
  call ptk_bcast(options%system_kind,root,comm)
  call ptk_bcast(options%coulomb_div_treatment,root,comm)
  if(options%coulomb_div_treatment=="vcut_ws") then
    call ptk_bcast(options%ecutvcut,root,comm)
  endif
  call ptk_bcast(options%outdir,root,comm)
  call ptk_bcast(options%calculation_kind,root,comm)
  call ptk_bcast(options%start_from,root,comm)
  call ptk_bcast(options%energy_shift,root,comm)
  call ptk_bcast(options%bse_emin,root,comm)
  call ptk_bcast(options%bse_emax,root,comm)
  call ptk_bcast(options%bse_spin,root,comm)
  call ptk_bcast(options%smearing,root,comm)
  call ptk_bcast(options%smearing_kind,root,comm)

  call ptk_bcast(options%convert_from_pwscf,root,comm)
  if(options%convert_from_pwscf) then
    call ptk_bcast(options%convert_from_pwscf_file,root,comm)
    call ptk_bcast(options%nelec,root,comm)
  endif
end subroutine bse_options_bcast

subroutine bse_options_readbcast(options,unit,root,comm,fmt,name)
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,           intent(in)    :: root,unit
  type (ptk_comm),   intent(in)    :: comm
  character(len=*),  intent(in)    :: fmt,name
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(rank==root) call bse_options_read(options,unit,fmt,name)
  call bse_options_bcast(options,root,comm)
  
end subroutine bse_options_readbcast

end module bse_options_module

