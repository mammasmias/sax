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
module pptools_options_module
! Module defining the options type and the relative methods
use tools_module
!@ END MANUAL

! Input options:
! calculation_kind ->
!       SI: compute the self-interaction 
!       PR: compute the participation ratio multiplied by volume
! system_kind ->
!       0D: 0D system spherical cutoff on coulomb interaction
!       1D: 1D system not implemented
!       2D: 2D systen not implemented
!       3D: 3D system

! CONVERSION FROM PWSCF FORMATS
! convert_from_pwscf : if true, the states are first converted from pwscf calculation
! convert_from_pwscf_file : index file obtained from pwscf calculation
! nelec: number of electrons in the system.!!!! is a real number!, needed just if convert_from_pwscf is .true.

! start_from ->
!       DFT: the SI or PR is calculated for previous DFT states
!       HF: the SI or PR is calculated for previous HF states 
!       GW: th Si or PR is calculated for previous GW states

! nbandmin,nbandmax : extrema for band index. nbandmax in states


! cutoff_fock : cutoff for coulomb potential in SI calculation
! cutoff_density : cutoff for density in particpation ratio calculation

! omegamin, omegamax, nomega, degauss: parameters for DOS calculation
! file_out -> output for DoS or band calculation
! interp_kind ->
!         1: Nearest-neighbor first order perturbation
!         2: Trilinear interpolation (default)

! Band calculation only: E=stretch_val*e+shift_val for valence, E=stretch_cond*e+shift_cond for conduction



implicit none

private
type sax_options
  integer :: nbandmin,nbandmax
  real    :: cutoff_fock
  real    :: cutoff_density
  real    :: omegamin,omegamax,degauss
  integer :: nomega
  character(10) :: system_kind
  character(10) :: calculation_kind
  character(10) :: start_from
  character(256) :: outdir

  real    :: nelec
  logical      :: convert_from_pwscf, diagonal
  character(256) :: convert_from_pwscf_file
  character(256) :: file_out
  integer  :: interp_kind
  real     :: shift_cond, shift_val, stretch_cond, stretch_val
end type sax_options

public :: sax_options, &
          pptools_options_read, &
          pptools_options_write, &
          pptools_options_readbcast, &
          pptools_options_bcast

contains

subroutine pptools_options_read(options,unit,fmt,name)
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
    call iotk_scan_attr (attr,"cutoff_fock", options%cutoff_fock,default=6.0)
    call iotk_scan_attr (attr,"cutoff_density", options%cutoff_density,default=4.0)

    call iotk_scan_attr(attr,"system_kind",options%system_kind,default="3D")
    call iotk_scan_attr(attr,"calculation_kind",options%calculation_kind)
    call iotk_scan_attr(attr,"start_from",options%start_from)
    call iotk_scan_attr(attr,"outdir",options%outdir,default="./")
   
    call iotk_scan_attr (attr,"convert_from_pwscf",options%convert_from_pwscf)
    if(options%convert_from_pwscf) then
      call iotk_scan_attr (attr,"convert_from_pwscf_file",options%convert_from_pwscf_file)
      call iotk_scan_attr (attr,"nelec",options%nelec)
    endif
    
    if(options%calculation_kind=="DOS") then
      call iotk_scan_attr (attr,"omegamin",options%omegamin)
      call iotk_scan_attr (attr,"omegamax",options%omegamax)
      call iotk_scan_attr (attr,"nomega",options%nomega)
      call iotk_scan_attr (attr,"degauss",options%degauss)
      call iotk_scan_attr (attr,"diagonal", options%diagonal)
    endif
    
    call iotk_scan_attr (attr,"file_out",options%file_out,default="plot.dat")
    if(options%calculation_kind=="BAND") then
      call iotk_scan_attr (attr,"interp_kind",options%interp_kind,default=2)
      call iotk_scan_attr (attr,"stretch_val",options%stretch_val,default=1.0)
      call iotk_scan_attr (attr,"stretch_cond",options%stretch_cond,default=1.0)
      call iotk_scan_attr (attr,"shift_val",options%shift_val,default=0.0)
      call iotk_scan_attr (attr,"shift_cond",options%shift_cond,default=0.0)
    endif
    
  case default
    ERROR("")
  end select
end subroutine pptools_options_read

subroutine pptools_options_write(options,unit,fmt)
  type (sax_options), intent(in)   :: options
  integer,           intent(in)  :: unit
  character(len=*),  intent(in)  :: fmt

  select case(fmt)
  case default
    ERROR("")
  end select
end subroutine pptools_options_write

subroutine pptools_options_bcast(options,root,comm)
  use ptk_module, only : ptk_bcast,ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,            intent(in)    :: root
  type (ptk_comm),    intent(in)    :: comm

  call ptk_bcast(options%nbandmin,root,comm)
  call ptk_bcast(options%nbandmax,root,comm)
  call ptk_bcast(options%cutoff_fock,root,comm)
  call ptk_bcast(options%cutoff_density,root,comm)
  call ptk_bcast(options%system_kind,root,comm)
  call ptk_bcast(options%calculation_kind,root,comm)
  call ptk_bcast(options%start_from,root,comm)
  call ptk_bcast(options%outdir,root,comm)

  call ptk_bcast(options%convert_from_pwscf,root,comm)

  if(options%convert_from_pwscf) then
    call ptk_bcast(options%convert_from_pwscf_file,root,comm)
    call ptk_bcast(options%nelec,root,comm)
  endif

  if(options%calculation_kind=="DOS") then
    call ptk_bcast(options%omegamin,root,comm)
    call ptk_bcast(options%omegamax,root,comm)
    call ptk_bcast(options%nomega,root,comm)
    call ptk_bcast(options%degauss,root,comm)
    call ptk_bcast(options%diagonal,root,comm)
  endif
  call ptk_bcast(options%file_out,root,comm)
  if(options%calculation_kind=="BAND") then
    call ptk_bcast(options%interp_kind,root,comm)
    call ptk_bcast(options%shift_val,root,comm)
    call ptk_bcast(options%shift_cond,root,comm)
    call ptk_bcast(options%stretch_val,root,comm)
    call ptk_bcast(options%stretch_cond,root,comm)
  endif
        
end subroutine pptools_options_bcast

subroutine pptools_options_readbcast(options,unit,root,comm,fmt,name)
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,           intent(in)    :: root,unit
  type (ptk_comm),   intent(in)    :: comm
  character(len=*),  intent(in)    :: fmt,name
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(rank==root) call pptools_options_read(options,unit,fmt,name)
  call pptools_options_bcast(options,root,comm)
  
end subroutine pptools_options_readbcast

end module pptools_options_module

