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
module excitons_options_module
! Module defining the options type and the relative methods
use tools_module
!@ END MANUAL

! Input options:
! calculation_kind --> WAVES calculate excitonic wave functionis and write then in xcrysden format
!                      ANALYSIS calculate the 4 most impostant single particle transition 
!                      for given excitons
! start_from --> DFT start with dft states
!                HF start with hf states
!                GW start with gw states

  
! nbandmin : min index of band in states
! nbandmax : max index of band in states

implicit none

private
type sax_options
  integer :: nbandmin,nbandmax
  real    :: bse_emin,bse_emax
  integer :: bse_spin
  character(10) :: start_from
  integer :: supergrid(3)
  real    :: r_hole(3)
  integer :: nstatemin,nstatemax
  character(10) :: calculation_kind 
  character(256) :: outdir 
end type sax_options

public :: sax_options, &
          excitons_options_read, &
          excitons_options_write, &
          excitons_options_readbcast, &
          excitons_options_bcast

contains

subroutine excitons_options_read(options,unit,fmt,name)
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
    call iotk_scan_attr (attr,"calculation_kind",options%calculation_kind,default="ANALYSIS")
    call iotk_scan_attr (attr,"nbandmin",options%nbandmin)
    call iotk_scan_attr (attr,"nbandmax",options%nbandmax)
    call iotk_scan_attr(attr,"start_from",options%start_from,default="GW")
    call iotk_scan_attr(attr,"bse_emax",options%bse_emax,default=1000.0)
    call iotk_scan_attr(attr,"bse_emin",options%bse_emin,default=0.0)
    call iotk_scan_attr(attr,"bse_spin",options%bse_spin,default=0)
    call iotk_scan_attr(attr,"supergrid",options%supergrid,default=(/50,50,50/))
    call iotk_scan_attr(attr,"r_hole",options%r_hole,default=(/0.5,0.5,0.5/))
    call iotk_scan_attr(attr,"nstatemin",options%nstatemin)
    call iotk_scan_attr(attr,"nstatemax",options%nstatemax)
    call iotk_scan_attr (attr,"outdir",options%outdir,default="./")
  case default
    ERROR("")
  end select
end subroutine excitons_options_read

subroutine excitons_options_write(options,unit,fmt)
  type (sax_options), intent(in)   :: options
  integer,           intent(in)  :: unit
  character(len=*),  intent(in)  :: fmt

  select case(fmt)
  case default
    ERROR("")
  end select
end subroutine excitons_options_write

subroutine excitons_options_bcast(options,root,comm)
  use ptk_module, only : ptk_bcast,ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,            intent(in)    :: root
  type (ptk_comm),    intent(in)    :: comm

  call ptk_bcast(options%calculation_kind,root,comm)
  call ptk_bcast(options%nbandmin,root,comm)
  call ptk_bcast(options%nbandmax,root,comm)
  call ptk_bcast(options%bse_emax,root,comm)
  call ptk_bcast(options%bse_emin,root,comm)
  call ptk_bcast(options%bse_spin,root,comm)
  call ptk_bcast(options%start_from,root,comm)
  call ptk_bcast(options%supergrid,root,comm)
  call ptk_bcast(options%r_hole,root,comm)
  call ptk_bcast(options%nstatemin,root,comm)
  call ptk_bcast(options%nstatemax,root,comm)
  call ptk_bcast(options%outdir,root,comm)
end subroutine excitons_options_bcast

subroutine excitons_options_readbcast(options,unit,root,comm,fmt,name)
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type (sax_options), intent(inout) :: options
  integer,           intent(in)    :: root,unit
  type (ptk_comm),   intent(in)    :: comm
  character(len=*),  intent(in)    :: fmt,name
  integer :: rank
  call ptk_comm_rank(comm,rank)
  if(rank==root) call excitons_options_read(options,unit,fmt,name)
  call excitons_options_bcast(options,root,comm)
  
end subroutine excitons_options_readbcast

end module excitons_options_module

