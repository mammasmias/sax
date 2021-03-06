# 1 "ptk_external.spp"
! Parallel Tool Kit (PTK)
! Copyright (C) 2004-2006 Giovanni Bussi
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

!------------------------------------------------------------------------------!
! Inclusion of configuration file
#include "ptk_config.h"
!------------------------------------------------------------------------------!

# 2 "../include/ptk_auxmacros.spp"
#ifndef __PTK_AUXMACROS
#define __PTK_AUXMACROS
! The macros are defined with -D option or inside ptk_config.h
! The default values are set here
! Maximum rank of an array
#ifndef __PTK_MAXRANK
#  define __PTK_MAXRANK 7
#endif

#define __PTK_CHARACTER1 kind("a")
#define __PTK_MPI_CHARACTER1 MPI_CHARACTER

! Some check
#if __PTK_MAXRANK > 7
#  error
#endif
#if __PTK_MAXRANK < 1
#  error
#endif

#endif

# 25 "../include/ptk_auxmacros.spp"

# 30 "ptk_external.spp"

! write_time is an internal subroutine for logging the time onto a unit
subroutine ptk_write_time_x(unit)
  use ptk_base
  use ptk_interface
  implicit none
  integer, intent(in) :: unit
  integer :: rank,ierr,values(8)
  call date_and_time(values=values)
  write(unit,"(a,i2.2,a,i2.2,a,i2.2,a,i3.3,a,i2.2,a,i2.2,a,i4.4)") &
        "Time and date: ", &
        values(5),":",values(6),":",values(7),".",values(8),"   ", &
        values(3),"/",values(2),"/",values(1)
end subroutine ptk_write_time_x

subroutine ptk_init_x(verbosity,log_unit,ierr)
  use ptk_base
  use ptk_interface
  implicit none
  integer, intent(in),  optional :: verbosity
  integer, intent(in),  optional :: log_unit
  integer, intent(out), optional :: ierr
  integer :: ierr_loc,rank,size
  ierr_loc = 0
#ifdef __PTK_MPI
  call MPI_Init(ierr_loc)
#endif
  if(ierr_loc/=0) goto 1
  if(present(verbosity)) ptk_verbosity = verbosity
  if(present(log_unit))  ptk_log_unit  = log_unit
  call ptk_comm_size(ptk_comm_world,size,ierr_loc)
  if(ierr_loc/=0) goto 1
  call ptk_comm_rank(ptk_comm_world,rank,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(ptk_verbosity>0 .and. rank==0) then
#ifdef __PTK_MPI
    write(ptk_log_unit,"(a,i3,a)") "Parallel execution initialized with ",size," processors"
#else
    write(ptk_log_unit,"(a)") "Serial execution initialized"
#endif
    call ptk_write_time(ptk_log_unit)
  end if
  call ptk_barrier(ierr=ierr_loc)
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_init_x

subroutine ptk_finalize_x(ierr)
  use ptk_base
  use ptk_interface
  implicit none
  integer, intent(out), optional :: ierr
  integer :: ierr_loc,rank
  ierr_loc = 0
  call ptk_barrier(ierr=ierr_loc)
  if(ierr_loc/=0) goto 1
  call ptk_comm_rank(ptk_comm_world,rank,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(ptk_verbosity>0 .and. rank==0) then
    call ptk_write_time(ptk_log_unit)
  end if
#ifdef __PTK_MPI
  if(ptk_verbosity>0 .and. rank==0) then
    write(ptk_log_unit,"(a)") "Parallel execution finalized"
  end if
#else
  if(ptk_verbosity>0) then
    write(ptk_log_unit,"(a)") "Serial execution finalized"
  end if
#endif
1 continue
#ifdef __PTK_MPI
  call MPI_Finalize(ierr_loc)
#endif
  if(present(ierr)) ierr=ierr_loc
end subroutine ptk_finalize_x

subroutine ptk_abort_x(code)
  use ptk_base
  use ptk_interface
  implicit none
  integer, intent(in), optional :: code
  integer :: code_loc
  integer :: ierr
  code_loc = 1
  if(present(code)) code_loc = code
#ifdef __PTK_MPI
  call MPI_Abort(ptk_comm_world%comm,code_loc,ierr)
#else
  stop
#endif
end subroutine ptk_abort_x

subroutine ptk_error_handler_x(code)
  use ptk_base
  use ptk_interface
  implicit none
  integer, intent(in) :: code
  write(ptk_log_unit,*) "Error in Parallel ToolKit"
  write(ptk_log_unit,*) "Code:",code
  call ptk_abort(code)
end subroutine ptk_error_handler_x

subroutine ptk_comm_rank_x(comm,rank,ierr)
  use ptk_base
  use ptk_interface
  implicit none
  type(ptk_comm), intent(in)            :: comm
  integer,        intent(out)           :: rank
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  ierr_loc = 0
#ifdef __PTK_MPI
  call MPI_Comm_rank(comm%comm,rank,ierr_loc)
#else
  rank = 0
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_comm_rank_x

subroutine ptk_comm_size_x(comm,size,ierr)
  use ptk_base
  use ptk_interface
  implicit none
  type(ptk_comm), intent(in)            :: comm
  integer,        intent(out)           :: size
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  ierr_loc = 0
#ifdef __PTK_MPI
  call MPI_Comm_size(comm%comm,size,ierr_loc)
#else
  size = 1
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_comm_size_x

subroutine ptk_barrier_x(comm,ierr)
  use ptk_base
  use ptk_interface
  implicit none
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  type(ptk_comm) :: comm_loc
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = ptk_comm_world
  ierr_loc = 0
#ifdef __PTK_MPI
  call MPI_Barrier(comm_loc%comm,ierr_loc)
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_barrier_x

subroutine ptk_comm_split_x(commin,color,key,commout,ierr)
  use ptk_base
  use ptk_interface
  implicit none
  type(ptk_comm), intent(in)            :: commin
  integer,        intent(in)            :: color, key
  type(ptk_comm), intent(out)           :: commout
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  ierr_loc = 0
#ifdef __PTK_MPI
  call MPI_Comm_split(commin%comm,color,key,commout%comm,ierr_loc)
#else
  commout = ptk_comm(ptk_mpi_comm_world)
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_comm_split_x
