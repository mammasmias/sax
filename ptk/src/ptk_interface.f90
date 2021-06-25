# 1 "ptk_interface.spp"
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

# 30 "ptk_interface.spp"

module ptk_interface
use ptk_mpi_module
implicit none
private
public :: ptk_write_time
public :: ptk_init
public :: ptk_finalize
public :: ptk_abort
public :: ptk_error_handler
public :: ptk_comm_size
public :: ptk_comm_rank
public :: ptk_barrier
public :: ptk_comm_split

interface ptk_write_time
subroutine ptk_write_time_x(unit)
  implicit none
  integer, intent(in) :: unit
end subroutine ptk_write_time_x
end interface

interface ptk_init
subroutine ptk_init_x(verbosity,log_unit,ierr)
  implicit none
  integer, intent(in),  optional :: verbosity
  integer, intent(in),  optional :: log_unit
  integer, intent(out), optional :: ierr
end subroutine ptk_init_x
end interface

interface ptk_finalize
subroutine ptk_finalize_x(ierr)
  implicit none
  integer, intent(out), optional :: ierr
end subroutine ptk_finalize_x
end interface

interface ptk_abort
subroutine ptk_abort_x(code)
  implicit none
  integer, intent(in), optional :: code
end subroutine ptk_abort_x
end interface

interface ptk_error_handler
subroutine ptk_error_handler_x(code)
  implicit none
  integer, intent(in) :: code
end subroutine ptk_error_handler_x
end interface

interface ptk_comm_rank
subroutine ptk_comm_rank_x(comm,rank,ierr)
  use ptk_base
  implicit none
  type(ptk_comm), intent(in)            :: comm
  integer,        intent(out)           :: rank
  integer,        intent(out), optional :: ierr
end subroutine ptk_comm_rank_x
end interface

interface ptk_comm_size
subroutine ptk_comm_size_x(comm,size,ierr)
  use ptk_base
  implicit none
  type(ptk_comm), intent(in)            :: comm
  integer,        intent(out)           :: size
  integer,        intent(out), optional :: ierr
end subroutine ptk_comm_size_x
end interface

interface ptk_barrier
subroutine ptk_barrier_x(comm,ierr)
  use ptk_base
  implicit none
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_barrier_x
end interface

interface ptk_comm_split
subroutine ptk_comm_split_x(commin,color,key,commout,ierr)
  use ptk_base
  implicit none
  type(ptk_comm), intent(in)            :: commin
  integer,        intent(in)            :: color, key
  type(ptk_comm), intent(out)           :: commout
  integer,        intent(out), optional :: ierr
end subroutine ptk_comm_split_x
end interface

end module ptk_interface
