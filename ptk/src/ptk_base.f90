# 1 "ptk_base.spp"
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

# 30 "ptk_base.spp"

module ptk_base
use ptk_mpi_module
implicit none
public
public :: ptk_verbosity
public :: ptk_log_unit
public :: ptk_comm
public :: ptk_op
public :: ptk_comm_world
public :: ptk_max
public :: ptk_min
public :: ptk_sum
public :: ptk_prod
public :: ptk_land
public :: ptk_band
public :: ptk_lor
public :: ptk_bor
public :: ptk_any_source
public :: ptk_any_tag
public :: ptk_status

integer, save :: ptk_verbosity = 0
integer, save :: ptk_log_unit  = 0

type ptk_comm
  integer :: comm
end type ptk_comm

type ptk_op
  integer :: op
end type ptk_op

type ptk_status
  integer :: status(ptk_mpi_status_size)
end type ptk_status

type (ptk_comm), parameter :: ptk_comm_world = ptk_comm(ptk_mpi_comm_world)

type (ptk_op),   parameter :: ptk_max        = ptk_op(ptk_mpi_max)
type (ptk_op),   parameter :: ptk_min        = ptk_op(ptk_mpi_min)
type (ptk_op),   parameter :: ptk_sum        = ptk_op(ptk_mpi_sum)
type (ptk_op),   parameter :: ptk_prod       = ptk_op(ptk_mpi_prod)
type (ptk_op),   parameter :: ptk_land       = ptk_op(ptk_mpi_land)
type (ptk_op),   parameter :: ptk_band       = ptk_op(ptk_mpi_band)
type (ptk_op),   parameter :: ptk_lor        = ptk_op(ptk_mpi_lor)
type (ptk_op),   parameter :: ptk_bor        = ptk_op(ptk_mpi_bor)

end module ptk_base
