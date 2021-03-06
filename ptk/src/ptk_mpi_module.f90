# 1 "ptk_mpi_module.spp"
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

# 30 "ptk_mpi_module.spp"

module ptk_mpi_module
implicit none
public

#ifdef __PTK_MPI

include 'mpif.h'
integer, parameter :: ptk_any_source      = MPI_ANY_SOURCE
integer, parameter :: ptk_any_tag         = MPI_ANY_TAG
integer, parameter :: ptk_mpi_comm_world  = MPI_Comm_world
integer, parameter :: ptk_mpi_status_size = MPI_STATUS_SIZE
integer, parameter :: ptk_mpi_max         = MPI_MAX
integer, parameter :: ptk_mpi_min         = MPI_MIN
integer, parameter :: ptk_mpi_sum         = MPI_SUM
integer, parameter :: ptk_mpi_prod        = MPI_PROD
integer, parameter :: ptk_mpi_land        = MPI_LAND
integer, parameter :: ptk_mpi_band        = MPI_BAND
integer, parameter :: ptk_mpi_lor         = MPI_LOR
integer, parameter :: ptk_mpi_bor         = MPI_BOR

#else

integer, parameter :: ptk_any_source      = -1
integer, parameter :: ptk_any_tag         = -1
integer, parameter :: ptk_mpi_comm_world  = -1
integer, parameter :: ptk_mpi_status_size = 0
integer, parameter :: ptk_mpi_max         = 1
integer, parameter :: ptk_mpi_min         = 2
integer, parameter :: ptk_mpi_sum         = 3
integer, parameter :: ptk_mpi_prod        = 4
integer, parameter :: ptk_mpi_land        = 5
integer, parameter :: ptk_mpi_band        = 6
integer, parameter :: ptk_mpi_lor         = 7
integer, parameter :: ptk_mpi_bor         = 8

#endif

end module ptk_mpi_module
