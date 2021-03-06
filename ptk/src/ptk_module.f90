# 1 "ptk_module.spp"
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

module ptk_module
! This module is simply a wrapper for ptk_base
! which hides the variables that should be private
  use ptk_base
  use ptk_interface
  use ptk_multitype_interf
! All names are private ...
  private
! ... except the names listed below
  public :: ptk_comm_world
  public :: ptk_init
  public :: ptk_finalize
  public :: ptk_comm_rank
  public :: ptk_comm_size
  public :: ptk_barrier
  public :: ptk_comm_split
  public :: ptk_bcast
  public :: ptk_send
  public :: ptk_recv
  public :: ptk_reduce
  public :: ptk_allreduce
  public :: ptk_reduce_inplace
  public :: ptk_allreduce_inplace
  public :: ptk_gather
  public :: ptk_allgather
  public :: ptk_max
  public :: ptk_min
  public :: ptk_sum
  public :: ptk_prod
  public :: ptk_land
  public :: ptk_band
  public :: ptk_lor
  public :: ptk_bor
  public :: ptk_any_tag
  public :: ptk_any_source
  public :: ptk_abort
  public :: ptk_comm
end module ptk_module


