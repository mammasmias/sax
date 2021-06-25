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
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
! USA.

#include "tools_error.h"
!----------------------------------------------------------------------------
MODULE mp_global
  !----------------------------------------------------------------------------
  !
! This module is inspired from mp_global.f90 of the quantum-espresso
! distribution Copyright (C) 2001-2004 PWSCF group.
!
use ptk_module, only : ptk_comm, ptk_comm_world, ptk_comm_split

implicit none
!
private
!
save
!
integer :: npool=1
type(ptk_comm) :: intra_pool_comm=ptk_comm_world
type(ptk_comm) :: inter_pool_comm=ptk_comm_world

public :: init_pool
public :: intra_pool_comm, inter_pool_comm
public :: npool

contains

!----------------------------------------------------------------------------
SUBROUTINE init_pool( npool_ , comm )
  !----------------------------------------------------------------------------
  !
  ! ... This routine initialize the pool :  MPI division in pools and images
  !

  use ptk_module, only : ptk_comm_size, ptk_comm_rank, ptk_comm, &
                         ptk_comm_split, ptk_barrier
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: npool_
  type(ptk_comm), intent(in) :: comm
  !
  INTEGER :: ierr = 0
  integer :: npe, rank
  integer :: my_pool_id, me_pool, nproc_pool
  !
  !
  call ptk_comm_size(comm,npe)
  call ptk_comm_rank(comm,rank)

  npool=npool_

  intra_pool_comm = comm
  inter_pool_comm = comm
  
  !
  ! ... number of cpus per pool of k-points (they are created inside each image)
  !

!  write(0,*) "npe", npe
!  write(0,*) "npool", npool
  nproc_pool = npe / npool
  IF ( MOD( npe, npool ) /= 0 ) &
     ERROR( "Number of proc not divisible by npool" )  
  !
  ! ... my_pool_id  =  pool index for this processor    ( 0 : npool - 1 )
  ! ... me_pool     =  processor index within the pool  ( 0 : nproc_pool - 1 )
  !
  my_pool_id = mod(rank,npool)
  me_pool    = rank/npool
!  write(0,*) "my_pool_id, me_pool ", my_pool_id, me_pool
  !
  call ptk_barrier(comm,ierr)
  !
  ! ... the intra_pool_comm communicator is created
  !
  call ptk_comm_split(comm,my_pool_id,me_pool,intra_pool_comm, ierr)
  !
  IF( ierr /= 0 ) ERROR( "init of intra_pool_comm is wrong" )
  !
  call ptk_barrier(comm,ierr)
  !
  ! ... the inter_pool_comm communicator is created
  !
  call ptk_comm_split(comm,me_pool,my_pool_id,inter_pool_comm,ierr)
  !
  IF( ierr /= 0 ) ERROR( "init of inter_pool_comm is wrong" )
  !
  RETURN
  !
END SUBROUTINE init_pool
!
     !
END MODULE mp_global
