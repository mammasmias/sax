# 1 "ptk_multitype_interf.spp"
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

# 30 "ptk_multitype_interf.spp"

module ptk_multitype_interf
use ptk_base
implicit none
private
public :: ptk_bcast
public :: ptk_send
public :: ptk_recv
public :: ptk_reduce
public :: ptk_allreduce
public :: ptk_reduce_inplace
public :: ptk_allreduce_inplace
public :: ptk_gather
public :: ptk_allgather

interface ptk_bcast
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL1_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL1_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL1_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL1_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL1_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL1_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL1_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL1_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL1_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL1_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL1_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL1_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL1_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL1_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL1_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL1_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL2_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL2_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL2_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL2_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL2_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL2_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL2_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL2_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL2_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL2_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL2_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL2_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL2_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL2_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL2_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL2_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL3_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL3_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL3_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL3_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL3_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL3_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL3_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL3_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL3_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL3_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL3_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL3_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL3_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL3_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL3_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL3_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL4_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL4_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL4_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL4_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL4_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL4_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL4_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL4_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL4_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL4_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL4_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL4_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL4_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL4_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_LOGICAL4_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_LOGICAL4_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER1_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER1_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER1_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER1_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER1_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER1_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER1_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER1_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER1_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER1_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER1_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER1_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER1_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER1_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER1_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER1_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER2_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER2_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER2_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER2_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER2_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER2_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER2_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER2_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER2_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER2_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER2_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER2_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER2_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER2_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER2_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER2_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER3_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER3_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER3_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER3_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER3_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER3_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER3_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER3_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER3_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER3_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER3_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER3_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER3_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER3_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER3_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER3_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER4_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER4_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER4_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER4_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER4_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER4_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER4_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER4_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER4_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER4_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER4_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER4_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER4_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER4_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_INTEGER4_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_INTEGER4_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL1_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL1_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL1_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL1_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL1_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL1_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL1_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL1_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL2_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL2_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL2_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL2_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL2_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL2_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL2_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL2_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL2_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL2_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL2_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL2_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL2_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL2_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL2_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL2_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL3_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL3_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL3_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL3_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL3_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL3_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL3_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL3_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL3_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL3_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL3_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL3_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL3_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL3_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL3_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL3_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL4_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL4_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL4_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL4_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL4_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL4_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL4_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL4_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL4_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL4_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL4_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL4_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL4_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL4_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL4_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_REAL4_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX1_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX1_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX1_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX1_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX1_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX1_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX1_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX1_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX1_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX1_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX1_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX1_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX1_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX1_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX1_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX1_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX2_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX2_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX2_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX2_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX2_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX2_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX2_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX2_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX2_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX2_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX2_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX2_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX2_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX2_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX2_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX2_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX3_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX3_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX3_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX3_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX3_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX3_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX3_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX3_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX3_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX3_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX3_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX3_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX3_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX3_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX3_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX3_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX4_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX4_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX4_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX4_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX4_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX4_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX4_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX4_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX4_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX4_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX4_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX4_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX4_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX4_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_COMPLEX4_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_COMPLEX4_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 49 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 52 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_CHARACTER1_0(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_CHARACTER1_0
#endif
# 52 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_CHARACTER1_1(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_CHARACTER1_1
#endif
# 52 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_CHARACTER1_2(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_CHARACTER1_2
#endif
# 52 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_bcast_CHARACTER1_3(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_CHARACTER1_3
#endif
# 52 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_bcast_CHARACTER1_4(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_CHARACTER1_4
#endif
# 52 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_bcast_CHARACTER1_5(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_CHARACTER1_5
#endif
# 52 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_bcast_CHARACTER1_6(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_CHARACTER1_6
#endif
# 52 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_bcast_CHARACTER1_7(buffer,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_bcast_CHARACTER1_7
#endif
# 63 "ptk_multitype_interf.spp"
#endif
# 67 "ptk_multitype_interf.spp"
end interface

interface ptk_send
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL1_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL1_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL1_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL1_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL1_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL1_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL1_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL1_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL1_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL1_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL1_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL1_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL1_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL1_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL1_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL1_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL2_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL2_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL2_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL2_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL2_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL2_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL2_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL2_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL2_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL2_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL2_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL2_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL2_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL2_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL2_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL2_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL3_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL3_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL3_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL3_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL3_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL3_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL3_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL3_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL3_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL3_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL3_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL3_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL3_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL3_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL3_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL3_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL4_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL4_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL4_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL4_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL4_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL4_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL4_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL4_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL4_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL4_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL4_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL4_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL4_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL4_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_LOGICAL4_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_LOGICAL4_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER1_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER1_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER1_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER1_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER1_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER1_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER1_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER1_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER1_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER1_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER1_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER1_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER1_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER1_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER1_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER1_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER2_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER2_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER2_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER2_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER2_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER2_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER2_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER2_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER2_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER2_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER2_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER2_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER2_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER2_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER2_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER2_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER3_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER3_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER3_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER3_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER3_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER3_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER3_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER3_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER3_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER3_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER3_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER3_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER3_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER3_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER3_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER3_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER4_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER4_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER4_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER4_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER4_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER4_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER4_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER4_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER4_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER4_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER4_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER4_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER4_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER4_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_INTEGER4_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_INTEGER4_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_REAL1_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL1_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_REAL1_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL1_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_REAL1_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL1_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_REAL1_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL1_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_REAL1_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL1_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_REAL1_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL1_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_REAL1_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL1_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_REAL1_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL1_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_REAL2_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL2_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_REAL2_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL2_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_REAL2_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL2_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_REAL2_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL2_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_REAL2_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL2_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_REAL2_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL2_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_REAL2_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL2_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_REAL2_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL2_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_REAL3_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL3_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_REAL3_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL3_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_REAL3_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL3_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_REAL3_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL3_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_REAL3_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL3_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_REAL3_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL3_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_REAL3_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL3_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_REAL3_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL3_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_REAL4_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL4_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_REAL4_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL4_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_REAL4_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL4_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_REAL4_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL4_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_REAL4_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL4_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_REAL4_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL4_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_REAL4_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL4_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_REAL4_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_REAL4_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX1_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX1_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX1_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX1_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX1_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX1_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX1_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX1_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX1_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX1_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX1_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX1_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX1_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX1_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX1_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX1_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX2_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX2_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX2_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX2_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX2_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX2_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX2_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX2_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX2_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX2_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX2_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX2_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX2_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX2_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX2_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX2_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX3_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX3_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX3_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX3_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX3_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX3_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX3_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX3_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX3_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX3_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX3_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX3_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX3_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX3_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX3_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX3_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX4_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX4_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX4_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX4_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX4_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX4_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX4_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX4_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX4_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX4_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX4_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX4_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX4_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX4_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_COMPLEX4_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_COMPLEX4_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 73 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 76 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_send_CHARACTER1_0(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_CHARACTER1_0
#endif
# 76 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_send_CHARACTER1_1(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_CHARACTER1_1
#endif
# 76 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_send_CHARACTER1_2(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_CHARACTER1_2
#endif
# 76 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_send_CHARACTER1_3(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_CHARACTER1_3
#endif
# 76 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_send_CHARACTER1_4(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_CHARACTER1_4
#endif
# 76 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_send_CHARACTER1_5(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_CHARACTER1_5
#endif
# 76 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_send_CHARACTER1_6(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_CHARACTER1_6
#endif
# 76 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_send_CHARACTER1_7(buffer,dest,tag,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:,:,:,:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_send_CHARACTER1_7
#endif
# 88 "ptk_multitype_interf.spp"
#endif
# 92 "ptk_multitype_interf.spp"
end interface

interface ptk_recv
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL1_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL1_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL1_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL1_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL1_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL1_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL1_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL1_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL1_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL1_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL1_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL1_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL1_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL1_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL1_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL1_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL2_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL2_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL2_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL2_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL2_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL2_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL2_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL2_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL2_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL2_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL2_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL2_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL2_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL2_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL2_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL2_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL3_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL3_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL3_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL3_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL3_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL3_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL3_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL3_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL3_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL3_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL3_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL3_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL3_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL3_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL3_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL3_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL4_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL4_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL4_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL4_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL4_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL4_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL4_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL4_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL4_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL4_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL4_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL4_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL4_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL4_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_LOGICAL4_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_LOGICAL4_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER1_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER1_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER1_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER1_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER1_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER1_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER1_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER1_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER1_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER1_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER1_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER1_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER1_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER1_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER1_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER1_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER2_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER2_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER2_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER2_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER2_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER2_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER2_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER2_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER2_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER2_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER2_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER2_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER2_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER2_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER2_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER2_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER3_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER3_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER3_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER3_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER3_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER3_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER3_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER3_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER3_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER3_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER3_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER3_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER3_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER3_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER3_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER3_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER4_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER4_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER4_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER4_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER4_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER4_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER4_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER4_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER4_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER4_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER4_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER4_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER4_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER4_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_INTEGER4_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_INTEGER4_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_REAL1_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL1_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_REAL1_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL1_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_REAL1_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL1_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_REAL1_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL1_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_REAL1_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL1_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_REAL1_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL1_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_REAL1_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL1_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_REAL1_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL1_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_REAL2_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL2_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_REAL2_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL2_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_REAL2_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL2_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_REAL2_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL2_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_REAL2_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL2_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_REAL2_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL2_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_REAL2_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL2_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_REAL2_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL2_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_REAL3_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL3_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_REAL3_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL3_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_REAL3_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL3_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_REAL3_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL3_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_REAL3_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL3_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_REAL3_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL3_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_REAL3_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL3_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_REAL3_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL3_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_REAL4_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL4_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_REAL4_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL4_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_REAL4_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL4_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_REAL4_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL4_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_REAL4_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL4_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_REAL4_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL4_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_REAL4_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL4_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_REAL4_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_REAL4_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX1_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX1_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX1_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX1_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX1_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX1_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX1_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX1_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX1_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX1_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX1_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX1_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX1_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX1_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX1_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX1_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX2_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX2_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX2_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX2_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX2_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX2_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX2_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX2_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX2_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX2_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX2_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX2_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX2_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX2_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX2_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX2_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX3_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX3_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX3_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX3_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX3_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX3_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX3_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX3_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX3_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX3_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX3_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX3_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX3_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX3_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX3_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX3_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX4_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX4_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX4_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX4_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX4_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX4_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX4_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX4_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX4_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX4_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX4_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX4_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX4_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX4_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_COMPLEX4_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_COMPLEX4_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 98 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 101 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_recv_CHARACTER1_0(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_CHARACTER1_0
#endif
# 101 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_recv_CHARACTER1_1(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_CHARACTER1_1
#endif
# 101 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_recv_CHARACTER1_2(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_CHARACTER1_2
#endif
# 101 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_recv_CHARACTER1_3(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: buffer (:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_CHARACTER1_3
#endif
# 101 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_recv_CHARACTER1_4(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: buffer (:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_CHARACTER1_4
#endif
# 101 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_recv_CHARACTER1_5(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: buffer (:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_CHARACTER1_5
#endif
# 101 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_recv_CHARACTER1_6(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: buffer (:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_CHARACTER1_6
#endif
# 101 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_recv_CHARACTER1_7(buffer,source,tag,comm,status,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: buffer (:,:,:,:,:,:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
end subroutine ptk_recv_CHARACTER1_7
#endif
# 114 "ptk_multitype_interf.spp"
#endif
# 118 "ptk_multitype_interf.spp"
end interface

interface ptk_get_count
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL1_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL1_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL1_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL1_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL1_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL1_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL1_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL1_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL1_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL1_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL1_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL1_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL1_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL1_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL1_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL1_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL2_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL2_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL2_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL2_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL2_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL2_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL2_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL2_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL2_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL2_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL2_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL2_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL2_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL2_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL2_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL2_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL3_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL3_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL3_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL3_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL3_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL3_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL3_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL3_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL3_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL3_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL3_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL3_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL3_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL3_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL3_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL3_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL4_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL4_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL4_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL4_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL4_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL4_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL4_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL4_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL4_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL4_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL4_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL4_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL4_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL4_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_LOGICAL4_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_LOGICAL4_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER1_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER1_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER1_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER1_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER1_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER1_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER1_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER1_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER1_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER1_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER1_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER1_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER1_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER1_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER1_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER1_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER2_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER2_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER2_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER2_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER2_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER2_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER2_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER2_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER2_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER2_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER2_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER2_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER2_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER2_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER2_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER2_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER3_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER3_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER3_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER3_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER3_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER3_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER3_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER3_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER3_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER3_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER3_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER3_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER3_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER3_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER3_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER3_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER4_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER4_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER4_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER4_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER4_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER4_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER4_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER4_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER4_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER4_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER4_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER4_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER4_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER4_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_INTEGER4_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_INTEGER4_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL1_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL1_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL1_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL1_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL1_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL1_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL1_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL1_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL1_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL1_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL1_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL1_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL1_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL1_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL1_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL1_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL2_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL2_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL2_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL2_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL2_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL2_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL2_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL2_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL2_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL2_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL2_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL2_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL2_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL2_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL2_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL2_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL3_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL3_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL3_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL3_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL3_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL3_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL3_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL3_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL3_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL3_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL3_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL3_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL3_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL3_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL3_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL3_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL4_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL4_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL4_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL4_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL4_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL4_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL4_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL4_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL4_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL4_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL4_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL4_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL4_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL4_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_REAL4_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_REAL4_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX1_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX1_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX1_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX1_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX1_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX1_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX1_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX1_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX1_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX1_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX1_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX1_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX1_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX1_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX1_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX1_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX2_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX2_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX2_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX2_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX2_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX2_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX2_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX2_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX2_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX2_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX2_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX2_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX2_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX2_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX2_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX2_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX3_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX3_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX3_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX3_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX3_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX3_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX3_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX3_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX3_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX3_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX3_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX3_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX3_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX3_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX3_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX3_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX4_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX4_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX4_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX4_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX4_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX4_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX4_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX4_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX4_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX4_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX4_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX4_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX4_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX4_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_COMPLEX4_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_COMPLEX4_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 124 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 127 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_get_count_CHARACTER1_0(buffer,status,count,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_CHARACTER1_0
#endif
# 127 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_get_count_CHARACTER1_1(buffer,status,count,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_CHARACTER1_1
#endif
# 127 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_get_count_CHARACTER1_2(buffer,status,count,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_CHARACTER1_2
#endif
# 127 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_get_count_CHARACTER1_3(buffer,status,count,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_CHARACTER1_3
#endif
# 127 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_get_count_CHARACTER1_4(buffer,status,count,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_CHARACTER1_4
#endif
# 127 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_get_count_CHARACTER1_5(buffer,status,count,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_CHARACTER1_5
#endif
# 127 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_get_count_CHARACTER1_6(buffer,status,count,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_CHARACTER1_6
#endif
# 127 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_get_count_CHARACTER1_7(buffer,status,count,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in) :: buffer (:,:,:,:,:,:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
end subroutine ptk_get_count_CHARACTER1_7
#endif
# 138 "ptk_multitype_interf.spp"
#endif
# 142 "ptk_multitype_interf.spp"
end interface

interface ptk_reduce
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL1_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL1_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL1_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL1_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL1_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL1_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL1_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL1_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL1_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL1_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL1_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL1_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL1_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL1_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL1_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL1_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL2_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL2_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL2_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL2_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL2_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL2_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL2_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL2_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL2_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL2_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL2_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL2_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL2_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL2_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL2_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL2_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL3_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL3_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL3_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL3_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL3_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL3_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL3_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL3_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL3_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL3_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL3_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL3_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL3_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL3_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL3_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL3_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL4_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL4_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL4_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL4_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL4_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL4_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL4_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL4_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL4_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL4_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL4_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL4_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL4_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL4_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_LOGICAL4_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_LOGICAL4_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER1_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER1_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER1_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER1_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER1_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER1_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER1_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER1_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER1_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER1_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER1_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER1_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER1_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER1_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER1_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER1_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER2_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER2_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER2_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER2_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER2_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER2_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER2_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER2_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER2_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER2_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER2_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER2_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER2_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER2_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER2_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER2_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER3_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER3_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER3_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER3_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER3_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER3_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER3_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER3_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER3_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER3_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER3_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER3_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER3_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER3_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER3_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER3_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER4_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER4_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER4_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER4_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER4_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER4_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER4_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER4_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER4_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER4_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER4_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER4_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER4_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER4_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_INTEGER4_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_INTEGER4_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL1_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL1_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL1_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL1_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL1_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL1_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL1_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL1_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL1_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL1_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL1_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL1_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL1_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL1_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL1_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL1_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL2_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL2_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL2_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL2_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL2_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL2_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL2_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL2_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL2_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL2_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL2_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL2_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL2_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL2_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL2_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL2_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL3_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL3_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL3_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL3_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL3_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL3_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL3_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL3_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL3_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL3_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL3_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL3_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL3_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL3_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL3_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL3_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL4_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL4_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL4_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL4_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL4_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL4_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL4_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL4_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL4_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL4_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL4_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL4_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL4_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL4_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_REAL4_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_REAL4_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX1_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX1_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX1_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX1_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX1_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX1_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX1_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX1_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX1_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX1_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX1_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX1_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX1_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX1_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX1_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX1_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX2_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX2_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX2_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX2_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX2_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX2_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX2_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX2_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX2_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX2_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX2_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX2_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX2_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX2_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX2_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX2_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX3_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX3_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX3_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX3_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX3_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX3_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX3_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX3_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX3_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX3_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX3_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX3_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX3_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX3_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX3_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX3_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX4_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX4_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX4_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX4_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX4_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX4_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX4_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX4_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX4_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX4_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX4_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX4_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX4_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX4_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_COMPLEX4_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_COMPLEX4_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 148 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 151 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_CHARACTER1_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf 
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_CHARACTER1_0
#endif
# 151 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_CHARACTER1_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_CHARACTER1_1
#endif
# 151 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_CHARACTER1_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_CHARACTER1_2
#endif
# 151 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_CHARACTER1_3(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_CHARACTER1_3
#endif
# 151 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_CHARACTER1_4(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_CHARACTER1_4
#endif
# 151 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_CHARACTER1_5(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_CHARACTER1_5
#endif
# 151 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_CHARACTER1_6(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_CHARACTER1_6
#endif
# 151 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_CHARACTER1_7(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_CHARACTER1_7
#endif
# 164 "ptk_multitype_interf.spp"
#endif
# 168 "ptk_multitype_interf.spp"
end interface

interface ptk_allreduce
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL1_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL1_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL1_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL1_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL1_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL1_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL1_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL1_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL1_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL1_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL1_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL1_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL1_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL1_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL1_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL1_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL2_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL2_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL2_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL2_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL2_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL2_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL2_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL2_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL2_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL2_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL2_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL2_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL2_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL2_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL2_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL2_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL3_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL3_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL3_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL3_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL3_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL3_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL3_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL3_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL3_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL3_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL3_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL3_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL3_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL3_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL3_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL3_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL4_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL4_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL4_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL4_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL4_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL4_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL4_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL4_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL4_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL4_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL4_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL4_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL4_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL4_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_LOGICAL4_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_LOGICAL4_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER1_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER1_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER1_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER1_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER1_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER1_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER1_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER1_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER1_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER1_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER1_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER1_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER1_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER1_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER1_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER1_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER2_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER2_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER2_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER2_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER2_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER2_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER2_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER2_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER2_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER2_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER2_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER2_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER2_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER2_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER2_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER2_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER3_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER3_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER3_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER3_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER3_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER3_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER3_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER3_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER3_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER3_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER3_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER3_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER3_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER3_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER3_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER3_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER4_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER4_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER4_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER4_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER4_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER4_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER4_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER4_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER4_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER4_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER4_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER4_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER4_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER4_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_INTEGER4_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_INTEGER4_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL1_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL1_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL1_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL1_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL1_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL1_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL1_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL1_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL1_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL1_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL1_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL1_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL1_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL1_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL1_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL1_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL2_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL2_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL2_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL2_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL2_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL2_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL2_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL2_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL2_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL2_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL2_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL2_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL2_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL2_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL2_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL2_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL3_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL3_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL3_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL3_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL3_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL3_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL3_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL3_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL3_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL3_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL3_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL3_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL3_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL3_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL3_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL3_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL4_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL4_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL4_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL4_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL4_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL4_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL4_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL4_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL4_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL4_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL4_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL4_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL4_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL4_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_REAL4_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_REAL4_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX1_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX1_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX1_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX1_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX1_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX1_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX1_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX1_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX1_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX1_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX1_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX1_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX1_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX1_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX1_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX1_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX2_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX2_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX2_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX2_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX2_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX2_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX2_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX2_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX2_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX2_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX2_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX2_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX2_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX2_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX2_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX2_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX3_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX3_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX3_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX3_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX3_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX3_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX3_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX3_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX3_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX3_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX3_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX3_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX3_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX3_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX3_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX3_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX4_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX4_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX4_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX4_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX4_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX4_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX4_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX4_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX4_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX4_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX4_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX4_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX4_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX4_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_COMPLEX4_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_COMPLEX4_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 174 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 177 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_CHARACTER1_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf 
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_CHARACTER1_0
#endif
# 177 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_CHARACTER1_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_CHARACTER1_1
#endif
# 177 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_CHARACTER1_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_CHARACTER1_2
#endif
# 177 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_CHARACTER1_3(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_CHARACTER1_3
#endif
# 177 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_CHARACTER1_4(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_CHARACTER1_4
#endif
# 177 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_CHARACTER1_5(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_CHARACTER1_5
#endif
# 177 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_CHARACTER1_6(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_CHARACTER1_6
#endif
# 177 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_CHARACTER1_7(sendbuf,recvbuf,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_CHARACTER1_7
#endif
# 189 "ptk_multitype_interf.spp"
#endif
# 193 "ptk_multitype_interf.spp"
end interface

interface ptk_reduce_inplace
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL1_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL1_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL1_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL1_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL1_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL1_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL1_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL1_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL1_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL1_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL1_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL1_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL1_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL1_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL1_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL1_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL2_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL2_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL2_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL2_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL2_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL2_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL2_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL2_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL2_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL2_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL2_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL2_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL2_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL2_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL2_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL2_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL3_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL3_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL3_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL3_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL3_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL3_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL3_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL3_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL3_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL3_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL3_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL3_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL3_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL3_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL3_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL3_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL4_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL4_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL4_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL4_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL4_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL4_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL4_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL4_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL4_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL4_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL4_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL4_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL4_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL4_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_LOGICAL4_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_LOGICAL4_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER1_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER1_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER1_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER1_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER1_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER1_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER1_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER1_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER1_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER1_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER1_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER1_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER1_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER1_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER1_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER1_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER2_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER2_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER2_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER2_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER2_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER2_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER2_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER2_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER2_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER2_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER2_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER2_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER2_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER2_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER2_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER2_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER3_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER3_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER3_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER3_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER3_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER3_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER3_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER3_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER3_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER3_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER3_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER3_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER3_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER3_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER3_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER3_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER4_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER4_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER4_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER4_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER4_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER4_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER4_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER4_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER4_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER4_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER4_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER4_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER4_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER4_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_INTEGER4_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_INTEGER4_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL1_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL1_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL1_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL1_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL1_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL1_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL1_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL1_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL1_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL1_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL1_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL1_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL1_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL1_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL1_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL1_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL2_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL2_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL2_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL2_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL2_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL2_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL2_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL2_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL2_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL2_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL2_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL2_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL2_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL2_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL2_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL2_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL3_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL3_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL3_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL3_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL3_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL3_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL3_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL3_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL3_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL3_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL3_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL3_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL3_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL3_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL3_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL3_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL4_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL4_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL4_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL4_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL4_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL4_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL4_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL4_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL4_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL4_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL4_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL4_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL4_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL4_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_REAL4_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_REAL4_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX1_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX1_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX1_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX1_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX1_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX1_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX1_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX1_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX1_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX1_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX1_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX1_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX1_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX1_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX1_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX1_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX2_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX2_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX2_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX2_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX2_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX2_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX2_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX2_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX2_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX2_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX2_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX2_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX2_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX2_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX2_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX2_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX3_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX3_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX3_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX3_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX3_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX3_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX3_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX3_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX3_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX3_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX3_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX3_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX3_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX3_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX3_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX3_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX4_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX4_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX4_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX4_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX4_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX4_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX4_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX4_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX4_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX4_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX4_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX4_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX4_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX4_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_COMPLEX4_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_COMPLEX4_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 199 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 202 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_CHARACTER1_0(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_CHARACTER1_0
#endif
# 202 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_CHARACTER1_1(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_CHARACTER1_1
#endif
# 202 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_CHARACTER1_2(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_CHARACTER1_2
#endif
# 202 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_CHARACTER1_3(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout)  :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_CHARACTER1_3
#endif
# 202 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_CHARACTER1_4(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout)  :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_CHARACTER1_4
#endif
# 202 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_CHARACTER1_5(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout)  :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_CHARACTER1_5
#endif
# 202 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_CHARACTER1_6(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout)  :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_CHARACTER1_6
#endif
# 202 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_reduce_ip_CHARACTER1_7(buffer,op,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout)  :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_reduce_ip_CHARACTER1_7
#endif
# 214 "ptk_multitype_interf.spp"
#endif
# 218 "ptk_multitype_interf.spp"
end interface

interface ptk_allreduce_inplace
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL1_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL1_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL1_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL1_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL1_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL1_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL1_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL1_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL1_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL1_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL1_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL1_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL1_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL1_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL1_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL1_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL2_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL2_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL2_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL2_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL2_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL2_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL2_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL2_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL2_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL2_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL2_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL2_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL2_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL2_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL2_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL2_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL3_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL3_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL3_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL3_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL3_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL3_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL3_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL3_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL3_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL3_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL3_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL3_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL3_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL3_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL3_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL3_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL4_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL4_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL4_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL4_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL4_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL4_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL4_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL4_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL4_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL4_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL4_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL4_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL4_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL4_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_LOGICAL4_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_LOGICAL4_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER1_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER1_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER1_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER1_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER1_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER1_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER1_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER1_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER1_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER1_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER1_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER1_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER1_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER1_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER1_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER1_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER2_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER2_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER2_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER2_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER2_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER2_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER2_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER2_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER2_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER2_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER2_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER2_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER2_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER2_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER2_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER2_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER3_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER3_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER3_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER3_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER3_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER3_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER3_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER3_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER3_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER3_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER3_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER3_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER3_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER3_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER3_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER3_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER4_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER4_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER4_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER4_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER4_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER4_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER4_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER4_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER4_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER4_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER4_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER4_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER4_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER4_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_INTEGER4_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_INTEGER4_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL1_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL1_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL1_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL1_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL1_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL1_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL1_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL1_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL1_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL1_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL1_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL1_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL1_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL1_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL1_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL1_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL2_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL2_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL2_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL2_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL2_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL2_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL2_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL2_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL2_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL2_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL2_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL2_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL2_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL2_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL2_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL2_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL3_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL3_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL3_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL3_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL3_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL3_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL3_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL3_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL3_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL3_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL3_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL3_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL3_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL3_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL3_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL3_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL4_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL4_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL4_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL4_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL4_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL4_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL4_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL4_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL4_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL4_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL4_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL4_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL4_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL4_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_REAL4_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_REAL4_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX1_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX1_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX1_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX1_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX1_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX1_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX1_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX1_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX1_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX1_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX1_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX1_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX1_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX1_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX1_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX1_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX2_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX2_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX2_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX2_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX2_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX2_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX2_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX2_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX2_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX2_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX2_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX2_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX2_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX2_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX2_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX2_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX3_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX3_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX3_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX3_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX3_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX3_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX3_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX3_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX3_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX3_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX3_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX3_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX3_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX3_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX3_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX3_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX4_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX4_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX4_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX4_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX4_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX4_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX4_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX4_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX4_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX4_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX4_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX4_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX4_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX4_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_COMPLEX4_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_COMPLEX4_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 224 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 227 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_CHARACTER1_0(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_CHARACTER1_0
#endif
# 227 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_CHARACTER1_1(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_CHARACTER1_1
#endif
# 227 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_CHARACTER1_2(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_CHARACTER1_2
#endif
# 227 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_CHARACTER1_3(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_CHARACTER1_3
#endif
# 227 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_CHARACTER1_4(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_CHARACTER1_4
#endif
# 227 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_CHARACTER1_5(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_CHARACTER1_5
#endif
# 227 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_CHARACTER1_6(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_CHARACTER1_6
#endif
# 227 "ptk_multitype_interf.spp"
#if 7 <= __PTK_MAXRANK
subroutine ptk_allreduce_ip_CHARACTER1_7(buffer,op,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(inout) :: buffer (:,:,:,:,:,:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allreduce_ip_CHARACTER1_7
#endif
# 238 "ptk_multitype_interf.spp"
#endif
# 242 "ptk_multitype_interf.spp"
end interface

interface ptk_gather
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL1_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL1_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL1_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL1_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL1_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL1_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL1_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL1_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL1_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL1_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL1_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL1_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL1_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL1_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL2_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL2_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL2_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL2_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL2_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL2_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL2_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL2_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL2_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL2_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL2_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL2_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL2_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL2_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL3_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL3_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL3_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL3_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL3_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL3_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL3_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL3_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL3_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL3_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL3_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL3_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL3_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL3_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL4_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL4_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL4_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL4_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL4_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL4_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL4_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL4_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL4_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL4_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL4_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL4_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_LOGICAL4_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_LOGICAL4_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER1_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER1_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER1_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER1_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER1_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER1_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER1_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER1_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER1_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER1_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER1_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER1_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER1_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER1_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER2_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER2_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER2_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER2_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER2_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER2_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER2_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER2_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER2_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER2_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER2_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER2_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER2_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER2_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER3_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER3_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER3_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER3_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER3_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER3_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER3_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER3_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER3_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER3_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER3_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER3_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER3_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER3_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER4_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER4_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER4_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER4_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER4_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER4_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER4_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER4_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER4_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER4_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER4_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER4_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_INTEGER4_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_INTEGER4_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_REAL1_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL1_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_REAL1_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL1_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_REAL1_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL1_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_REAL1_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL1_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_REAL1_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL1_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_REAL1_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL1_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_REAL1_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL1_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_REAL2_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL2_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_REAL2_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL2_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_REAL2_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL2_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_REAL2_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL2_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_REAL2_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL2_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_REAL2_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL2_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_REAL2_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL2_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_REAL3_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL3_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_REAL3_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL3_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_REAL3_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL3_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_REAL3_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL3_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_REAL3_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL3_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_REAL3_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL3_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_REAL3_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL3_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_REAL4_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL4_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_REAL4_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL4_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_REAL4_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL4_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_REAL4_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL4_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_REAL4_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL4_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_REAL4_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL4_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_REAL4_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_REAL4_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX1_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX1_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX1_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX1_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX1_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX1_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX1_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX1_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX1_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX1_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX1_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX1_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX1_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX1_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX2_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX2_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX2_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX2_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX2_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX2_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX2_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX2_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX2_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX2_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX2_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX2_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX2_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX2_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX3_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX3_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX3_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX3_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX3_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX3_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX3_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX3_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX3_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX3_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX3_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX3_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX3_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX3_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX4_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX4_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX4_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX4_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX4_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX4_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX4_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX4_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX4_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX4_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX4_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX4_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_COMPLEX4_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_COMPLEX4_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 248 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 252 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_gather_CHARACTER1_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf 
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_CHARACTER1_0
#endif
# 252 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_gather_CHARACTER1_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_CHARACTER1_1
#endif
# 252 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_gather_CHARACTER1_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_CHARACTER1_2
#endif
# 252 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_gather_CHARACTER1_3(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_CHARACTER1_3
#endif
# 252 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_gather_CHARACTER1_4(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_CHARACTER1_4
#endif
# 252 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_gather_CHARACTER1_5(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_CHARACTER1_5
#endif
# 252 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_gather_CHARACTER1_6(sendbuf,recvbuf,root,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_gather_CHARACTER1_6
#endif
# 265 "ptk_multitype_interf.spp"
#endif
# 269 "ptk_multitype_interf.spp"
end interface

interface ptk_allgather
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL1
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL1_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL1_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL1_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL1_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL1_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL1_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL1_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL1_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL1_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL1_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL1_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL1_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL1_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL1_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL2
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL2_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL2_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL2_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL2_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL2_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL2_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL2_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL2_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL2_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL2_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL2_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL2_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL2_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL2_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL3
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL3_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL3_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL3_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL3_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL3_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL3_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL3_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL3_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL3_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL3_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL3_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL3_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL3_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL3_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_LOGICAL4
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL4_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf 
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL4_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL4_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL4_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL4_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL4_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL4_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL4_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL4_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL4_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL4_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL4_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_LOGICAL4_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  LOGICAL (kind=__PTK_LOGICAL4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  LOGICAL (kind=__PTK_LOGICAL4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_LOGICAL4_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER1
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER1_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER1_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER1_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER1_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER1_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER1_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER1_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER1_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER1_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER1_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER1_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER1_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER1_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER1_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER2
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER2_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER2_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER2_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER2_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER2_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER2_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER2_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER2_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER2_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER2_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER2_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER2_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER2_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER2_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER3
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER3_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER3_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER3_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER3_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER3_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER3_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER3_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER3_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER3_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER3_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER3_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER3_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER3_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER3_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_INTEGER4
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER4_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf 
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER4_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER4_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER4_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER4_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER4_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER4_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER4_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER4_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER4_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER4_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER4_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_INTEGER4_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  INTEGER (kind=__PTK_INTEGER4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  INTEGER (kind=__PTK_INTEGER4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_INTEGER4_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL1
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL1_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL1_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL1_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL1_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL1_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL1_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL1_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL1_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL1_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL1_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL1_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL1_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL1_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL1_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL2
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL2_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL2_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL2_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL2_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL2_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL2_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL2_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL2_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL2_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL2_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL2_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL2_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL2_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL2_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL3
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL3_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL3_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL3_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL3_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL3_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL3_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL3_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL3_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL3_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL3_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL3_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL3_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL3_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL3_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_REAL4
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL4_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL4_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL4_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL4_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL4_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL4_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL4_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL4_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL4_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL4_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL4_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL4_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_REAL4_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  REAL (kind=__PTK_REAL4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_REAL4_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX1
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX1_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX1_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX1_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX1_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX1_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX1_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX1_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX1_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX1_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX1_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX1_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX1_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX1_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX1), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX1), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX1_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX2
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX2_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX2_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX2_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX2_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX2_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX2_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX2_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX2_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX2_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX2_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX2_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX2_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX2_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX2), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX2), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX2_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX3
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX3_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX3_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX3_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX3_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX3_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX3_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX3_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX3_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX3_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX3_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX3_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX3_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX3_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX3), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX3), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX3_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_COMPLEX4
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX4_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf 
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX4_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX4_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX4_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX4_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX4_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX4_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX4_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX4_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX4_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX4_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX4_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_COMPLEX4_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  COMPLEX (kind=__PTK_COMPLEX4), intent(in)  :: sendbuf (:,:,:,:,:,:)
  COMPLEX (kind=__PTK_COMPLEX4), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_COMPLEX4_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 275 "ptk_multitype_interf.spp"
#ifdef __PTK_CHARACTER1
# 279 "ptk_multitype_interf.spp"
#if 0 <= __PTK_MAXRANK
subroutine ptk_allgather_CHARACTER1_0(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf 
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_CHARACTER1_0
#endif
# 279 "ptk_multitype_interf.spp"
#if 1 <= __PTK_MAXRANK
subroutine ptk_allgather_CHARACTER1_1(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_CHARACTER1_1
#endif
# 279 "ptk_multitype_interf.spp"
#if 2 <= __PTK_MAXRANK
subroutine ptk_allgather_CHARACTER1_2(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_CHARACTER1_2
#endif
# 279 "ptk_multitype_interf.spp"
#if 3 <= __PTK_MAXRANK
subroutine ptk_allgather_CHARACTER1_3(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_CHARACTER1_3
#endif
# 279 "ptk_multitype_interf.spp"
#if 4 <= __PTK_MAXRANK
subroutine ptk_allgather_CHARACTER1_4(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_CHARACTER1_4
#endif
# 279 "ptk_multitype_interf.spp"
#if 5 <= __PTK_MAXRANK
subroutine ptk_allgather_CHARACTER1_5(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_CHARACTER1_5
#endif
# 279 "ptk_multitype_interf.spp"
#if 6 <= __PTK_MAXRANK
subroutine ptk_allgather_CHARACTER1_6(sendbuf,recvbuf,comm,ierr)
  use ptk_base
  implicit none
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(in)  :: sendbuf (:,:,:,:,:,:)
  CHARACTER (kind=__PTK_CHARACTER1,len=*), intent(out) :: recvbuf (:,:,:,:,:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
end subroutine ptk_allgather_CHARACTER1_6
#endif
# 291 "ptk_multitype_interf.spp"
#endif
# 295 "ptk_multitype_interf.spp"
end interface

end module ptk_multitype_interf
