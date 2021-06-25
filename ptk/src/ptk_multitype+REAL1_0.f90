# 64 "ptk_multitype.spp"
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

# 74 "ptk_multitype.spp"

#ifdef __PTK_REAL1
#if 0 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_0(buffer,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer 
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc,root_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  root_loc = 0
  if(present(root)) root_loc = root
#ifdef __PTK_MPI
  call MPI_Bcast(buffer,1,__PTK_MPI_REAL1, &
                 root_loc,comm_loc%comm,ierr_loc)
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_bcast_REAL1_0

subroutine ptk_send_REAL1_0(buffer,dest,tag,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer 
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: tag_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  tag_loc = 0
  if(present(tag)) tag_loc = tag
#ifdef __PTK_MPI
  call MPI_Send(buffer,1,__PTK_MPI_REAL1, &
                dest,tag_loc,comm_loc%comm,ierr_loc)
#else
  ierr_loc = __LINE__
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_send_REAL1_0

subroutine ptk_recv_REAL1_0(buffer,source,tag,comm,status,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer 
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
  integer :: ierr_loc,source_loc,tag_loc
  type(ptk_status) :: status_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  source_loc = ptk_any_source
  if(present(source)) source_loc = source
  tag_loc = ptk_any_tag
  if(present(tag)) tag_loc = tag
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Recv(buffer,1,__PTK_MPI_REAL1, &
                source_loc,tag_loc,comm_loc%comm,status_loc%status,ierr_loc)
#else
# 164 "ptk_multitype.spp"
  buffer = 0
# 166 "ptk_multitype.spp"
  ierr_loc = __LINE__
  status_loc%status = 0 ! otherwise the compiler complains
#endif
  if(present(status)) status = status_loc
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_recv_REAL1_0

subroutine ptk_get_count_REAL1_0(buffer,status,count,ierr)
  use ptk_interface, only : ptk_error_handler
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer 
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
  integer :: ierr_loc
#ifdef __PTK_MPI
  call MPI_Get_count(status%status,__PTK_MPI_REAL1,count,ierr_loc)
#else
  count = 0
  ierr_loc = __LINE__
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_get_count_REAL1_0

subroutine ptk_reduce_REAL1_0(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  if(1 /= 1) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Reduce(sendbuf,recvbuf,1,__PTK_MPI_REAL1, &
                  op%op,root_loc,comm_loc%comm,ierr_loc)
#else
  recvbuf = sendbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_reduce_REAL1_0

subroutine ptk_allreduce_REAL1_0(sendbuf,recvbuf,op,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  if(1 /= 1) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Allreduce(sendbuf,recvbuf,1,__PTK_MPI_REAL1, &
                     op%op,comm_loc%comm,ierr_loc)
#else
  recvbuf = sendbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allreduce_REAL1_0

subroutine ptk_reduce_ip_REAL1_0(buffer,op,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer 
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,ierr_loc,rank
  type(ptk_comm) :: comm_loc
# 280 "ptk_multitype.spp"
  REAL (kind=__PTK_REAL1) :: tempbuf 
# 282 "ptk_multitype.spp"
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Reduce(buffer,tempbuf,1,__PTK_MPI_REAL1, &
                  op%op,root_loc,comm_loc%comm,ierr_loc)
  if(ierr_loc/=0) goto 1
  call ptk_comm_rank(comm_loc,rank,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(rank==root_loc) buffer = tempbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_reduce_ip_REAL1_0

subroutine ptk_allreduce_ip_REAL1_0(buffer,op,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer 
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  type(ptk_comm) :: comm_loc
# 318 "ptk_multitype.spp"
  REAL (kind=__PTK_REAL1) :: tempbuf 
# 320 "ptk_multitype.spp"
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Allreduce(buffer,tempbuf,1,__PTK_MPI_REAL1, &
                     op%op,comm_loc%comm,ierr_loc)
  if(ierr_loc/=0) goto 1
  buffer = tempbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allreduce_ip_REAL1_0

# 338 "ptk_multitype.spp"
subroutine ptk_gather_REAL1_0(sendbuf,recvbuf,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,size_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  call ptk_comm_size(comm_loc,size_loc,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(1 * size_loc /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Gather(sendbuf,1,__PTK_MPI_REAL1, &
                  recvbuf,1,__PTK_MPI_REAL1, &
                  root_loc,comm_loc%comm,ierr_loc)
#else
# 365 "ptk_multitype.spp"
  recvbuf(1) = sendbuf
# 369 "ptk_multitype.spp"
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_gather_REAL1_0

subroutine ptk_allgather_REAL1_0(sendbuf,recvbuf,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf 
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: size_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  call ptk_comm_size(comm_loc,size_loc,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(1 * size_loc /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Allgather(sendbuf,1,__PTK_MPI_REAL1, &
                     recvbuf,1,__PTK_MPI_REAL1, &
                     comm_loc%comm,ierr_loc)
#else
# 402 "ptk_multitype.spp"
  recvbuf(1) = sendbuf
# 406 "ptk_multitype.spp"
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allgather_REAL1_0

# 416 "ptk_multitype.spp"

#endif
#endif

subroutine ptk_dummy_REAL1_0
write(0,*)
end subroutine ptk_dummy_REAL1_0

# 64 "ptk_multitype.spp"
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

# 74 "ptk_multitype.spp"

#ifdef __PTK_REAL1
#if 1 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_1(buffer,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc,root_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  root_loc = 0
  if(present(root)) root_loc = root
#ifdef __PTK_MPI
  call MPI_Bcast(buffer,size(buffer),__PTK_MPI_REAL1, &
                 root_loc,comm_loc%comm,ierr_loc)
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_bcast_REAL1_1

subroutine ptk_send_REAL1_1(buffer,dest,tag,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: tag_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  tag_loc = 0
  if(present(tag)) tag_loc = tag
#ifdef __PTK_MPI
  call MPI_Send(buffer,size(buffer),__PTK_MPI_REAL1, &
                dest,tag_loc,comm_loc%comm,ierr_loc)
#else
  ierr_loc = __LINE__
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_send_REAL1_1

subroutine ptk_recv_REAL1_1(buffer,source,tag,comm,status,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
  integer :: ierr_loc,source_loc,tag_loc
  type(ptk_status) :: status_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  source_loc = ptk_any_source
  if(present(source)) source_loc = source
  tag_loc = ptk_any_tag
  if(present(tag)) tag_loc = tag
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Recv(buffer,size(buffer),__PTK_MPI_REAL1, &
                source_loc,tag_loc,comm_loc%comm,status_loc%status,ierr_loc)
#else
# 164 "ptk_multitype.spp"
  buffer = 0
# 166 "ptk_multitype.spp"
  ierr_loc = __LINE__
  status_loc%status = 0 ! otherwise the compiler complains
#endif
  if(present(status)) status = status_loc
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_recv_REAL1_1

subroutine ptk_get_count_REAL1_1(buffer,status,count,ierr)
  use ptk_interface, only : ptk_error_handler
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
  integer :: ierr_loc
#ifdef __PTK_MPI
  call MPI_Get_count(status%status,__PTK_MPI_REAL1,count,ierr_loc)
#else
  count = 0
  ierr_loc = __LINE__
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_get_count_REAL1_1

subroutine ptk_reduce_REAL1_1(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  if(size(sendbuf) /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Reduce(sendbuf,recvbuf,size(sendbuf),__PTK_MPI_REAL1, &
                  op%op,root_loc,comm_loc%comm,ierr_loc)
#else
  recvbuf = sendbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_reduce_REAL1_1

subroutine ptk_allreduce_REAL1_1(sendbuf,recvbuf,op,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  if(size(sendbuf) /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Allreduce(sendbuf,recvbuf,size(sendbuf),__PTK_MPI_REAL1, &
                     op%op,comm_loc%comm,ierr_loc)
#else
  recvbuf = sendbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allreduce_REAL1_1

subroutine ptk_reduce_ip_REAL1_1(buffer,op,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,ierr_loc,rank
  type(ptk_comm) :: comm_loc
# 280 "ptk_multitype.spp"
  REAL (kind=__PTK_REAL1) :: tempbuf (lbound(buffer,1):ubound(buffer,1))
# 282 "ptk_multitype.spp"
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Reduce(buffer,tempbuf,size(buffer),__PTK_MPI_REAL1, &
                  op%op,root_loc,comm_loc%comm,ierr_loc)
  if(ierr_loc/=0) goto 1
  call ptk_comm_rank(comm_loc,rank,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(rank==root_loc) buffer = tempbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_reduce_ip_REAL1_1

subroutine ptk_allreduce_ip_REAL1_1(buffer,op,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  type(ptk_comm) :: comm_loc
# 318 "ptk_multitype.spp"
  REAL (kind=__PTK_REAL1) :: tempbuf (lbound(buffer,1):ubound(buffer,1))
# 320 "ptk_multitype.spp"
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Allreduce(buffer,tempbuf,size(buffer),__PTK_MPI_REAL1, &
                     op%op,comm_loc%comm,ierr_loc)
  if(ierr_loc/=0) goto 1
  buffer = tempbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allreduce_ip_REAL1_1

# 338 "ptk_multitype.spp"
subroutine ptk_gather_REAL1_1(sendbuf,recvbuf,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,size_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  call ptk_comm_size(comm_loc,size_loc,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(size(sendbuf) * size_loc /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Gather(sendbuf,size(sendbuf),__PTK_MPI_REAL1, &
                  recvbuf,size(sendbuf),__PTK_MPI_REAL1, &
                  root_loc,comm_loc%comm,ierr_loc)
#else
# 367 "ptk_multitype.spp"
  recvbuf = reshape(sendbuf,shape(recvbuf))
# 369 "ptk_multitype.spp"
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_gather_REAL1_1

subroutine ptk_allgather_REAL1_1(sendbuf,recvbuf,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: size_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  call ptk_comm_size(comm_loc,size_loc,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(size(sendbuf) * size_loc /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Allgather(sendbuf,size(sendbuf),__PTK_MPI_REAL1, &
                     recvbuf,size(sendbuf),__PTK_MPI_REAL1, &
                     comm_loc%comm,ierr_loc)
#else
# 404 "ptk_multitype.spp"
  recvbuf = reshape (sendbuf,shape(recvbuf))
# 406 "ptk_multitype.spp"
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allgather_REAL1_1

# 416 "ptk_multitype.spp"

#endif
#endif

subroutine ptk_dummy_REAL1_1
write(0,*)
end subroutine ptk_dummy_REAL1_1

# 64 "ptk_multitype.spp"
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

# 74 "ptk_multitype.spp"

#ifdef __PTK_REAL1
#if 2 <= __PTK_MAXRANK
subroutine ptk_bcast_REAL1_2(buffer,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc,root_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  root_loc = 0
  if(present(root)) root_loc = root
#ifdef __PTK_MPI
  call MPI_Bcast(buffer,size(buffer),__PTK_MPI_REAL1, &
                 root_loc,comm_loc%comm,ierr_loc)
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_bcast_REAL1_2

subroutine ptk_send_REAL1_2(buffer,dest,tag,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:)
  integer,        intent(in)            :: dest
  integer,        intent(in),  optional :: tag
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: tag_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  tag_loc = 0
  if(present(tag)) tag_loc = tag
#ifdef __PTK_MPI
  call MPI_Send(buffer,size(buffer),__PTK_MPI_REAL1, &
                dest,tag_loc,comm_loc%comm,ierr_loc)
#else
  ierr_loc = __LINE__
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_send_REAL1_2

subroutine ptk_recv_REAL1_2(buffer,source,tag,comm,status,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(out) :: buffer (:,:)
  integer,          intent(in),  optional :: source
  integer,          intent(in),  optional :: tag
  type(ptk_comm),   intent(in),  optional :: comm
  type(ptk_status), intent(out), optional :: status
  integer,          intent(out), optional :: ierr
  integer :: ierr_loc,source_loc,tag_loc
  type(ptk_status) :: status_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  source_loc = ptk_any_source
  if(present(source)) source_loc = source
  tag_loc = ptk_any_tag
  if(present(tag)) tag_loc = tag
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Recv(buffer,size(buffer),__PTK_MPI_REAL1, &
                source_loc,tag_loc,comm_loc%comm,status_loc%status,ierr_loc)
#else
# 164 "ptk_multitype.spp"
  buffer = 0
# 166 "ptk_multitype.spp"
  ierr_loc = __LINE__
  status_loc%status = 0 ! otherwise the compiler complains
#endif
  if(present(status)) status = status_loc
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_recv_REAL1_2

subroutine ptk_get_count_REAL1_2(buffer,status,count,ierr)
  use ptk_interface, only : ptk_error_handler
  use ptk_base
  implicit none
  REAL (kind=__PTK_REAL1), intent(in) :: buffer (:,:)
  type(ptk_status), intent(in)            :: status
  integer,          intent(out)           :: count
  integer,          intent(out), optional :: ierr
  integer :: ierr_loc
#ifdef __PTK_MPI
  call MPI_Get_count(status%status,__PTK_MPI_REAL1,count,ierr_loc)
#else
  count = 0
  ierr_loc = __LINE__
#endif
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_get_count_REAL1_2

subroutine ptk_reduce_REAL1_2(sendbuf,recvbuf,op,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  if(size(sendbuf) /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Reduce(sendbuf,recvbuf,size(sendbuf),__PTK_MPI_REAL1, &
                  op%op,root_loc,comm_loc%comm,ierr_loc)
#else
  recvbuf = sendbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_reduce_REAL1_2

subroutine ptk_allreduce_REAL1_2(sendbuf,recvbuf,op,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  if(size(sendbuf) /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Allreduce(sendbuf,recvbuf,size(sendbuf),__PTK_MPI_REAL1, &
                     op%op,comm_loc%comm,ierr_loc)
#else
  recvbuf = sendbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allreduce_REAL1_2

subroutine ptk_reduce_ip_REAL1_2(buffer,op,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout)  :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,ierr_loc,rank
  type(ptk_comm) :: comm_loc
# 280 "ptk_multitype.spp"
  REAL (kind=__PTK_REAL1) :: tempbuf (lbound(buffer,1):ubound(buffer,1),lbound(buffer,2):ubound(buffer,2))
# 282 "ptk_multitype.spp"
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Reduce(buffer,tempbuf,size(buffer),__PTK_MPI_REAL1, &
                  op%op,root_loc,comm_loc%comm,ierr_loc)
  if(ierr_loc/=0) goto 1
  call ptk_comm_rank(comm_loc,rank,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(rank==root_loc) buffer = tempbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_reduce_ip_REAL1_2

subroutine ptk_allreduce_ip_REAL1_2(buffer,op,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(inout) :: buffer (:,:)
  type(ptk_op),   intent(in)            :: op
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: ierr_loc
  type(ptk_comm) :: comm_loc
# 318 "ptk_multitype.spp"
  REAL (kind=__PTK_REAL1) :: tempbuf (lbound(buffer,1):ubound(buffer,1),lbound(buffer,2):ubound(buffer,2))
# 320 "ptk_multitype.spp"
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
#ifdef __PTK_MPI
  call MPI_Allreduce(buffer,tempbuf,size(buffer),__PTK_MPI_REAL1, &
                     op%op,comm_loc%comm,ierr_loc)
  if(ierr_loc/=0) goto 1
  buffer = tempbuf
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allreduce_ip_REAL1_2

# 338 "ptk_multitype.spp"
subroutine ptk_gather_REAL1_2(sendbuf,recvbuf,root,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:)
  integer,        intent(in),  optional :: root
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: root_loc,size_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  root_loc = 0
  if(present(root)) root_loc = root
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  call ptk_comm_size(comm_loc,size_loc,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(size(sendbuf) * size_loc /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Gather(sendbuf,size(sendbuf),__PTK_MPI_REAL1, &
                  recvbuf,size(sendbuf),__PTK_MPI_REAL1, &
                  root_loc,comm_loc%comm,ierr_loc)
#else
# 367 "ptk_multitype.spp"
  recvbuf = reshape(sendbuf,shape(recvbuf))
# 369 "ptk_multitype.spp"
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_gather_REAL1_2

subroutine ptk_allgather_REAL1_2(sendbuf,recvbuf,comm,ierr)
  use ptk_interface
  use ptk_base
  use ptk_mpi_module
  implicit none
  REAL (kind=__PTK_REAL1), intent(in)  :: sendbuf (:,:)
  REAL (kind=__PTK_REAL1), intent(out) :: recvbuf (:,:,:)
  type(ptk_comm), intent(in),  optional :: comm
  integer,        intent(out), optional :: ierr
  integer :: size_loc,ierr_loc
  type(ptk_comm) :: comm_loc
  ierr_loc = 0
  comm_loc = ptk_comm_world
  if(present(comm)) comm_loc = comm
  call ptk_comm_size(comm_loc,size_loc,ierr_loc)
  if(ierr_loc/=0) goto 1
  if(size(sendbuf) * size_loc /= size(recvbuf)) ierr_loc = __LINE__
  if(ierr_loc/=0) goto 1
#ifdef __PTK_MPI
  call MPI_Allgather(sendbuf,size(sendbuf),__PTK_MPI_REAL1, &
                     recvbuf,size(sendbuf),__PTK_MPI_REAL1, &
                     comm_loc%comm,ierr_loc)
#else
# 404 "ptk_multitype.spp"
  recvbuf = reshape (sendbuf,shape(recvbuf))
# 406 "ptk_multitype.spp"
#endif
1 continue
  if(present(ierr)) then
    ierr = ierr_loc
  else
    if(ierr_loc/=0) call ptk_error_handler(ierr_loc)
  end if
end subroutine ptk_allgather_REAL1_2

# 416 "ptk_multitype.spp"

#endif
#endif

subroutine ptk_dummy_REAL1_2
write(0,*)
end subroutine ptk_dummy_REAL1_2

