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
module pw_pseudo_module
use pw_common_module
use num_module
implicit none
! Module containing the pw_pseudo type and its methods
private
public :: pw_pseudo,           &
          pw_pseudo_init,      &
          pw_pseudo_destroy,   &
          pw_pseudo_read,      &
          pw_pseudo_readbcast, &
          pw_pseudo_bcast,     &
          pw_pseudo_write,     &
          pw_pseudo_set_table
!@ END MANUAL

!@ MANUAL
type pw_pseudo
  real             :: z               ! Ionic charge
  integer          :: nbeta           ! Number of nonlocal projectors
  integer          :: nmesh           ! Number of points in mesh
  real, pointer    :: mesh(:)         ! Mesh [nmesh]
  real, pointer    :: wmesh(:)        ! weights for integrals [nmesh]
  real, pointer    :: vloc(:)         ! Local potential [nmesh]
  integer, pointer :: lbeta(:)
  integer, pointer :: mesh_beta(:)
  real, pointer    :: beta(:,:)       ! Non-local projectors [nmesh,nbeta]
  real, pointer    :: d(:)            ! D_ii factors (diagonal for NC) [nbeta]
  real             :: cutoff          ! Maximum useful cutoff (Ry)
  type(num_interpolation), &
           pointer :: interpolation(:)      ! Interpolation table
end type pw_pseudo
!@ END MANUAL

!@ MANUAL
interface pw_pseudo_init
  module procedure pw_pseudo_init0
  module procedure pw_pseudo_init1
end interface
interface pw_pseudo_destroy
  module procedure pw_pseudo_destroy0
  module procedure pw_pseudo_destroy1
end interface
interface pw_pseudo_bcast
  module procedure pw_pseudo_bcast0
  module procedure pw_pseudo_bcast1
end interface
!@ END MANUAL


contains

!@ MANUAL
subroutine pw_pseudo_init0(pseudo)
  type (pw_pseudo), intent(out) :: pseudo
!@ END MANUAL
  pseudo%z    = 0.0
  pseudo%nbeta = 0
  pseudo%nmesh = 0
  allocate(pseudo%mesh(0))
  call pw_allocate(pseudo%mesh)
  allocate(pseudo%wmesh(0))
  call pw_allocate(pseudo%wmesh)
  allocate(pseudo%vloc(0))
  call pw_allocate(pseudo%vloc)
  allocate(pseudo%lbeta(0))
  call pw_allocate(pseudo%lbeta)
  allocate(pseudo%mesh_beta(0))
  call pw_allocate(pseudo%mesh_beta)
  allocate(pseudo%beta(0,0))
  call pw_allocate(pseudo%beta)
  allocate(pseudo%d(0))
  call pw_allocate(pseudo%d)
  allocate(pseudo%interpolation(0))
  pseudo%cutoff = 0.0
end subroutine pw_pseudo_init0

subroutine pw_pseudo_init1(pseudo)
  type (pw_pseudo), intent(out) :: pseudo(:)
  integer ::i
  do i=1,size(pseudo)
    call pw_pseudo_init(pseudo(i))
  end do
end subroutine pw_pseudo_init1

!@ MANUAL
subroutine pw_pseudo_destroy0(pseudo)
  type (pw_pseudo), intent(inout) :: pseudo
  integer ::i
!@ END MANUAL
  pseudo%z    = 0.0
  pseudo%nbeta = 0
  pseudo%nmesh = 0
  call pw_deallocate(pseudo%mesh)
  deallocate(pseudo%mesh)
  call pw_deallocate(pseudo%wmesh)
  deallocate(pseudo%wmesh)
  call pw_deallocate(pseudo%vloc)
  deallocate(pseudo%vloc)
  call pw_deallocate(pseudo%lbeta)
  deallocate(pseudo%lbeta)
  call pw_deallocate(pseudo%mesh_beta)
  deallocate(pseudo%mesh_beta)
  call pw_deallocate(pseudo%beta)
  deallocate(pseudo%beta)
  call pw_deallocate(pseudo%d)
  deallocate(pseudo%d)
  do i=1,size(pseudo%interpolation)
    call num_interpolation_destroy(pseudo%interpolation(i))
  end do
  deallocate(pseudo%interpolation)
  pseudo%cutoff = 0.0
end subroutine pw_pseudo_destroy0

subroutine pw_pseudo_destroy1(pseudo)
  type (pw_pseudo), intent(inout) :: pseudo(:)
  integer ::i
  do i=1,size(pseudo)
    call pw_pseudo_destroy(pseudo(i))
  end do
end subroutine pw_pseudo_destroy1

subroutine pw_pseudo_set_table(pseudo,cutoff)
  use tools_module
  use num_module
  type (pw_pseudo), intent(inout) :: pseudo
  real,             intent(in)    :: cutoff
  integer :: nr
  real    :: q_max,q,delta_q
  integer :: ir,iq,ibeta,l
  real    :: aux(pseudo%nmesh),aux1(pseudo%nmesh)
  pseudo%cutoff = cutoff
  q_max = sqrt(2.0*cutoff)
  delta_q = 0.01
  deallocate(pseudo%interpolation)
  allocate(pseudo%interpolation(pseudo%nbeta))
  do ibeta=1,pseudo%nbeta
    call num_interpolation_init(pseudo%interpolation(ibeta),0.0,q_max, &
                                delta_q,parity=+1)
    nr = pseudo%mesh_beta(ibeta)
    if(nr>pseudo%nmesh) ERROR("")
    l = pseudo%lbeta(ibeta)
    do ir=1,nr
      aux(ir) = pseudo%beta(ir,ibeta)*pseudo%wmesh(ir)*pseudo%mesh(ir)**(l+1)
    end do
    do iq=0,pseudo%interpolation(ibeta)%n
      q = pseudo%interpolation(ibeta)%x(iq)
      do ir=1,nr
        aux1(ir) = aux(ir) * num_xmlsphbes(q*pseudo%mesh(ir),l)
      end do
      pseudo%interpolation(ibeta)%y(iq) = &
              num_4pi*num_simpson(aux1(1:nr))
    end do
  end do
end subroutine pw_pseudo_set_table

!@ MANUAL
subroutine pw_pseudo_set_dim(pseudo,nbeta,nmesh)
  type (pw_pseudo), intent(inout) :: pseudo
  integer,          intent(in)    :: nbeta,nmesh
!@ END MANUAL
  call pw_pseudo_destroy(pseudo)
  pseudo%nbeta = nbeta
  pseudo%nmesh = nmesh
  allocate(pseudo%mesh(nmesh))
  call pw_allocate(pseudo%mesh)
  allocate(pseudo%wmesh(nmesh))
  call pw_allocate(pseudo%wmesh)
  allocate(pseudo%vloc(nmesh))
  call pw_allocate(pseudo%vloc)
  allocate(pseudo%lbeta(nbeta))
  call pw_allocate(pseudo%lbeta)
  allocate(pseudo%mesh_beta(nbeta))
  call pw_allocate(pseudo%mesh_beta)
  allocate(pseudo%beta(nmesh,nbeta))
  call pw_allocate(pseudo%beta)
  allocate(pseudo%d(nbeta))
  call pw_allocate(pseudo%d)
  allocate(pseudo%interpolation(0))
end subroutine pw_pseudo_set_dim

subroutine pw_pseudo_bcast1(pseudo,root,comm)
  use ptk_module, only : ptk_comm
  type (pw_pseudo), intent(inout) :: pseudo(:)
  integer,          intent(in)    :: root
  type (ptk_comm),  intent(in)    :: comm
  integer :: i
  do i=1,size(pseudo)
    call pw_pseudo_bcast(pseudo(i),root,comm)
  end do
end subroutine pw_pseudo_bcast1

!@ MANUAL
subroutine pw_pseudo_bcast0(pseudo,root,comm)
  use ptk_module, only : ptk_comm_rank,ptk_bcast,ptk_comm
  type (pw_pseudo), intent(inout) :: pseudo
  integer,          intent(in)    :: root
  type (ptk_comm),  intent(in)    :: comm
!@ END MANUAL
  integer :: rank,nbeta,nmesh
  call ptk_comm_rank(comm,rank)
  nbeta = pseudo%nbeta
  nmesh = pseudo%nmesh
  call ptk_bcast(nbeta,root,comm)
  call ptk_bcast(nmesh,root,comm)
  if(rank/=root) then
    call pw_pseudo_set_dim(pseudo,nbeta,nmesh)
  end if
  call ptk_bcast(pseudo%z,root,comm)
  call ptk_bcast(pseudo%mesh,root,comm)
  call ptk_bcast(pseudo%wmesh,root,comm)
  call ptk_bcast(pseudo%vloc,root,comm)
  call ptk_bcast(pseudo%lbeta,root,comm)
  call ptk_bcast(pseudo%mesh_beta,root,comm)
  call ptk_bcast(pseudo%beta,root,comm)
  call ptk_bcast(pseudo%d,root,comm)
  call ptk_bcast(pseudo%cutoff,root,comm)
end subroutine pw_pseudo_bcast0

subroutine pw_pseudo_readbcast(pseudo,unit,fmt,root,comm)
  use ptk_module, only : ptk_comm_rank,ptk_comm
  type (pw_pseudo), intent(inout) :: pseudo
  integer,          intent(in)    :: unit
  character(*),     intent(in)    :: fmt
  integer,          intent(in)    :: root
  type (ptk_comm),  intent(in)    :: comm

  integer :: rank

  call ptk_comm_rank(comm,rank)
  if(rank==root) then
    call pw_pseudo_read(pseudo,unit,fmt)
  end if
  call pw_pseudo_bcast(pseudo,root,comm)
end subroutine pw_pseudo_readbcast

subroutine pw_pseudo_read(pseudo,unit,fmt)
  use tools_module
  use iotk_module
  type (pw_pseudo), intent(inout) :: pseudo
  integer,          intent(in)    :: unit
  character(*),     intent(in)    :: fmt
  integer :: iostat
  character(50) :: dummy
  logical       :: ldummy
  real          :: z
  integer :: nmesh,nbeta,idummy,ibeta,idummy2

  select case(fmt)
  case("upf")
    rewind(unit)
    call iotk_scan_begin(unit,"PP_HEADER")
    read(unit,*,iostat=iostat) dummy
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) dummy
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) dummy
    IOCHECK(unit,iostat)
    if(.not. tools_uppercase(trim(dummy)) == "NC") ERROR(trim(dummy))
    read(unit,*,iostat=iostat) ldummy
    IOCHECK(unit,iostat)
    if(ldummy) WARNING("")
    read(unit,*,iostat=iostat) dummy
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) z
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) dummy
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) dummy
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) dummy
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) nmesh
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) idummy,nbeta
    IOCHECK(unit,iostat)

    call pw_pseudo_set_dim(pseudo,nbeta,nmesh)
    pseudo%z = z

    rewind(unit)

    call iotk_scan_begin(unit,"PP_MESH")
    call iotk_scan_begin(unit,"PP_R")
    read(unit,*,iostat=iostat) pseudo%mesh
    IOCHECK(unit,iostat)
    call iotk_scan_end  (unit,"PP_R")
    call iotk_scan_begin(unit,"PP_RAB")
    read(unit,*,iostat=iostat) pseudo%wmesh
    IOCHECK(unit,iostat)

    rewind(unit)

    call iotk_scan_begin(unit,"PP_LOCAL")
    read(unit,*,iostat=iostat) pseudo%vloc
    IOCHECK(unit,iostat)

    rewind(unit)

    call iotk_scan_begin(unit,"PP_NONLOCAL")
    do ibeta=1,nbeta
      call iotk_scan_begin(unit,"PP_BETA")
      read(unit,*,iostat=iostat) idummy,pseudo%lbeta(ibeta)
      IOCHECK(unit,iostat)
      if(idummy/=ibeta) ERROR(tools_char(idummy,10))
      read(unit,*,iostat=iostat) pseudo%mesh_beta(ibeta)
      IOCHECK(unit,iostat)
      pseudo%beta(:,ibeta) = 0.0
      read(unit,*,iostat=iostat) pseudo%beta(1:pseudo%mesh_beta(ibeta),ibeta)
      IOCHECK(unit,iostat)
      call iotk_scan_end(unit,"PP_BETA")
    end do
    call iotk_scan_begin(unit,"PP_DIJ")
    IOCHECK(unit,iostat)
    read(unit,*,iostat=iostat) idummy
    IOCHECK(unit,iostat)
    if(idummy/=nbeta) ERROR(tools_char(idummy,10))
    do ibeta=1,nbeta
      read(unit,*,iostat=iostat) idummy,idummy2,pseudo%d(ibeta)
      IOCHECK(unit,iostat)
      if(idummy/=ibeta) ERROR("")
      if(idummy2/=ibeta) ERROR("")
    end do
    rewind(unit)
    
  case default
    ERROR(fmt)
  end select

end subroutine pw_pseudo_read

subroutine pw_pseudo_write(pseudo,unit,fmt)
  type (pw_pseudo), intent(in) :: pseudo
  integer,          intent(in) :: unit
  character(*),     intent(in) :: fmt
  integer :: iostat,i
  select case (fmt)
  case("txt")
    write(unit,"(a)",iostat=iostat) "%PW_PSEUDO"
    IOCHECK(unit,iostat)
    write(unit,"(a,f15.9)",iostat=iostat) "z ",pseudo%z
    IOCHECK(unit,iostat)
    write(unit,"(a,f15.9)",iostat=iostat) "nbeta ",pseudo%nbeta
    IOCHECK(unit,iostat)
    write(unit,"(a,f15.9)",iostat=iostat) "nmesh ",pseudo%nmesh
    IOCHECK(unit,iostat)
    write(unit,"(a)",iostat=iostat)       "mesh        wmesh"
    IOCHECK(unit,iostat)
    do i=1,pseudo%nmesh
      write(unit,"(f15.9)",iostat=iostat)    pseudo%mesh(i),pseudo%wmesh(i)
      IOCHECK(unit,iostat)
    end do
! DAFINIRE
    
    
  case default
    ERROR(fmt)
  end select
end subroutine pw_pseudo_write

end module pw_pseudo_module
