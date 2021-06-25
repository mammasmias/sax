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
module pw_basis_module
use pw_common_module
use pw_struct_module
implicit none
private
public :: pw_basis_int_kind,   &
          pw_basis,            &
          pw_basis_init,       &
          pw_basis_destroy,    &
          pw_basis_set_struct, &
          pw_basis_create,     &
          pw_basis_set_npw,    &
          pw_basis_iolength,   &
          pw_basis_maxiolength,&
          pw_basis_write,      &
          pw_basis_get_cutoff, &
          pw_basis_read,       &
          pw_basis_bcast,      &
          assignment(=),       &
          operator(==),        &
          pw_basis_apply_symm, &
          pw_basis_set_kmesh,  &
          pw_basis_sort_index, &
          pw_basis_sort,       &
          pw_basis_set_g_extr
!@ END MANUAL

!@ MANUAL
type pw_basis
  type(pw_struct),                 pointer :: struct
  integer                                  :: npw
  real                                     :: r0(3),k(3)
  logical                                  :: conjg
  integer(kind=pw_basis_int_kind), pointer :: g(:,:)
  integer                                  :: gmax(3),gmin(3)
  integer                                  :: index_Gzero
end type pw_basis
! This object contains a plane-waves grid, a pointer to the
!   pw_struct object with lattice parameters
! struct   :: pointer to the struct object
! npw      :: number of plane waves
! r0(3)    :: origin of the grid
! k(3)     :: k vector, in unit of struct%b
!             NOTE this is the effective vector, eventually including the time-reversal
! conjg    :: a flag for time-reversal. if set to true, the Fourier coefficients
!             should be conjugated in reciprocal space.
! CHECK IF EVERYWHERE conjg IS CONSISTENT!!!
! g(3,npw) :: grid, in unit of struct%b
! gmax,gmin :: private utilities; calculated as
!                                gmax(1) = maxval(g(1,:)) ; gmax(2) = maxval(g(2,:)) gmax(3) = maxval(g(3,:))
!                                gmin(1) = minval(g(1,:)) ; gmin(2) = minval(g(2,:)) gmin(3) = minval(g(3,:))
!@ END MANUAL

!@ MANUAL
interface operator(==)
  module procedure pw_basis_is_equal
end interface
interface assignment(=)
  module procedure pw_basis_copy
end interface
interface pw_basis_init
  module procedure pw_basis_init00
  module procedure pw_basis_init10
end interface
interface pw_basis_destroy
  module procedure pw_basis_destroy0
  module procedure pw_basis_destroy1
end interface
interface pw_basis_create
  module procedure pw_basis_create000
  module procedure pw_basis_create110
  module procedure pw_basis_create1mesh0
end interface
interface pw_basis_bcast
  module procedure pw_basis_bcast0
  module procedure pw_basis_bcast1
end interface
interface pw_basis_get_cutoff
  module procedure pw_basis_get_cutoff0
  module procedure pw_basis_get_cutoff1
end interface
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_basis_init00(basis,struct)
! Initializes a basis object
  type(pw_basis),                    intent(out) :: basis
  type(pw_struct), optional, intent(in)  :: struct
! struct :: structure to be pointed to. it can be omitted and assigned later
!@ END MANUAL
  basis%npw   = 0
  basis%r0    = 0.0
  basis%k     = 0.0
  basis%conjg = .false.
  basis%gmax  = 0
  basis%gmin  = 0
  allocate(basis%g(3,0))
  call pw_allocate(basis%g)
  nullify(basis%struct)
  if(present(struct)) call pw_basis_set_struct(basis,struct)
end subroutine pw_basis_init00

!@ MANUAL
subroutine pw_basis_init10(basis,struct)
! Initializes an array of basis objects, all af them pointing to the same struct
  type(pw_basis),                    intent(out) :: basis(:)
  type(pw_struct), optional, intent(in)  :: struct
! struct :: structure to be pointed to. it can be omitted and assigned later
!@ END MANUAL
  integer :: i
  do i=1,size(basis)
    call pw_basis_init(basis(i),struct)
  end do
end subroutine pw_basis_init10

!@ MANUAL
subroutine pw_basis_destroy0(basis)
! Destroys an object
  type(pw_basis), intent(inout) :: basis
!@ END MANUAL
!  if(.not.associated(basis%g)) ERROR("")
  call pw_deallocate(basis%g)
  if(associated(basis%g)) deallocate(basis%g)
end subroutine pw_basis_destroy0

!@ MANUAL
subroutine pw_basis_destroy1(basis)
! Destroys an array of basis objects
  type(pw_basis), intent(inout) :: basis(:)
!@ END MANUAL
  integer :: i
  do i=1,size(basis)
    call pw_basis_destroy0(basis(i))
  end do
end subroutine pw_basis_destroy1

!@ MANUAL
subroutine pw_basis_set_struct(basis,struct)
! Associates a struct to a basis
  type(pw_basis),          intent(inout) :: basis
  type(pw_struct), target, intent(in)    :: struct
!@ END MANUAL
  if(.not.associated(basis%g)) ERROR("")
  basis%struct => struct
end subroutine pw_basis_set_struct

!@ MANUAL
subroutine pw_basis_sort_index(basis,index)
! Calculate the ordering indices so that the vectors g(:,index)
! are ordered by increasing module of the total wave vector (k+G)
  use num_module
  use numrec_module, only : numrec_indexx
  type(pw_basis), intent(in)  :: basis
  integer,        intent(out) :: index(:)
!@ END MANUAL
  real            :: tmp_mod(size(index))
  real            :: b(3,3)
  real            :: k(3)
  real, parameter :: epsilon(3) = (/1.0d-12,1.0d-9,1.0d-6/)
  integer :: ipw,npw

  real :: tmp(3)

  npw = basis%npw
  if(size(index)<npw) ERROR("")
  b = basis%struct%b
  k = basis%k
  do ipw=1,npw
    tmp = num_matmul(b,real(basis%g(:,ipw))+k)
    tmp_mod(ipw) = sum(tmp**2) &
                 + sum(epsilon*real(basis%g(:,ipw)))
  end do 
  call numrec_indexx(npw,tmp_mod(1:npw),index(1:npw))
end subroutine pw_basis_sort_index

!@ MANUAL
subroutine pw_basis_sort(basis,index)
! Sort the basis by increasing module of the total wave vector (k+G)
! and returns the indeices array as in pw_basis_sort_index
  type(pw_basis), intent(inout) :: basis
  integer,        intent(in)    :: index(:)

  integer :: ipw1
  integer :: g(3,basis%npw)
!@ END MANUAL
  if(size(index)<basis%npw) ERROR("")
  g(:,:) = 0
  do ipw1=1,basis%npw
    g(:,ipw1) = basis%g(:,index(ipw1))
  enddo
  basis%g(:,:) = g(:,:)
  do ipw1=1,basis%npw
    if(basis%g(1,ipw1)/=0)cycle
    if(basis%g(2,ipw1)/=0)cycle
    if(basis%g(3,ipw1)/=0)cycle
      basis%index_Gzero=ipw1
  enddo
end subroutine pw_basis_sort

!@ MANUAL
subroutine pw_basis_create000(basis,k,cutoff)
! Builds a basis grid with a given cut-off. Every information already
! present in the basis is reset, except for the struct pointer.
  use num_module
  type(pw_basis), intent(inout) :: basis
  real,           intent(in)    :: k(3)
  real,           intent(in)    :: cutoff
! k(:)   :: wave vector, is the center of the cutoff sphere and also sets the %k component
! cutoff :: kinetic energy cutoff, so that |G+k|^2 < cutoff
!           (consequently, it is in RYDBERG units)
!@ END MANUAL
  integer :: maxvec(3),minvec(3),i1,i2,i3,npwmax,npw
  real    :: b(3,3),a(3,3)
  integer(kind=pw_basis_int_kind),allocatable :: tmp(:,:)
  integer,                        allocatable :: index(:)
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  basis%k = k
  basis%conjg=.false.
  b = basis%struct%b
  a = basis%struct%a
! these numbers are the maximum useful integer components
  maxvec(1) = - k(1) + sqrt( cutoff * sum(a(:,1)**2) ) / num_2pi +2
  minvec(1) = - k(1) - sqrt( cutoff * sum(a(:,1)**2) ) / num_2pi -2
  maxvec(2) = - k(2) + sqrt( cutoff * sum(a(:,2)**2) ) / num_2pi +2
  minvec(2) = - k(2) - sqrt( cutoff * sum(a(:,2)**2) ) / num_2pi -2
  maxvec(3) = - k(3) + sqrt( cutoff * sum(a(:,3)**2) ) / num_2pi +2
  minvec(3) = - k(3) - sqrt( cutoff * sum(a(:,3)**2) ) / num_2pi -2
  npwmax = product(maxvec-minvec+1)
  allocate(tmp(3,npwmax),index(npwmax))
  npw = 0
  do i3=minvec(3),maxvec(3)
    do i2=minvec(2),maxvec(2)
      do i1=minvec(1),maxvec(1)
        if (sum(num_matmul(b,(/i1,i2,i3/)+k)**2)<=cutoff) then
          npw=npw+1
          tmp(:,npw)=(/i1,i2,i3/)
        end if
      end do
    end do
  end do
  call pw_basis_set_npw(basis,npw)
  basis%g = tmp(:,1:npw)
  call pw_basis_sort_index(basis,index)
  call pw_basis_sort(basis,index)
  deallocate(tmp,index)
  call pw_basis_set_g_extr(basis)
end subroutine pw_basis_create000

!@ MANUAL
subroutine pw_basis_create110(basis,k,cutoff)
! Similar to pw_basis_create000, but creates more basis in one shot, with different
! k vectors.
  type(pw_basis), intent(inout) :: basis(:)
  real,           intent(in)    :: k(:,:)
  real,           intent(in)    :: cutoff
!@ END MANUAL
  integer :: i
  if(size(basis)/=ubound(k,2)) ERROR("")
  if(ubound(k,1)/=3) ERROR("")
  do i=1,size(basis)
    call pw_basis_create(basis(i),k(:,i),cutoff)
  end do
end subroutine pw_basis_create110

!@ MANUAL
function pw_basis_get_cutoff0(basis)
! Calculate the maximum value of |G+k|^2 for a basis object
  use num_module
  real :: pw_basis_get_cutoff0
  type (pw_basis), intent(in) :: basis
!@ END MANUAL
  real :: max, b(3,3),k(3),actual
  integer :: npw,ipw
  max=0.0
  b = basis%struct%b
  npw = basis%npw
  k = basis%k
  do ipw=1,npw
    actual = sum(num_matmul(b,basis%g(:,ipw)+k)**2)
    if(actual > max) max = actual
  end do
  pw_basis_get_cutoff0 = max
end function pw_basis_get_cutoff0

!@ MANUAL
function pw_basis_get_cutoff1(basis)
! Similar to pw_basis_get_cutoff0, but get the maximum values
! in an array of basis
  real :: pw_basis_get_cutoff1
  type (pw_basis), intent(in) :: basis(:)
!@ END MANUAL
  real :: max,actual
  integer :: i
  max = 0.0
  do i =1,size(basis)
    actual = pw_basis_get_cutoff(basis(i))
    if(actual > max) max = actual
  end do
  pw_basis_get_cutoff1 = max
end function pw_basis_get_cutoff1

!@ MANUAL
subroutine pw_basis_create1mesh0(basis,kmesh,cutoff)
! Similar to pw_basis_create000, but takes as an input a kmesh
! object
  use pw_kmesh_module
  type(pw_basis), intent(inout) :: basis(:)
  type(pw_kmesh), intent(in)    :: kmesh
  real                          :: cutoff
!@ END MANUAL
  integer :: i,n
  n = size(basis)
  if(n/=kmesh%nkibz) ERROR("")
  do i=1,n
    call pw_basis_create(basis(i),kmesh%kibz(:,i),cutoff)
  end do
end subroutine pw_basis_create1mesh0

!@ MANUAL
subroutine pw_basis_set_g_extr(basis)
! Recalculates the internal component gmax and gmin
  type(pw_basis), intent(inout) :: basis
!@ END MANUAL
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  basis%gmax(1) = maxval(basis%g(1,:))
  basis%gmax(2) = maxval(basis%g(2,:))
  basis%gmax(3) = maxval(basis%g(3,:))
  basis%gmin(1) = minval(basis%g(1,:))
  basis%gmin(2) = minval(basis%g(2,:))
  basis%gmin(3) = minval(basis%g(3,:))
end subroutine pw_basis_set_g_extr

!@ MANUAL
subroutine pw_basis_set_npw(basis,npw)
! Sets the number of plane waves and evetually reallocates the grid
  type(pw_basis), intent(inout) :: basis
  integer,        intent(in)    :: npw
!@ END MANUAL
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  if(basis%npw /= npw) then
    basis%npw = npw
    call pw_deallocate(basis%g)
    deallocate(basis%g)
    allocate(basis%g(3,npw))
    call pw_allocate(basis%g)
  end if
  basis%g = 0
end subroutine pw_basis_set_npw

!@ MANUAL
function pw_basis_iolength(basis)
! Calculates the iolength for a direct access write
! Then, a file for storing basis with the same size can be opened with:
!     open(unit=1,file="basisfile",access="direct",recl=pw_basis_iolength(basis))
  integer                  :: pw_basis_iolength
  type(pw_basis), intent(in) :: basis
!@ END MANUAL
  inquire(iolength=pw_basis_iolength) &
          basis%npw,basis%k,basis%r0,basis%conjg,basis%g
end function pw_basis_iolength

!@ MANUAL
function pw_basis_maxiolength(basis)
! Calculates the iolength for a direct access write able to contain
! all the basis passed as argument, i.e. the maximum one.
! See also pw_basis_iolength
  integer                    :: pw_basis_maxiolength
  type(pw_basis), intent(in) :: basis(:)
!@ END MANUAL
  integer                    :: iolength(size(basis)),i
  do i=1,size(basis)
    iolength(i)=pw_basis_iolength(basis(i))
  end do
  pw_basis_maxiolength = maxval(iolength)
end function pw_basis_maxiolength

!@ MANUAL
subroutine pw_basis_write(basis,unit,fmt,rec,name)
  use iotk_module
! Writes an object on an unit.
  type(pw_basis),         intent(in) :: basis
  integer,                intent(in) :: unit
  character(*), optional, intent(in) :: fmt
  integer,      optional, intent(in) :: rec
  character(*), optional, intent(in) :: name
! fmt   :: format, can be
!   "iotk"  -> standard xml file [default]
!   "dir"   -> file binario sequenziale
! rec   :: used when fmt="dir", is the number of the record where the basis will be written
! name  :: name of the object written (only for "iotk")
!@ END MANUAL
  character(100) :: fmt_local
  integer        :: rec_local
  character(len=iotk_namlenx) :: name_loc
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  fmt_local = "iotk"
  rec_local = -1
  name_loc = ""
  if(present(fmt)) fmt_local = fmt
  if(present(rec)) rec_local = rec
  if(present(name)) name_loc = name
  if(fmt_local == "iotk" .and. .not. present(name)) ERROR("")
  if(fmt_local == "dir"  .and. .not. present(rec)) ERROR("")
  call pw_basis_write_external(basis,unit,fmt_local,rec_local,name_loc)
end subroutine pw_basis_write

!@ MANUAL
subroutine pw_basis_read(basis,unit,fmt,rec,name)
! Reads an object from an unit.
  use iotk_module
  type(pw_basis),         intent(inout) :: basis
  integer,                intent(in)    :: unit
  character(*), optional, intent(in)    :: fmt
  integer,      optional, intent(in)    :: rec
  character(*), optional, intent(in)    :: name
! fmt   :: format, can be
!   "iotk"  -> standard xml file [default]
!   "dir"   -> file binario sequenziale
!   "pw104" -> file style 'grid' produced by pw104 [Alice Ruini's version]
!@ END MANUAL
  character(100) :: fmt_local
  character(iotk_namlenx) :: name_loc
  integer        :: rec_local
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  fmt_local = "iotk"
  rec_local = -1
  name_loc = ""
  if(present(fmt)) fmt_local = fmt
  if(present(rec)) rec_local = rec
  if(present(name)) name_loc = name
  if(fmt_local == "iotk" .and. .not. present(name)) ERROR("")
  if(fmt_local == "dir"  .and. .not. present(rec)) ERROR("")
  call pw_basis_read_external(basis,unit,fmt_local,rec_local,name_loc)
end subroutine pw_basis_read

!@ MANUAL
function pw_basis_is_equal(basis1,basis2)
! Compares two objects
! Interfaced with operator(==)
  logical pw_basis_is_equal
  type(pw_basis), intent(in) :: basis1,basis2
!@ END MANUAL
  if(.not.associated(basis1%g)) ERROR("")
  if(.not.associated(basis1%struct)) ERROR("")
  if(.not.associated(basis2%g)) ERROR("")
  if(.not.associated(basis2%struct)) ERROR("")
! Shortcut if the two basis are the same one
  if(associated(basis1%g,basis2%g)) then
    pw_basis_is_equal=.true.
    return
  end if
  pw_basis_is_equal = (basis1%struct == basis2%struct) .and. &
&                     all(basis1%k  - basis2%k  < 1e-6) .and. &
&                     all(basis1%r0 - basis2%r0 < 1e-6) .and. &
&                     basis1%npw == basis2%npw          .and. &
&   ((basis1%conjg.and.basis2%conjg).or.(.not.basis1%conjg.and..not.basis2%conjg)) .and. &
&                     all(basis1%g == basis2%g)        
end function pw_basis_is_equal

!@ MANUAL
subroutine pw_basis_copy(new_basis,old_basis)
! Copies an object
! Interfaced with operator(=)
  type (pw_basis), intent(inout) :: new_basis
  type (pw_basis), intent(in)    :: old_basis
!@ END MANUAL
  if(.not.associated(old_basis%g)) ERROR("")
  if(.not.associated(old_basis%struct)) ERROR("")
  if(.not.associated(new_basis%g)) ERROR("")
  if(.not.associated(new_basis%struct)) ERROR("")
  if(.not.new_basis%struct == old_basis%struct) ERROR("")
  call pw_basis_set_npw(new_basis,old_basis%npw)
  new_basis%k  = old_basis%k
  new_basis%r0 = old_basis%r0
  new_basis%g  = old_basis%g
  new_basis%conjg  = old_basis%conjg
  new_basis%gmax  = old_basis%gmax
  new_basis%gmin  = old_basis%gmin
end subroutine pw_basis_copy

!@ MANUAL
subroutine pw_basis_apply_symm(new_basis,old_basis,symm)
! Applies a symmetry operation to a basis
  use num_module
  use pw_symm_module
  type (pw_basis), intent(inout) :: new_basis
  type (pw_basis), intent(in)    :: old_basis
  type (pw_symm),  intent(in)    :: symm
!@ END MANUAL
  integer :: ipw
  if(.not.associated(old_basis%g)) ERROR("")
  if(.not.associated(old_basis%struct)) ERROR("")
  if(.not.associated(new_basis%g)) ERROR("")
  if(.not.new_basis%struct == old_basis%struct) ERROR("")
  call pw_basis_set_npw(new_basis,old_basis%npw)
  new_basis%k  = num_matmul(real(symm%grot),old_basis%k)
  new_basis%r0 = num_matmul(real(symm%rrot),old_basis%r0) + symm%rshift
  new_basis%conjg  = (old_basis%conjg.and..not.symm%conjg) .or. &
                     (.not.old_basis%conjg.and.symm%conjg)
  do ipw=1,new_basis%npw
    new_basis%g(:,ipw) = num_matmul(int(symm%grot),int(old_basis%g(:,ipw)))
  end do
  new_basis%gmax = num_matmul(symm%grot,old_basis%gmax)
  new_basis%gmin = num_matmul(symm%grot,old_basis%gmin)
  call pw_basis_set_g_extr(new_basis)
end subroutine pw_basis_apply_symm

!@ MANUAL
subroutine pw_basis_bcast0(basis,root,comm)
  use ptk_module, only : ptk_comm
! Bcasts an object
  type(pw_basis), intent(inout) :: basis
  integer,        intent(in)    :: root
  type(ptk_comm), intent(in)    :: comm
!@ END MANUAL
  if(.not.associated(basis%g)) ERROR("")
  if(.not.associated(basis%struct)) ERROR("")
  call pw_basis_bcast_external(basis,root,comm)
end subroutine pw_basis_bcast0

!@ MANUAL
subroutine pw_basis_bcast1(basis,root,comm)
  use ptk_module, only : ptk_comm
! Bcasts an array of objects
  type(pw_basis), intent(inout) :: basis(:)
  integer,        intent(in)    :: root
  type(ptk_comm), intent(in)    :: comm
!@ END MANUAL
  integer :: i
  do i=1,size(basis)
    call pw_basis_bcast(basis(i),root,comm)
  end do
end subroutine pw_basis_bcast1

!@ MANUAL
subroutine pw_basis_set_kmesh(basis,kmesh)
! Forces the k vectors of an array of basis on a k mesh
! Does not affect the basis in any other way
  use pw_kmesh_module
  type (pw_basis), intent(inout) :: basis(:)
  type (pw_kmesh), intent(in)    :: kmesh
!@ END MANUAL
  integer :: i
  if(size(basis)/=kmesh%nkibz) ERROR("")
  do i=1,size(basis)
    basis(i)%k(:) = kmesh%kibz(:,i)
  end do
end subroutine pw_basis_set_kmesh

end module pw_basis_module
