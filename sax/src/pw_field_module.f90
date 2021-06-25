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
module pw_field_module
use pw_common_module
use pw_struct_module
use tools_module
implicit none
private
public :: pw_field,            &
          pw_field_init,       &
          pw_field_destroy,    &
          pw_field_set_struct, &
          pw_field_set_dim,    &
          assignment(=),       &
          operator(==),        &
          pw_field_random,     &
          pw_field_norm,       &
          pw_field_normalize,  &
          pw_field_braket,     &
          pw_field_scale,      &
          pw_field_mul,        &
          pw_field_umul,        &
          pw_field_write,     &
          pw_field_gaussian, &
          pw_field_grid,       &
          pw_field_sum,pw_field_apply_symm, &
          pw_field_braket_potential, &
          pw_field_dim_from_basis, &
          pw_field_dim_from_dipole, &
          pw_field_add_density, &
          pw_field_calc_vxc
!@ END MANUAL

!@ MANUAL
interface operator(==)
  module procedure pw_field_is_equal
end interface
interface assignment(=)
  module procedure pw_field_copy
end interface
interface pw_field_init
  module procedure pw_field_init00
  module procedure pw_field_init10
  module procedure pw_field_init20
end interface
interface pw_field_destroy
  module procedure pw_field_destroy0
  module procedure pw_field_destroy1
  module procedure pw_field_destroy2
end interface
interface pw_field_set_dim
  module procedure pw_field_set_dim0
  module procedure pw_field_set_dim1
  module procedure pw_field_set_dim2
end interface
!@ END MANUAL

!@ MANUAL
type pw_field
  type(pw_struct), pointer :: struct
  integer                  :: dim(3),str2,str3
  real                     :: k(3),r0(3)
  complex,         pointer :: val(:)
end type pw_field
!@ END MANUAL

contains

!@ MANUAL
subroutine pw_field_init00(field,struct)
  type(pw_field),  intent(out)                  :: field
  type(pw_struct), intent(in), optional :: struct
!@ END MANUAL
  field%dim  = 0
  field%str2 = 0
  field%str3 = 0
  field%k    = 0.0
  field%r0   = 0.0
  allocate(field%val(0))
  call pw_allocate(field%val)
  if(present(struct)) call pw_field_set_struct(field,struct)
end subroutine pw_field_init00

subroutine pw_field_init10(field,struct)
  type(pw_field),  intent(out)                  :: field(:)
  type(pw_struct), intent(in), optional :: struct
  integer :: n,i
  n = size(field)
  do i=1,n
    if(present(struct)) call pw_field_init(field(i),struct)
    if(.not.present(struct)) call pw_field_init(field(i))
  end do
end subroutine pw_field_init10

subroutine pw_field_init20(field,struct)
  type(pw_field),  intent(out)                  :: field(:,:)
  type(pw_struct), intent(in), optional :: struct
  integer :: n1,n2,i1,i2
  n1 = ubound(field,1)
  n2 = ubound(field,2)
  do i2=1,n2
    do i1=1,n1
      if(present(struct)) call pw_field_init(field(i1,i2),struct)
      if(.not.present(struct)) call pw_field_init(field(i1,i2))
    end do
  end do
end subroutine pw_field_init20


!@ MANUAL
subroutine pw_field_destroy0(field)
  type(pw_field),  intent(inout) :: field
!@ END MANUAL
  if(.not.associated(field%val)) ERROR("")
  call pw_deallocate(field%val)
  deallocate(field%val)
end subroutine pw_field_destroy0

subroutine pw_field_destroy1(field)
  type(pw_field),  intent(inout) :: field(:)
  integer :: n,i
  n = size(field)
  do i=1,n
    call pw_field_destroy(field(i))
  end do
end subroutine pw_field_destroy1

subroutine pw_field_destroy2(field)
  type(pw_field),  intent(out) :: field(:,:)
  integer :: n1,n2,i1,i2
  n1 = ubound(field,1)
  n2 = ubound(field,2)
  do i2=1,n2
    do i1=1,n1
      call pw_field_destroy(field(i1,i2))
    end do
  end do
end subroutine pw_field_destroy2

!@ MANUAL
subroutine pw_field_set_struct(field,struct)
  type(pw_field),          intent(inout) :: field
  type(pw_struct), target, intent(in)    :: struct  
!@ END MANUAL
  if(.not.associated(field%val)) ERROR("")
  field%struct => struct
end subroutine pw_field_set_struct

!@ MANUAL
subroutine pw_field_set_dim0(field,dim,str2,str3,k,r0)
  use num_module
  type(pw_field), intent(inout)        :: field
  integer,        intent(in)           :: dim(3)
  integer,        intent(in), optional :: str2,str3
  real,           intent(in), optional :: k(3)
  real,           intent(in), optional :: r0(3)
!@ END MANUAL
  integer :: n
  if(.not.associated(field%val)) ERROR("")
  if(.not.associated(field%struct)) ERROR("")
  if(present(str2)) then
    if(str2<dim(1)) ERROR("")
    field%str2 = str2
  else
    field%str2 = dim(1)
  end if
  if(present(str3)) then
    if(str3<dim(2)*field%str2) ERROR("")
    field%str3 = str3
  else
    field%str3 = dim(2)*field%str2
  end if
  if(present(k)) then
    field%k = k
  else
    field%k = 0.0
  end if
  if(present(r0)) then
    field%r0 = r0
  else
    field%r0 = 0.0
  end if
  field%dim = dim
  n =  (dim(1)-1) + (dim(2)-1)*field%str2 + (dim(3)-1)*field%str3 + 1 
  if(size(field%val)==n) then
    field%val=0.0
    return
  end if
  call pw_deallocate(field%val)
  deallocate(field%val)
  allocate(field%val(n))
  call pw_allocate(field%val)
  field%val=0.0
end subroutine pw_field_set_dim0

subroutine pw_field_set_dim1(field,dim,k,r0)
  type(pw_field), intent(inout) :: field(:)
  integer,        intent(in)    :: dim(3)
  real,           intent(in)    :: k(3)
  real,           intent(in)    :: r0(3)
  integer :: n,i
  n=size(field)
  do i=1,n
    call pw_field_set_dim(field(i),dim=dim,k=k,r0=r0)
  end do
end subroutine pw_field_set_dim1

subroutine pw_field_set_dim2(field,dim,k,r0)
  type(pw_field), intent(inout) :: field(:,:)
  integer,        intent(in)    :: dim(3)
  real,           intent(in)    :: k(3)
  real,           intent(in)    :: r0(3)
  integer :: n1,n2,i1,i2
  n1=ubound(field,1)
  n2=ubound(field,2)
  do i2=1,n2
    do i1=1,n1
      call pw_field_set_dim(field(i1,i2),dim=dim,k=k,r0=r0)
    end do
  end do
end subroutine pw_field_set_dim2
  
subroutine pw_field_apply_symm(new_field,old_field,symm)
! CREDO CHE NON METTA LA TRASLAZIONE
  use num_module
  use pw_symm_module
  type(pw_field), intent(inout) :: new_field
  type(pw_field), intent(in)    :: old_field
  type(pw_symm),  intent(in)    :: symm
  real :: new_r0(3),new_k(3)
  integer :: i1,i2,i3,new_vec(3),i,new_i
  integer :: dim(3),str2,str3
  logical :: lconjg
  integer :: rrot(3,3)
  if(.not.associated(old_field%struct)) ERROR("")
  if(.not.associated(old_field%val)) ERROR("")
  if(.not.associated(new_field%val)) ERROR("")
  if(.not.associated(symm%struct,old_field%struct)) ERROR("")
  new_r0 = num_matmul(real(symm%rrot),old_field%r0)
  new_k  = num_matmul(real(symm%grot),old_field%k)
  dim  = old_field%dim
  str2 = old_field%str2
  str3 = old_field%str3
  lconjg = symm%conjg
  rrot   = symm%rrot
  call pw_field_set_dim(new_field,dim,str2,str3,new_k,new_r0)
  do i3=1,dim(3)
    do i2=1,dim(2)
      do i1=1,dim(1)
        i = (i1-1) + (i2-1)*str2 + (i3-1)*str3 +1
        new_vec = modulo(num_matmul(rrot,(/i1,i2,i3/)),dim)
        new_i = (new_vec(1)-1) + (new_vec(2)-1)*str2 + (new_vec(3)-1)*str3 + 1
        if(lconjg) then
          new_field%val(new_i)   = old_field%val(i)
        else
          new_field%val(new_i)   = conjg(old_field%val(i))
        end if
      end do
    end do
  end do
end subroutine pw_field_apply_symm
  

subroutine pw_field_write(field,unit)
  type(pw_field), intent(in) :: field
  integer,        intent(in) :: unit
  logical       :: opened
  character(20) :: form,access
  integer       :: iostat
  character(20) :: fmt_local
  complex :: field_tmp(1:field%dim(1),1:field%dim(2),1:field%dim(3))
  integer :: i,i1,i2,i3,str2,str3

  inquire(unit=unit,opened=opened,form=form,access=access,iostat=iostat)
  IOCHECK(unit,iostat)
  if(form=="FORMATTED" .or. .not. opened) then
    fmt_local = "txt"
  else if(opened .and. form=="UNFORMATTED") then
    fmt_local = "bin"
  else
    ERROR("")
  end if
  str2 = field%str2
  str3 = field%str3
  do i3=1,field%dim(3)
    do i2=1,field%dim(2)
      do i1=1,field%dim(1)
        i  = (i1-1) + (i2-1)*str2 + (i3-1)*str3 + 1
        field_tmp(i1,i2,i3) = field%val(i)
      end do
    end do
  end do
  select case(fmt_local)
  case("txt")
    write(unit,*) field%dim(1), field%dim(2), field%dim(3)
    write(unit,*) field%str2, field%str3
    write(unit,"(2f15.9)",iostat=iostat) field_tmp
    IOCHECK(unit,iostat)
  case("bin")
    write(unit) field%dim(1), field%dim(2), field%dim(3)
    write(unit) field%str2, field%str3
    write(unit,iostat=iostat) field_tmp
    IOCHECK(unit,iostat)
  case default
    ERROR("Unrecognized fmt - "//fmt_local)
  end select

end subroutine pw_field_write

function pw_field_is_equal(field1,field2)
  logical                    :: pw_field_is_equal
  type(pw_field), intent(in) :: field1,field2
  pw_field_is_equal = all(field1%k  - field2%k  < 1e-6) .and. &
&                     all(field1%r0 - field2%r0 < 1e-6) .and. &
&                     all(field1%dim == field2%dim    ) .and. &
&                     field1%str2 == field2%str2        .and. &
&                     field1%str3 == field2%str3        .and. &
&                     all(abs(field1%val - field2%val) < 1e-6 )
end function pw_field_is_equal

subroutine pw_field_copy(field1,field2)
  type(pw_field), intent(inout) :: field1
  type(pw_field), intent(in)    :: field2
  if(.not. all(field1%k  - field2%k  < 1e-6) .or. &
     .not. all(field1%r0 - field2%r0 < 1e-6) .or. &
&    .not. field1%str2 == field2%str2        .or. &
&    .not. field1%str3 == field2%str3        .or. &
     .not. all(field1%dim == field2%dim    ) ) ERROR("")
  field1%val = field2%val
end subroutine pw_field_copy

subroutine pw_field_random(field)
  type(pw_field), intent(inout)  :: field
  integer i1,i2,i3,str2,str3
  real r,i
  str2 = field%str2
  str3 = field%str3
  do i3=1,field%dim(3)
    do i2=1,field%dim(2)
      do i1=1,field%dim(1)
        call random_number(r)
        call random_number(i)
          field%val(((i1-1)+(i2-1)*str2+(i3-1)*str3)+1) = cmplx(r*2-1.,i*2-1.)
      end do
    end do
  end do
end subroutine pw_field_random

subroutine pw_field_gaussian(field,sigma,center)
  use num_module
  type(pw_field), intent(inout) :: field
  real,        intent(in)    :: sigma,center(3)
  integer :: i1,i2,i3,t1,t2,t3,tmin(3),tmax(3),i
  real    :: rmt(3),rmt2
  complex :: add
  tmin = -5
  tmax = +5
  field%val=0.0
  do i3=0,field%dim(3)-1
    do i2=0,field%dim(2)-1
      do i1=0,field%dim(1)-1
        i = (i1+i2*field%str2+i3*field%str3) +1
        do t3=tmin(3),tmax(3)
          do t2=tmin(2),tmax(2)
            do t1=tmin(1),tmax(1)
              rmt = real((/i1,i2,i3/))/field%dim - (/t1,t2,t3/) +field%r0-center
              rmt2 = sum(num_matmul(field%struct%a,rmt)**2)
              add = exp(cmplx(0.,1.) * dot_product(field%k,real((/t1,t2,t3/))) - &
&                              rmt2/(2.0*sigma**2))
              field%val(i) = field%val(i) + add
            end do
          end do
        end do
      end do
    end do
  end do
  call pw_field_normalize(field)
end subroutine pw_field_gaussian

function pw_field_norm(field)
  use num_module
  real                        :: pw_field_norm
  type (pw_field), intent(in) :: field
  pw_field_norm = num_nrm2(field%val)/sqrt(real(product(field%dim)))
end function pw_field_norm

subroutine pw_field_grid(field,grid)
  use num_module
  type(pw_field), intent(in) :: field
  real,           intent(out) :: grid(3,0:field%dim(1)-1,0:field%dim(2)-1,0:field%dim(3)-1)
  integer :: i1,i2,i3
  do i3=0,field%dim(3)-1
    do i2=0,field%dim(2)-1
      do i1=0,field%dim(1)-1
        grid(:,i1,i2,i3) = num_matmul(field%struct%a,(field%r0+(/i1,i2,i3/))/field%dim)
      end do
    end do
  end do
end subroutine pw_field_grid

subroutine pw_field_scale(field,scale)
  use num_module
  type (pw_field), intent(inout) :: field
  real                           :: scale
  field%val = field%val * scale
end subroutine pw_field_scale

subroutine pw_field_normalize(field)
  type (pw_field), intent(inout) :: field
  real :: norm
  norm = pw_field_norm(field)
  call pw_field_scale(field,1.0/norm)
end subroutine pw_field_normalize

function pw_field_braket(bra,ket)
  use num_module
  complex pw_field_braket
  type (pw_field), intent(in) :: bra,ket
    if(.not. all(bra%k  - ket%k  < 1e-6) .or. &
     .not. all(bra%r0 - ket%r0 < 1e-6) .or. &
&    .not. bra%str2 == ket%str2        .or. &
&    .not. bra%str3 == ket%str3        .or. &
     .not. all(bra%dim == ket%dim    ) ) ERROR("")
  pw_field_braket = dot_product(bra%val,ket%val) / product(bra%dim)
end function pw_field_braket

subroutine pw_field_sum(res,field1,field2)
  use num_module
  type(pw_field), intent(inout) :: res
  type(pw_field), intent(in)    :: field1,field2
  if(any(field1%dim/=field2%dim)) ERROR("")
  if(any(field1%dim/=res%dim)) ERROR("")
  if(field1%str2/=field2%str2) ERROR("")
  if(field1%str3/=field2%str3) ERROR("")
  if(field1%str2/=res%str2) ERROR("")
  if(field1%str3/=res%str3) ERROR("")
  call num_vea(res%val,field1%val,field2%val)
end subroutine pw_field_sum

subroutine pw_field_mul(res,field1,field2)
  use num_module
  type(pw_field), intent(inout) :: res
  type(pw_field), intent(in)    :: field1,field2
  if(any(field1%dim/=field2%dim)) ERROR("")
  if(any(field1%dim/=res%dim)) ERROR("")
  if(field1%str2/=field2%str2) ERROR("")
  if(field1%str3/=field2%str3) ERROR("")
  if(field1%str2/=res%str2) ERROR("")
  if(field1%str3/=res%str3) ERROR("")
  if(any(abs(res%k+field1%k-field2%k)>1d-6)) then
    write(0,*) res%k+field1%k-field2%k
    ERROR("")
  end if
  call num_vemul(res%val,field1%val,field2%val)
end subroutine pw_field_mul

subroutine pw_field_umul(res,field1,field2)
  use num_module
  type(pw_field), intent(inout) :: res
  type(pw_field), intent(in)    :: field1,field2
  if(any(field1%dim/=field2%dim)) ERROR("")
  if(any(field1%dim/=res%dim)) ERROR("")
  if(field1%str2/=field2%str2) ERROR("")
  if(field1%str3/=field2%str3) ERROR("")
  if(field1%str2/=res%str2) ERROR("")
  if(field1%str3/=res%str3) ERROR("")
  if(any(abs(res%k-field1%k-field2%k)>1d-6)) then
    write(0,*) res%k+field1%k-field2%k
    ERROR("")
  end if
!  call num_vemul(res%val,field1%val,field2%val)
  res%val = field1%val * field2%val
end subroutine pw_field_umul

function pw_field_braket_potential(bra,potential,ket)
  use num_module
  complex :: pw_field_braket_potential
  type (pw_field), intent(in) :: bra,potential,ket
    if(.not. all(bra%k  - ket%k  < 1e-6) .or. &
     .not. all(bra%r0 - ket%r0 < 1e-6) .or. &
&    .not. bra%str2 == ket%str2        .or. &
&    .not. bra%str3 == ket%str3        .or. &
     .not. all(bra%dim == ket%dim ) ) ERROR("") ! Aggiungere i check su potential
  pw_field_braket_potential = num_triple_dot(bra%val,potential%val,ket%val)/ product(bra%dim)
end function pw_field_braket_potential

function pw_field_dim_from_basis(basis)
  use ffti_module, only : ffti_good_order
  use pw_basis_module
  use num_module
  integer                     :: pw_field_dim_from_basis(3)
  type (pw_basis), intent(in) :: basis
  pw_field_dim_from_basis(1) = ffti_good_order(basis%gmax(1)-basis%gmin(1))
  pw_field_dim_from_basis(2) = ffti_good_order(basis%gmax(2)-basis%gmin(2))
  pw_field_dim_from_basis(3) = ffti_good_order(basis%gmax(3)-basis%gmin(3))
!  pw_field_dim_from_basis = num_fft_next_allowed(basis%gmax - basis%gmin)
end function pw_field_dim_from_basis

function pw_field_dim_from_dipole(basis1,basis_dip,basis2)
  use ffti_module, only : ffti_good_order
  use pw_basis_module
  use num_module
  integer :: pw_field_dim_from_dipole(3)
  type (pw_basis), intent(in) :: basis1,basis_dip,basis2
  integer :: gmin1(3),gmin2(3),gmin_dip(3),gmax1(3),gmax2(3),gmax_dip(3),dim(3)
! Non sono sicuro se questo controllo si puo' togliere, provo ...
!  if(any(abs(basis1%k+basis_dip%k-basis2%k)>1e-6)) ERROR("")
  gmin1 = basis1%gmin
  gmin2 = basis2%gmin
  gmax1 = basis1%gmax
  gmax2 = basis2%gmax
  gmin_dip = basis_dip%gmin
  gmax_dip = basis_dip%gmax
  dim(1) = max(gmax1(1)-gmin2(1)-gmin_dip(1),-gmin1(1)+gmax2(1)+gmax_dip(1))
  dim(2) = max(gmax1(2)-gmin2(2)-gmin_dip(2),-gmin1(2)+gmax2(2)+gmax_dip(2))
  dim(3) = max(gmax1(3)-gmin2(3)-gmin_dip(3),-gmin1(3)+gmax2(3)+gmax_dip(3))
  pw_field_dim_from_dipole(1) = ffti_good_order(dim(1))
  pw_field_dim_from_dipole(2) = ffti_good_order(dim(2))
  pw_field_dim_from_dipole(3) = ffti_good_order(dim(3))
!  pw_field_dim_from_dipole = num_fft_next_allowed(dim)
end function pw_field_dim_from_dipole

subroutine pw_field_add_density(density,field)
  type(pw_field), intent(inout) :: density
  type(pw_field), intent(in)    :: field
  if(any(field%dim/=density%dim)) ERROR("")
  if(field%str2/=density%str2) ERROR("")
  if(field%str3/=density%str3) ERROR("")
  if(any(density%k/=0.0)) ERROR("")

  density%val = density%val + abs(field%val)**2

end subroutine pw_field_add_density

subroutine pw_field_calc_vxc(vxc,density,opt)
  use pw_lda_module
  type(pw_field), intent(inout) :: vxc
  type(pw_field), intent(in)    :: density
  character(*),   intent(in)    :: opt
  integer :: str2,str3,pos,i1,i2,i3
  logical :: rdone

  if(any(vxc%dim/=density%dim)) ERROR("")
  if(vxc%str2/=density%str2) ERROR("")
  if(vxc%str3/=density%str3) ERROR("")
  if(any(density%k/=0.0)) ERROR("")

  rdone=.false.

  str2 = density%str2
  str3 = density%str3
  do i3=0,density%dim(3)-1
    do i2=0,density%dim(2)-1
      do i1=0,density%dim(1)-1
        pos = (i1 + i2*str2 + i3*str3)+1
!        if(real(density%val(pos))<0.0) then 
!          density%val(pos)=(0.00000001,0.00000001)
!          rdone=.true.
!        endif
        vxc%val(pos) = pw_lda_vxc(real(density%val(pos)),trim(opt))
      end do
    end do
  end do
  if(rdone) call tools_artwork_print(tools_log_unit(),"rabbit")
end subroutine pw_field_calc_vxc
  

end module pw_field_module
