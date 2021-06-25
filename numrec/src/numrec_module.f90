# 2 "numrec_module.spp"

module numrec_module
implicit none
private
public :: numrec_polint
public :: numrec_indexx
public :: numrec_gauleg
public :: numrec_locate
public :: numrec_erfcc
public :: numrec_ddpoly
public :: numrec_polcof
public :: numrec_polcoe

interface numrec_polint
# 17 "numrec_module.spp"
subroutine numrec_polint_sgl(xa,ya,n,x,y,dy)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xa(n)
  real(sgl), intent(in)  :: ya(n)
  real(sgl), intent(in)  :: x
  real(sgl), intent(out) :: y
  real(sgl), intent(out) :: dy
end subroutine numrec_polint_sgl
# 17 "numrec_module.spp"
subroutine numrec_polint_dbl(xa,ya,n,x,y,dy)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xa(n)
  real(dbl), intent(in)  :: ya(n)
  real(dbl), intent(in)  :: x
  real(dbl), intent(out) :: y
  real(dbl), intent(out) :: dy
end subroutine numrec_polint_dbl
# 28 "numrec_module.spp"
end interface

interface numrec_indexx
# 32 "numrec_module.spp"
subroutine numrec_indexx_sgl(n,arr,indx)
! returns in indx an array of integer numbers calculated so that
! arr(indx(1)) < arr(indx(2)) < ... < arr(indx(n-1)) < arr(indx(n))
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: arr(n)
  integer,     intent(out) :: indx(n)
end subroutine numrec_indexx_sgl
# 32 "numrec_module.spp"
subroutine numrec_indexx_dbl(n,arr,indx)
! returns in indx an array of integer numbers calculated so that
! arr(indx(1)) < arr(indx(2)) < ... < arr(indx(n-1)) < arr(indx(n))
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: arr(n)
  integer,     intent(out) :: indx(n)
end subroutine numrec_indexx_dbl
# 42 "numrec_module.spp"
end interface

interface numrec_gauleg
# 46 "numrec_module.spp"
subroutine numrec_gauleg_sgl(x1,x2,x,w,n)
  use numrec_kinds
  implicit none
  real(sgl), intent(in)  :: x1
  real(sgl), intent(in)  :: x2
  integer,     intent(in)  :: n
  real(sgl), intent(out) :: x(n)
  real(sgl), intent(out) :: w(n)
end subroutine numrec_gauleg_sgl
# 46 "numrec_module.spp"
subroutine numrec_gauleg_dbl(x1,x2,x,w,n)
  use numrec_kinds
  implicit none
  real(dbl), intent(in)  :: x1
  real(dbl), intent(in)  :: x2
  integer,     intent(in)  :: n
  real(dbl), intent(out) :: x(n)
  real(dbl), intent(out) :: w(n)
end subroutine numrec_gauleg_dbl
# 56 "numrec_module.spp"
end interface

interface numrec_locate
# 60 "numrec_module.spp"
subroutine numrec_locate_sgl(xx,n,x,j)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xx(n)
  real(sgl), intent(in)  :: x
  integer,     intent(out) :: j
end subroutine numrec_locate_sgl
# 60 "numrec_module.spp"
subroutine numrec_locate_dbl(xx,n,x,j)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xx(n)
  real(dbl), intent(in)  :: x
  integer,     intent(out) :: j
end subroutine numrec_locate_dbl
# 69 "numrec_module.spp"
end interface

interface numrec_erfcc
# 73 "numrec_module.spp"
function numrec_erfcc_sgl(x) result(res)
  use numrec_kinds
  implicit none
  real(sgl)             :: res
  real(sgl), intent(in) :: x
end function numrec_erfcc_sgl
# 73 "numrec_module.spp"
function numrec_erfcc_dbl(x) result(res)
  use numrec_kinds
  implicit none
  real(dbl)             :: res
  real(dbl), intent(in) :: x
end function numrec_erfcc_dbl
# 80 "numrec_module.spp"
end interface

interface numrec_ddpoly
# 84 "numrec_module.spp"
subroutine numrec_ddpoly_sgl(c,nc,x,pd,nd)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: nc
  real(sgl), intent(in)  :: c(nc)
  real(sgl), intent(in)  :: x
  integer,     intent(in)  :: nd
  real(sgl), intent(out) :: pd(nd)
end subroutine numrec_ddpoly_sgl
# 84 "numrec_module.spp"
subroutine numrec_ddpoly_dbl(c,nc,x,pd,nd)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: nc
  real(dbl), intent(in)  :: c(nc)
  real(dbl), intent(in)  :: x
  integer,     intent(in)  :: nd
  real(dbl), intent(out) :: pd(nd)
end subroutine numrec_ddpoly_dbl
# 94 "numrec_module.spp"
end interface

interface numrec_polcof
# 98 "numrec_module.spp"
subroutine numrec_polcof_sgl(xa,ya,n,cof)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(sgl), intent(in)  :: xa(n)
  real(sgl), intent(in)  :: ya(n)
  real(sgl), intent(out) :: cof(n)
end subroutine numrec_polcof_sgl
# 98 "numrec_module.spp"
subroutine numrec_polcof_dbl(xa,ya,n,cof)
  use numrec_kinds
  implicit none
  integer,     intent(in)  :: n
  real(dbl), intent(in)  :: xa(n)
  real(dbl), intent(in)  :: ya(n)
  real(dbl), intent(out) :: cof(n)
end subroutine numrec_polcof_dbl
# 107 "numrec_module.spp"
end interface

interface numrec_polcoe
# 112 "numrec_module.spp"
subroutine numrec_polcoe_REALsgl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  REAL(sgl), intent(in)  :: x(n)
  REAL(sgl), intent(in)  :: y(n)
  REAL(sgl), intent(out) :: cof(n)
end subroutine numrec_polcoe_REALsgl
# 112 "numrec_module.spp"
subroutine numrec_polcoe_REALdbl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  REAL(dbl), intent(in)  :: x(n)
  REAL(dbl), intent(in)  :: y(n)
  REAL(dbl), intent(out) :: cof(n)
end subroutine numrec_polcoe_REALdbl
# 112 "numrec_module.spp"
subroutine numrec_polcoe_COMPLEXsgl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  COMPLEX(sgl), intent(in)  :: x(n)
  COMPLEX(sgl), intent(in)  :: y(n)
  COMPLEX(sgl), intent(out) :: cof(n)
end subroutine numrec_polcoe_COMPLEXsgl
# 112 "numrec_module.spp"
subroutine numrec_polcoe_COMPLEXdbl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  COMPLEX(dbl), intent(in)  :: x(n)
  COMPLEX(dbl), intent(in)  :: y(n)
  COMPLEX(dbl), intent(out) :: cof(n)
end subroutine numrec_polcoe_COMPLEXdbl
# 122 "numrec_module.spp"

end interface

end module numrec_module
