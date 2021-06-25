# 3 "numrec_gauleg.spp"
subroutine numrec_gauleg_sgl(x1,x2,x,w,n)
  use numrec_kinds
  implicit none
  real(sgl), intent(in)  :: x1
  real(sgl), intent(in)  :: x2
  integer,     intent(in)  :: n
  real(sgl), intent(out) :: x(n)
  real(sgl), intent(out) :: w(n)
! double precision at least is required for internal calculations
  double precision, parameter :: eps = 3.d-14
  integer :: i,j,m
  double precision :: p1,p2,p3,pp,xl,xm,z,z1
  m=(n+1)/2
  xm=0.5d0*(x2+x1)
  xl=0.5d0*(x2-x1)
  do i=1,m
    z=cos(3.141592654d0*(i-.25d0)/(n+.5d0))
    do
      p1=1.d0
      p2=0.d0
      do j=1,n
        p3=p2
        p2=p1
        p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
      end do
      pp=n*(z*p1-p2)/(z*z-1.d0)
      z1=z
      z=z1-p1/pp
      if(abs(z-z1)<=EPS)exit
    end do
    x(i)=xm-xl*z
    x(n+1-i)=xm+xl*z
    w(i)=2.d0*xl/((1.d0-z*z)*pp*pp)
    w(n+1-i)=w(i)
  end do
end subroutine numrec_gauleg_sgl
# 3 "numrec_gauleg.spp"
subroutine numrec_gauleg_dbl(x1,x2,x,w,n)
  use numrec_kinds
  implicit none
  real(dbl), intent(in)  :: x1
  real(dbl), intent(in)  :: x2
  integer,     intent(in)  :: n
  real(dbl), intent(out) :: x(n)
  real(dbl), intent(out) :: w(n)
! double precision at least is required for internal calculations
  double precision, parameter :: eps = 3.d-14
  integer :: i,j,m
  double precision :: p1,p2,p3,pp,xl,xm,z,z1
  m=(n+1)/2
  xm=0.5d0*(x2+x1)
  xl=0.5d0*(x2-x1)
  do i=1,m
    z=cos(3.141592654d0*(i-.25d0)/(n+.5d0))
    do
      p1=1.d0
      p2=0.d0
      do j=1,n
        p3=p2
        p2=p1
        p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
      end do
      pp=n*(z*p1-p2)/(z*z-1.d0)
      z1=z
      z=z1-p1/pp
      if(abs(z-z1)<=EPS)exit
    end do
    x(i)=xm-xl*z
    x(n+1-i)=xm+xl*z
    w(i)=2.d0*xl/((1.d0-z*z)*pp*pp)
    w(n+1-i)=w(i)
  end do
end subroutine numrec_gauleg_dbl
