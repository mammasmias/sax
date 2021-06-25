# 4 "numrec_polcoe.spp"
subroutine numrec_polcoe_REALsgl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  REAL(sgl), intent(in)  :: x(n)
  REAL(sgl), intent(in)  :: y(n)
  REAL(sgl), intent(out) :: cof(n)
  REAL(sgl) :: b,ff,phi,s(n)
  integer :: i,j,k
  do i=1,n
    s(i)=0.0_sgl
    cof(i)=0.0_sgl
  end do
  s(n)=-x(1)
  do i=2,n
    do j=n+1-i,n-1
      s(j)=s(j)-x(i)*s(j+1)
    end do
    s(n)=s(n)-x(i)
  end do
  do j=1,n
    phi=n
    do k=n-1,1,-1
      phi=k*s(k+1)+x(j)*phi
    end do
    ff=y(j)/phi
    b=1.0_sgl
    do k=n,1,-1
      cof(k)=cof(k)+b*ff
      b=s(k)+x(j)*b
    end do
  end do
end subroutine numrec_polcoe_REALsgl
# 4 "numrec_polcoe.spp"
subroutine numrec_polcoe_REALdbl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  REAL(dbl), intent(in)  :: x(n)
  REAL(dbl), intent(in)  :: y(n)
  REAL(dbl), intent(out) :: cof(n)
  REAL(dbl) :: b,ff,phi,s(n)
  integer :: i,j,k
  do i=1,n
    s(i)=0.0_dbl
    cof(i)=0.0_dbl
  end do
  s(n)=-x(1)
  do i=2,n
    do j=n+1-i,n-1
      s(j)=s(j)-x(i)*s(j+1)
    end do
    s(n)=s(n)-x(i)
  end do
  do j=1,n
    phi=n
    do k=n-1,1,-1
      phi=k*s(k+1)+x(j)*phi
    end do
    ff=y(j)/phi
    b=1.0_dbl
    do k=n,1,-1
      cof(k)=cof(k)+b*ff
      b=s(k)+x(j)*b
    end do
  end do
end subroutine numrec_polcoe_REALdbl
# 4 "numrec_polcoe.spp"
subroutine numrec_polcoe_COMPLEXsgl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  COMPLEX(sgl), intent(in)  :: x(n)
  COMPLEX(sgl), intent(in)  :: y(n)
  COMPLEX(sgl), intent(out) :: cof(n)
  COMPLEX(sgl) :: b,ff,phi,s(n)
  integer :: i,j,k
  do i=1,n
    s(i)=0.0_sgl
    cof(i)=0.0_sgl
  end do
  s(n)=-x(1)
  do i=2,n
    do j=n+1-i,n-1
      s(j)=s(j)-x(i)*s(j+1)
    end do
    s(n)=s(n)-x(i)
  end do
  do j=1,n
    phi=n
    do k=n-1,1,-1
      phi=k*s(k+1)+x(j)*phi
    end do
    ff=y(j)/phi
    b=1.0_sgl
    do k=n,1,-1
      cof(k)=cof(k)+b*ff
      b=s(k)+x(j)*b
    end do
  end do
end subroutine numrec_polcoe_COMPLEXsgl
# 4 "numrec_polcoe.spp"
subroutine numrec_polcoe_COMPLEXdbl(x,y,n,cof)
  use numrec_kinds
  implicit none
  integer,      intent(in)  :: n
  COMPLEX(dbl), intent(in)  :: x(n)
  COMPLEX(dbl), intent(in)  :: y(n)
  COMPLEX(dbl), intent(out) :: cof(n)
  COMPLEX(dbl) :: b,ff,phi,s(n)
  integer :: i,j,k
  do i=1,n
    s(i)=0.0_dbl
    cof(i)=0.0_dbl
  end do
  s(n)=-x(1)
  do i=2,n
    do j=n+1-i,n-1
      s(j)=s(j)-x(i)*s(j+1)
    end do
    s(n)=s(n)-x(i)
  end do
  do j=1,n
    phi=n
    do k=n-1,1,-1
      phi=k*s(k+1)+x(j)*phi
    end do
    ff=y(j)/phi
    b=1.0_dbl
    do k=n,1,-1
      cof(k)=cof(k)+b*ff
      b=s(k)+x(j)*b
    end do
  end do
end subroutine numrec_polcoe_COMPLEXdbl
