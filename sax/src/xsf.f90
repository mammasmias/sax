!
! Copyright (C) 2003 Tone Kokalj
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! This file holds XSF (=Xcrysden Structure File) utilities.
! Routines written by Tone Kokalj on Mon Jan 27 18:51:17 CET 2003
!
! -------------------------------------------------------------------
!   this routine writes the crystal structure in XSF format
! -------------------------------------------------------------------
subroutine xsf_struct (at, nat, tau, atm, ityp, ounit)
!  USE kinds, only : DP
!  USE constants, only : BOHR_RADIUS_ANGS
  implicit none
  integer          :: nat, ityp (nat), ounit
  character(len=*) :: atm(*)
  real    :: tau (3, nat), at (3, 3), pos(3)
  ! --
  integer          :: i, j, n
  real    :: at1 (3, 3)
  real, parameter :: BOHR_RADIUS_ANGS = 0.5291772083

!  write(0,*) "at", at
!  write(0,*) "nat", nat
!  write(0,*) "tau", tau
!  write(0,*) "atm", atm(1), atm(2)
!  write(0,*) "ityp", ityp

  ! convert lattice vectors to ANGSTROM units ...
  do i=1,3
     do j=1,3
        at1(j,i) = at(j,i)*BOHR_RADIUS_ANGS
     enddo
  enddo

  write(ounit,*) 'CRYSTAL'
  write(ounit,*) 'PRIMVEC'
  write(ounit,'(2(3F15.9/),3f15.9)') at1
  write(ounit,*) 'PRIMCOORD'
  write(ounit,*) nat, 1

  do n=1,nat
     ! positions are in Angstroms
     pos(:)=tau(:,n)

     pos(1)=at(1,1)*tau(1,n)+at(1,2)*tau(2,n)+at(1,3)*tau(3,n)
     pos(2)=at(2,1)*tau(1,n)+at(2,2)*tau(2,n)+at(2,3)*tau(3,n)
     pos(3)=at(3,1)*tau(1,n)+at(3,2)*tau(2,n)+at(3,3)*tau(3,n)

     write(ounit,'(a3,3x,3f15.9)') trim(atm(ityp(n))), &
          pos(1)*BOHR_RADIUS_ANGS, &
          pos(2)*BOHR_RADIUS_ANGS, &
          pos(3)*BOHR_RADIUS_ANGS
  enddo
  return
end subroutine xsf_struct



! -------------------------------------------------------------------
!   this routine writes the 3D scalar field (i.e. uniform mesh of points)
!   in XSF format using the FFT mesh (i.e. fast write)
! -------------------------------------------------------------------
subroutine xsf_fast_datagrid_3d &
     (rho, nr1, nr2, nr3, nrx1, nrx2, nrx3, at, ounit)
!  USE kinds, only : DP
!  USE constants, ONLY : BOHR_RADIUS_ANGS
  implicit none
  integer       :: nrx1, nrx2, nrx3, nr1, nr2, nr3, ounit
  real :: at (3, 3), rho(nr1,nr2,nr3)
  ! --
  integer       :: i1, i2, i3, ix, iy, iz, count, i, &
       ind_x(10), ind_y(10),ind_z(10)
  !
  real, parameter :: BOHR_RADIUS_ANGS = 0.5291772083
  !
  ! XSF scalar-field header
  write(ounit,'(a)') 'BEGIN_BLOCK_DATAGRID_3D'
  write(ounit,'(a)') '3D_PWSCF'
  write(ounit,'(a)') 'DATAGRID_3D_UNKNOWN'

  write(0,*) "ulbound(roh)", ubound(rho,1), lbound(rho,1), ubound(rho,2), lbound(rho,2), &
     ubound(rho,3), lbound(rho,3)
  write(0,*) "dim", nr1, nr2, nr3
  write(0,*) "nrx", nrx1, nrx2, nrx3

  ! number of points in each direction
  write(ounit,*) nr1+1, nr2+1, nr3+1
  ! origin
  write(ounit,'(3f10.6)') 0.0, 0.0, 0.0
  ! 1st spanning (=lattice) vector
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*at(i,1),i=1,3) ! in ANSTROMS
  ! 2nd spanning (=lattice) vector
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*at(i,2),i=1,3)
  ! 3rd spanning (=lattice) vector
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*at(i,3),i=1,3)

  count=0
  do i3=0,nr3
     !iz = mod(i3,nr3)
     iz = mod(i3,nr3) + 1

     do i2=0,nr2
        !iy = mod(i2,nr2)
        iy = mod(i2,nr2) + 1

        do i1=0,nr1
           !ix = mod(i1,nr1)
           ix = mod(i1,nr1) + 1

           !ii = (1+ix) + iy*nrx1 + iz*nrx1*nrx2
           if (count.lt.6) then
              count = count + 1
              !ind(count) = ii
           else
              write(ounit,'(6e13.5)') &
                   (rho(ind_x(i),ind_y(i),ind_z(i)),i=1,6)
              count=1
              !ind(count) = ii
           endif
           ind_x(count) = ix
           ind_y(count) = iy
           ind_z(count) = iz
        enddo
     enddo
  enddo
  write(ounit,'(6e13.5:)') (rho(ind_x(i),ind_y(i),ind_z(i)),i=1,count)

!  write(ounit,*) (((rho(ix,iy,iz),ix=1,nr1),iy=1,nr2),iz=1,nr3)
  write(ounit,'(a)') 'END_DATAGRID_3D'
  write(ounit,'(a)') 'END_BLOCK_DATAGRID_3D'
  return
end subroutine xsf_fast_datagrid_3d

subroutine xsf_datagrid_2d (rho, nx, ny, m1, m2, x0, e1, e2, ounit)
!  USE kinds, only : DP
!  USE constants, ONLY : BOHR_RADIUS_ANGS
  implicit none
  integer       :: nx, ny, ounit
  real :: m1, m2, x0(3), e1(3), e2(3), rho(2, nx, ny)
  ! --
  integer       :: ix, iy, count, i, ind_x(10), ind_y(10)

  real, parameter :: BOHR_RADIUS_ANGS = 0.5291772083

  ! XSF scalar-field header
  write(ounit,'(a)') 'BEGIN_BLOCK_DATAGRID_2D'
  write(ounit,'(a)') '2D_PWSCF'
  write(ounit,'(a)') 'DATAGRID_2D_UNKNOWN'

  ! number of points in each direction
  write(ounit,*) nx, ny
  ! origin
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*x0(i),i=1,3) ! in ANSTROMS
  ! 1st spanning (=lattice) vector
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*e1(i)*m1,i=1,3) ! in ANSTROMS
  ! 2nd spanning (=lattice) vector
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*e2(i)*m2,i=1,3) ! in ANSTROMS

  count=0
  do iy=1,ny
     do ix=1,nx
        if (count < 6) then
           count = count + 1
        else
           write(ounit,'(6e13.5)') (rho(1,ind_x(i),ind_y(i)),i=1,6)
           count=1
        endif
        ind_x(count) = ix
        ind_y(count) = iy
     enddo
  enddo

  write(ounit,'(6e13.5:)') (rho(1,ind_x(i),ind_y(i)),i=1,count)
  write(ounit,'(a)') 'END_DATAGRID_2D'
  write(ounit,'(a)') 'END_BLOCK_DATAGRID_2D'
  return
end subroutine xsf_datagrid_2d



subroutine xsf_datagrid_3d &
     (rho, nx, ny, nz, m1, m2, m3, x0, e1, e2, e3, ounit)
!  USE kinds, only : DP
!  USE constants, ONLY : BOHR_RADIUS_ANGS
  implicit none
  integer       :: nx, ny, nz, ounit
  real :: m1, m2, m3, x0(3), e1(3),e2(3),e3(3), rho(nx, ny, nz)
  ! --
  integer       :: ix, iy, iz, count, i, ind_x(10), ind_y(10), ind_z(10)

  real, parameter :: BOHR_RADIUS_ANGS = 0.5291772083

  ! XSF scalar-field header
  write(ounit,'(a)') 'BEGIN_BLOCK_DATAGRID_3D'
  write(ounit,'(a)') '3D_PWSCF'
  write(ounit,'(a)') 'DATAGRID_3D_UNKNOWN'

  ! number of points in each direction
  write(ounit,*) nx, ny, nz
  ! origin
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*x0(i),i=1,3) ! in ANSTROMS
  ! 1st spanning (=lattice) vector
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*e1(i)*m1,i=1,3) ! in ANSTROMS
  ! 2nd spanning (=lattice) vector
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*e2(i)*m2,i=1,3) ! in ANSTROMS
  ! 3rd spanning (=lattice) vector
  write(ounit,'(3f10.6)') (BOHR_RADIUS_ANGS*e3(i)*m3,i=1,3)

  count=0
  do iz=1,nz
     do iy=1,ny
        do ix=1,nx
           if (count.lt.6) then
              count = count + 1
           else
              write(ounit,'(6e13.5)') (rho(ind_x(i),ind_y(i),ind_z(i)),i=1,6)
              count=1
           endif
           ind_x(count) = ix
           ind_y(count) = iy
           ind_z(count) = iz
        enddo
     enddo
  enddo

  write(ounit,'(6e13.5:)') (rho(ind_x(i),ind_y(i),ind_z(i)),i=1,count)
  write(ounit,'(a)') 'END_DATAGRID_3D'
  write(ounit,'(a)') 'END_BLOCK_DATAGRID_3D'
  return
end subroutine xsf_datagrid_3d
