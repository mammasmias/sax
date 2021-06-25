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

module pw_smearing_module
implicit none
private
public :: pw_smearing
public :: pw_smearing_init
public :: efermig
public :: wgauss
public :: gauss_freq

type pw_smearing
  character(30) :: smearing_kind
  real          :: smearing 
end type pw_smearing

contains

subroutine pw_smearing_init(smearing,sval,skind)
  implicit none
  type (pw_smearing), intent(inout) :: smearing
  real, intent(in)                  :: sval
  character(*),intent(in)           :: skind
  smearing%smearing = sval
  smearing%smearing_kind = skind
end subroutine pw_smearing_init

!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
FUNCTION efermig (eband, nbnd, nks, nelec, weight, smearing, Ngauss)
!----------------------------------------------------------------------------
  
!       Finds the Fermi energy - Gaussian Broadening
!      (see Methfessel and Paxton, PRB 40, 3616 (1989)
  
  use num_module
  implicit none
!
! I/O variables
!
  integer, intent(in)   :: nks, nbnd, Ngauss
  real, intent(in)      :: weight(nks), eband(nbnd,nks), smearing, nelec
  real                  :: wk(nks)
  real                  :: efermig
  real, parameter       :: eps= 1.0d-10
  integer, parameter    :: maxiter = 300
  integer               :: is, isk(nks)
!
! internal variables
!
  real                  :: Ef, Eup, Elw, sumkup, sumklw, sumkmid
  integer               :: i, kpoint

!
! the k-points weight in sax is different from the one of pw, to use this pw
! routine a renormalizzation in needed
!
  wk(:) = 2 * weight(:)
!
! Actually spin component resolution is disabled, to enable it
! delate the following initializzation and put 'is' and 'isk' as 
! intent(in) parameters of the function 
!
  is = 0
  isk(:) = 0

!
! find bounds for the Fermi energy. Very safe choice!
!
  Elw = eband (1, 1)
  Eup = eband (nbnd, 1)
  do kpoint = 2, nks
     Elw = min (Elw, eband (1, kpoint) )
     Eup = max (Eup, eband (nbnd, kpoint) )
  enddo
  Eup = Eup + 2 * smearing 
  Elw = Elw - 2 * smearing 
!
! Bisection method
!
  sumkup = sumkg (eband, nbnd, nks, wk, smearing, Ngauss, Eup, is, isk)
  sumklw = sumkg (eband, nbnd, nks, wk, smearing, Ngauss, Elw, is, isk)
  if ( (sumkup - nelec) < -eps .or. (sumklw - nelec) > eps )  &
       ERROR ("internal error, cannot braket Ef")
  do i = 1, maxiter
     Ef = (Eup + Elw) / 2.d0
     sumkmid = sumkg (eband, nbnd, nks, wk, smearing, Ngauss, Ef, is, isk)
     if (abs (sumkmid-nelec) < eps) then
        efermig = Ef
        return
     elseif ( (sumkmid-nelec) < -eps) then
        Elw = Ef
     else
        Eup = Ef
     endif
  enddo
  if (is /= 0) WRITE(*, '(5x,"Spin Component #",i3)') is
  WRITE( *, '(5x,"Warning: too many iterations in bisection"/ &
       &      5x,"Ef = ",f10.6," sumk = ",f10.6," electrons")' ) &
       Ef * num_ry2ev, sumkmid
  !
  efermig = Ef
  return
end FUNCTION efermig

!
! Copyright (C) 2001-2003 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!--------------------------------------------------------------
function sumkg (et, nbnd, nks, wk, degauss, ngauss, e, is, isk)
!--------------------------------------------------------------
!
!     This function computes the number of states under a given energy e
!
  implicit none
!
! Output variable
!
  real :: sumkg
!
! Input variables
!
  integer, intent(in)    :: nks, nbnd, ngauss
!
! input: the total number of K points
! input: the number of bands
! input: the type of smearing
!
  real, intent(in)       :: wk (nks), et (nbnd, nks), degauss, e
!
! input: the weight of the k points
! input: the energy eigenvalues
! input: gaussian broadening
! input: the energy to check
!
  integer, intent(in)    :: is, isk(nks)
!
! local variables
!
!
! function which compute the smearing
!
  real                   ::sum1
  integer                :: ik, ibnd
!
! counter on k points
! counter on the band energy
!
  sumkg = 0.d0
  do ik = 1, nks
     sum1 = 0.d0
     if (is /= 0) then
        if (isk(ik).ne.is) cycle
     end if
     do ibnd = 1, nbnd
        sum1 = sum1 + wgauss ( (e-et (ibnd, ik) ) / degauss, ngauss)
     enddo
     sumkg = sumkg + wk (ik) * sum1
  enddo
  return
end function sumkg

!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!-----------------------------------------------------------------------
function wgauss (x, n)
!-----------------------------------------------------------------------
!
!     this function computes the approximate theta function for the
!     given order n, at the point x.
!
! --> (n>=0) : Methfessel-Paxton case. See PRB 40, 3616 (1989).
!
! --> (n=-1 ): Cold smearing (Marzari-Vanderbilt). See PRL 82, 3296 (1999)
!       1/2*erf(x-1/sqrt(2)) + 1/sqrt(2*pi)*exp(-(x-1/sqrt(2))**2) + 1/2
!
! --> (n=-99): Fermi-Dirac case: 1.0/(1.0+exp(-x)).
!
  implicit none
  real :: wgauss, x
! output: the value of the function
! input: the argument of the function
  integer :: n
! input: the order of the function
!
!    the local variables
!
  real :: a, hp, arg, maxarg, hd, pi, xp
! the coefficient a_n
! the hermitean function
! the argument of the exponential
! maximum value for the argument of the exponen
! the hermitean function
! pi
! the freq function
! the erf function
! auxiliary variable (cold smearing)
  integer :: i, ni
! counter on the n indices
! counter on 2n

  parameter (maxarg = 200.0)

  pi = 3.14159265358979
! Fermi-Dirac smearing
  if (n.eq. - 99) then
     if (x.lt. - maxarg) then
        wgauss = 0.0
     elseif (x.gt.maxarg) then
        wgauss = 1.0
     else
        wgauss = 1.00 / (1.0 + exp ( - x) )
     endif
     return

  endif
! Cold smearing
  if (n.eq. - 1) then
     xp = x - 1.0 / sqrt (2.0)
     arg = min (maxarg, xp**2)
     wgauss = 0.5 * erf (xp) + 1.0 / sqrt (2.0 * pi) * exp ( - &
          arg) + 0.5
     return

  endif
! Methfessel-Paxton
  wgauss = gauss_freq (x * sqrt (2.0) )
  if (n.eq.0) return
  hd = 0.d0
  arg = min (maxarg, x**2)
  hp = exp ( - arg)
  ni = 0
  a = 1.d0 / sqrt (pi)
  do i = 1, n
     hd = 2.0 * x * hp - 2.0 * DBLE (ni) * hd
     ni = ni + 1
     a = - a / (DBLE (i) * 4.0)
     wgauss = wgauss - a * hd
     hp = 2.0 * x * hd-2.0 * DBLE (ni) * hp
     ni = ni + 1
  enddo
  return
end function wgauss

!
! Copyright (C) 2002-2003 PWSCF+CP group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!---------------------------------------------------------------------
real function erf (x)  
!---------------------------------------------------------------------
!
!     Error function - computed from the rational approximations of
!     W. J. Cody, Math. Comp. 22 (1969), pages 631-637.
!
!     for abs(x) le 0.47 erf is calculated directly
!     for abs(x) gt 0.47 erf is calculated via erf(x)=1-erfc(x)
!
  implicit none  
  real :: x, x2, p1 (4), q1 (4)
  data p1 / 2.42667955230532d2, 2.19792616182942d1, &
       6.99638348861914d0, -3.56098437018154d-2 /
  data q1 / 2.15058875869861d2, 9.11649054045149d1, &
       1.50827976304078d1, 1.00000000000000d0 /
!
  if (abs (x) .gt.6.0) then  
!
!  erf(6)=1-10^(-17) cannot be distinguished from 1 with 16-byte words
!
     erf = sign (1.0, x)  
  else  
     if (abs (x) .le.0.47) then  
        x2 = x**2  
        erf = x * (p1 (1) + x2 * (p1 (2) + x2 * (p1 (3) + x2 * p1 ( &
             4) ) ) ) / (q1 (1) + x2 * (q1 (2) + x2 * (q1 (3) + x2 * q1 ( &
             4) ) ) )
     else  
        erf = 1.0 - erfc (x)  
     endif
  endif

  return  
end function erf

!
!---------------------------------------------------------------------
real(8) function erfc (x)
!---------------------------------------------------------------------
!
!     erfc(x) = 1-erf(x)  - See comments in erf
!
  implicit none
  real           :: x, ax, x2, xm2, p2 (8), q2 (8), p3 (5), q3 (5), pim1
  
  data p2 / 3.00459261020162d2, 4.51918953711873d2, &
       3.39320816734344d2, 1.52989285046940d2, 4.31622272220567d1, &
       7.21175825088309d0, 5.64195517478974d-1, -1.36864857382717d-7 /
  data q2 / 3.00459260956983d2, 7.90950925327898d2, &
       9.31354094850610d2, 6.38980264465631d2, 2.77585444743988d2, &
       7.70001529352295d1, 1.27827273196294d1, 1.00000000000000d0 /
  data p3 / -2.99610707703542d-3, -4.94730910623251d-2, &
       -2.26956593539687d-1, -2.78661308609648d-1, -2.23192459734185d-2 &
       /
  data q3 / 1.06209230528468d-2, 1.91308926107830d-1, &
       1.05167510706793d0, 1.98733201817135d0, 1.00000000000000d0 /

  data pim1 / 0.564189583547756d0 /

!        ( pim1= sqrt(1/pi) )

  ax = abs (x)
  if (ax.gt.26.d0) then
!
!  erfc(26.0)=10^(-296); erfc( 9.0)=10^(-37);
!
     erfc = 0.d0
  elseif (ax.gt.4.d0) then
     x2 = x**2
     xm2 = (1.d0 / ax) **2
     erfc = (1.d0 / ax) * exp ( - x2) * (pim1 + xm2 * (p3 (1) &
          + xm2 * (p3 (2) + xm2 * (p3 (3) + xm2 * (p3 (4) + xm2 * p3 (5) &
          ) ) ) ) / (q3 (1) + xm2 * (q3 (2) + xm2 * (q3 (3) + xm2 * &
          (q3 (4) + xm2 * q3 (5) ) ) ) ) )
  elseif (ax.gt.0.47d0) then
     x2 = x**2
     erfc = exp ( - x2) * (p2 (1) + ax * (p2 (2) + ax * (p2 (3) &
          + ax * (p2 (4) + ax * (p2 (5) + ax * (p2 (6) + ax * (p2 (7) &
          + ax * p2 (8) ) ) ) ) ) ) ) / (q2 (1) + ax * (q2 (2) + ax * &
          (q2 (3) + ax * (q2 (4) + ax * (q2 (5) + ax * (q2 (6) + ax * &
          (q2 (7) + ax * q2 (8) ) ) ) ) ) ) )
  else
     erfc = 1.d0 - erf (ax)
  endif
!
! erf(-x)=-erf(x)  =>  erfc(-x) = 2-erfc(x)
!
  if (x.lt.0.d0) erfc = 2.d0 - erfc

  return
end function erfc

!
!---------------------------------------------------------------------
real function gauss_freq(x)
!---------------------------------------------------------------------
!
!     gauss_freq(x) = (1+erf(x/sqrt(2)))/2 = erfc(-x/sqrt(2))/2
!             - See comments in erf

  real            :: x
  real, parameter :: c =  0.707106781186548d0

!        ( c= sqrt(1/2) )

  gauss_freq = 0.5d0 * erfc ( - x * c)

  return
end function gauss_freq

end module pw_smearing_module
