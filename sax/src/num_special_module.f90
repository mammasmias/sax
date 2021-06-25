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
module num_special_module
implicit none
! Module containing special functions
private
public :: num_ylm,        &
          num_fact,       &
          num_xlylm,      &
          num_xlylm_grad, &
          num_factfact,   &
          num_xmlsphbes,  &
          num_inverse_tensor, &
          num_discontinuity_value, &
          num_discontinuity_value_old, &
          num_vlr0_plus_vsk0_value, &
          num_vl_value, &
          num_v0_vcut_spheric_value, &
          num_erfc,       &
          num_pole_integral, &
          num_pole_total_integral, &
          num_pole_integral_coeff, &
          num_kk_coeff,num_gaussian_pole,num_kk_ccoeff,num_gaussmesh, num_mesh
!@ END MANUAL

interface num_inverse_tensor
  module procedure num_inverse_tensor_r
  module procedure num_inverse_tensor_c
end interface

interface num_pole_integral_coeff
  module procedure num_pole_integral_coeff_m
  module procedure num_pole_integral_coeff_v
end interface

! Private parameters for ylm
real, parameter :: c00 =  .28209479177387814347, &
                   c11 =  .34549414947133547925, &
                   c10 =  .48860251190291992158, &
                   c22 =  .38627420202318958029, &
                   c21 =  .77254840404637916067, &
                   c20 =  .63078313050504001206, &
                   c33 =  .41722382363278408972, &
                   c32 = 1.02198547643328236339, &
                   c31 =  .32318018411415065300, &
                   c30 =  .74635266518023078282

contains

!@ MANUAL
function num_xlylm_grad(x,l,m)
! Returns the gradient of xlylm
  complex             :: num_xlylm_grad(3)
  real, intent(in)    :: x(3)
  integer, intent(in) :: l,m
!@ END MANUAL
  if(abs(m)>l) ERROR("")
  select case(l)
  case(0)
    num_xlylm_grad(:) = 0.0
  case(1)
    select case(m)
    case(+1)
      num_xlylm_grad(1) = - c11
      num_xlylm_grad(2) = - c11 * (0.0,1.0)
      num_xlylm_grad(3) = 0.0
    case(0)
      num_xlylm_grad(1) = 0.0
      num_xlylm_grad(2) = 0.0
      num_xlylm_grad(3) = + c10
    case(-1)
      num_xlylm_grad(1) = + c11
      num_xlylm_grad(2) = + c11 * (-(0.0,1.0))
      num_xlylm_grad(3) = 0.0
    end select
  case(2)
    select case(m)
    case(+2)
      num_xlylm_grad(1) = + c22 * 2.0 * (x(1) + (0.0,1.0)*x(2))
      num_xlylm_grad(2) = + c22 * 2.0 * (x(1) + (0.0,1.0)*x(2)) * (0.0,1.0)
      num_xlylm_grad(3) = 0.0
    case(+1)
      num_xlylm_grad(1) = - c21 * x(3)
      num_xlylm_grad(2) = - c21 * x(3) * (0.0,1.0)
      num_xlylm_grad(3) = - c21 * (x(1) + (0.0,1.0)*x(2))
    case(0)
      num_xlylm_grad(1) = + c20 * (-x(1))
      num_xlylm_grad(2) = + c20 * (-x(2))
      num_xlylm_grad(3) = + c20 * 2.0 * x(3)
    case(-1)
      num_xlylm_grad(1) = + c21 * x(3)
      num_xlylm_grad(2) = + c21 * x(3) * (-(0.0,1.0))
      num_xlylm_grad(3) = + c21 * (x(1) - (0.0,1.0)*x(2))
    case(-2)
      num_xlylm_grad(1) = + c22 * 2.0 * (x(1) - (0.0,1.0)*x(2))
      num_xlylm_grad(2) = + c22 * 2.0 * (x(1) - (0.0,1.0)*x(2)) * (-(0.0,1.0))
      num_xlylm_grad(3) = 0.0
    end select
  case(3)
    select case(m)
    case(+3)
      num_xlylm_grad(1) = - c33 * (3.0*x(1)**2+(0.0,1.0)*6.0*x(1)*x(2)-3.0*x(2)**2)
      num_xlylm_grad(2) = - c33 * (3.0*(0.0,1.0)*x(1)**2-6.0*x(1)*x(2)-3.0*(0.0,1.0)*x(2)**2)
      num_xlylm_grad(3) = 0.0
    case(+2)
      num_xlylm_grad(1) = + c32 * x(3)*(2.0*x(1)+2.0*(0.0,1.0)*x(2))
      num_xlylm_grad(2) = + c32 * x(3)*(2.0*(0.0,1.0)*x(1)-2.0*x(2))
      num_xlylm_grad(3) = + c32 * (x(1) + (0.0,1.0)*x(2))**2
    case(+1)
      num_xlylm_grad(1) = - c31 * (4.0*x(3)**2-3.0*x(1)**2-x(2)**2-2.0*(0.0,1.0)*x(1)*x(2))
      num_xlylm_grad(2) = - c31 * (-2.0*x(2)*x(1)+(0.0,1.0)*4.0*x(3)**2- &
                                  (0.0,1.0)*x(1)**2-3.0*(0.0,1.0)*x(2)**2)
      num_xlylm_grad(3) = - c31 * (x(1)+(0.0,1.0)*x(2))*8.0*x(3)
    case(0)
      num_xlylm_grad(1) = - c30*0.5*6.0*x(3)*x(1)
      num_xlylm_grad(2) = - c30*0.5*6.0*x(3)*x(2)
      num_xlylm_grad(3) = + c30*0.5*(6.0*x(3)**2-3.0*(x(1)**2+x(2)**2))
    case(-1)
      num_xlylm_grad(1) = + c31 * (4.0*x(3)**2-3.0*x(1)**2-x(2)**2+2.0*(0.0,1.0)*x(2)*x(1))
      num_xlylm_grad(2) = + c31 * (-2.0*x(2)*x(1)-(0.0,1.0)*4.0*x(3)**2+ &
                                  (0.0,1.0)*x(1)**2+3.0*(0.0,1.0)*x(2)**2)
      num_xlylm_grad(3) = + c31 * (x(1)-(0.0,1.0)*x(2))*8.0*x(3)
    case(-2)
      num_xlylm_grad(1) = + c32 * x(3)*(2.0*x(1)-2.0*(0.0,1.0)*x(2))
      num_xlylm_grad(2) = + c32 * x(3)*(-2.0*(0.0,1.0)*x(1)-2.0*x(2))
      num_xlylm_grad(3) = + c32 * (x(1) - (0.0,1.0)*x(2))**2
    case(-3)
      num_xlylm_grad(1) = + c33 * (3.0*x(1)**2-(0.0,1.0)*6.0*x(1)*x(2)-3.0*x(2)**2)
      num_xlylm_grad(2) = + c33 * (-3.0*(0.0,1.0)*x(1)**2-6.0*x(1)*x(2)+3.0*(0.0,1.0)*x(2)**2)
      num_xlylm_grad(3) = 0.0
    end select
  case default
    ERROR("")
  end select
end function num_xlylm_grad

!@ MANUAL
function num_xlylm(x,l,m)
! Returns |x|^l * y_lm(x).
! With this definitions, the spherical harmonics are well
! defined also for null vectors, continuous and derivable everywhere
  complex             :: num_xlylm
  real, intent(in)    :: x(3)
  integer, intent(in) :: l,m
!@ END MANUAL
  if(abs(m)>l) ERROR("")
  select case(l)
  case(0)
    num_xlylm = c00
  case(1)
    select case(m)
    case(+1)
      num_xlylm = - c11 * (x(1) + (0.0,1.0)*x(2))
    case(0)
      num_xlylm = + c10 * x(3)
    case(-1)
      num_xlylm = + c11 * (x(1) - (0.0,1.0)*x(2))
    end select
  case(2)
    select case(m)
    case(+2)
      num_xlylm = + c22 * (x(1) + (0.0,1.0)*x(2))**2
    case(+1)
      num_xlylm = - c21 * (x(1) + (0.0,1.0)*x(2)) * x(3)
    case(0)
      num_xlylm = + c20 * (2.0*x(3)**2 - x(1)**2 - x(2)**2) * 0.5
    case(-1)
      num_xlylm = + c21 * (x(1) - (0.0,1.0)*x(2)) * x(3)
    case(-2)
      num_xlylm = + c22 * (x(1) - (0.0,1.0)*x(2))**2
    end select
  case(3)
    select case(m)
    case(+3)
      num_xlylm = - c33 * (x(1) + (0.0,1.0)*x(2))**3
    case(+2)
      num_xlylm = + c32 * (x(1) + (0.0,1.0)*x(2))**2*x(3)
    case(+1)
      num_xlylm = - c31 * (x(1) + (0.0,1.0)*x(2)) * (5.0*x(3)**2-(x(1)**2 + x(2)**2 + x(3)**2))
    case(0)
      num_xlylm = + c30 * (5.0*x(3)**3 - 3.0*x(3)*(x(1)**2 + x(2)**2 + x(3)**2)) * 0.5
    case(-1)
      num_xlylm = + c31 * (x(1) - (0.0,1.0)*x(2)) * (5.0*x(3)**2-(x(1)**2 + x(2)**2 + x(3)**2))
    case(-2)
      num_xlylm = + c32 * (x(1) - (0.0,1.0)*x(2))**2*x(3)
    case(-3)
      num_xlylm = + c33 * (x(1) - (0.0,1.0)*x(2))**3
    end select
  case default
    ERROR("")
  end select
end function num_xlylm

!@ MANUAL
function num_ylm(x,l,m)
! spherical harmonics
! NOTE ill defined for x=0.0
  complex             :: num_ylm
  real, intent(in)    :: x(3)
  integer, intent(in) :: l,m
!@ END MANUAL
  select case(l)
  case(0)
    num_ylm = num_xlylm(x,l,m)
  case(1)
    num_ylm = num_xlylm(x,l,m) / sqrt(sum(x**2))
  case(2)
    num_ylm = num_xlylm(x,l,m) / sum(x**2)
  case default
    ERROR("")
  end select
end function num_ylm

!@ MANUAL
function num_factfact(n)
! Returns the double factorial n!!
  integer             :: num_factfact
  integer, intent(in) :: n
!@ END MANUAL
  integer :: fact,i
  if(modulo(n,2)/=1) ERROR("")
  select case(n)
  case(:0)
    ERROR("")
  case(1)
    fact = 1
  case(3:)
    fact = 1
    do i=3,n,2
      fact = fact * i
    end do
  end select
  num_factfact = fact
end function num_factfact

!@ MANUAL
function num_fact(n)
! Returns the factorial n!
  integer :: num_fact
  integer, intent(in) :: n
!@ END MANUAL
  integer :: fact,i
  select case (n)
  case(:-1)
    ERROR("")
  case(0:1)
    fact = 1
  case(2:)
    fact = 1
    do i=2,n
      fact = fact*i
    end do
  end select
  num_fact = fact
end function num_fact

!@ MANUAL
function num_xmlsphbes(x,l)
! Returns x^(-l) * j_l(x)
  real :: num_xmlsphbes
  real,    intent(in) :: x
  integer, intent(in) :: l
!@ END MANUAL
! Note: the following lines can be generated with GNU bc -m
! and the following input <<EOF
!  scale = 60
!  lmax  = 3
!  smax  = 15
!  define fact (i) {
!    auto p,j;
!    p = 1;
!    for (j=1; j<=i; j++) p = p*j
!    return (p) ;
!  }
!  define coeff (s,l)
!  {
!    auto num,den;
!    num = 2^l * (-1)^s * fact(s+l) ;
!    den = fact(s) * fact(2*s+2*l+1) ;
!    return (num/den) ;
!  }
!  print "  real, parameter :: coeff(0:",smax,",0:",lmax,")=reshape((/ &\n" ;
!  for ( l=0; l<=lmax; l++)
!  for ( s=0; s<=smax; s++) {
!    print coeff(s,l) ;
!    if( (s != smax) || (l != lmax)) print ", &\n" ;
!  }
!  print "/), (/",smax+1,",",lmax+1,"/))\n" ;
!  quit
!  EOF
  real, parameter :: coeff(0:15,0:3)=reshape((/ &
1.000000000000000000000000000000000000000000000000000000000000, &
-.166666666666666666666666666666666666666666666666666666666666, &
.008333333333333333333333333333333333333333333333333333333333, &
-.000198412698412698412698412698412698412698412698412698412698, &
.000002755731922398589065255731922398589065255731922398589065, &
-.000000025052108385441718775052108385441718775052108385441718, &
.000000000160590438368216145993923771701549479327257105034882, &
-.000000000000764716373181981647590113198578807044415510023975, &
.000000000000002811457254345520763198945583010320016233492735, &
-.000000000000000008220635246624329716955981236872280749220738, &
.000000000000000000019572941063391261230847574373505430355287, &
-.000000000000000000000038681701706306840377169119315228123231, &
.000000000000000000000000064469502843844733961948532192046872, &
-.000000000000000000000000000091836898637955461484257168364739, &
.000000000000000000000000000000113099628864477169315587645769, &
-.000000000000000000000000000000000121612504155351794962997468, &
.333333333333333333333333333333333333333333333333333333333333, &
-.033333333333333333333333333333333333333333333333333333333333, &
.001190476190476190476190476190476190476190476190476190476190, &
-.000022045855379188712522045855379188712522045855379188712522, &
.000000250521083854417187750521083854417187750521083854417187, &
-.000000001927085260418593751927085260418593751927085260418593, &
.000000000010706029224547743066261584780103298621817140335658, &
-.000000000000044983316069528332211183129328165120259735883763, &
.000000000000000147971434439237934905207662263701053485973301, &
-.000000000000000000391458821267825224616951487470108607105749, &
.000000000000000000000850997437538750488297720624935018711099, &
-.000000000000000000000001547268068252273615086764772609124929, &
.000000000000000000000000002387759364586841998590686377483217, &
-.000000000000000000000000000003166789608205360740836454081542, &
.000000000000000000000000000000003648375124660553848889924057, &
-.000000000000000000000000000000000003685227398647024089787802, &
.066666666666666666666666666666666666666666666666666666666666, &
-.004761904761904761904761904761904761904761904761904761904761, &
.000132275132275132275132275132275132275132275132275132275132, &
-.000002004168670835337502004168670835337502004168670835337502, &
.000000019270852604185937519270852604185937519270852604185937, &
-.000000000128472350694572916795139017361239583461805684027906, &
.000000000000629766424973396650956563810594311683636302372685, &
-.000000000000002367542951027806958483322596219216855775572829, &
.000000000000000007046258782820854043105126774461954927903490, &
-.000000000000000000017019948750775009765954412498700374221989, &
.000000000000000000000034039897501550019531908824997400748443, &
-.000000000000000000000000057306224750084207966176473059597219, &
.000000000000000000000000000082336529813339379261747806120110, &
-.000000000000000000000000000000102154503490495507768917873598, &
.000000000000000000000000000000000110556821959410722693634062, &
-.000000000000000000000000000000000000105292211389914973993937, &
.009523809523809523809523809523809523809523809523809523809523, &
-.000529100529100529100529100529100529100529100529100529100529, &
.000012025012025012025012025012025012025012025012025012025012, &
-.000000154166820833487500154166820833487500154166820833487500, &
.000000001284723506945729167951390173612395834618056840279062, &
-.000000000007557197099680759811478765727131740203635628472229, &
.000000000000033145601314389297418766516347069035980858019615, &
-.000000000000000112740140525133664689682028391391278846455849, &
.000000000000000000306359077513950175787179424976606735995803, &
-.000000000000000000000680797950031000390638176499948014968879, &
.000000000000000000000001260736944501852575255882407311138831, &
-.000000000000000000000000001976076715520145102281947346882662, &
.000000000000000000000000000002656017090752883201991864713551, &
-.000000000000000000000000000000003095591014863500235421753745, &
.000000000000000000000000000000000003158766341697449219818116, &
-.000000000000000000000000000000000000002845735442970674972809/), (/16,4/))

  integer :: is
  real :: t
  if(l>3 .or. l<0) ERROR("l>3 or l<0")
  if(abs(x) < .01) then
    t = 0.0
    do is=15,0,-1
      t = t + x**(2*is) * coeff(is,l)
    end do
    num_xmlsphbes = t
  else
    select case(l)
    case(0)
      num_xmlsphbes = sin(x)/x
    case(1)
      num_xmlsphbes = (sin(x)-x*cos(x))/x**3
    case(2)
      num_xmlsphbes = ((3.0/x - x)*sin(x) - 3.0*cos(x)) / x**4
    case(3)
      num_xmlsphbes = ((15.0/x - 6.0*x)*sin(x) + (x**2-15.0)*cos(x))/x**6
    case default
      ERROR("")
    end select
  end if
end function num_xmlsphbes

function num_inverse_tensor_r(tensor) result(inverse)
! same as num_inverse_tensor_c, wrapped for real argument
  real, intent(in)  :: tensor(3,3)
  real              :: inverse(3,3)
  inverse = num_inverse_tensor_c(cmplx(tensor))
end function num_inverse_tensor_r

!@ MANUAL
function num_inverse_tensor_c(tensor) result(inverse)
! Calculates the inverse tensor defined as the spherical average of
! q_a q_b / (q_c T_cd q_d)
  use num_constants_module
  complex, intent(in)  :: tensor(3,3)
  complex              :: inverse(3,3)
!@ END MANUAL

  integer, parameter :: n=500
!  real,    parameter :: d=1.0/real(n)
  real, parameter :: d=0.002000
  complex :: tmp
  complex :: inv(3,3)

  real,    save :: ct(n),st(n),cct(n),sst(n),cst(n)
  real,    save :: cp(n),sp(n),ccp(n),ssp(n),csp(n)
  real,    save :: w(n)
  logical, save :: initialized = .false.

  integer :: i,i1,i2

  if(.not. initialized) then
    do i=1,n
      ct(i)  = 2.0*(i*d)-1.0
      cct(i) = ct(i)**2
      sst(i) = 1.0 - cct(i)
      if(i<n) then
        st(i) = sqrt(sst(i))
      else
        st(i) = 0.0 ! it's safer when optimizing, it could be sst(n)=-epsilon
      end if
      cst(i) = ct(i)*st(i)
      cp(i)  = cos(num_2pi*i*d)
      sp(i)  = sin(num_2pi*i*d)
      ccp(i) = cp(i)**2
      ssp(i) = sp(i)**2
      csp(i) = cp(i)*sp(i)
    end do
    w(1::2) = 4.0 / 3.0
    w(2::2) = 2.0 / 3.0
    initialized = .true.
  end if

  inv  = 0.0

  do i2=1,n
    do i1=1,n
      tmp = tensor(1,1)               * sst(i1) * ccp(i2) +  &
            (tensor(1,2)+tensor(2,1)) * sst(i1) * csp(i2) +  &
            (tensor(1,3)+tensor(3,1)) * cst(i1) * cp(i2)  +  &
            tensor(2,2)               * sst(i1) * ssp(i2) +  &
            (tensor(2,3)+tensor(3,2)) * cst(i1) * sp(i2)  +  &
            tensor(3,3)               * cct(i1)
      tmp  = d * d * w(i1)*w(i2) / tmp
      inv(1,1) = inv(1,1) + tmp * sst(i1) * ccp(i2)
      inv(2,1) = inv(2,1) + tmp * sst(i1) * csp(i2)
      inv(3,1) = inv(3,1) + tmp * cst(i1) * cp(i2)
      inv(1,2) = inv(1,2) + tmp * sst(i1) * csp(i2)
      inv(2,2) = inv(2,2) + tmp * sst(i1) * ssp(i2)
      inv(3,2) = inv(3,2) + tmp * cst(i1) * sp(i2)
      inv(1,3) = inv(1,3) + tmp * cst(i1) * cp(i2)
      inv(2,3) = inv(2,3) + tmp * cst(i1) * sp(i2)
      inv(3,3) = inv(3,3) + tmp * cct(i1)
    end do
  end do
  inverse = inv
end function num_inverse_tensor_c

!@ MANUAL
function num_discontinuity_value(lattice,cutoff,epsilon,security,iopt) result(value)
! Returns the value of v(k=0) for the sum over special points.
! If epsilon is not present (or the identical matrix), the
! reference function is v(k) = 8pi/(k**2)
! If epsilon is present, the reference function is v(k) = 8pi/(k epsilon k)
! If iopt is present and equal to 1, the reference function is
! v(k) = 8pi (k epsilon k) / k**4 ! CHECK IF THIS IS CORRECT !
! lattice(:,1),(:,2) and (:,3) are the generating vectors for the reciprocal
! space mesh (e.g. in a Monkorst-Pack grid are b/N)
! security is a convergence factor, 4.0 (default) should be ok
  use num_la_module
  use num_constants_module
  real,              intent(in) :: lattice(3,3)
  real,              intent(in) :: cutoff 
  complex, optional, intent(in) :: epsilon(3,3)
  real,    optional, intent(in) :: security
  integer, optional, intent(in) :: iopt
!@ END MANUAL
  complex  :: value
  real :: inverse_lattice(3,3)
  real :: volume
  real :: sigma
  real :: security_loc
  real :: k(3),k2
  complex :: k2_eps
  integer :: n(3),i1,i2,i3
  real :: vlr0,vsk0,vl
  complex :: average_epsilon,inverse_average_epsilon
  integer :: iopt_loc
!  real :: cutoff2pi

  security_loc = 3.0
  if(present(security)) security_loc = security

  iopt_loc = 0
  if(present(iopt)) iopt_loc = iopt

  volume = (2.0 * num_pi)**3 / abs(num_determinant(lattice))

!  cutoff2pi = cutoff/num_2pi**2

  inverse_lattice = num_inverse(lattice)
! lattice is the new b, inverse_lattice is the new cell (real space)
  sigma = sqrt(cutoff)*maxval(sqrt(sum(lattice**2,1)))/security_loc
!  sigma = security_loc * maxval(sqrt(sum(lattice**2,1)))

!  n     = 8.0 * sigma * sqrt(sum(inverse_lattice**2,1))
   
  n(1) = sqrt(cutoff*sum(inverse_lattice(:,1)**2)) + 2
  n(2) = sqrt(cutoff*sum(inverse_lattice(:,2)**2)) + 2
  n(3) = sqrt(cutoff*sum(inverse_lattice(:,3)**2)) + 2 

!  write(0,*) "from discontinuity value n", n(1), n(2), n(3)

  vl = 0.0
  do i3=-n(3),+n(3)
    do i2=-n(2),+n(2)
      do i1=-n(1),+n(1)
      if(i1==0 .and. i2==0 .and. i3==0) cycle
      k = num_matmul(lattice,real((/i1,i2,i3/)))
      k2 = sum(k**2)
      if(k2>cutoff) cycle
      if(present(epsilon)) then
        k2_eps = dot_product(k,num_matmul(epsilon,cmplx(k)))
      else
        k2_eps = k2
      end if
      select case(iopt_loc)
      case(0)
        vl = vl + exp(-k2/(2.0*sigma**2))/k2_eps
      case(1)
        vl = vl + exp(-k2/(2.0*sigma**2)) * k2_eps / (k2 * k2)
      end select
      end do
    end do
  end do
  vl= vl * 8.0*num_pi
  select case(iopt_loc)
  case(0)
    inverse_average_epsilon = 1.0
    if(present(epsilon)) inverse_average_epsilon = num_trace(num_inverse_tensor(epsilon))
    vlr0 = 2.0 * sqrt(2.0/num_pi) * sigma * inverse_average_epsilon
    vsk0 = 4.0*num_pi / sigma**2          * inverse_average_epsilon
  case(1)
    average_epsilon = 1.0
    if(present(epsilon)) average_epsilon = num_trace(epsilon) / 3.0
    vlr0 = 2.0 * sqrt(2.0/num_pi) * sigma * average_epsilon
    vsk0 = 4.0*num_pi / sigma**2          * average_epsilon
  end select
  value = (volume * vlr0 - vl + vsk0)

!  write(0,*) "from discontinuity value, vlr0,vl,vsk0", volume*vlr0, vl, vsk0
end function num_discontinuity_value

function num_discontinuity_value_old(lattice,epsilon,security,iopt) result(value)
! Returns the value of v(k=0) for the sum over special points.
! If epsilon is not present (or the identical matrix), the
! reference function is v(k) = 8pi/(k**2)
! If epsilon is present, the reference function is v(k) = 8pi/(k epsilon k)
! If iopt is present and equal to 1, the reference function is
! v(k) = 8pi (k epsilon k) / k**4 ! CHECK IF THIS IS CORRECT !
! lattice(:,1),(:,2) and (:,3) are the generating vectors for the reciprocal
! space mesh (e.g. in a Monkorst-Pack grid are b/N)
! security is a convergence factor, 4.0 (default) should be ok
  use num_la_module
  use num_constants_module
  real,              intent(in) :: lattice(3,3)
  real, optional, intent(in) :: epsilon(3,3)
! from the original discontinuity_value change epsilon from cmplx to real
  real,    optional, intent(in) :: security
  integer, optional, intent(in) :: iopt
!@ END MANUAL
  complex  :: value
  real :: inverse_lattice(3,3)
  real :: volume
  real :: sigma
  real :: security_loc
  real :: k(3),k2
  complex :: k2_eps
  integer :: n(3),i1,i2,i3
  real :: vlr0,vsk0,vl
  complex :: average_epsilon,inverse_average_epsilon
  integer :: iopt_loc

  security_loc = 4.0
  if(present(security)) security_loc = security

  iopt_loc = 0
  if(present(iopt)) iopt_loc = iopt

  volume = (2.0 * num_pi)**3 / abs(num_determinant(lattice))

  inverse_lattice = num_inverse(lattice)

  sigma = security_loc * maxval(sqrt(sum(lattice**2,1)))
  n     = 8.0 * sigma * sqrt(sum(inverse_lattice**2,1))
  vl = 0.0
  do i3=-n(3),+n(3)
    do i2=-n(2),+n(2)
      do i1=-n(1),+n(1)
      if(i1==0 .and. i2==0 .and. i3==0) cycle
      k = num_matmul(lattice,real((/i1,i2,i3/)))
      k2 = sum(k**2)
      if(present(epsilon)) then
        k2_eps = dot_product(k,num_matmul(epsilon,k))
      else
        k2_eps = k2
      end if
      select case(iopt_loc)
      case(0)
        vl = vl + exp(-k2/(2.0*sigma**2))/k2_eps
      case(1)
        vl = vl + exp(-k2/(2.0*sigma**2)) * k2_eps / (k2 * k2)
      end select
      end do
    end do
  end do
  vl= vl * 8.0*num_pi
  select case(iopt_loc)
  case(0)
    inverse_average_epsilon = 1.0
    if(present(epsilon)) inverse_average_epsilon = num_trace(num_inverse_tensor(epsilon))
    vlr0 = 2.0 * sqrt(2.0/num_pi) * sigma * inverse_average_epsilon
    vsk0 = 4.0*num_pi / sigma**2          * inverse_average_epsilon
  case(1)
    average_epsilon = 1.0
    if(present(epsilon)) average_epsilon = num_trace(epsilon) / 3.0
    vlr0 = 2.0 * sqrt(2.0/num_pi) * sigma * average_epsilon
    vsk0 = 4.0*num_pi / sigma**2          * average_epsilon
  end select
  value = (volume * vlr0 - vl + vsk0)
end function num_discontinuity_value_old

!@ MANUAL
function num_vlr0_plus_vsk0_value(lattice,cutoff,epsilon,security,iopt) result(value)
! Returns the value of v(k=0) for the sum over special points.
! If epsilon is not present (or the identical matrix), the
! reference function is v(k) = 8pi/(k**2)
! If epsilon is present, the reference function is v(k) = 8pi/(k epsilon k)
! If iopt is present and equal to 1, the reference function is
! v(k) = 8pi (k epsilon k) / k**4 ! CHECK IF THIS IS CORRECT !
! lattice(:,1),(:,2) and (:,3) are the generating vectors for the reciprocal
! space mesh (e.g. in a Monkorst-Pack grid are b/N)
! security is a convergence factor, 4.0 (default) should be ok
  use num_la_module
  use num_constants_module
  real,              intent(in) :: lattice(3,3)
  real,              intent(in) :: cutoff 
  complex, optional, intent(in) :: epsilon(3,3)
  real,    optional, intent(in) :: security
  integer, optional, intent(in) :: iopt
!@ END MANUAL
  complex  :: value
  real :: inverse_lattice(3,3)
  real :: volume
  real :: sigma
  real :: security_loc
  real :: k(3),k2
  complex :: k2_eps
  integer :: n(3),i1,i2,i3
  real :: vlr0,vsk0
  complex :: average_epsilon,inverse_average_epsilon
  integer :: iopt_loc
  real :: beta

  security_loc = 3.0
  if(present(security)) security_loc = security

  iopt_loc = 0
  if(present(iopt)) iopt_loc = iopt

  volume = (2.0 * num_pi)**3 / abs(num_determinant(lattice))

  inverse_lattice = num_inverse(lattice)

!  sigma = sqrt(cutoff)*maxval(sqrt(sum(lattice**2,1)))/security_loc
!  sigma = 3.0/0.5 * maxval(sqrt(sum(lattice**2,1)))/(num_pi * 2.0)
  sigma = 3.0/0.5 * 1.0/(sqrt(1.0/maxval(sum(lattice**2,1)))*(num_pi * 2.0))
  beta = sigma * sigma * 0.25 
!  sigma = security_loc * maxval(sqrt(sum(lattice**2,1)))

  select case(iopt_loc)
  case(0)
    inverse_average_epsilon = 1.0
    if(present(epsilon)) inverse_average_epsilon = num_trace(num_inverse_tensor(epsilon))
    vlr0 = 2.0 * sqrt(2.0/num_pi) * sigma * inverse_average_epsilon
!    vsk0 = 4.0*num_pi / sigma**2 + 4.0*num_pi*(beta*0.5)         * inverse_average_epsilon
    vsk0 = 4.0*num_pi / sigma**2         * inverse_average_epsilon
  case(1)
    average_epsilon = 1.0
    if(present(epsilon)) average_epsilon = num_trace(epsilon) / 3.0
    vlr0 = 2.0 * sqrt(2.0/num_pi) * sigma * average_epsilon
!    vsk0 = 4.0*num_pi / sigma**2 + 4.0*num_pi*(beta*0.5)        * average_epsilon
    vsk0 = 4.0*num_pi / sigma**2         * average_epsilon
  end select
  value = (volume * vlr0 + vsk0)
end function num_vlr0_plus_vsk0_value

!@ MANUAL
function num_vl_value(lattice,cutoff,epsilon,security,iopt) result(value)
! Returns the value of v(k=0) for the sum over special points.
! If epsilon is not present (or the identical matrix), the
! reference function is v(k) = 8pi/(k**2)
! If epsilon is present, the reference function is v(k) = 8pi/(k epsilon k)
! If iopt is present and equal to 1, the reference function is
! v(k) = 8pi (k epsilon k) / k**4 ! CHECK IF THIS IS CORRECT !
! lattice(:,1),(:,2) and (:,3) are the generating vectors for the reciprocal
! space mesh (e.g. in a Monkorst-Pack grid are b/N)
! security is a convergence factor, 4.0 (default) should be ok
  use num_la_module
  use num_constants_module
  real,              intent(in) :: lattice(3,3)
  real,              intent(in) :: cutoff
  complex, optional, intent(in) :: epsilon(3,3)
  real,    optional, intent(in) :: security
  integer, optional, intent(in) :: iopt
!@ END MANUAL
  complex  :: value
  real :: inverse_lattice(3,3)
  real :: volume
  real :: sigma
  real :: security_loc
  real :: k(3),k2
  complex :: k2_eps
  integer :: n(3),i1,i2,i3
  real :: vl
  complex :: average_epsilon,inverse_average_epsilon
  integer :: iopt_loc
!  real :: cutoff2pi

  security_loc = 3.0
  if(present(security)) security_loc = security

  iopt_loc = 0
  if(present(iopt)) iopt_loc = iopt

  volume = (2.0 * num_pi)**3 / abs(num_determinant(lattice))

!  cutoff2pi = cutoff/num_2pi**2

  inverse_lattice = num_inverse(lattice)

!  sigma = security_loc * maxval(sqrt(sum(lattice**2,1)))
  sigma = sqrt(cutoff)*maxval(sqrt(sum(lattice**2,1)))/security_loc


!  n     = 8.0 * sigma * sqrt(sum(inverse_lattice**2,1))

  n(1) = sqrt(cutoff*sum(inverse_lattice(:,1)**2)) + 2
  n(2) = sqrt(cutoff*sum(inverse_lattice(:,2)**2)) + 2
  n(3) = sqrt(cutoff*sum(inverse_lattice(:,3)**2)) + 2 

  write(0,*) "from num_vl, n", n(1), n(2), n(3)

  vl = 0.0
  do i3=-n(3),+n(3)
    do i2=-n(2),+n(2)
      do i1=-n(1),+n(1)
      if(i1==0 .and. i2==0 .and. i3==0) cycle
      k = num_matmul(lattice,real((/i1,i2,i3/)))
      k2 = sum(k**2)
      if(k2>cutoff) cycle
      if(present(epsilon)) then
        k2_eps = dot_product(k,num_matmul(epsilon,cmplx(k)))
      else
        k2_eps = k2
      end if
      select case(iopt_loc)
      case(0)
        vl = vl + exp(-k2/(2.0*sigma**2))/k2_eps
      case(1)
        vl = vl + exp(-k2/(2.0*sigma**2)) * k2_eps / (k2 * k2)
      end select
      end do
    end do
  end do
  vl= vl * 8.0*num_pi
  write(0,*) "from num_vl,vl:", vl
  value = vl
end function num_vl_value

function num_v0_vcut_spheric_value(lattice,epsilon) result(value)
! Returns the value of v(k=0) for sperical cutoff on the supercell.
! If epsilon is not present (or the identical matrix), the
! reference function is v(k) = 8pi/(k**2)
! If epsilon is present, the reference function is v(k) = 8pi/(k epsilon k)
! lattice(:,1),(:,2) and (:,3) are the generating vectors for the reciprocal
! space mesh (e.g. in a Monkorst-Pack grid are b/N)
  use num_la_module
  use num_constants_module
  real,              intent(in) :: lattice(3,3)
  complex, optional, intent(in) :: epsilon(3,3)
! ??? non mi ricordo!!! from the original discontinuity_value change epsilon from cmplx to real
!@ END MANUAL
  complex  :: value
  real :: inverse_lattice(3,3)
  real :: volume
  real :: v0, Rcut
  complex :: inverse_average_epsilon

  volume = (2.0 * num_pi)**3 / abs(num_determinant(lattice))

  inverse_lattice = num_inverse(lattice)
  inverse_lattice = inverse_lattice * num_2pi

  Rcut=0.5*minval(sqrt(sum(inverse_lattice**2,1)))
  Rcut=Rcut-Rcut/50.0

  inverse_average_epsilon = 1.0
  if(present(epsilon)) inverse_average_epsilon = num_trace(num_inverse_tensor(epsilon))
  v0 =  num_8pi* Rcut**2/2.0 * inverse_average_epsilon
  value = v0

end function num_v0_vcut_spheric_value



function num_erfc(x) result(res)
! returns the complementary error function
  use numrec_module, only : numrec_erfcc
  implicit none
  real :: res
  real, intent(in) :: x
  res = numrec_erfcc(x)
end function num_erfc

complex function num_pole_total_integral(x,f,order,pole)
  implicit none
  real,    intent(in) :: x(:)
  integer, intent(in) :: order
  complex, intent(in) :: f(:)
  complex, intent(in) :: pole
  integer :: n,i
  complex :: tot
  real    :: xmin,xmax
 
  n = size(f)
  if(size(x)/=n) ERROR("")
  tot = 0.0
  xmin = x(1)
  if(modulo(order,2)==0) then
    xmax = 0.5*(x((order+2)/2) + x((order+4)/2))
  else
    xmax = x((order+3)/2)
  end if
  tot = tot + num_pole_integral(x(1:1+order),f(1:1+order),xmin,xmax,pole)
  do i=2,n-order-1
    if(modulo(order,2)==0) then
      xmin = 0.5*(x(i+(order-2)/2) + x(i+(order)/2))
      xmax = 0.5*(x(i+(order)/2)   + x(i+(order+2)/2))
    else
      xmin = x(i+(order-1)/2)
      xmax = x(i+(order+1)/2)
    end if
    tot = tot + num_pole_integral(x(i:i+order),f(i:i+order),xmin,xmax,pole)
  end do
  if(modulo(order,2)==0) then
    xmin = 0.5*(x(n+1-(order+2)/2) + x(n+1-(order+4)/2))
  else
    xmin = x(n+1-(order+3)/2)
  end if
  xmax = x(n)
  tot = tot + num_pole_integral(x(n-order:n),f(n-order:n),xmin,xmax,pole)
  num_pole_total_integral = tot
end function num_pole_total_integral

complex function num_pole_integral(x,f,xmin,xmax,pole)
  use numrec_module, only : numrec_polcoe
  implicit none
  real,    intent(in) :: x(:)
  real,    intent(in) :: xmin,xmax
  complex, intent(in) :: f(:)
  complex, intent(in) :: pole
  integer :: n,i
  complex :: a(0:size(x)-1)
  complex :: int(0:size(x)-1)
  real    :: p1,p2
  complex    :: xmp(size(x))
  n = size(f)
  if(size(x)/=n) ERROR("")
  p1 = real(pole)
  p2 = aimag(pole)
  int(0) = 0.5*log(abs(((xmax-p1)**2+p2**2)/((xmin-p1)**2+p2**2))) + &
         (0.0,1.0) * (atan((xmax-p1)/p2)-atan((xmin-p1)/p2))
  do i=1,n-1
    int(i) = 1/real(i) * ((xmax-pole)**i - (xmin-pole)**i)
  end do
  xmp = x - pole
  call numrec_polcoe(xmp,f,n,a)
!  write(*,"(a,2f15.9)") "##X",x
!  write(*,"(a,4f15.9)") "##F",f
!  write(*,"(a,4f15.9)") "%%A",a
  num_pole_integral = sum(a(:)*int(:))
end function num_pole_integral

function num_kk_coeff(newx,oldx,delta)
  use num_constants_module
! advanced, imaginary to real
  real, intent(in) :: newx(:),oldx(:)
  real, intent(in) :: delta    
  real :: num_kk_coeff(ubound(newx,1),ubound(oldx,1))
  integer :: i
  do  i = 1 , ubound(newx,1)
    num_kk_coeff(i,:) = -1.0/num_pi * &
     num_pole_integral_coeff_v(oldx,newx(i)+cmplx(0.0,delta),order=3) 
  end do
end function num_kk_coeff

function num_kk_ccoeff(newx,oldx,delta)
  use num_constants_module
! advanced, imaginary to full
  real, intent(in) :: newx(:),oldx(:)
  real, intent(in) :: delta
  complex :: num_kk_ccoeff(ubound(newx,1),ubound(oldx,1))
  integer :: i
  do  i = 1 , ubound(newx,1)
    num_kk_ccoeff(i,:) = -1.0/num_pi * &
     num_pole_integral_coeff_v(oldx,newx(i)+cmplx(0.0,delta),order=3)
  end do
end function num_kk_ccoeff


function num_pole_integral_coeff_m(x,poles,order)
  real,    intent(in) :: x(:)
  integer, intent(in) :: order
  complex, intent(in) :: poles(:)
  complex             :: num_pole_integral_coeff_m(ubound(poles,1),ubound(x,1))
  integer :: i
  do i = 1 , ubound(poles,1)
    num_pole_integral_coeff_m(i,:) = num_pole_integral_coeff_v(x,poles(i),order)
  end do
end function num_pole_integral_coeff_m

function num_pole_integral_coeff_v(x,pole,order)
  use numrec_module, only : numrec_polcoe
  implicit none
  real,    intent(in) :: x(:)
  integer, intent(in) :: order
  complex, intent(in) :: pole
  complex             :: num_pole_integral_coeff_v(ubound(x,1))

  integer :: n,i,i1,imin,imax
  complex :: coefficients(ubound(x,1))
  complex :: trial(-order:+order)
  complex :: xtmp(ubound(x,1))
  real    :: logs(ubound(x,1)),atans(ubound(x,1))
  real    :: arg(ubound(x,1))
  complex :: polin(0:order)
  complex :: integrals(ubound(x,1),0:order)

  if(modulo(order,2)==0) ERROR("")

  trial    = 0.0
  trial(0) = 1.0

  coefficients = 0

  n = ubound(x,1)

  xtmp = x - pole
  arg   = (real(xtmp))**2+aimag(pole)**2
  logs  = log(arg)
  arg   = (real(xtmp))/aimag(pole)
  atans = atan(arg)
  integrals(:,0) = cmplx(0.5*logs,atans)
  do i=1,order
    integrals(:,i) = 1/real(i) * xtmp(:)**i
  end do

  do i = 1 , n-order
    if(i>1) then
      imin = i+(order-1)/2
    else
      imin = 1
    end if
    if(i<n-order) then
      imax = i+(order+1)/2
    else
      imax = n
    end if
    do i1 = 0 , order
      call numrec_polcoe(xtmp(i:i+order),trial(-i1:-i1+order),order+1,polin)
      coefficients(i+i1) = coefficients(i+i1) + sum(polin*(integrals(imax,:)-integrals(imin,:)))
    end do
  end do

  num_pole_integral_coeff_v = coefficients

end function num_pole_integral_coeff_v

function num_gaussian_pole(x,sigma)
  use num_la_module
  use num_interpolation_module
  use num_constants_module
  real, intent(in) :: x,sigma
  complex :: num_gaussian_pole

  logical, save :: init = .false.
  type(num_interpolation), save :: table
  real, allocatable :: imaginary(:)
  real, allocatable :: imaginary_grid(:)
  integer :: i
  real :: xscale,realpart

  if(.not.init) then
    call num_interpolation_init(table,0.0,15.0,0.01,parity=-1)
    allocate(imaginary(-1000:1000),imaginary_grid(-1000:1000))
    do i=-1000,1000
      imaginary_grid(i) = i * 15.0/real(1000)
      imaginary(i) = num_pi/sqrt(2.0*num_pi) * exp(-0.5*imaginary_grid(i)**2)
    end do
    do i=0,table%n
      table%y(i:i) = num_matmul(num_kk_coeff(table%x(i:i),imaginary_grid,0.000000001),imaginary)
    end do
    deallocate(imaginary,imaginary_grid)
    init = .true.
  end if

  xscale = x/sigma

  if(abs(xscale)<15.0) then
    realpart = num_interpolation_calc(table,x=xscale,order=3)
  else
    realpart = 1.0/xscale + 1.0/xscale**3 + 3.0/xscale**5 + 15.0/xscale**7 + 105.0/xscale**9 + 945.0/xscale**11
  end if

  num_gaussian_pole = cmplx(realpart,num_pi/sqrt(2.0*num_pi)*exp(-0.5*(xscale)**2)) / sigma
  
  
end function num_gaussian_pole

!@ MANUAL
subroutine num_gaussmesh(alpha,x,w)
! Calculates the points and weights for a Gauss-Legendre integral between 0 and infinity
! That is, \int_0^{+\infty} dx f(x) = \sum_i w(i) * f(x(i))
! The integral is calculated through a change of variable:
!    x = alpha*tan(s)
!   dx = alpha*(1+tan(s)**2) * ds
  use num_constants_module
  use numrec_module, only : numrec_gauleg
  real,    intent(in)  :: alpha
  real,    intent(out) :: x(:) ! the number of points is obtained from the size of these arrays
  real,    intent(out) :: w(:)
!@ END MANUAL
  real :: s(ubound(x,1))
  real :: ws(ubound(w,1))
  if(size(x)/=size(w)) ERROR("")
  call numrec_gauleg(0.0,0.5*num_pi,s,ws,size(s))
  x = alpha * tan(s)
  w = alpha * (1.0+tan(s)**2) * ws
end subroutine num_gaussmesh

subroutine num_mesh(alpha,x,w)
! Calculates the points and weights for a uniform grid 
  use num_constants_module
  real,    intent(in)  :: alpha
  real,    intent(out) :: x(:) ! the number of points is obtained from the size of these arrays
  real,    intent(out) :: w(:)

  integer :: m,i
  real :: deltax
  if(size(x)/=size(w)) ERROR("")
  m=size(x)
  deltax=alpha/real(m)
  do i=1,size(x)
    x(i)=0.0+real(i-1)*deltax
  enddo
  w(:)=1.0
end subroutine num_mesh


end module num_special_module
