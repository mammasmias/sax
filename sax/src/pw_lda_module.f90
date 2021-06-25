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
module pw_lda_module
implicit none
private
!This module contains the LDA functionals
public :: pw_lda_vxc
!@ END MANUAL

contains

function pw_lda(density,ev,exchange,correlation)
  use num_module
  real             :: pw_lda ! in Rydberg oppure Rydberg / elettrone
  real, intent(in) :: density ! in (a.u.)^-3
  character(*), intent(in) :: exchange,correlation,ev
! exchange    = "sla" or "no"
! correlation = "pz"  or "gg"
! ev          = "e" or "v"
  real :: ec,vc,rs
  real, parameter :: pz_gamma = -0.2846,  &
                     pz_beta_1 = 1.0529,  &
                     pz_beta_2 = 0.3334,  &
                     pz_a      = 0.0622,  &
                     pz_b      = -0.096,  &
                     pz_c      = 0.004,   &
                     pz_d      = -0.0232, &
                     gg_gamma  = 2.0 * 0.04054, &
                     gg_beta1  = 2.086,   &
                     gg_beta2  = 0.1209,  &
                     sla_x     = -.91633058656628533332

  pw_lda = 0.0
  rs = exp (1.0 / 3.0 * log( 3.0 / (4.0 * num_pi * density)))
  if(trim(exchange)=="sla") then
    select case(trim(ev))
    case("e")
      pw_lda = pw_lda - sla_x / rs
    case("v")
      pw_lda = pw_lda - sla_x / rs * 4.0 / 3.0
    case default
      ERROR(trim(ev))
    end select
  end if
  if(trim(correlation)=="pz") then
    if(rs>1.0) then
      ec = pz_gamma / (1.0 + pz_beta_1*sqrt(rs) + pz_beta_2*rs)
    else
      ec = pz_a * log(rs) + pz_b + pz_c * rs * log(rs) + pz_d * rs
    end if
    select case(trim(ev))
    case("e")
      pw_lda = pw_lda + ec
    case("v")
      if(rs>1.0) then
        vc = ec * (1.0 + 7.0/6.0 * pz_beta_1*sqrt(rs) + 4.0/3.0 * pz_beta_2*rs) &
           /(1.0 + pz_beta_1 * sqrt(rs) + pz_beta_2 * rs)
      else
        vc = pz_a * log(rs) + pz_b - pz_a/3.0 + 2.0/3.0*pz_c*rs*log(rs) + &
           (2.0*pz_d-pz_c)/3.0 * rs
      end if
      pw_lda = pw_lda + vc
    case default
      ERROR(trim(ev))
    end select
  end if
  if(trim(correlation)=="gg") then
    if(rs>1.0) then
      ec = pz_gamma / (1.0 + pz_beta_1*sqrt(rs) + pz_beta_2*rs)
    else
      ec = pz_a * log(rs) + pz_b + pz_c * rs * log(rs) + pz_d * rs
    end if
    if(trim(ev)/="e") ERROR("")
    pw_lda = pw_lda + ec
  end if
end function pw_lda

function pw_lda_vxc(density,opt)
! Per l'implementazione di pz VEDI TESI DELLA CLELIA
  use num_module
  real             :: pw_lda_vxc ! in Rydberg
  real, intent(in) :: density ! in (a.u.)^-3
  character(*), intent(in) :: opt
  real :: vc,vx
  real :: rs ! in a.u.
  real :: ec
  real, parameter :: pz_gamma = -0.2846,  &
                     pz_beta_1 = 1.0529,  &
                     pz_beta_2 = 0.3334,  &
                     pz_a      = 0.0622,  &
                     pz_b      = -0.096,  &
                     pz_c      = 0.004,   &
                     pz_d      = -0.0232


  rs = exp (1.0 / 3.0 * log( 3.0 / (4.0 * num_pi * density)))

  vx = -1.222 / rs

  select case(opt)
  case("pz")
    if(rs>1.0) then
      ec = pz_gamma / (1.0 + pz_beta_1*sqrt(rs) + pz_beta_2*rs)
      vc = ec * (1.0 + 7.0/6.0 * pz_beta_1*sqrt(rs) + 4.0/3.0 * pz_beta_2*rs) &
          /(1.0 + pz_beta_1 * sqrt(rs) + pz_beta_2 * rs)
    else
      vc = pz_a * log(rs) + pz_b - pz_a/3.0 + 2.0/3.0*pz_c*rs*log(rs) + &
           (2.0*pz_d-pz_c)/3.0 * rs
    end if
  case default
    ERROR(opt)
  end select

  pw_lda_vxc = vc + vx
  
end function pw_lda_vxc
end module pw_lda_module
