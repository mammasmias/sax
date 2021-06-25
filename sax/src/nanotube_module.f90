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

module nanotube_module
implicit none
! This module contains tools to generate
! atomic positions in nanotubes

! I don't use num_module to avoid dependencies
real, parameter :: pi = 3.1415926535897932384626433832795028841968

! Definition of type nanotube
type nanotube
  integer       :: n,m   ! Generating vector (on a_1 a_2 basis)
  integer       :: nn,mm ! N and M in Saito notation
  integer       :: t1,t2 ! Translational vector (on a_1 a_2 basis)
  integer       :: r1,r2 ! 'Unrolled' rotational vector (on a_1 a_2 basis)
  real          :: ac    ! interatomic distance
  real          :: d_stretch,t_stretch ! stretching factors on radial and translational directions
  real          :: dt,tt ! diameter and length of the translational cell
  real, pointer :: pos(:,:) ! atomic positions (3d)
end type nanotube

contains

! Creates a nanotube object
subroutine nanotube_create(nt,n,m,ac,d_stretch,t_stretch)
  type(nanotube), intent(out) :: nt
  integer,        intent(in)  :: n,m
  real,           intent(in)  :: ac
  real, optional, intent(in)  :: d_stretch,t_stretch
! Default d_stretch=1.0 t_stretch=1.0
  integer dr,iatom
  real :: pos2d(2)
  real :: d_stretchl,t_stretchl

  d_stretchl = 1.0
  if(present(d_stretch)) d_stretchl = d_stretch
  t_stretchl = 1.0
  if(present(t_stretch)) t_stretchl = t_stretch

  nt%n = n
  nt%m = m
  nt%ac = ac
  nt%d_stretch = d_stretchl
  nt%t_stretch = t_stretchl
  dr = gcd(2*nt%n+nt%m,nt%n+2*nt%m)
  nt%t1 = (nt%n+2*nt%m)/dr
  nt%t2 = -(2*nt%n+nt%m)/dr
  nt%dt = nt%ac*sqrt(3*real(nt%n **2+nt%m **2+nt%n *nt%m )) / pi * nt%d_stretch
  nt%tt = nt%ac*sqrt(3*real(nt%t1**2+nt%t2**2+nt%t1*nt%t2))      * nt%t_stretch
  nt%nn = 2*(nt%m**2+nt%n**2+nt%n*nt%m)/dr
  call find_r(nt%r1,nt%r2,nt%n,nt%m,nt%t1,nt%t2,nt%nn)
  nt%mm = nt%m*nt%r1 - nt%n*nt%r2
  allocate(nt%pos(3,2*nt%nn))
  do iatom=1,nt%nn
    pos2d(1) = iatom * nt%r1
    pos2d(2) = iatom * nt%r2
    call pos2_pos3d(pos2d,nt%pos(:,iatom*2-1),nt%n,nt%m,nt%t1,nt%t2,nt%dt,nt%tt)
    pos2d(1) = iatom * nt%r1 + 1.0/3.0
    pos2d(2) = iatom * nt%r2 + 1.0/3.0
    call pos2_pos3d(pos2d,nt%pos(:,iatom*2),nt%n,nt%m,nt%t1,nt%t2,nt%dt,nt%tt)
  end do
  
end subroutine nanotube_create

subroutine nanotube_destroy(nt)
  type(nanotube), intent(inout) :: nt
  deallocate(nt%pos)
end subroutine nanotube_destroy

subroutine nanotube_write(nt,unit,fmt,scale,ncells)
  type(nanotube),   intent(in) :: nt
  integer,          intent(in) :: unit
  character(len=*), intent(in) :: fmt
  real, optional,   intent(in) :: scale
  integer, optional,intent(in) :: ncells
  integer :: lncells
  integer :: iatom
  integer :: qk1,qk2,qh1,qh2
  integer :: icell
  real :: lscale
  lscale = 1.0
  lncells = 1
  if(present(scale)) lscale = scale
  if(present(ncells)) lncells = ncells
  select case(fmt)
  case("log")
    write(unit,"(a,2i5)")     "(n,m)     ",nt%n,nt%m
    write(unit,"(a,i5)")      "N         ",nt%nn
    write(unit,"(a,i5)")      "M         ",nt%mm
    write(unit,"(a,2i5)")     "R         ",nt%r1,nt%r2
    write(unit,"(a,2i5)")     "T         ",nt%t1,nt%t2
    write(unit,"(a,f15.9)")   "ac        ",nt%ac
    write(unit,"(a,f15.9)")   "d_stretch ",nt%d_stretch
    write(unit,"(a,f15.9)")   "t_stretch ",nt%t_stretch
    write(unit,"(a,f15.9)")   "dt        ",nt%dt
    write(unit,"(a,f15.9)")   "tt        ",nt%tt
    qk1 = + nt%nn*nt%r2 - nt%mm*nt%t2
    qk2 = - nt%nn*nt%r1 + nt%mm*nt%t1
    qh1 = - nt%t2
    qh2 = + nt%t1
    write(unit,"(a,i5,i5,a)") "q_k       ",qk1,qk2,"(1/N)"
    write(unit,"(a,i5,i5,a)") "q_h       ",qh1,qh2,"(1/N)"
  case("pos")
    do iatom=1,2*nt%nn
      write(unit,"(3f15.9)") nt%pos(:,iatom)/lscale
    end do
  case("pos_pw")
    do iatom=1,2*nt%nn
      write(unit,"(3f15.9,i5)") nt%pos(:,iatom)/lscale,1
    end do
  case("xyz")
    write(unit,*) 2*nt%nn*lncells
    write(unit,*)
    do icell=1,lncells
      do iatom=1,2*nt%nn
        write(unit,"(a,3f15.9)") "C ",(nt%pos(:,iatom)+icell*(/0.0,0.0,nt%tt/))/lscale 
      end do
    end do
  case default
    stop "errore!"
  end select
end subroutine nanotube_write

function gcd(a,b)
! Calcola il Massimo Comun Divisore tra a e b
  integer a,b,gcd
  integer try
  if(a==0 .or. b==0) then
    gcd = 1
    return
  end if
  do try=a,1,-1
    if(modulo(a,try)==0 .and. modulo(b,try)==0) exit
  end do
  gcd = try
end function gcd

subroutine find_r(p,q,n,m,t1,t2,nn)
  integer p,q,n,m,t1,t2,nn
  integer p_try,q_try
  do p_try=1,10000
    q_try=(p_try*t2+1)/t1
    if(t1*q_try-t2*p_try==1 .and. m*p_try-n*q_try>0 .and. m*p_try-n*q_try<=nn) exit
  end do
  p=p_try
  q=q_try
end subroutine find_r

subroutine pos2_pos3d(pos2d,pos3d,n,m,t1,t2,diametro,lunghezza)
  integer n,m,t1,t2
  real    pos2d(2),pos3d(3),diametro,lunghezza,raggio
  real    theta,tau
  raggio = diametro*0.5
  tau    = (pos2d(1)*t1 + pos2d(2)*t2 + 0.5*pos2d(1)*t2 + 0.5*pos2d(2)*t1) / (t1**2+t2**2+t1*t2)
  theta  = (pos2d(1)*n  + pos2d(2)*m  + 0.5*pos2d(1)*m  + 0.5*pos2d(2)*n ) / (n**2 +m**2 +n*m) * 2.0 * pi
  pos3d(1) = raggio * cos(theta)
  pos3d(2) = raggio * sin(theta)
  pos3d(3) = lunghezza * modulo(tau,1.0)
end subroutine pos2_pos3d

function nanotube_tb(nt,q,ih,ip,haa,hab,sab,lambda_hab,lambda_sab,delta)
! Check: sembra che ci sia qualche problemino
  real                       :: nanotube_tb
  type(nanotube), intent(in) :: nt
  real,           intent(in) :: q
  integer,        intent(in) :: ih
  integer,        intent(in) :: ip
  real,           intent(in) :: haa,hab,sab
  real,           intent(in) :: lambda_hab,lambda_sab,delta

  real :: k1,k2
  real :: habv(3),sabv(3)
  complex :: habk,sabk,habk_bar

  integer :: n,m

  real :: sign,lambda_bar

  n = nt%n
  m = nt%m

  k1 = (+ m * q - nt%t2 * ih) / nt%nn
  k2 = (- n * q + nt%t1 * ih) / nt%nn

  sabv(1) = sab + lambda_sab * delta * (3.0 * (n+m)**2 )/(4.0 * (n**2+m**2+n*m))
  sabv(2) = sab + lambda_sab * delta * (3.0 * (2*n-m)**2 )/(4.0 * (n**2+m**2+n*m))
  sabv(3) = sab + lambda_sab * delta * (3.0 * (-n+2*m)**2 )/(4.0 * (n**2+m**2+n*m))

  habv(1) = hab + lambda_hab * delta * (3.0 * (n+m)**2 )/(4.0 * (n**2+m**2+n*m))
  habv(2) = hab + lambda_hab * delta * (3.0 * (2*n-m)**2 )/(4.0 * (n**2+m**2+n*m))
  habv(3) = hab + lambda_hab * delta * (3.0 * (-n+2*m)**2 )/(4.0 * (n**2+m**2+n*m))

  sabk = exp( 2.0 * pi * (0.0,1.0) / 3.0 * (       k1 +       k2)) * sabv(1) + &
         exp( 2.0 * pi * (0.0,1.0) / 3.0 * (-2.0 * k1 +       k2)) * sabv(2) + &
         exp( 2.0 * pi * (0.0,1.0) / 3.0 * (       k1 - 2.0 * k2)) * sabv(3)

  habk = exp( 2.0 * pi * (0.0,1.0) / 3.0 * (       k1 +       k2)) * habv(1) + &
         exp( 2.0 * pi * (0.0,1.0) / 3.0 * (-2.0 * k1 +       k2)) * habv(2) + &
         exp( 2.0 * pi * (0.0,1.0) / 3.0 * (       k1 - 2.0 * k2)) * habv(3)

  sign = 0.0
  if(ip>0) sign = 1.0
  if(ip<0) sign = -1.0

  habk_bar = habk * (1-sabk)

  lambda_bar = ( -real(conjg(habk_bar)*sabk) + sign * sqrt( real(conjg(habk_bar)*sabk)**2 + &
                  abs(habk)**2 - abs(conjg(habk_bar)*sabk)**2) ) &
               / (1-abs(sabk)**2)

  nanotube_tb = haa + lambda_bar
  
end function nanotube_tb

end module nanotube_module

