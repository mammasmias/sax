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

!@ MANUAL
module num_constants_module
implicit none
! Module containing physical and mathematical constants
!@ END MANUAL

!@ MANUAL
public

real,    parameter :: num_sqrt2   = 1.41421356237309504880
real,    parameter :: num_pi      = 3.14159265358979323844
real,    parameter :: num_2pi     = num_pi * 2.0
real,    parameter :: num_4pi     = num_pi * 4.0
real,    parameter :: num_8pi     = num_pi * 8.0
real,    parameter :: num_sqrtpi  = 1.77245385090551602729
real,    parameter :: num_sqrt8pi = 2.0 * num_sqrt2 * num_sqrtpi
complex, parameter :: num_2pi_i   = num_pi * (0.0,2.0)
complex, parameter :: num_pi_i    = num_pi * (0.0,1.0)

! Rydberg <-> eV
real, parameter :: num_ry2ev = 13.60569172
real, parameter :: num_ev2ry = 1.0/num_ry2ev

! a.u. <-> Angstrom
real, parameter :: num_au2a  = 0.5291772083 
real, parameter :: num_a2au  = 1.0/0.5291772083 
!@ END MANUAL

end module num_constants_module
