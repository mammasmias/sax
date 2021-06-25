
! Copyright (C) 2005 Giovanni Bussi
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .

!*************************
MODULE ffti_module
   !*************************
   !
   ! Contains the list of public objects from FFTI pack;
   ! NOTE: commented routines are not properly implemented
   !       in the current version of the module.
   !
   USE ffti_base
   USE ffti_interface
   PRIVATE
   !
   PUBLIC :: ffti_set, ffti_which
   PUBLIC :: ffti_good_dimension, ffti_allowed, ffti_good_order
   PUBLIC :: ffti_1dz
!   PUBLIC :: ffti_2dxy
   PUBLIC :: ffti_3d
!   PUBLIC :: ffti_3db
!   PUBLIC :: ffti_3ds
   !
END MODULE ffti_module

