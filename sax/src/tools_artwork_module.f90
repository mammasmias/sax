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

module tools_artwork_module
implicit none
private

public :: tools_artwork_print

contains

subroutine tools_artwork_print(unit,artwork)
  integer,                    intent(in) :: unit
  character(len=*),           intent(in) :: artwork
  select case(artwork)
  case("sax")
    write(unit,"(a)") "    "   
    write(unit,"(a)") "          *            @@@@      @@     @@"
    write(unit,"(a)") "         * *o         @@          @@   @@"
    write(unit,"(a)") "        *** *o         @@          @@ @@"
    write(unit,"(a)") "       **  * *o         @@  @@@@   @@ @@"
    write(unit,"(a)") "       v    * *o        @@ @@  @  @@   @@"
    write(unit,"(a)") "             * *     @@@@  @@@@@ @@     @@"
    write(unit,"(a)") "              * *"
    write(unit,"(a)") "               * *"
    write(unit,"(a)") "               * *  *******"
    write(unit,"(a)") "               * *  *    *"
    write(unit,"(a)") "               * * *    *"
    write(unit,"(a)") "               *  *    *"
    write(unit,"(a)") "               *      *"
    write(unit,"(a)") "               *     *"
    write(unit,"(a)") "                *   *"
    write(unit,"(a)") "                 ***"
    write(unit,"(a)") "     "
    write(unit,"(a)") "FOLLOW THE WHITE RABBIT ...and try to catch it!"
  case ("smile")
    write(unit,"(a)") ":-)"
  case ("rabbit")
    write(unit,"(a)") " "
    write(unit,"(a)")      "            *     *            "
    write(unit,"(a)")      "           * *   * *           "
    write(unit,"(a)")      "           * *   * *           "
    write(unit,"(a)")      "           * *   * *           "
    write(unit,"(a)")      "           * *   * *           "
    write(unit,"(a)")      "           * * * * *           "
    write(unit,"(a)")      "          *         *          "
    write(unit,"(a)")      "          *  @   @  *          "
    write(unit,"(a)")      "          *    ^    *          "
    write(unit,"(a)")      "          *   )-(   *          "
    write(unit,"(a)")      "           *       *           "
    write(unit,"(a)")      "            *     *            "
    write(unit,"(a)")      "           *       *           "
    write(unit,"(a)")      "       =================       "
    write(unit,"(a)")      "         |           |         "
    write(unit,"(a)")      "         |===========|         "
    write(unit,"(a)")      "         |===========|         "
    write(unit,"(a)")      "         |           |         "
    write(unit,"(a)")      "         |           |         "
    write(unit,"(a)")      "         |           |         "
    write(unit,"(a)")      "         -------------         "
    write(unit,"(a)") " "
    write(unit,"(a)") "  WELCOME TO THE DARK SIDE OF FFT!    "
    write(unit,"(a)") " "
  case default
    write(unit,"(a)") "UNKNONW ARTWORK"
  end select
end subroutine tools_artwork_print

end module tools_artwork_module

