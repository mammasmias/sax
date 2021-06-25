/* /Users/marsamos/SaX_svn/SaX/ffti/include/ffti_cdefs.h.  Generated from ffti_cdefs.h.in by configure.  */
/*
Copyright (C) 2006 Quantum-ESPRESSO group
This file is distributed under the terms of the
GNU General Public License. See the file `License'
in the root directory of the present distribution,
or http://www.gnu.org/copyleft/gpl.txt .
*/

/* fortran-to-C naming convention, for functions with and without
   underscores in the name (some compilers treat them differently) */

#define F77_FUNC(name,NAME) name ## _
#define F77_FUNC_(name,NAME) name ## __

#ifdef F77_FUNC
#   define FC_FUNC F77_FUNC
#endif
#ifdef F77_FUNC_
#   define FC_FUNC_ F77_FUNC_
#endif

/* do we have the mallinfo structure ? */

/* #undef HAVE_MALLINFO */
