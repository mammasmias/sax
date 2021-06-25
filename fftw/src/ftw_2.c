/*
 * Copyright (c) 1997-1999, 2003 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/* This file was automatically generated --- DO NOT EDIT */
/* Generated on Mon Mar 24 02:07:31 EST 2003 */

#include "fftw-int.h"
#include "fftw.h"

/* Generated by: /homee/stevenj/cvs/fftw/gensrc/genfft -magic-alignment-check -magic-twiddle-load-all -magic-variables 4 -magic-loopi -twiddle 2 */

/*
 * This function contains 6 FP additions, 4 FP multiplications,
 * (or, 4 additions, 2 multiplications, 2 fused multiply/add),
 * 10 stack variables, and 8 memory accesses
 */

/*
 * Generator Id's : 
 * $Id: ftw_2.c,v 1.1.1.1 2004/12/28 16:08:07 software Exp $
 * $Id: ftw_2.c,v 1.1.1.1 2004/12/28 16:08:07 software Exp $
 * $Id: ftw_2.c,v 1.1.1.1 2004/12/28 16:08:07 software Exp $
 */

void fftw_twiddle_2(fftw_complex *A, const fftw_complex *W, int iostride,
		    int m, int dist)
{
     int i;
     fftw_complex *inout;
     inout = A;
     for (i = m; i > 0; i = i - 1, inout = inout + dist, W = W + 1) {
	  fftw_real tmp1;
	  fftw_real tmp8;
	  fftw_real tmp6;
	  fftw_real tmp7;
	  ASSERT_ALIGNED_DOUBLE;
	  tmp1 = c_re(inout[0]);
	  tmp8 = c_im(inout[0]);
	  {
	       fftw_real tmp3;
	       fftw_real tmp5;
	       fftw_real tmp2;
	       fftw_real tmp4;
	       ASSERT_ALIGNED_DOUBLE;
	       tmp3 = c_re(inout[iostride]);
	       tmp5 = c_im(inout[iostride]);
	       tmp2 = c_re(W[0]);
	       tmp4 = c_im(W[0]);
	       tmp6 = (tmp2 * tmp3) - (tmp4 * tmp5);
	       tmp7 = (tmp4 * tmp3) + (tmp2 * tmp5);
	  }
	  c_re(inout[iostride]) = tmp1 - tmp6;
	  c_re(inout[0]) = tmp1 + tmp6;
	  c_im(inout[0]) = tmp7 + tmp8;
	  c_im(inout[iostride]) = tmp8 - tmp7;
     }
}

static const int twiddle_order[] = { 1 };
fftw_codelet_desc fftw_twiddle_2_desc = {
     "fftw_twiddle_2",
     (void (*)()) fftw_twiddle_2,
     2,
     FFTW_FORWARD,
     FFTW_TWIDDLE,
     44,
     1,
     twiddle_order,
};
