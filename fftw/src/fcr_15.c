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
/* Generated on Mon Mar 24 02:07:16 EST 2003 */

#include "fftw-int.h"
#include "fftw.h"

/* Generated by: /homee/stevenj/cvs/fftw/gensrc/genfft -magic-alignment-check -magic-twiddle-load-all -magic-variables 4 -magic-loopi -hc2real 15 */

/*
 * This function contains 64 FP additions, 31 FP multiplications,
 * (or, 47 additions, 14 multiplications, 17 fused multiply/add),
 * 36 stack variables, and 30 memory accesses
 */
static const fftw_real K1_118033988 =
FFTW_KONST(+1.118033988749894848204586834365638117720309180);
static const fftw_real K1_902113032 =
FFTW_KONST(+1.902113032590307144232878666758764286811397268);
static const fftw_real K1_175570504 =
FFTW_KONST(+1.175570504584946258337411909278145537195304875);
static const fftw_real K500000000 =
FFTW_KONST(+0.500000000000000000000000000000000000000000000);
static const fftw_real K866025403 =
FFTW_KONST(+0.866025403784438646763723170752936183471402627);
static const fftw_real K2_000000000 =
FFTW_KONST(+2.000000000000000000000000000000000000000000000);
static const fftw_real K1_732050807 =
FFTW_KONST(+1.732050807568877293527446341505872366942805254);

/*
 * Generator Id's : 
 * $Id: fcr_15.c,v 1.1.1.1 2004/12/28 16:08:07 software Exp $
 * $Id: fcr_15.c,v 1.1.1.1 2004/12/28 16:08:07 software Exp $
 * $Id: fcr_15.c,v 1.1.1.1 2004/12/28 16:08:07 software Exp $
 */

void fftw_hc2real_15(const fftw_real *real_input,
		     const fftw_real *imag_input, fftw_real *output,
		     int real_istride, int imag_istride, int ostride)
{
     fftw_real tmp3;
     fftw_real tmp30;
     fftw_real tmp18;
     fftw_real tmp37;
     fftw_real tmp61;
     fftw_real tmp62;
     fftw_real tmp45;
     fftw_real tmp40;
     fftw_real tmp23;
     fftw_real tmp31;
     fftw_real tmp42;
     fftw_real tmp28;
     fftw_real tmp32;
     fftw_real tmp8;
     fftw_real tmp13;
     fftw_real tmp14;
     ASSERT_ALIGNED_DOUBLE;
     {
	  fftw_real tmp17;
	  fftw_real tmp1;
	  fftw_real tmp2;
	  fftw_real tmp15;
	  fftw_real tmp16;
	  ASSERT_ALIGNED_DOUBLE;
	  tmp16 = imag_input[5 * imag_istride];
	  tmp17 = K1_732050807 * tmp16;
	  tmp1 = real_input[0];
	  tmp2 = real_input[5 * real_istride];
	  tmp15 = tmp1 - tmp2;
	  tmp3 = tmp1 + (K2_000000000 * tmp2);
	  tmp30 = tmp15 - tmp17;
	  tmp18 = tmp15 + tmp17;
     }
     {
	  fftw_real tmp4;
	  fftw_real tmp39;
	  fftw_real tmp5;
	  fftw_real tmp6;
	  fftw_real tmp7;
	  fftw_real tmp22;
	  fftw_real tmp38;
	  fftw_real tmp9;
	  fftw_real tmp44;
	  fftw_real tmp10;
	  fftw_real tmp11;
	  fftw_real tmp12;
	  fftw_real tmp27;
	  fftw_real tmp43;
	  fftw_real tmp19;
	  fftw_real tmp24;
	  ASSERT_ALIGNED_DOUBLE;
	  {
	       fftw_real tmp20;
	       fftw_real tmp21;
	       fftw_real tmp25;
	       fftw_real tmp26;
	       ASSERT_ALIGNED_DOUBLE;
	       tmp4 = real_input[3 * real_istride];
	       tmp39 = imag_input[3 * imag_istride];
	       tmp5 = real_input[7 * real_istride];
	       tmp6 = real_input[2 * real_istride];
	       tmp7 = tmp5 + tmp6;
	       tmp20 = imag_input[7 * imag_istride];
	       tmp21 = imag_input[2 * imag_istride];
	       tmp22 = K866025403 * (tmp20 - tmp21);
	       tmp38 = tmp20 + tmp21;
	       tmp9 = real_input[6 * real_istride];
	       tmp44 = imag_input[6 * imag_istride];
	       tmp10 = real_input[4 * real_istride];
	       tmp11 = real_input[real_istride];
	       tmp12 = tmp10 + tmp11;
	       tmp25 = imag_input[4 * imag_istride];
	       tmp26 = imag_input[imag_istride];
	       tmp27 = K866025403 * (tmp25 + tmp26);
	       tmp43 = tmp25 - tmp26;
	  }
	  tmp37 = K866025403 * (tmp5 - tmp6);
	  tmp61 = tmp39 - tmp38;
	  tmp62 = tmp44 - tmp43;
	  tmp45 = (K500000000 * tmp43) + tmp44;
	  tmp40 = (K500000000 * tmp38) + tmp39;
	  tmp19 = tmp4 - (K500000000 * tmp7);
	  tmp23 = tmp19 - tmp22;
	  tmp31 = tmp19 + tmp22;
	  tmp42 = K866025403 * (tmp10 - tmp11);
	  tmp24 = tmp9 - (K500000000 * tmp12);
	  tmp28 = tmp24 - tmp27;
	  tmp32 = tmp24 + tmp27;
	  tmp8 = tmp4 + tmp7;
	  tmp13 = tmp9 + tmp12;
	  tmp14 = tmp8 + tmp13;
     }
     output[0] = tmp3 + (K2_000000000 * tmp14);
     {
	  fftw_real tmp63;
	  fftw_real tmp65;
	  fftw_real tmp60;
	  fftw_real tmp64;
	  fftw_real tmp58;
	  fftw_real tmp59;
	  ASSERT_ALIGNED_DOUBLE;
	  tmp63 = (K1_175570504 * tmp61) - (K1_902113032 * tmp62);
	  tmp65 = (K1_902113032 * tmp61) + (K1_175570504 * tmp62);
	  tmp58 = tmp3 - (K500000000 * tmp14);
	  tmp59 = K1_118033988 * (tmp8 - tmp13);
	  tmp60 = tmp58 - tmp59;
	  tmp64 = tmp59 + tmp58;
	  output[12 * ostride] = tmp60 - tmp63;
	  output[3 * ostride] = tmp60 + tmp63;
	  output[6 * ostride] = tmp64 - tmp65;
	  output[9 * ostride] = tmp64 + tmp65;
     }
     {
	  fftw_real tmp51;
	  fftw_real tmp29;
	  fftw_real tmp50;
	  fftw_real tmp55;
	  fftw_real tmp57;
	  fftw_real tmp53;
	  fftw_real tmp54;
	  fftw_real tmp56;
	  fftw_real tmp52;
	  ASSERT_ALIGNED_DOUBLE;
	  tmp51 = K1_118033988 * (tmp23 - tmp28);
	  tmp29 = tmp23 + tmp28;
	  tmp50 = tmp18 - (K500000000 * tmp29);
	  tmp53 = tmp40 - tmp37;
	  tmp54 = tmp45 - tmp42;
	  tmp55 = (K1_175570504 * tmp53) - (K1_902113032 * tmp54);
	  tmp57 = (K1_902113032 * tmp53) + (K1_175570504 * tmp54);
	  output[5 * ostride] = tmp18 + (K2_000000000 * tmp29);
	  tmp56 = tmp51 + tmp50;
	  output[11 * ostride] = tmp56 - tmp57;
	  output[14 * ostride] = tmp56 + tmp57;
	  tmp52 = tmp50 - tmp51;
	  output[2 * ostride] = tmp52 - tmp55;
	  output[8 * ostride] = tmp52 + tmp55;
     }
     {
	  fftw_real tmp35;
	  fftw_real tmp33;
	  fftw_real tmp34;
	  fftw_real tmp47;
	  fftw_real tmp49;
	  fftw_real tmp41;
	  fftw_real tmp46;
	  fftw_real tmp48;
	  fftw_real tmp36;
	  ASSERT_ALIGNED_DOUBLE;
	  tmp35 = K1_118033988 * (tmp31 - tmp32);
	  tmp33 = tmp31 + tmp32;
	  tmp34 = tmp30 - (K500000000 * tmp33);
	  tmp41 = tmp37 + tmp40;
	  tmp46 = tmp42 + tmp45;
	  tmp47 = (K1_175570504 * tmp41) - (K1_902113032 * tmp46);
	  tmp49 = (K1_902113032 * tmp41) + (K1_175570504 * tmp46);
	  output[10 * ostride] = tmp30 + (K2_000000000 * tmp33);
	  tmp48 = tmp35 + tmp34;
	  output[ostride] = tmp48 - tmp49;
	  output[4 * ostride] = tmp48 + tmp49;
	  tmp36 = tmp34 - tmp35;
	  output[7 * ostride] = tmp36 - tmp47;
	  output[13 * ostride] = tmp36 + tmp47;
     }
}

fftw_codelet_desc fftw_hc2real_15_desc = {
     "fftw_hc2real_15",
     (void (*)()) fftw_hc2real_15,
     15,
     FFTW_BACKWARD,
     FFTW_HC2REAL,
     345,
     0,
     (const int *) 0,
};
