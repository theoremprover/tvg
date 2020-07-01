/**
*  Copyright (C) Validas AG, see QKit license for more details
*  see float_cmp_ulp.h for more details
*/
#include "compare.h"
#include "float_cmp_ulp.h"
#include "float_data.h"

extern void printfloat(float v);

boolean float_equal_ulp(float val, float ref, uint32 n_ulp) {
 float_int tmp1;
 float_int tmp2;
 float_mag_class fmc_ref;
 boolean res;
 tmp1.fval = val;
 tmp2.fval = ref;
 if (n_ulp == 0U) {
   /* check for bit-identity. */
   res = (tmp1.ival == tmp2.ival);
 } else {
     fmc_ref = classify_float_mag(ref);
     switch (fmc_ref) {
      case FMC_NAN_FLOAT:
        /* we treat all represenations of NaN as equal */
        res = (fmc_ref == classify_float_mag(val));
        break;
      case FMC_INF_FLOAT:
        res = (tmp1.ival == tmp2.ival);
        break;
      default:
       tmp1.fval = ref - val;
       tmp1.ival = tmp1.ival & 0x7FFFFFFF;  /* set sign to 0 */
       res = (tmp1.fval <= n_ulp * float_ulp(ref));
       break;
     }
 }
 return res;
}

float float_ulp(float ref) {
 float_bits fb;
 fb = float2fb(ref);
 if (fb.e == 0xFF) {
    /* ref is NaN or Inf */
    fb.s = 0U;
    fb.m = 0x7FFFFFU;
 } else if (fb.e >= 24U) { 
    /* ulp_ref is normalized. */
    fb.s = 0U;
    fb.e = fb.e - 23U; 
    fb.m = 0U;
  } else {
    /* ulp_ref is denormalized. */
    /* ulp_ref = 2^(-149+fb.e) which is 0.m * 2^-126 
       where m[fb.e] = 1 and m[x] = 0 for x!=fb.e */
    fb.s = 0U;
    if (fb.e > 0U) {
       /* tranistion from normal to denormal. */
       fb.m = 1U << (fb.e - 1U);
       /* hidden bit m[0] switches from 1 to 0 (1.m -> 0.m), 
          which is effectively already one shift. 
          Hence we only need to shift another fb.e - 1U times. */
    } else {
       /* ref has already been denormal */
       fb.m = 1U;
    }
    fb.e = 0U;
  }
  return fb2float(fb);
}

