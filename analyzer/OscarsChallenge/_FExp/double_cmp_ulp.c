/**
*  Copyright (C) Validas AG, see QKit license for more details
*  see double_cmp_ulp.h for more details
*/
#include "compare.h"
#include "double_cmp_ulp.h"
#include "double_data.h"

boolean double_equal_ulp(double val, double ref, uint8 n_ulp) {
 double_int tmp1;
 double_int tmp2;
 double_mag_class fmc_ref;
 boolean res;
 tmp1.dval = val;
 tmp2.dval = ref;
 if (n_ulp == 0U) {
   /* check for bit-identity. */
   res = (tmp1.ival == tmp2.ival);
 } else {
     fmc_ref = classify_double_mag(ref);
     switch (fmc_ref) {
      case FMC_NAN:
        /* we treat all represenations of NaN as equal */
        res = (fmc_ref == classify_double_mag(val));
        break;
      case FMC_INF:
        res = (tmp1.ival == tmp2.ival);
        break;
      default:
       tmp1.dval = ref - val;
       tmp1.ival = tmp1.ival & 0x7FFFFFFFFFFFFFFF;  /* set sign to 0 */
       res = (tmp1.dval <= n_ulp * double_ulp(ref));
       break;
     }
 }
 return res;
}

double double_ulp(double ref) {
 double_bits db;
 db = double2db(ref);
 if (db.e == 0x7FF) {
    /* ref is NaN or Inf */
    db.s = 0U;
    db.m = 0xFFFFFFFFFFFFFU;
 } else if (db.e >= 53U) { 
    /* ulp_ref is normalized. */
    db.s = 0U;
    db.e = db.e - 52U; 
    db.m = 0U;
  } else {
    /* ulp_ref is denormalized. */
    /* ulp_ref = 2^(-1074+db.e) which is 0.m * 2^-1022 
       where m[db.e] = 1 and m[x] = 0 for x!=db.e */
    db.s = 0U;
    if (db.e > 0U) {
       /* tranistion from normal to denormal. */
       db.m = 1U << (db.e - 1U);
       /* hidden bit m[0] switches from 1 to 0 (1.m -> 0.m), 
          which is effectively already one shift. 
          Hence we only need to shift another db.e - 1U times. */
    } else {
       /* ref has already been denormal */
       db.m = 1U;
    }
    db.e = 0U;
  }
  return db2double(db);
}

