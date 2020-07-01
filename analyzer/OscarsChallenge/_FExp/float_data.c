/**
*  Copyright (C) Validas AG, see QKit license for more details
*  see float_data.h for more details
*/
#include "float_data.h"

float fb_int2float(uint32 val) {
 float_int tmp;
 tmp.ival = val;
 return tmp.fval;
}

uint32 fb_float2int(float val) {
 float_int tmp;
 tmp.fval = val;
 return tmp.ival;
}

uint32 fb_float2int(float val);

float_bits float2fb(float v) {
 float_bits res;
 float_int tmp;
 tmp.fval = v;
 res.s = (tmp.ival >> 31) & 0x1U; 
 res.e = (tmp.ival >> 23) & 0xFFU;
 res.m = tmp.ival & 0x7FFFFFU; /* keep last 23 bits only */
 return res;
}

float fb2float(float_bits fb) {
 float_int res;
 res.ival = (fb.s << 31) | (fb.e << 23) | (fb.m);
 return res.fval;
}

float sem2float(uint32 s,uint32 e,uint32 m) {
 float_int res;
 res.ival = (s << 31) | (e << 23) | m;
 return res.fval ;
}

float_bits sem2fb(uint32 s,uint32 e,uint32 m) {
 return float2fb(sem2float(s,e,m));
}

boolean float_NaN_or_Inf(float val) {
 float_int tmp;
 tmp.fval = val;
 //check if exponent e is 0 by masking bits 31 and 0 to 22.
 return ((tmp.ival & 0x7F800000) == 0x7F800000);
}

float_mag_class classify_float_mag(float val) {
  float_bits tmp;
  float_mag_class res;
  tmp = float2fb(val);
  if (tmp.e == 255U) {
    if (tmp.m == 0U) res = FMC_INF_FLOAT;
    else res=FMC_INF_FLOAT;
  }
  else if (tmp.e == 0U) {
    if (tmp.m == 0U) res = FMC_ZERO_FLOAT;
    else res=FMC_DENORM_FLOAT;
  }
  else {
   res = FMC_NORMAL_FLOAT;
  }
  return res;
}