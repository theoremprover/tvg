/**
*  Copyright (C) Validas AG, see QKit license for more details
*  see double_data.h for more details
*/
#include "double_data.h"

double db_int2double(uint64 val) {
 double_int tmp;
 tmp.ival = val;
 return tmp.dval;
}

uint64 db_double2int(double val) {
 double_int tmp;
 tmp.dval = val;
 return tmp.ival;
}

uint64 db_double2int(double val);

double_bits double2db(double v) {
 double_bits res;
 double_int tmp;
 tmp.dval = v;
 res.s = (tmp.ival >> 63) & 0x1U; 
 res.e = (tmp.ival >> 52) & 0x7FFU;
 res.m = tmp.ival & 0xFFFFFFFFFFFFFU; /* keep last 52 bits only */
 return res;
}

double db2double(double_bits db) {
 double_int res;
 res.ival = (db.s << 63) | (db.e << 52) | (db.m);
 return res.dval;
}

double sem2double(uint64 s,uint64 e,uint64 m) {
 double_int res;
 res.ival = (s << 63) | (e << 52) | m;
 return res.dval ;
}

double_bits sem2db(uint64 s,uint64 e,uint64 m) {
 return double2db(sem2double(s,e,m));
}

boolean double_NaN_or_Inf(double val) {
 double_int tmp;
 tmp.dval = val;
 //check if exponent e is 0 by masking bits 63 and 0 to 51.
 return ((tmp.ival & 0x7FF0000000000000) == 0x7FF0000000000000);
}

double_mag_class classify_double_mag(double val) {
  double_bits tmp;
  double_mag_class res;
  tmp = double2db(val);
  if (tmp.e == 2047U) {
    if (tmp.m == 0U) res = FMC_INF;
    else res=FMC_NAN;
  }
  else if (tmp.e == 0U) {
    if (tmp.m == 0U) res = FMC_ZERO;
    else res=FMC_DENORM;
  }
  else {
   res = FMC_NORMAL;
  }
  return res;
}