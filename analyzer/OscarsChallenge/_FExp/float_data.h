/***************************************************************************************
  Copyright (C) Validas AG, see QKit license for more details
  
  @file float_data.h
  @brief Provides alternative data types for representing floats.
  @author Martin Wildmoser, Oscar Slotosch (Validas AG)
  @version 1.0
  
  History (Version,Date,Author,Change):
  0.1,2016-09-13,MW,Initial version
  0.2,2016-09-19,MW,Added classify_float_mag
  1.0,2017-03-23,OS,Integrated into LTG
***************************************************************************************/

#ifndef _FLOAT_DATA_H
#define _FLOAT_DATA_H

#include "platform_types.h"

/**
Single Float Representation according to IEEE754:
s=sign
e=biased exponent (e=E+127)
E=unbiased exponent (Emin:-126 ,Emax:127,
                     Emin - 1 for +/- 0.0 and denorms, Emax + 1 for NaN,INF)
m=fraction of mantissa (.b[1]b[2]...b[23])


number = (-1)^s * b[0].m * 2^E
where b[0]=1 for normalized numbers, b[0]=0 for denormalized numbers.


1     8               23            ... widths
+-+--------+-----------------------+
|s|    e   |           m           |
+-+--------+-----------------------+
  msb  lsb msb                  lsb
                    
*/

/* used to represent floats as uint32 */
typedef union {
 float fval;
 uint32 ival;
} float_int;

/* conversion functions */
float fb_int2float(uint32 val);
uint32 fb_float2int(float val);


/* used to represent floats with sign, exponent and fraction of mantissa */
typedef struct {
     uint32 s;
     uint32 e; /* e=biased exponent, e=E+127 */
     uint32 m;
} float_bits;

/* conversion functions */
float_bits float2fb(float v);
float fb2float(float_bits fb);
float sem2float(uint32 s,uint32 e,uint32 m);
float_bits sem2fb(uint32 s,uint32 e,uint32 m);

/* float magnitude classes */
typedef enum float_mag_class {
    FMC_ZERO_FLOAT = 0,
    FMC_DENORM_FLOAT = 1,
    FMC_NORMAL_FLOAT = 2,
    FMC_INF_FLOAT = 3,
    FMC_NAN_FLOAT = 4} float_mag_class;

/* @brief classifies a given floating point number 
*
* @param val number to be classified
* @return floating point class of val.
*/
float_mag_class classify_float_mag(float val);

#endif /* _FLOAT_DATA_H */