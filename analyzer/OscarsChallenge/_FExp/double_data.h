/***************************************************************************************
  Copyright (C) Validas AG, see QKit license for more details
  
  @file double_data.h
  @brief Provides alternative data types for representing doubles.
  @author Oscar Slotosch (Validas AG)
  @version 1.0
  
  History (Version,Date,Author,Change):
  1.0,2017-03-23,OS,Extended from float to double
***************************************************************************************/

#ifndef _DOUBLE_DATA_H
#define _DOUBLE_DATA_H

#include "platform_types.h"

/**
Double Representation according to IEEE754:
s=sign
e=biased exponent (e=E+1023)
E=unbiased exponent (Emin:-1022 ,Emax:1023,
                     Emin - 1 for +/- 0.0 and denorms, Emax + 1 for NaN,INF)
m=fraction of mantissa (.b[1]b[2]...b[52])


number = (-1)^s * b[0].m * 2^E
where b[0]=1 for normalized numbers, b[0]=0 for denormalized numbers.


1     11               52            ... widths
+-+--------+-----------------------+
|s|    e   |           m           |
+-+--------+-----------------------+
  msb  lsb msb                  lsb
                    
*/

/* used to represent doubles as uint64 */
typedef union {
 double dval;
 uint64 ival;
} double_int;

/* conversion functions */
double db_int2double(uint64 val);
uint64 db_double2int(double val);


/* used to represent doubles with sign, exponent and fraction of mantissa */
typedef struct {
     uint64 s;
     uint64 e; /* e=biased exponent, e=E+1023 */
     uint64 m;
} double_bits;

/* conversion functions */
double_bits double2db(double v);
double db2double(double_bits db);
double sem2double(uint64 s,uint64 e,uint64 m);
double_bits sem2db(uint64 s,uint64 e,uint64 m);

/* double magnitude classes */
typedef enum double_mag_class {
    FMC_ZERO = 0,
    FMC_DENORM = 1,
    FMC_NORMAL = 2,
    FMC_INF = 3,
    FMC_NAN = 4} double_mag_class;

/* @brief classifies a given doubleing point number 
*
* @param val number to be classified
* @return doubleing point class of val.
*/
double_mag_class classify_double_mag(double val);

#endif /* _DOUBLE_DATA_H */