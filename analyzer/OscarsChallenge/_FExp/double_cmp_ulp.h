/***************************************************************************************
  Copyright (C) Validas AG, see QKit license for more details
  
  @file cmp_equal_ulp.h
  @brief Provides functionality for comparing floating point numbers.
  @author Martin Wildmoser, Oscar Slotosch (Validas AG)
  @version 1.0
  
  History (Version,Date,Author,Change):
  0.1,2016-09-13,MW,Initial version
  0.2,2016-09-19,MW,Fully implemented double_equal_ulp and double_ulp.
  0.3,2016-09-20,MW,Fixed bug in double_ulp
  1.0,2017-03-23,OS,Integrated into LTG

***************************************************************************************/
#ifndef _DOUBLE_CMP_ULP_H
#define _DOUBLE_CMP_ULP_H

#include "platform_types.h"

/**
* @brief checks whether two doubles are "almost equal" (=different up to n times ULP).
*
* * Requirements: *
* 
*   R1: If n_ulp == 0 the result shall be 
*            a) 1 if and only if val and ref are bit-identical or 
*            b) 0 otherwise.
*   R2: If n_ulp > 0 the result shall be 
*            a) 1 if |val - ref| <= n_ulp * double_ulp(ref), or
*            b) 1 if val = +inf and ref = +inf, or
*            c) 1 if val = -inf and ref = -inf, or
*            d) 1 if val is NaN and ref is NaN (no matter which NaN), or
*            e) 0 otherwise.
*      
* @param val actual value to be compared
* @param ref reference value to be compared
* @param n_ulp determines tolerance. Both numbers are considered equal if the difference
*              is no greater than n_ulp times ULP of @p ref. 
* @return result of comparisson (0 or 1), where 1 means "equal" and 0 means "not equal". 
*/
boolean double_equal_ulp(double val, double ref, uint8 n_ulp);


/**
* @brief computes the value of the unit in the last place of ref.
* 
* Requirements:
*   R1: If |ref| = 1.m * 2^E and E >= -970 then double_ulp(ref) is 2^(E-52) and normalized.
*   R2: If |ref| = 1.m * 2^E and -1022 <= E < -970 then 
*       double_ulp(ref) is 2^(E-52) and denormalized.
*   R3: If |ref| = 0.m * 2^-1022 (denormalized) then 
*       double_ulp(ref) is 2^-1077 (smallest positive denormalized value).
*       Note: This case also applied to ref = 0.0 or -0.0.
*   R4: If ref is a special value, e.g. +/- Inf or NaN, then double_ulp(ref) is NaN.             
*
* @param ref reference value whose ULP is computed.
* @return double number representing the ULP of ref.
* 
*/
double double_ulp(double ref);

#endif /* _DOUBLE_CMP_ULP_H */