#ifndef DOUBLE_ANALYSIS_H
#define DOUBLE_ANALYSIS_H

/* neseccary for getDeltaULP and getDeltaULP_BIT */
#include <math.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <stdlib.h>
#include "compare.h"
#include "platform_types.h"
#include "double_data.h"

/*only neseccary for main-fkt */
#define FLT_MIN_DENORM_POS 1.40129846e-45
#define FLT_MAX_DENORM_POS 1.17549421e-38
#define FLT_MIN_NORMAL_POS 1.17549435e-38
#define FLT_NAN_MINONES_POS sem2float(0U,0xFFU,1U)
#define FLT_NAN_MINONES_NEG sem2float(1U,0xFFU,1U)
#define FLT_NAN_MAXONES_POS sem2float(0U,0xFFU,0x7FFFFFU)
#define FLT_NAN_MAXONES_NEG sem2float(1U,0xFFU,0x7FFFFFU)


/**
* @brief computes the amount of double numbers between x and y.
* @author: Martin Keﬂler
*
* much faster than getDeltaULP(), because of direct computation without loop
*
* *	Requirements: *
*		R1: If
*				a) x or y are NaN, or
*				b) x or y are ±Inf
*			the result shall be UINT_MAX.
*		R2: If x == y the result shall be 0.
*		R3: Otherwise the result shall be equal to the times of computing
*			x = x + double_ulp(x) until its equal to y (oBdA x < y)
*
* @param x float value to be compared
* @param y float value to be compared
* @return uint64 number representing the ULP steps between input values.
*/
uint64 double_getDeltaULP(double x, double y);

void double_computeAndPrint(double x, double y);

#endif
