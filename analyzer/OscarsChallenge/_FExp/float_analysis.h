#ifndef FLOAT_ANALYSIS_H
#define FLOAT_ANALYSIS_H

/* neseccary for getDeltaULP and getDeltaULP_BIT */
#include <math.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include "compare.h"
#include "platform_types.h"
#include "float_data.h"
#ifndef NEXTAFTERF
#define NEXTAFTERF nextafterf
#endif
#include <stdlib.h>

/*only neseccary for main-fkt */
#define FLT_MIN_DENORM_POS 1.40129846e-45
#define FLT_MAX_DENORM_POS 1.17549421e-38
#define FLT_MIN_NORMAL_POS 1.17549435e-38
#define FLT_NAN_MINONES_POS sem2float(0U,0xFFU,1U) 
#define FLT_NAN_MINONES_NEG sem2float(1U,0xFFU,1U) 
#define FLT_NAN_MAXONES_POS sem2float(0U,0xFFU,0x7FFFFFU)
#define FLT_NAN_MAXONES_NEG sem2float(1U,0xFFU,0x7FFFFFU)

/**
* @brief computes the amount of float numbers between x and y.
* @author: Martin Keßler
*           
* *	Requirements: *
*		R1: If 
*				a) x or y are NaN, or
*				b) x or y are ±Inf
*			the result shall be UINT_MAX.
*		R2: If x == y the result shall be 0.
*		R3: Otherwise the result shall be equal to the times of computing
*			x = x + float_ulp(x) until its equal to y (oBdA x < y)
*
* @param x float value to be compared
* @param y float value to be compared
* @return uint32 number representing the ULP steps between input values.
*/
uint32 getDeltaULP(float x, float y);


/**
* @brief computes the amount of float numbers between x and y.
* @author: Martin Keßler 
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
*			x = x + float_ulp(x) until its equal to y (oBdA x < y)
*
* @param x float value to be compared
* @param y float value to be compared
* @return uint32 number representing the ULP steps between input values.
*/
uint32 getDeltaULP_BIT(float x, float y);

void printfloat(float v);
void computeAndPrint(float x, float y);

#endif
