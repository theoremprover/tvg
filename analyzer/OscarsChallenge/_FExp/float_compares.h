/**
 * float specific specifications of the compare routine and required structures
*/
#ifndef FLOAT_COMPARES_H
#define FLOAT_COMPARES_H

#include "compare.h"
#include "float_cmp_ulp.h"
#include "float_data.h"

/**
 * type declaration to allow converting of hex numbers and floats
 */
typedef union _hexfloat {
	float f_val;
	uint32 x_val;
} hexfloat;

/**
 * main comparison function for float values
 * - found: the tested values
 * - expected: the expected value
 * - comparetype: indicates which comparison method shall be used (ABS/REL/ULP/ABS_ULP/EXAKT/EQUAL/BINHEX)
 * - limit: the limit in case of combined comparisons
 * - maxtol_below: tolerances (below the limit if limit is specified)
 * - maxtol_above: tolerances (above the limit if limit is specified)
 * - maxdeviation_below: maximal deviation (below the limit if limit is specified)
 * - maxdeviation_above: maximal deviation (above the limit if limit is specified)
*/
TEST_BOOL test_compare_float(float found, float expected,int comparetype, float maxtol_below,float maxtol_above,float limit,float* maxdeviation_below, float* maxdeviation_above);
#endif /* FLOAT_COMPARES_H */

