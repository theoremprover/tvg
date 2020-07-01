/**
 * double specific specifications of the compare routine and required structures
*/
#ifndef DOUBLE_COMPARES_H
#define DOUBLE_COMPARES_H

#include "compare.h"
#include "double_cmp_ulp.h"
#include "double_data.h"

/**
 * type declaration to allow converting of hex numbers and doubles
 */
typedef union _hexdouble {
	double f_val;
	uint64 x_val;
} hexdouble;

/**
 * main comparison function for double values
 * - found: the tested values
 * - expected: the expected value
 * - comparetype: indicates which comparison method shall be used (ABS/REL/ULP/ABS_ULP/EXAKT/EQUAL/BINHEX)
 * - limit: the limit in case of combined comparisons
 * - maxtol_below: tolerances (below the limit if limit is specified)
 * - maxtol_above: tolerances (above the limit if limit is specified)
 * - maxdeviation_below: maximal deviation (below the limit if limit is specified)
 * - maxdeviation_above: maximal deviation (above the limit if limit is specified)
*/
TEST_BOOL test_compare_double(double found, double expected,int comparetype, double maxtol_below,double maxtol_above,double limit,double* maxdeviation_below, double* maxdeviation_above);
#endif /* DOUBLE_COMPARES_H */

