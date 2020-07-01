/**
 * this file (powf_uv__uv_terms.c) contains test cases for powf
 * all test cases for powf have been specified and split into separate files
 * this file contains 118 test cases in 4 functions for the following purpose:
 * 49 tests for combination with powf
 * combination term: (powf(x,y)==powf(x,y)) checked with int
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 * 49 tests for combination with powf
 * combination term: ((2*powf(((3*x)+4),((3*y)+4)))-(1)) checked with Default Equality
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 * 100 tests for combination with powf
 * combination term: sqrtf(powf(x,2)) checked with Default Equality
 * range from from (0) to (9)
 * returns the number of failing tests
 * 100 tests for combination with powf
 * combination term: powf(sqrtf(x),2) checked with Default Equality
 * range from from (0) to (9)
 * returns the number of failing tests
 * this has been generated using the following settings:
 *  - Input-Outputs: variables for inputs and outputs
 *  - Casts: no casts for inputs and outputs
 *  - Input Type: float ( f4 )
 *  - Input Type: float ( f4 )
 *  - Output Type: float ( f4 )
 * Reference values have been computed from Java reference implementation.
 * They are compared using the following method:
 * - COMPARE_ABS_REL_TOLERANCE <= 1.0E-5
 * Copyright: Validas AG
*/
/* project specific includes */ 
#include <math.h>
#include "utt_forceCpp.h"
#include "double_data.h"
#include "float_data.h"


/* define DEBUG/SUMMARY using compilation option -DDEBUG / -DSUMMARY */
#if ( defined(DEBUG) || defined(SUMMARY) ) && !defined(PRINTF)
#include <stdio.h>
#endif

#include "float_compares.h"

#define ABS_REL_TOLERANCE_POWF_UV__UV_TERMS 1.0E-5

float max_dif_below_powf_uv__uv_terms=0.0;
float max_dif_above_powf_uv__uv_terms=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of powf_combine */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_terms_powf_combine_io_table_type;
/* type for expected input & output values of sqrt_pow */ 
typedef struct {
    float powf_x;
    float powf_out;
} powf_uv__uv_terms_sqrt_pow_io_table_type;
/* type for expected input & output values of powf_deterministic */ 
typedef struct {
    float powf_x;
    float powf_y;
    int powf_out;
} powf_uv__uv_terms_powf_deterministic_io_table_type;
/* type for expected input & output values of pow_sqrt */ 
typedef struct {
    float powf_x;
    float powf_out;
} powf_uv__uv_terms_pow_sqrt_io_table_type;
/**
 * 49 tests for combination with powf
 * combination term: (powf(x,y)==powf(x,y)) checked with int
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_terms_powf_deterministic in the following table */
powf_uv__uv_terms_powf_deterministic_io_table_type powf_uv__uv_terms_powf_deterministic_io_table [49] = {
   {0.0,0.0,1.00000000},
   {0.0,1.66666663,1.00000000},
   {0.0,3.33333325,1.00000000},
   {0.0,5.00000000,1.00000000},
   {0.0,6.66666651,1.00000000},
   {0.0,8.33333302,1.00000000},
   {0.0,10.0000000,1.00000000},
   {1.66666663,0.0,1.00000000},
   {1.66666663,1.66666663,1.00000000},
   {1.66666663,3.33333325,1.00000000},
   {1.66666663,5.00000000,1.00000000},
   {1.66666663,6.66666651,1.00000000},
   {1.66666663,8.33333302,1.00000000},
   {1.66666663,10.0000000,1.00000000},
   {3.33333325,0.0,1.00000000},
   {3.33333325,1.66666663,1.00000000},
   {3.33333325,3.33333325,1.00000000},
   {3.33333325,5.00000000,1.00000000},
   {3.33333325,6.66666651,1.00000000},
   {3.33333325,8.33333302,1.00000000},
   {3.33333325,10.0000000,1.00000000},
   {5.00000000,0.0,1.00000000},
   {5.00000000,1.66666663,1.00000000},
   {5.00000000,3.33333325,1.00000000},
   {5.00000000,5.00000000,1.00000000},
   {5.00000000,6.66666651,1.00000000},
   {5.00000000,8.33333302,1.00000000},
   {5.00000000,10.0000000,1.00000000},
   {6.66666651,0.0,1.00000000},
   {6.66666651,1.66666663,1.00000000},
   {6.66666651,3.33333325,1.00000000},
   {6.66666651,5.00000000,1.00000000},
   {6.66666651,6.66666651,1.00000000},
   {6.66666651,8.33333302,1.00000000},
   {6.66666651,10.0000000,1.00000000},
   {8.33333302,0.0,1.00000000},
   {8.33333302,1.66666663,1.00000000},
   {8.33333302,3.33333325,1.00000000},
   {8.33333302,5.00000000,1.00000000},
   {8.33333302,6.66666651,1.00000000},
   {8.33333302,8.33333302,1.00000000},
   {8.33333302,10.0000000,1.00000000},
   {10.0000000,0.0,1.00000000},
   {10.0000000,1.66666663,1.00000000},
   {10.0000000,3.33333325,1.00000000},
   {10.0000000,5.00000000,1.00000000},
   {10.0000000,6.66666651,1.00000000},
   {10.0000000,8.33333302,1.00000000},
   {10.0000000,10.0000000,1.00000000},
};
/** function powf_uv__uv_terms_powf_deterministic executes the tests and returns the number of failing tests */
int powf_uv__uv_terms_powf_deterministic() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	float x;
	float y;
	/* output variable (IO_VV) */
	int res;
	int res1;
	int res2;
	/* main test starts here */
	for (ind=0;ind<49;ind++) {
		x=powf_uv__uv_terms_powf_deterministic_io_table[ind].powf_x;
		y=powf_uv__uv_terms_powf_deterministic_io_table[ind].powf_y;
		res=(powf(x,y)==powf(x,y));
		if (res!=powf_uv__uv_terms_powf_deterministic_io_table[ind].powf_out) {
			/* cast to int is only defined if value is in range, otherwise ignore deviation */
			if ((powf(x,y)==powf(x,y))>=-2147483648.0 && (powf(x,y)==powf(x,y))<=2147483647.0) {
				/* test for integer rounding errors */
				res1=((powf(x,y)==powf(x,y))+0.000001);
				res2=((powf(x,y)==powf(x,y))-0.000001);
				if ( (res1!=powf_uv__uv_terms_powf_deterministic_io_table[ind].powf_out)
				 && (res2!=powf_uv__uv_terms_powf_deterministic_io_table[ind].powf_out) ) {
					errors++;
#if defined(DEBUG)
					PRINTF("powf_uv__uv_terms_powf_deterministic: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %d, found %d\n",ind+1,x,y,powf_uv__uv_terms_powf_deterministic_io_table[ind].powf_out,res);
#endif
				} else {
					passes++;
				}
			} else {
				passes++;
			}
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("powf_uv__uv_terms_powf_deterministic: successfully tested: 49 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_terms_powf_deterministic: %d tests failed for powf (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 tests for combination with powf
 * combination term: ((2*powf(((3*x)+4),((3*y)+4)))-(1)) checked with Default Equality
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_terms_powf_combine in the following table */
powf_uv__uv_terms_powf_combine_io_table_type powf_uv__uv_terms_powf_combine_io_table [49] = {
   {0.0,0.0,511.000000},
   {0.0,1.66666663,524287.000},
   {0.0,3.33333325,536870912.0},
   {0.0,5.00000000,5.49755814e+11},
   {0.0,6.66666651,5.62949953e+14},
   {0.0,8.33333302,5.76460752e+17},
   {0.0,10.0000000,5.90295810e+20},
   {1.66666663,0.0,13121.0000},
   {1.66666663,1.66666663,774840960.0},
   {1.66666663,3.33333325,4.57535862e+13},
   {1.66666663,5.00000000,2.70170348e+18},
   {1.66666663,6.66666651,1.59532883e+23},
   {1.66666663,8.33333302,9.42025753e+27},
   {1.66666663,10.0000000,5.56256790e+32},
   {3.33333325,0.0,76831.0000},
   {3.33333325,1.66666663,4.13220946e+10},
   {3.33333325,3.33333325,2.22240141e+16},
   {3.33333325,5.00000000,1.19526075e+22},
   {3.33333325,6.66666651,6.42839929e+27},
   {3.33333325,8.33333302,3.45734741e+33},
   {3.33333325,10.0000000,INFINITY},
   {5.00000000,0.0,260641.000},
   {5.00000000,1.66666663,6.45375394e+11},
   {5.00000000,3.33333325,1.59801343e+18},
   {5.00000000,5.00000000,3.95683929e+24},
   {5.00000000,6.66666651,9.79752593e+30},
   {5.00000000,8.33333302,2.42596431e+37},
   {5.00000000,10.0000000,INFINITY},
   {6.66666651,0.0,663551.000},
   {6.66666651,1.66666663,5.28361508e+12},
   {6.66666651,3.33333325,4.20714402e+19},
   {6.66666651,5.00000000,3.34999070e+26},
   {6.66666651,6.66666651,2.66747142e+33},
   {6.66666651,8.33333302,INFINITY},
   {6.66666651,10.0000000,INFINITY},
   {8.33333302,0.0,1414561.00},
   {8.33333302,1.66666663,2.90142930e+13},
   {8.33333302,3.33333325,5.95116492e+20},
   {8.33333302,5.00000000,1.22065228e+28},
   {8.33333302,6.66666651,2.50369808e+35},
   {8.33333302,8.33333302,INFINITY},
   {8.33333302,10.0000000,INFINITY},
   {10.0000000,0.0,2672671.00},
   {10.0000000,1.66666663,1.21433984e+14},
   {10.0000000,3.33333325,5.51740438e+21},
   {10.0000000,5.00000000,2.50685616e+29},
   {10.0000000,6.66666651,1.13900080e+37},
   {10.0000000,8.33333302,INFINITY},
   {10.0000000,10.0000000,INFINITY},
};
/** function powf_uv__uv_terms_powf_combine executes the tests and returns the number of failing tests */
int powf_uv__uv_terms_powf_combine() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	float x;
	float y;
	/* output variable (IO_VV) */
	float res;
	/* main test starts here */
	for (ind=0;ind<49;ind++) {
		x=powf_uv__uv_terms_powf_combine_io_table[ind].powf_x;
		y=powf_uv__uv_terms_powf_combine_io_table[ind].powf_y;
		res=((2*powf(((3*x)+4),((3*y)+4)))-(1));
		if (test_compare_float(res,powf_uv__uv_terms_powf_combine_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TERMS,ABS_REL_TOLERANCE_POWF_UV__UV_TERMS,10.0,&max_dif_below_powf_uv__uv_terms,&max_dif_above_powf_uv__uv_terms)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_terms_powf_combine: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_terms_powf_combine_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("powf_uv__uv_terms_powf_combine: successfully tested: 49 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_terms_powf_combine: %d tests failed for powf (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 100 tests for combination with powf
 * combination term: sqrtf(powf(x,2)) checked with Default Equality
 * range from from (0) to (9)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_terms_sqrt_pow in the following table */
powf_uv__uv_terms_sqrt_pow_io_table_type powf_uv__uv_terms_sqrt_pow_io_table [10] = {
   {0.0,0.0},
   {1.00000000,1.00000000},
   {2.00000000,2.00000000},
   {3.00000000,3.00000000},
   {4.00000000,4.00000000},
   {5.00000000,5.00000000},
   {6.00000000,6.00000000},
   {7.00000000,7.00000000},
   {8.00000000,8.00000000},
   {9.00000000,9.00000000},
};
/** function powf_uv__uv_terms_sqrt_pow executes the tests and returns the number of failing tests */
int powf_uv__uv_terms_sqrt_pow() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	float x;
	/* output variable (IO_VV) */
	float res;
	/* main test starts here */
	for (ind=0;ind<10;ind++) {
		x=powf_uv__uv_terms_sqrt_pow_io_table[ind].powf_x;
		res=sqrtf(powf(x,2));
		if (test_compare_float(res,powf_uv__uv_terms_sqrt_pow_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TERMS,ABS_REL_TOLERANCE_POWF_UV__UV_TERMS,10.0,&max_dif_below_powf_uv__uv_terms,&max_dif_above_powf_uv__uv_terms)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_terms_sqrt_pow: test %d (IO_VV) failed for powf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,powf_uv__uv_terms_sqrt_pow_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==10) {
		PRINTF("powf_uv__uv_terms_sqrt_pow: successfully tested: 10 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_terms_sqrt_pow: %d tests failed for powf (out of 10)\n",errors);
	}
#endif
	return errors;
}

/**
 * 100 tests for combination with powf
 * combination term: powf(sqrtf(x),2) checked with Default Equality
 * range from from (0) to (9)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_terms_pow_sqrt in the following table */
powf_uv__uv_terms_pow_sqrt_io_table_type powf_uv__uv_terms_pow_sqrt_io_table [10] = {
   {0.0,0.0},
   {1.00000000,1.00000000},
   {2.00000000,1.99999988},
   {3.00000000,3.00000000},
   {4.00000000,4.00000000},
   {5.00000000,5.00000000},
   {6.00000000,6.00000048},
   {7.00000000,6.99999952},
   {8.00000000,7.99999952},
   {9.00000000,9.00000000},
};
/** function powf_uv__uv_terms_pow_sqrt executes the tests and returns the number of failing tests */
int powf_uv__uv_terms_pow_sqrt() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	float x;
	/* output variable (IO_VV) */
	float res;
	/* main test starts here */
	for (ind=0;ind<10;ind++) {
		x=powf_uv__uv_terms_pow_sqrt_io_table[ind].powf_x;
		res=powf(sqrtf(x),2);
		if (test_compare_float(res,powf_uv__uv_terms_pow_sqrt_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TERMS,ABS_REL_TOLERANCE_POWF_UV__UV_TERMS,10.0,&max_dif_below_powf_uv__uv_terms,&max_dif_above_powf_uv__uv_terms)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_terms_pow_sqrt: test %d (IO_VV) failed for powf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,powf_uv__uv_terms_pow_sqrt_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==10) {
		PRINTF("powf_uv__uv_terms_pow_sqrt: successfully tested: 10 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_terms_pow_sqrt: %d tests failed for powf (out of 10)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (4 functions) of powf_uv__uv_terms
*/
int powf_uv__uv_terms_main_test() {
	int errors=0;
	int index=0;
	errors+=powf_uv__uv_terms_powf_deterministic(); /* 1. 49 tests */
	errors+=powf_uv__uv_terms_powf_combine();       /* 2. 49 tests */
	errors+=powf_uv__uv_terms_sqrt_pow();           /* 3. 10 tests */
	errors+=powf_uv__uv_terms_pow_sqrt();           /* 4. 10 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of powf_uv__uv_terms: successfully tested ALL 118 cases for powf\n");
	} else {
		PRINTF("SUMMARY of powf_uv__uv_terms: %d tests failed in powf_uv__uv_terms (out of 118 in powf_uv__uv_terms)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.9g. ABS_TOLERANCE_POWF_UV__UV_TERMS=%.9g\n",powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,ABS_TOLERANCE_POWF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.9g. REL_TOLERANCE_POWF_UV__UV_TERMS=%.9g\n",powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,REL_TOLERANCE_POWF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.9g. ABS_REL_TOLERANCE_POWF_UV__UV_TERMS=%.9g\n",powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,ABS_REL_TOLERANCE_POWF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.9g. ULP_TOLERANCE_POWF_UV__UV_TERMS=%.9g\n",powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,ULP_TOLERANCE_POWF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.9g. EXAKT_TOLERANCE_POWF_UV__UV_TERMS=%.9g\n",powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,EXAKT_TOLERANCE_POWF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.9g. EQUAL_TOLERANCE_POWF_UV__UV_TERMS=%.9g\n",powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,EQUAL_TOLERANCE_POWF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.9g. BINHEX_TOLERANCE_POWF_UV__UV_TERMS=%.9g\n",powf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,BINHEX_TOLERANCE_POWF_UV__UV_TERMS);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls powf_uv__uv_terms_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = powf_uv__uv_terms_main_test();
	return result;
}
#endif /* NO_MAIN */

