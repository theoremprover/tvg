/**
 * this file (expf_uv__uv_trivial.c) contains test cases for expf
 * all test cases for expf have been specified and split into separate files
 * this file contains 16 test cases in 3 functions for the following purpose:
 * 2 specified tests for expf
 * 0.0; 1.0
 * returns the number of failing tests
 * 7 specified tests for expf
 * -2; -1; 0; 1; 2; -3.141592653589793; 6.283185307179586
 * returns the number of failing tests
 * 7 linear tests for expf
 * range from from -4 to 4
 * returns the number of failing tests
 * this has been generated using the following settings:
 *  - Input-Outputs: variables for inputs and outputs
 *  - Casts: no casts for inputs and outputs
 *  - Input Type: float ( f4 )
 *  - Output Type: float ( f4 )
 * Reference values have been computed from Java reference implementation.
 * They are compared using the following method:
 * - COMPARE_ABS_REL_TOLERANCE <= 1.0E-6
 * Copyright: Validas AG
*/
/* project specific includes */ 
#include <math.h>
#include "utt_forceCpp.h"
//#include <htc-aeabi.h>
#include "double_data.h"
#include "float_data.h"


/* define DEBUG/SUMMARY using compilation option -DDEBUG / -DSUMMARY */
#if ( defined(DEBUG) || defined(SUMMARY) ) && !defined(PRINTF)
#include <stdio.h>
#endif

#include "float_compares.h"

#define ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL 1.0E-6

float max_dif_below_expf_uv__uv_trivial=0.0;
float max_dif_above_expf_uv__uv_trivial=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of zero_one */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_trivial_zero_one_io_table_type;
/* type for expected input & output values of seven */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_trivial_seven_io_table_type;
/* type for expected input & output values of simple_values */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_trivial_simple_values_io_table_type;
/**
 * 2 specified tests for expf
 * 0.0; 1.0
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_trivial_zero_one in the following table */
expf_uv__uv_trivial_zero_one_io_table_type expf_uv__uv_trivial_zero_one_io_table [2] = {
   {0.0,1.00000000},
   {1.00000000,2.71828175},
};
/** function expf_uv__uv_trivial_zero_one executes the tests and returns the number of failing tests */
int expf_uv__uv_trivial_zero_one() {
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
	for (ind=0;ind<2;ind++) {
		x=expf_uv__uv_trivial_zero_one_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_trivial_zero_one_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL,10.0,&max_dif_below_expf_uv__uv_trivial,&max_dif_above_expf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_trivial_zero_one: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_trivial_zero_one_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==2) {
		PRINTF("expf_uv__uv_trivial_zero_one: successfully tested: 2 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_trivial_zero_one: %d tests failed for expf (out of 2)\n",errors);
	}
#endif
	return errors;
}

/**
 * 7 specified tests for expf
 * -2; -1; 0; 1; 2; -3.141592653589793; 6.283185307179586
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_trivial_simple_values in the following table */
expf_uv__uv_trivial_simple_values_io_table_type expf_uv__uv_trivial_simple_values_io_table [7] = {
   {-2.00000000,0.135335281},
   {-1.00000000,0.367879450},
   {0,1.00000000},
   {1.00000000,2.71828175},
   {2.00000000,7.38905621},
   {-3.14159274,0.0432139151},
   {6.28318548,535.491760},
};
/** function expf_uv__uv_trivial_simple_values executes the tests and returns the number of failing tests */
int expf_uv__uv_trivial_simple_values() {
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
	for (ind=0;ind<7;ind++) {
		x=expf_uv__uv_trivial_simple_values_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_trivial_simple_values_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL,10.0,&max_dif_below_expf_uv__uv_trivial,&max_dif_above_expf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_trivial_simple_values: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_trivial_simple_values_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==7) {
		PRINTF("expf_uv__uv_trivial_simple_values: successfully tested: 7 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_trivial_simple_values: %d tests failed for expf (out of 7)\n",errors);
	}
#endif
	return errors;
}

/**
 * 7 linear tests for expf
 * range from from -4 to 4
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_trivial_seven in the following table */
expf_uv__uv_trivial_seven_io_table_type expf_uv__uv_trivial_seven_io_table [7] = {
   {-4.00000000,0.0183156393},
   {-2.66666675,0.0694834441},
   {-1.33333337,0.263597131},
   {0.0,1.00000000},
   {1.33333337,3.79366803},
   {2.66666675,14.3919172},
   {4.00000000,54.5981483},
};
/** function expf_uv__uv_trivial_seven executes the tests and returns the number of failing tests */
int expf_uv__uv_trivial_seven() {
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
	for (ind=0;ind<7;ind++) {
		x=expf_uv__uv_trivial_seven_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_trivial_seven_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL,10.0,&max_dif_below_expf_uv__uv_trivial,&max_dif_above_expf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_trivial_seven: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_trivial_seven_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==7) {
		PRINTF("expf_uv__uv_trivial_seven: successfully tested: 7 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_trivial_seven: %d tests failed for expf (out of 7)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (3 functions) of expf_uv__uv_trivial
*/
int expf_uv__uv_trivial_main_test() {
	int errors=0;
	int index=0;
	errors+=expf_uv__uv_trivial_zero_one();      /* 1. 2 tests */
	errors+=expf_uv__uv_trivial_simple_values(); /* 2. 7 tests */
	errors+=expf_uv__uv_trivial_seven();         /* 3. 7 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of expf_uv__uv_trivial: successfully tested ALL 16 cases for expf\n");
	} else {
		PRINTF("SUMMARY of expf_uv__uv_trivial: %d tests failed in expf_uv__uv_trivial (out of 16 in expf_uv__uv_trivial)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.9g. ABS_TOLERANCE_EXPF_UV__UV_TRIVIAL=%.9g\n",expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,ABS_TOLERANCE_EXPF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.9g. REL_TOLERANCE_EXPF_UV__UV_TRIVIAL=%.9g\n",expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,REL_TOLERANCE_EXPF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.9g. ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL=%.9g\n",expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,ABS_REL_TOLERANCE_EXPF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.9g. ULP_TOLERANCE_EXPF_UV__UV_TRIVIAL=%.9g\n",expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,ULP_TOLERANCE_EXPF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.9g. EXAKT_TOLERANCE_EXPF_UV__UV_TRIVIAL=%.9g\n",expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,EXAKT_TOLERANCE_EXPF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.9g. EQUAL_TOLERANCE_EXPF_UV__UV_TRIVIAL=%.9g\n",expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,EQUAL_TOLERANCE_EXPF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.9g. BINHEX_TOLERANCE_EXPF_UV__UV_TRIVIAL=%.9g\n",expf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,BINHEX_TOLERANCE_EXPF_UV__UV_TRIVIAL);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls expf_uv__uv_trivial_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = expf_uv__uv_trivial_main_test();
	return result;
}
#endif /* NO_MAIN */

