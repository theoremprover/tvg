/**
 * this file (pow_uv__uv_extreme.c) contains test cases for pow
 * all test cases for pow have been specified and split into separate files
 * this file contains 1898 test cases in 25 functions for the following purpose:
 * 5 specified tests for pow
 * (NaN,0); (NaN,1); (0,NaN); (1,NaN); (NaN,NaN)
 * returns the number of failing tests
 * 5 specified tests for pow
 * (Infinity,0); (Infinity,1); (0,Infinity); (1,Infinity); (Infinity,Infinity)
 * returns the number of failing tests
 * 5 specified tests for pow
 * (-Infinity,0); (-Infinity,1); (0,-Infinity); (1,-Infinity); (-Infinity,-Infinity) ...
 * returns the number of failing tests
 * 6 specified tests for pow
 * (-Infinity,Infinity); (Infinity,-Infinity); (NaN,-Infinity); (NaN,Infinity); (-Infinity,NaN) ...
 * returns the number of failing tests
 * 9 specified tests for pow
 * (NaN,NaN); (NaN,Infinity); (NaN,-Infinity); (Infinity,NaN); (Infinity,Infinity); (Infinity,-Infinity) ...
 * returns the number of failing tests
 * 256 specified tests for pow
 * (2.472304287230225E-309,2.472304287230225E-309); (2.472304287230225E-309,2.74700476358915E-310) ...
 * returns the number of failing tests
 * 272 specified tests for pow
 * (2.472304287230225E-309,-2.781342323134002E-309); (2.472304287230225E-309,-3.4766779039175E-310) ...
 * returns the number of failing tests
 * 272 specified tests for pow
 * (-2.781342323134002E-309,2.472304287230225E-309); (-2.781342323134002E-309,2.74700476358915E-310) ...
 * returns the number of failing tests
 * 289 specified tests for pow
 * (-2.781342323134002E-309,-2.781342323134002E-309); (-2.781342323134002E-309,-3.4766779039175E-310) ...
 * returns the number of failing tests
 * 1 specified tests for pow
 * (0,0)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (0,1) to (2,2)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (-1,0) to (2,2)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (-10,-10) to (-1,0)
 * returns the number of failing tests
 * 49 linear tests for pow
 * range: (1000,2) to (100000,10)
 * returns the number of failing tests
 * 49 linear tests for pow
 * range: (1000,1) to (100000,1000)
 * returns the number of failing tests
 * 64 linear tests for pow
 * range: (1000,1024) to (100000,10000)
 * returns the number of failing tests
 * 64 linear tests for pow
 * range: (-100000,1024) to (1000,10000)
 * returns the number of failing tests
 * 64 linear tests for pow
 * range: (0.00001,2) to (0.001,256)
 * returns the number of failing tests
 * 64 linear tests for pow
 * range: (0.000000001,2) to (0.00001,1028)
 * returns the number of failing tests
 * 64 linear tests for pow
 * range: (-0.0001,1) to (-0.000000001,99)
 * returns the number of failing tests
 * 64 linear tests for pow
 * range: (0.000000001,1000) to (0.00001,100000)
 * returns the number of failing tests
 * 54 approximation tests with factors 2.0,2.0
 * range: (0,-2) to (1,-2)
 * returns the number of failing tests
 * 54 approximation tests with factors 2.0,2.0
 * range: (0,0) to (1,-10)
 * returns the number of failing tests
 * 20 (1 omitted out of input range) specified tests for pow
 * (2,21); (2,22); (2,23); (2,24); (2,25); (2,24); (2,27); (2,28); (2,29); (2,30); (2,31) ...
 * returns the number of failing tests
 * 18 specified tests for pow
 * (-1.0,Infinity); (-1.0,-Infinity); (-0.5,Infinity); (-0.5,-Infinity); (0.5,Infinity) ...
 * returns the number of failing tests
 * this has been generated using the following settings:
 *  - Input-Outputs: variables for inputs and outputs
 *  - Casts: no casts for inputs and outputs
 *  - Input Type: double ( d8 )
 *  - Input Type: double ( d8 )
 *  - Output Type: double ( d8 )
 * Reference values have been computed from Java reference implementation.
 * They are compared using the following method:
 * - COMPARE_ABS_REL_TOLERANCE <= 1.0E-12
 * Copyright: Validas AG
*/
/* project specific includes */ 
#include <math.h>
#include "utt_forceCpp.h"
#include <stdio.h>
#include "double_data.h"
#include "float_data.h"


/* define DEBUG/SUMMARY using compilation option -DDEBUG / -DSUMMARY */
#if ( defined(DEBUG) || defined(SUMMARY) ) && !defined(PRINTF)
#include <stdio.h>
#endif

#include "double_compares.h"

#define ABS_REL_TOLERANCE_POW_UV__UV_EXTREME 1.0E-12

double max_dif_below_pow_uv__uv_extreme=0.0;
double max_dif_above_pow_uv__uv_extreme=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of overflowing_powers */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_overflowing_powers_io_table_type;
/* type for expected input & output values of big_powers2 */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_big_powers2_io_table_type;
/* type for expected input & output values of roots_of_minus_two */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_roots_of_minus_two_io_table_type;
/* type for expected input & output values of big_powers */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_big_powers_io_table_type;
/* type for expected input & output values of zero */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_zero_io_table_type;
/* type for expected input & output values of overflowing_powers_pos */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_overflowing_powers_pos_io_table_type;
/* type for expected input & output values of random_small_squares */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_random_small_squares_io_table_type;
/* type for expected input & output values of undeflowing_powers2 */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_undeflowing_powers2_io_table_type;
/* type for expected input & output values of coverage_dw */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_coverage_dw_io_table_type;
/* type for expected input & output values of nan */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_nan_io_table_type;
/* type for expected input & output values of random_negative */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_random_negative_io_table_type;
/* type for expected input & output values of extreme_values */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_extreme_values_io_table_type;
/* type for expected input & output values of undeflowing_powers */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_undeflowing_powers_io_table_type;
/* type for expected input & output values of pos_infinity */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_pos_infinity_io_table_type;
/* type for expected input & output values of neg_infinity */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_neg_infinity_io_table_type;
/* type for expected input & output values of random_small_neg_sqr */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_random_small_neg_sqr_io_table_type;
/* type for expected input & output values of undeflowing_powers3 */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_undeflowing_powers3_io_table_type;
/* type for expected input & output values of undeflowing_powers4 */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_undeflowing_powers4_io_table_type;
/* type for expected input & output values of overflowing_powers_neg */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_overflowing_powers_neg_io_table_type;
/* type for expected input & output values of roots_to_minus_ten */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_roots_to_minus_ten_io_table_type;
/* type for expected input & output values of pow_denom_neg_neg */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_pow_denom_neg_neg_io_table_type;
/* type for expected input & output values of pow_denom_pos_pos */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_pow_denom_pos_pos_io_table_type;
/* type for expected input & output values of infinity */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_infinity_io_table_type;
/* type for expected input & output values of pow_denom_neg_pos */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_pow_denom_neg_pos_io_table_type;
/* type for expected input & output values of pow_denom_pos_neg */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_extreme_pow_denom_pos_neg_io_table_type;
/**
 * 5 specified tests for pow
 * (NaN,0); (NaN,1); (0,NaN); (1,NaN); (NaN,NaN)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_nan in the following table */
pow_uv__uv_extreme_nan_io_table_type pow_uv__uv_extreme_nan_io_table [5] = {
   {NAN,0,1.0},
   {NAN,1.0,NAN},
   {0,NAN,NAN},
   {1.0,NAN,1.0},
   {NAN,NAN,NAN},
};
/** function pow_uv__uv_extreme_nan executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_nan() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<5;ind++) {
		x=pow_uv__uv_extreme_nan_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_nan_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_nan_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_nan: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_nan_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==5) {
		PRINTF("pow_uv__uv_extreme_nan: successfully tested: 5 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_nan: %d tests failed for pow (out of 5)\n",errors);
	}
#endif
	return errors;
}

/**
 * 5 specified tests for pow
 * (Infinity,0); (Infinity,1); (0,Infinity); (1,Infinity); (Infinity,Infinity)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_pos_infinity in the following table */
pow_uv__uv_extreme_pos_infinity_io_table_type pow_uv__uv_extreme_pos_infinity_io_table [5] = {
   {INFINITY,0,1.0},
   {INFINITY,1.0,INFINITY},
   {0,INFINITY,0.0},
   {1.0,INFINITY,1.0},
   {INFINITY,INFINITY,INFINITY},
};
/** function pow_uv__uv_extreme_pos_infinity executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_pos_infinity() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<5;ind++) {
		x=pow_uv__uv_extreme_pos_infinity_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_pos_infinity_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_pos_infinity_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_pos_infinity: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_pos_infinity_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==5) {
		PRINTF("pow_uv__uv_extreme_pos_infinity: successfully tested: 5 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_pos_infinity: %d tests failed for pow (out of 5)\n",errors);
	}
#endif
	return errors;
}

/**
 * 5 specified tests for pow
 * (-Infinity,0); (-Infinity,1); (0,-Infinity); (1,-Infinity); (-Infinity,-Infinity) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_neg_infinity in the following table */
pow_uv__uv_extreme_neg_infinity_io_table_type pow_uv__uv_extreme_neg_infinity_io_table [5] = {
   {-INFINITY,0,1.0},
   {-INFINITY,1.0,-INFINITY},
   {0,-INFINITY,INFINITY},
   {1.0,-INFINITY,1.0},
   {-INFINITY,-INFINITY,0.0},
};
/** function pow_uv__uv_extreme_neg_infinity executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_neg_infinity() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<5;ind++) {
		x=pow_uv__uv_extreme_neg_infinity_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_neg_infinity_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_neg_infinity_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_neg_infinity: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_neg_infinity_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==5) {
		PRINTF("pow_uv__uv_extreme_neg_infinity: successfully tested: 5 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_neg_infinity: %d tests failed for pow (out of 5)\n",errors);
	}
#endif
	return errors;
}

/**
 * 6 specified tests for pow
 * (-Infinity,Infinity); (Infinity,-Infinity); (NaN,-Infinity); (NaN,Infinity); (-Infinity,NaN) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_infinity in the following table */
pow_uv__uv_extreme_infinity_io_table_type pow_uv__uv_extreme_infinity_io_table [6] = {
   {-INFINITY,INFINITY,INFINITY},
   {INFINITY,-INFINITY,0.0},
   {NAN,-INFINITY,NAN},
   {NAN,INFINITY,NAN},
   {-INFINITY,NAN,NAN},
   {INFINITY,NAN,NAN},
};
/** function pow_uv__uv_extreme_infinity executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_infinity() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<6;ind++) {
		x=pow_uv__uv_extreme_infinity_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_infinity_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_infinity_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_infinity: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_infinity_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==6) {
		PRINTF("pow_uv__uv_extreme_infinity: successfully tested: 6 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_infinity: %d tests failed for pow (out of 6)\n",errors);
	}
#endif
	return errors;
}

/**
 * 9 specified tests for pow
 * (NaN,NaN); (NaN,Infinity); (NaN,-Infinity); (Infinity,NaN); (Infinity,Infinity); (Infinity,-Infinity) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_extreme_values in the following table */
pow_uv__uv_extreme_extreme_values_io_table_type pow_uv__uv_extreme_extreme_values_io_table [9] = {
   {NAN,NAN,NAN},
   {NAN,INFINITY,NAN},
   {NAN,-INFINITY,NAN},
   {INFINITY,NAN,NAN},
   {INFINITY,INFINITY,INFINITY},
   {INFINITY,-INFINITY,0.0},
   {-INFINITY,NAN,NAN},
   {-INFINITY,INFINITY,INFINITY},
   {-INFINITY,-INFINITY,0.0},
};
/** function pow_uv__uv_extreme_extreme_values executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_extreme_values() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<9;ind++) {
		x=pow_uv__uv_extreme_extreme_values_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_extreme_values_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_extreme_values_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_extreme_values: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_extreme_values_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==9) {
		PRINTF("pow_uv__uv_extreme_extreme_values: successfully tested: 9 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_extreme_values: %d tests failed for pow (out of 9)\n",errors);
	}
#endif
	return errors;
}

/**
 * 256 specified tests for pow
 * (2.472304287230225E-309,2.472304287230225E-309); (2.472304287230225E-309,2.74700476358915E-310) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_pow_denom_pos_pos in the following table */
pow_uv__uv_extreme_pow_denom_pos_pos_io_table_type pow_uv__uv_extreme_pow_denom_pos_pos_io_table [256] = {
   {2.472304287230225E-309,2.472304287230225E-309,1.0},
   {2.472304287230225E-309,2.74700476358915E-310,1.0},
   {2.472304287230225E-309,3.052227515099E-311,1.0},
   {2.472304287230225E-309,3.391363905667E-312,1.0},
   {2.472304287230225E-309,3.76818211743E-313,1.0},
   {2.472304287230225E-309,4.1868690193E-314,1.0},
   {2.472304287230225E-309,4.652076687E-315,1.0},
   {2.472304287230225E-309,5.16897407E-316,1.0},
   {2.472304287230225E-309,5.7433046E-317,1.0},
   {2.472304287230225E-309,6.38145E-318,1.0},
   {2.472304287230225E-309,7.0905E-319,1.0},
   {2.472304287230225E-309,7.8784E-320,1.0},
   {2.472304287230225E-309,8.755E-321,1.0},
   {2.472304287230225E-309,9.73E-322,1.0},
   {2.472304287230225E-309,1.1E-322,1.0},
   {2.472304287230225E-309,1.0E-323,1.0},
   {2.74700476358915E-310,2.472304287230225E-309,1.0},
   {2.74700476358915E-310,2.74700476358915E-310,1.0},
   {2.74700476358915E-310,3.052227515099E-311,1.0},
   {2.74700476358915E-310,3.391363905667E-312,1.0},
   {2.74700476358915E-310,3.76818211743E-313,1.0},
   {2.74700476358915E-310,4.1868690193E-314,1.0},
   {2.74700476358915E-310,4.652076687E-315,1.0},
   {2.74700476358915E-310,5.16897407E-316,1.0},
   {2.74700476358915E-310,5.7433046E-317,1.0},
   {2.74700476358915E-310,6.38145E-318,1.0},
   {2.74700476358915E-310,7.0905E-319,1.0},
   {2.74700476358915E-310,7.8784E-320,1.0},
   {2.74700476358915E-310,8.755E-321,1.0},
   {2.74700476358915E-310,9.73E-322,1.0},
   {2.74700476358915E-310,1.1E-322,1.0},
   {2.74700476358915E-310,1.0E-323,1.0},
   {3.052227515099E-311,2.472304287230225E-309,1.0},
   {3.052227515099E-311,2.74700476358915E-310,1.0},
   {3.052227515099E-311,3.052227515099E-311,1.0},
   {3.052227515099E-311,3.391363905667E-312,1.0},
   {3.052227515099E-311,3.76818211743E-313,1.0},
   {3.052227515099E-311,4.1868690193E-314,1.0},
   {3.052227515099E-311,4.652076687E-315,1.0},
   {3.052227515099E-311,5.16897407E-316,1.0},
   {3.052227515099E-311,5.7433046E-317,1.0},
   {3.052227515099E-311,6.38145E-318,1.0},
   {3.052227515099E-311,7.0905E-319,1.0},
   {3.052227515099E-311,7.8784E-320,1.0},
   {3.052227515099E-311,8.755E-321,1.0},
   {3.052227515099E-311,9.73E-322,1.0},
   {3.052227515099E-311,1.1E-322,1.0},
   {3.052227515099E-311,1.0E-323,1.0},
   {3.391363905667E-312,2.472304287230225E-309,1.0},
   {3.391363905667E-312,2.74700476358915E-310,1.0},
   {3.391363905667E-312,3.052227515099E-311,1.0},
   {3.391363905667E-312,3.391363905667E-312,1.0},
   {3.391363905667E-312,3.76818211743E-313,1.0},
   {3.391363905667E-312,4.1868690193E-314,1.0},
   {3.391363905667E-312,4.652076687E-315,1.0},
   {3.391363905667E-312,5.16897407E-316,1.0},
   {3.391363905667E-312,5.7433046E-317,1.0},
   {3.391363905667E-312,6.38145E-318,1.0},
   {3.391363905667E-312,7.0905E-319,1.0},
   {3.391363905667E-312,7.8784E-320,1.0},
   {3.391363905667E-312,8.755E-321,1.0},
   {3.391363905667E-312,9.73E-322,1.0},
   {3.391363905667E-312,1.1E-322,1.0},
   {3.391363905667E-312,1.0E-323,1.0},
   {3.76818211743E-313,2.472304287230225E-309,1.0},
   {3.76818211743E-313,2.74700476358915E-310,1.0},
   {3.76818211743E-313,3.052227515099E-311,1.0},
   {3.76818211743E-313,3.391363905667E-312,1.0},
   {3.76818211743E-313,3.76818211743E-313,1.0},
   {3.76818211743E-313,4.1868690193E-314,1.0},
   {3.76818211743E-313,4.652076687E-315,1.0},
   {3.76818211743E-313,5.16897407E-316,1.0},
   {3.76818211743E-313,5.7433046E-317,1.0},
   {3.76818211743E-313,6.38145E-318,1.0},
   {3.76818211743E-313,7.0905E-319,1.0},
   {3.76818211743E-313,7.8784E-320,1.0},
   {3.76818211743E-313,8.755E-321,1.0},
   {3.76818211743E-313,9.73E-322,1.0},
   {3.76818211743E-313,1.1E-322,1.0},
   {3.76818211743E-313,1.0E-323,1.0},
   {4.1868690193E-314,2.472304287230225E-309,1.0},
   {4.1868690193E-314,2.74700476358915E-310,1.0},
   {4.1868690193E-314,3.052227515099E-311,1.0},
   {4.1868690193E-314,3.391363905667E-312,1.0},
   {4.1868690193E-314,3.76818211743E-313,1.0},
   {4.1868690193E-314,4.1868690193E-314,1.0},
   {4.1868690193E-314,4.652076687E-315,1.0},
   {4.1868690193E-314,5.16897407E-316,1.0},
   {4.1868690193E-314,5.7433046E-317,1.0},
   {4.1868690193E-314,6.38145E-318,1.0},
   {4.1868690193E-314,7.0905E-319,1.0},
   {4.1868690193E-314,7.8784E-320,1.0},
   {4.1868690193E-314,8.755E-321,1.0},
   {4.1868690193E-314,9.73E-322,1.0},
   {4.1868690193E-314,1.1E-322,1.0},
   {4.1868690193E-314,1.0E-323,1.0},
   {4.652076687E-315,2.472304287230225E-309,1.0},
   {4.652076687E-315,2.74700476358915E-310,1.0},
   {4.652076687E-315,3.052227515099E-311,1.0},
   {4.652076687E-315,3.391363905667E-312,1.0},
   {4.652076687E-315,3.76818211743E-313,1.0},
   {4.652076687E-315,4.1868690193E-314,1.0},
   {4.652076687E-315,4.652076687E-315,1.0},
   {4.652076687E-315,5.16897407E-316,1.0},
   {4.652076687E-315,5.7433046E-317,1.0},
   {4.652076687E-315,6.38145E-318,1.0},
   {4.652076687E-315,7.0905E-319,1.0},
   {4.652076687E-315,7.8784E-320,1.0},
   {4.652076687E-315,8.755E-321,1.0},
   {4.652076687E-315,9.73E-322,1.0},
   {4.652076687E-315,1.1E-322,1.0},
   {4.652076687E-315,1.0E-323,1.0},
   {5.16897407E-316,2.472304287230225E-309,1.0},
   {5.16897407E-316,2.74700476358915E-310,1.0},
   {5.16897407E-316,3.052227515099E-311,1.0},
   {5.16897407E-316,3.391363905667E-312,1.0},
   {5.16897407E-316,3.76818211743E-313,1.0},
   {5.16897407E-316,4.1868690193E-314,1.0},
   {5.16897407E-316,4.652076687E-315,1.0},
   {5.16897407E-316,5.16897407E-316,1.0},
   {5.16897407E-316,5.7433046E-317,1.0},
   {5.16897407E-316,6.38145E-318,1.0},
   {5.16897407E-316,7.0905E-319,1.0},
   {5.16897407E-316,7.8784E-320,1.0},
   {5.16897407E-316,8.755E-321,1.0},
   {5.16897407E-316,9.73E-322,1.0},
   {5.16897407E-316,1.1E-322,1.0},
   {5.16897407E-316,1.0E-323,1.0},
   {5.7433046E-317,2.472304287230225E-309,1.0},
   {5.7433046E-317,2.74700476358915E-310,1.0},
   {5.7433046E-317,3.052227515099E-311,1.0},
   {5.7433046E-317,3.391363905667E-312,1.0},
   {5.7433046E-317,3.76818211743E-313,1.0},
   {5.7433046E-317,4.1868690193E-314,1.0},
   {5.7433046E-317,4.652076687E-315,1.0},
   {5.7433046E-317,5.16897407E-316,1.0},
   {5.7433046E-317,5.7433046E-317,1.0},
   {5.7433046E-317,6.38145E-318,1.0},
   {5.7433046E-317,7.0905E-319,1.0},
   {5.7433046E-317,7.8784E-320,1.0},
   {5.7433046E-317,8.755E-321,1.0},
   {5.7433046E-317,9.73E-322,1.0},
   {5.7433046E-317,1.1E-322,1.0},
   {5.7433046E-317,1.0E-323,1.0},
   {6.38145E-318,2.472304287230225E-309,1.0},
   {6.38145E-318,2.74700476358915E-310,1.0},
   {6.38145E-318,3.052227515099E-311,1.0},
   {6.38145E-318,3.391363905667E-312,1.0},
   {6.38145E-318,3.76818211743E-313,1.0},
   {6.38145E-318,4.1868690193E-314,1.0},
   {6.38145E-318,4.652076687E-315,1.0},
   {6.38145E-318,5.16897407E-316,1.0},
   {6.38145E-318,5.7433046E-317,1.0},
   {6.38145E-318,6.38145E-318,1.0},
   {6.38145E-318,7.0905E-319,1.0},
   {6.38145E-318,7.8784E-320,1.0},
   {6.38145E-318,8.755E-321,1.0},
   {6.38145E-318,9.73E-322,1.0},
   {6.38145E-318,1.1E-322,1.0},
   {6.38145E-318,1.0E-323,1.0},
   {7.0905E-319,2.472304287230225E-309,1.0},
   {7.0905E-319,2.74700476358915E-310,1.0},
   {7.0905E-319,3.052227515099E-311,1.0},
   {7.0905E-319,3.391363905667E-312,1.0},
   {7.0905E-319,3.76818211743E-313,1.0},
   {7.0905E-319,4.1868690193E-314,1.0},
   {7.0905E-319,4.652076687E-315,1.0},
   {7.0905E-319,5.16897407E-316,1.0},
   {7.0905E-319,5.7433046E-317,1.0},
   {7.0905E-319,6.38145E-318,1.0},
   {7.0905E-319,7.0905E-319,1.0},
   {7.0905E-319,7.8784E-320,1.0},
   {7.0905E-319,8.755E-321,1.0},
   {7.0905E-319,9.73E-322,1.0},
   {7.0905E-319,1.1E-322,1.0},
   {7.0905E-319,1.0E-323,1.0},
   {7.8784E-320,2.472304287230225E-309,1.0},
   {7.8784E-320,2.74700476358915E-310,1.0},
   {7.8784E-320,3.052227515099E-311,1.0},
   {7.8784E-320,3.391363905667E-312,1.0},
   {7.8784E-320,3.76818211743E-313,1.0},
   {7.8784E-320,4.1868690193E-314,1.0},
   {7.8784E-320,4.652076687E-315,1.0},
   {7.8784E-320,5.16897407E-316,1.0},
   {7.8784E-320,5.7433046E-317,1.0},
   {7.8784E-320,6.38145E-318,1.0},
   {7.8784E-320,7.0905E-319,1.0},
   {7.8784E-320,7.8784E-320,1.0},
   {7.8784E-320,8.755E-321,1.0},
   {7.8784E-320,9.73E-322,1.0},
   {7.8784E-320,1.1E-322,1.0},
   {7.8784E-320,1.0E-323,1.0},
   {8.755E-321,2.472304287230225E-309,1.0},
   {8.755E-321,2.74700476358915E-310,1.0},
   {8.755E-321,3.052227515099E-311,1.0},
   {8.755E-321,3.391363905667E-312,1.0},
   {8.755E-321,3.76818211743E-313,1.0},
   {8.755E-321,4.1868690193E-314,1.0},
   {8.755E-321,4.652076687E-315,1.0},
   {8.755E-321,5.16897407E-316,1.0},
   {8.755E-321,5.7433046E-317,1.0},
   {8.755E-321,6.38145E-318,1.0},
   {8.755E-321,7.0905E-319,1.0},
   {8.755E-321,7.8784E-320,1.0},
   {8.755E-321,8.755E-321,1.0},
   {8.755E-321,9.73E-322,1.0},
   {8.755E-321,1.1E-322,1.0},
   {8.755E-321,1.0E-323,1.0},
   {9.73E-322,2.472304287230225E-309,1.0},
   {9.73E-322,2.74700476358915E-310,1.0},
   {9.73E-322,3.052227515099E-311,1.0},
   {9.73E-322,3.391363905667E-312,1.0},
   {9.73E-322,3.76818211743E-313,1.0},
   {9.73E-322,4.1868690193E-314,1.0},
   {9.73E-322,4.652076687E-315,1.0},
   {9.73E-322,5.16897407E-316,1.0},
   {9.73E-322,5.7433046E-317,1.0},
   {9.73E-322,6.38145E-318,1.0},
   {9.73E-322,7.0905E-319,1.0},
   {9.73E-322,7.8784E-320,1.0},
   {9.73E-322,8.755E-321,1.0},
   {9.73E-322,9.73E-322,1.0},
   {9.73E-322,1.1E-322,1.0},
   {9.73E-322,1.0E-323,1.0},
   {1.1E-322,2.472304287230225E-309,1.0},
   {1.1E-322,2.74700476358915E-310,1.0},
   {1.1E-322,3.052227515099E-311,1.0},
   {1.1E-322,3.391363905667E-312,1.0},
   {1.1E-322,3.76818211743E-313,1.0},
   {1.1E-322,4.1868690193E-314,1.0},
   {1.1E-322,4.652076687E-315,1.0},
   {1.1E-322,5.16897407E-316,1.0},
   {1.1E-322,5.7433046E-317,1.0},
   {1.1E-322,6.38145E-318,1.0},
   {1.1E-322,7.0905E-319,1.0},
   {1.1E-322,7.8784E-320,1.0},
   {1.1E-322,8.755E-321,1.0},
   {1.1E-322,9.73E-322,1.0},
   {1.1E-322,1.1E-322,1.0},
   {1.1E-322,1.0E-323,1.0},
   {1.0E-323,2.472304287230225E-309,1.0},
   {1.0E-323,2.74700476358915E-310,1.0},
   {1.0E-323,3.052227515099E-311,1.0},
   {1.0E-323,3.391363905667E-312,1.0},
   {1.0E-323,3.76818211743E-313,1.0},
   {1.0E-323,4.1868690193E-314,1.0},
   {1.0E-323,4.652076687E-315,1.0},
   {1.0E-323,5.16897407E-316,1.0},
   {1.0E-323,5.7433046E-317,1.0},
   {1.0E-323,6.38145E-318,1.0},
   {1.0E-323,7.0905E-319,1.0},
   {1.0E-323,7.8784E-320,1.0},
   {1.0E-323,8.755E-321,1.0},
   {1.0E-323,9.73E-322,1.0},
   {1.0E-323,1.1E-322,1.0},
   {1.0E-323,1.0E-323,1.0},
};
/** function pow_uv__uv_extreme_pow_denom_pos_pos executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_pow_denom_pos_pos() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<256;ind++) {
		x=pow_uv__uv_extreme_pow_denom_pos_pos_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_pow_denom_pos_pos_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_pow_denom_pos_pos_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_pow_denom_pos_pos: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_pow_denom_pos_pos_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==256) {
		PRINTF("pow_uv__uv_extreme_pow_denom_pos_pos: successfully tested: 256 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_pow_denom_pos_pos: %d tests failed for pow (out of 256)\n",errors);
	}
#endif
	return errors;
}

/**
 * 272 specified tests for pow
 * (2.472304287230225E-309,-2.781342323134002E-309); (2.472304287230225E-309,-3.4766779039175E-310) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_pow_denom_pos_neg in the following table */
pow_uv__uv_extreme_pow_denom_pos_neg_io_table_type pow_uv__uv_extreme_pow_denom_pos_neg_io_table [272] = {
   {2.472304287230225E-309,-2.781342323134002E-309,1.0},
   {2.472304287230225E-309,-3.4766779039175E-310,1.0},
   {2.472304287230225E-309,-4.345847379897E-311,1.0},
   {2.472304287230225E-309,-5.43230922487E-312,1.0},
   {2.472304287230225E-309,-6.7903865311E-313,1.0},
   {2.472304287230225E-309,-8.4879831639E-314,1.0},
   {2.472304287230225E-309,-1.0609978955E-314,1.0},
   {2.472304287230225E-309,-1.32624737E-315,1.0},
   {2.472304287230225E-309,-1.6578092E-316,1.0},
   {2.472304287230225E-309,-2.0722615E-317,1.0},
   {2.472304287230225E-309,-2.590327E-318,1.0},
   {2.472304287230225E-309,-3.2379E-319,1.0},
   {2.472304287230225E-309,-4.0474E-320,1.0},
   {2.472304287230225E-309,-5.06E-321,1.0},
   {2.472304287230225E-309,-6.32E-322,1.0},
   {2.472304287230225E-309,-7.9E-323,1.0},
   {2.472304287230225E-309,-1.0E-323,1.0},
   {2.74700476358915E-310,-2.781342323134002E-309,1.0},
   {2.74700476358915E-310,-3.4766779039175E-310,1.0},
   {2.74700476358915E-310,-4.345847379897E-311,1.0},
   {2.74700476358915E-310,-5.43230922487E-312,1.0},
   {2.74700476358915E-310,-6.7903865311E-313,1.0},
   {2.74700476358915E-310,-8.4879831639E-314,1.0},
   {2.74700476358915E-310,-1.0609978955E-314,1.0},
   {2.74700476358915E-310,-1.32624737E-315,1.0},
   {2.74700476358915E-310,-1.6578092E-316,1.0},
   {2.74700476358915E-310,-2.0722615E-317,1.0},
   {2.74700476358915E-310,-2.590327E-318,1.0},
   {2.74700476358915E-310,-3.2379E-319,1.0},
   {2.74700476358915E-310,-4.0474E-320,1.0},
   {2.74700476358915E-310,-5.06E-321,1.0},
   {2.74700476358915E-310,-6.32E-322,1.0},
   {2.74700476358915E-310,-7.9E-323,1.0},
   {2.74700476358915E-310,-1.0E-323,1.0},
   {3.052227515099E-311,-2.781342323134002E-309,1.0},
   {3.052227515099E-311,-3.4766779039175E-310,1.0},
   {3.052227515099E-311,-4.345847379897E-311,1.0},
   {3.052227515099E-311,-5.43230922487E-312,1.0},
   {3.052227515099E-311,-6.7903865311E-313,1.0},
   {3.052227515099E-311,-8.4879831639E-314,1.0},
   {3.052227515099E-311,-1.0609978955E-314,1.0},
   {3.052227515099E-311,-1.32624737E-315,1.0},
   {3.052227515099E-311,-1.6578092E-316,1.0},
   {3.052227515099E-311,-2.0722615E-317,1.0},
   {3.052227515099E-311,-2.590327E-318,1.0},
   {3.052227515099E-311,-3.2379E-319,1.0},
   {3.052227515099E-311,-4.0474E-320,1.0},
   {3.052227515099E-311,-5.06E-321,1.0},
   {3.052227515099E-311,-6.32E-322,1.0},
   {3.052227515099E-311,-7.9E-323,1.0},
   {3.052227515099E-311,-1.0E-323,1.0},
   {3.391363905667E-312,-2.781342323134002E-309,1.0},
   {3.391363905667E-312,-3.4766779039175E-310,1.0},
   {3.391363905667E-312,-4.345847379897E-311,1.0},
   {3.391363905667E-312,-5.43230922487E-312,1.0},
   {3.391363905667E-312,-6.7903865311E-313,1.0},
   {3.391363905667E-312,-8.4879831639E-314,1.0},
   {3.391363905667E-312,-1.0609978955E-314,1.0},
   {3.391363905667E-312,-1.32624737E-315,1.0},
   {3.391363905667E-312,-1.6578092E-316,1.0},
   {3.391363905667E-312,-2.0722615E-317,1.0},
   {3.391363905667E-312,-2.590327E-318,1.0},
   {3.391363905667E-312,-3.2379E-319,1.0},
   {3.391363905667E-312,-4.0474E-320,1.0},
   {3.391363905667E-312,-5.06E-321,1.0},
   {3.391363905667E-312,-6.32E-322,1.0},
   {3.391363905667E-312,-7.9E-323,1.0},
   {3.391363905667E-312,-1.0E-323,1.0},
   {3.76818211743E-313,-2.781342323134002E-309,1.0},
   {3.76818211743E-313,-3.4766779039175E-310,1.0},
   {3.76818211743E-313,-4.345847379897E-311,1.0},
   {3.76818211743E-313,-5.43230922487E-312,1.0},
   {3.76818211743E-313,-6.7903865311E-313,1.0},
   {3.76818211743E-313,-8.4879831639E-314,1.0},
   {3.76818211743E-313,-1.0609978955E-314,1.0},
   {3.76818211743E-313,-1.32624737E-315,1.0},
   {3.76818211743E-313,-1.6578092E-316,1.0},
   {3.76818211743E-313,-2.0722615E-317,1.0},
   {3.76818211743E-313,-2.590327E-318,1.0},
   {3.76818211743E-313,-3.2379E-319,1.0},
   {3.76818211743E-313,-4.0474E-320,1.0},
   {3.76818211743E-313,-5.06E-321,1.0},
   {3.76818211743E-313,-6.32E-322,1.0},
   {3.76818211743E-313,-7.9E-323,1.0},
   {3.76818211743E-313,-1.0E-323,1.0},
   {4.1868690193E-314,-2.781342323134002E-309,1.0},
   {4.1868690193E-314,-3.4766779039175E-310,1.0},
   {4.1868690193E-314,-4.345847379897E-311,1.0},
   {4.1868690193E-314,-5.43230922487E-312,1.0},
   {4.1868690193E-314,-6.7903865311E-313,1.0},
   {4.1868690193E-314,-8.4879831639E-314,1.0},
   {4.1868690193E-314,-1.0609978955E-314,1.0},
   {4.1868690193E-314,-1.32624737E-315,1.0},
   {4.1868690193E-314,-1.6578092E-316,1.0},
   {4.1868690193E-314,-2.0722615E-317,1.0},
   {4.1868690193E-314,-2.590327E-318,1.0},
   {4.1868690193E-314,-3.2379E-319,1.0},
   {4.1868690193E-314,-4.0474E-320,1.0},
   {4.1868690193E-314,-5.06E-321,1.0},
   {4.1868690193E-314,-6.32E-322,1.0},
   {4.1868690193E-314,-7.9E-323,1.0},
   {4.1868690193E-314,-1.0E-323,1.0},
   {4.652076687E-315,-2.781342323134002E-309,1.0},
   {4.652076687E-315,-3.4766779039175E-310,1.0},
   {4.652076687E-315,-4.345847379897E-311,1.0},
   {4.652076687E-315,-5.43230922487E-312,1.0},
   {4.652076687E-315,-6.7903865311E-313,1.0},
   {4.652076687E-315,-8.4879831639E-314,1.0},
   {4.652076687E-315,-1.0609978955E-314,1.0},
   {4.652076687E-315,-1.32624737E-315,1.0},
   {4.652076687E-315,-1.6578092E-316,1.0},
   {4.652076687E-315,-2.0722615E-317,1.0},
   {4.652076687E-315,-2.590327E-318,1.0},
   {4.652076687E-315,-3.2379E-319,1.0},
   {4.652076687E-315,-4.0474E-320,1.0},
   {4.652076687E-315,-5.06E-321,1.0},
   {4.652076687E-315,-6.32E-322,1.0},
   {4.652076687E-315,-7.9E-323,1.0},
   {4.652076687E-315,-1.0E-323,1.0},
   {5.16897407E-316,-2.781342323134002E-309,1.0},
   {5.16897407E-316,-3.4766779039175E-310,1.0},
   {5.16897407E-316,-4.345847379897E-311,1.0},
   {5.16897407E-316,-5.43230922487E-312,1.0},
   {5.16897407E-316,-6.7903865311E-313,1.0},
   {5.16897407E-316,-8.4879831639E-314,1.0},
   {5.16897407E-316,-1.0609978955E-314,1.0},
   {5.16897407E-316,-1.32624737E-315,1.0},
   {5.16897407E-316,-1.6578092E-316,1.0},
   {5.16897407E-316,-2.0722615E-317,1.0},
   {5.16897407E-316,-2.590327E-318,1.0},
   {5.16897407E-316,-3.2379E-319,1.0},
   {5.16897407E-316,-4.0474E-320,1.0},
   {5.16897407E-316,-5.06E-321,1.0},
   {5.16897407E-316,-6.32E-322,1.0},
   {5.16897407E-316,-7.9E-323,1.0},
   {5.16897407E-316,-1.0E-323,1.0},
   {5.7433046E-317,-2.781342323134002E-309,1.0},
   {5.7433046E-317,-3.4766779039175E-310,1.0},
   {5.7433046E-317,-4.345847379897E-311,1.0},
   {5.7433046E-317,-5.43230922487E-312,1.0},
   {5.7433046E-317,-6.7903865311E-313,1.0},
   {5.7433046E-317,-8.4879831639E-314,1.0},
   {5.7433046E-317,-1.0609978955E-314,1.0},
   {5.7433046E-317,-1.32624737E-315,1.0},
   {5.7433046E-317,-1.6578092E-316,1.0},
   {5.7433046E-317,-2.0722615E-317,1.0},
   {5.7433046E-317,-2.590327E-318,1.0},
   {5.7433046E-317,-3.2379E-319,1.0},
   {5.7433046E-317,-4.0474E-320,1.0},
   {5.7433046E-317,-5.06E-321,1.0},
   {5.7433046E-317,-6.32E-322,1.0},
   {5.7433046E-317,-7.9E-323,1.0},
   {5.7433046E-317,-1.0E-323,1.0},
   {6.38145E-318,-2.781342323134002E-309,1.0},
   {6.38145E-318,-3.4766779039175E-310,1.0},
   {6.38145E-318,-4.345847379897E-311,1.0},
   {6.38145E-318,-5.43230922487E-312,1.0},
   {6.38145E-318,-6.7903865311E-313,1.0},
   {6.38145E-318,-8.4879831639E-314,1.0},
   {6.38145E-318,-1.0609978955E-314,1.0},
   {6.38145E-318,-1.32624737E-315,1.0},
   {6.38145E-318,-1.6578092E-316,1.0},
   {6.38145E-318,-2.0722615E-317,1.0},
   {6.38145E-318,-2.590327E-318,1.0},
   {6.38145E-318,-3.2379E-319,1.0},
   {6.38145E-318,-4.0474E-320,1.0},
   {6.38145E-318,-5.06E-321,1.0},
   {6.38145E-318,-6.32E-322,1.0},
   {6.38145E-318,-7.9E-323,1.0},
   {6.38145E-318,-1.0E-323,1.0},
   {7.0905E-319,-2.781342323134002E-309,1.0},
   {7.0905E-319,-3.4766779039175E-310,1.0},
   {7.0905E-319,-4.345847379897E-311,1.0},
   {7.0905E-319,-5.43230922487E-312,1.0},
   {7.0905E-319,-6.7903865311E-313,1.0},
   {7.0905E-319,-8.4879831639E-314,1.0},
   {7.0905E-319,-1.0609978955E-314,1.0},
   {7.0905E-319,-1.32624737E-315,1.0},
   {7.0905E-319,-1.6578092E-316,1.0},
   {7.0905E-319,-2.0722615E-317,1.0},
   {7.0905E-319,-2.590327E-318,1.0},
   {7.0905E-319,-3.2379E-319,1.0},
   {7.0905E-319,-4.0474E-320,1.0},
   {7.0905E-319,-5.06E-321,1.0},
   {7.0905E-319,-6.32E-322,1.0},
   {7.0905E-319,-7.9E-323,1.0},
   {7.0905E-319,-1.0E-323,1.0},
   {7.8784E-320,-2.781342323134002E-309,1.0},
   {7.8784E-320,-3.4766779039175E-310,1.0},
   {7.8784E-320,-4.345847379897E-311,1.0},
   {7.8784E-320,-5.43230922487E-312,1.0},
   {7.8784E-320,-6.7903865311E-313,1.0},
   {7.8784E-320,-8.4879831639E-314,1.0},
   {7.8784E-320,-1.0609978955E-314,1.0},
   {7.8784E-320,-1.32624737E-315,1.0},
   {7.8784E-320,-1.6578092E-316,1.0},
   {7.8784E-320,-2.0722615E-317,1.0},
   {7.8784E-320,-2.590327E-318,1.0},
   {7.8784E-320,-3.2379E-319,1.0},
   {7.8784E-320,-4.0474E-320,1.0},
   {7.8784E-320,-5.06E-321,1.0},
   {7.8784E-320,-6.32E-322,1.0},
   {7.8784E-320,-7.9E-323,1.0},
   {7.8784E-320,-1.0E-323,1.0},
   {8.755E-321,-2.781342323134002E-309,1.0},
   {8.755E-321,-3.4766779039175E-310,1.0},
   {8.755E-321,-4.345847379897E-311,1.0},
   {8.755E-321,-5.43230922487E-312,1.0},
   {8.755E-321,-6.7903865311E-313,1.0},
   {8.755E-321,-8.4879831639E-314,1.0},
   {8.755E-321,-1.0609978955E-314,1.0},
   {8.755E-321,-1.32624737E-315,1.0},
   {8.755E-321,-1.6578092E-316,1.0},
   {8.755E-321,-2.0722615E-317,1.0},
   {8.755E-321,-2.590327E-318,1.0},
   {8.755E-321,-3.2379E-319,1.0},
   {8.755E-321,-4.0474E-320,1.0},
   {8.755E-321,-5.06E-321,1.0},
   {8.755E-321,-6.32E-322,1.0},
   {8.755E-321,-7.9E-323,1.0},
   {8.755E-321,-1.0E-323,1.0},
   {9.73E-322,-2.781342323134002E-309,1.0},
   {9.73E-322,-3.4766779039175E-310,1.0},
   {9.73E-322,-4.345847379897E-311,1.0},
   {9.73E-322,-5.43230922487E-312,1.0},
   {9.73E-322,-6.7903865311E-313,1.0},
   {9.73E-322,-8.4879831639E-314,1.0},
   {9.73E-322,-1.0609978955E-314,1.0},
   {9.73E-322,-1.32624737E-315,1.0},
   {9.73E-322,-1.6578092E-316,1.0},
   {9.73E-322,-2.0722615E-317,1.0},
   {9.73E-322,-2.590327E-318,1.0},
   {9.73E-322,-3.2379E-319,1.0},
   {9.73E-322,-4.0474E-320,1.0},
   {9.73E-322,-5.06E-321,1.0},
   {9.73E-322,-6.32E-322,1.0},
   {9.73E-322,-7.9E-323,1.0},
   {9.73E-322,-1.0E-323,1.0},
   {1.1E-322,-2.781342323134002E-309,1.0},
   {1.1E-322,-3.4766779039175E-310,1.0},
   {1.1E-322,-4.345847379897E-311,1.0},
   {1.1E-322,-5.43230922487E-312,1.0},
   {1.1E-322,-6.7903865311E-313,1.0},
   {1.1E-322,-8.4879831639E-314,1.0},
   {1.1E-322,-1.0609978955E-314,1.0},
   {1.1E-322,-1.32624737E-315,1.0},
   {1.1E-322,-1.6578092E-316,1.0},
   {1.1E-322,-2.0722615E-317,1.0},
   {1.1E-322,-2.590327E-318,1.0},
   {1.1E-322,-3.2379E-319,1.0},
   {1.1E-322,-4.0474E-320,1.0},
   {1.1E-322,-5.06E-321,1.0},
   {1.1E-322,-6.32E-322,1.0},
   {1.1E-322,-7.9E-323,1.0},
   {1.1E-322,-1.0E-323,1.0},
   {1.0E-323,-2.781342323134002E-309,1.0},
   {1.0E-323,-3.4766779039175E-310,1.0},
   {1.0E-323,-4.345847379897E-311,1.0},
   {1.0E-323,-5.43230922487E-312,1.0},
   {1.0E-323,-6.7903865311E-313,1.0},
   {1.0E-323,-8.4879831639E-314,1.0},
   {1.0E-323,-1.0609978955E-314,1.0},
   {1.0E-323,-1.32624737E-315,1.0},
   {1.0E-323,-1.6578092E-316,1.0},
   {1.0E-323,-2.0722615E-317,1.0},
   {1.0E-323,-2.590327E-318,1.0},
   {1.0E-323,-3.2379E-319,1.0},
   {1.0E-323,-4.0474E-320,1.0},
   {1.0E-323,-5.06E-321,1.0},
   {1.0E-323,-6.32E-322,1.0},
   {1.0E-323,-7.9E-323,1.0},
   {1.0E-323,-1.0E-323,1.0},
};
/** function pow_uv__uv_extreme_pow_denom_pos_neg executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_pow_denom_pos_neg() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<272;ind++) {
		x=pow_uv__uv_extreme_pow_denom_pos_neg_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_pow_denom_pos_neg_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_pow_denom_pos_neg_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_pow_denom_pos_neg: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_pow_denom_pos_neg_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==272) {
		PRINTF("pow_uv__uv_extreme_pow_denom_pos_neg: successfully tested: 272 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_pow_denom_pos_neg: %d tests failed for pow (out of 272)\n",errors);
	}
#endif
	return errors;
}

/**
 * 272 specified tests for pow
 * (-2.781342323134002E-309,2.472304287230225E-309); (-2.781342323134002E-309,2.74700476358915E-310) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_pow_denom_neg_pos in the following table */
pow_uv__uv_extreme_pow_denom_neg_pos_io_table_type pow_uv__uv_extreme_pow_denom_neg_pos_io_table [272] = {
   {-2.781342323134002E-309,2.472304287230225E-309,NAN},
   {-2.781342323134002E-309,2.74700476358915E-310,NAN},
   {-2.781342323134002E-309,3.052227515099E-311,NAN},
   {-2.781342323134002E-309,3.391363905667E-312,NAN},
   {-2.781342323134002E-309,3.76818211743E-313,NAN},
   {-2.781342323134002E-309,4.1868690193E-314,NAN},
   {-2.781342323134002E-309,4.652076687E-315,NAN},
   {-2.781342323134002E-309,5.16897407E-316,NAN},
   {-2.781342323134002E-309,5.7433046E-317,NAN},
   {-2.781342323134002E-309,6.38145E-318,NAN},
   {-2.781342323134002E-309,7.0905E-319,NAN},
   {-2.781342323134002E-309,7.8784E-320,NAN},
   {-2.781342323134002E-309,8.755E-321,NAN},
   {-2.781342323134002E-309,9.73E-322,NAN},
   {-2.781342323134002E-309,1.1E-322,NAN},
   {-2.781342323134002E-309,1.0E-323,NAN},
   {-3.4766779039175E-310,2.472304287230225E-309,NAN},
   {-3.4766779039175E-310,2.74700476358915E-310,NAN},
   {-3.4766779039175E-310,3.052227515099E-311,NAN},
   {-3.4766779039175E-310,3.391363905667E-312,NAN},
   {-3.4766779039175E-310,3.76818211743E-313,NAN},
   {-3.4766779039175E-310,4.1868690193E-314,NAN},
   {-3.4766779039175E-310,4.652076687E-315,NAN},
   {-3.4766779039175E-310,5.16897407E-316,NAN},
   {-3.4766779039175E-310,5.7433046E-317,NAN},
   {-3.4766779039175E-310,6.38145E-318,NAN},
   {-3.4766779039175E-310,7.0905E-319,NAN},
   {-3.4766779039175E-310,7.8784E-320,NAN},
   {-3.4766779039175E-310,8.755E-321,NAN},
   {-3.4766779039175E-310,9.73E-322,NAN},
   {-3.4766779039175E-310,1.1E-322,NAN},
   {-3.4766779039175E-310,1.0E-323,NAN},
   {-4.345847379897E-311,2.472304287230225E-309,NAN},
   {-4.345847379897E-311,2.74700476358915E-310,NAN},
   {-4.345847379897E-311,3.052227515099E-311,NAN},
   {-4.345847379897E-311,3.391363905667E-312,NAN},
   {-4.345847379897E-311,3.76818211743E-313,NAN},
   {-4.345847379897E-311,4.1868690193E-314,NAN},
   {-4.345847379897E-311,4.652076687E-315,NAN},
   {-4.345847379897E-311,5.16897407E-316,NAN},
   {-4.345847379897E-311,5.7433046E-317,NAN},
   {-4.345847379897E-311,6.38145E-318,NAN},
   {-4.345847379897E-311,7.0905E-319,NAN},
   {-4.345847379897E-311,7.8784E-320,NAN},
   {-4.345847379897E-311,8.755E-321,NAN},
   {-4.345847379897E-311,9.73E-322,NAN},
   {-4.345847379897E-311,1.1E-322,NAN},
   {-4.345847379897E-311,1.0E-323,NAN},
   {-5.43230922487E-312,2.472304287230225E-309,NAN},
   {-5.43230922487E-312,2.74700476358915E-310,NAN},
   {-5.43230922487E-312,3.052227515099E-311,NAN},
   {-5.43230922487E-312,3.391363905667E-312,NAN},
   {-5.43230922487E-312,3.76818211743E-313,NAN},
   {-5.43230922487E-312,4.1868690193E-314,NAN},
   {-5.43230922487E-312,4.652076687E-315,NAN},
   {-5.43230922487E-312,5.16897407E-316,NAN},
   {-5.43230922487E-312,5.7433046E-317,NAN},
   {-5.43230922487E-312,6.38145E-318,NAN},
   {-5.43230922487E-312,7.0905E-319,NAN},
   {-5.43230922487E-312,7.8784E-320,NAN},
   {-5.43230922487E-312,8.755E-321,NAN},
   {-5.43230922487E-312,9.73E-322,NAN},
   {-5.43230922487E-312,1.1E-322,NAN},
   {-5.43230922487E-312,1.0E-323,NAN},
   {-6.7903865311E-313,2.472304287230225E-309,NAN},
   {-6.7903865311E-313,2.74700476358915E-310,NAN},
   {-6.7903865311E-313,3.052227515099E-311,NAN},
   {-6.7903865311E-313,3.391363905667E-312,NAN},
   {-6.7903865311E-313,3.76818211743E-313,NAN},
   {-6.7903865311E-313,4.1868690193E-314,NAN},
   {-6.7903865311E-313,4.652076687E-315,NAN},
   {-6.7903865311E-313,5.16897407E-316,NAN},
   {-6.7903865311E-313,5.7433046E-317,NAN},
   {-6.7903865311E-313,6.38145E-318,NAN},
   {-6.7903865311E-313,7.0905E-319,NAN},
   {-6.7903865311E-313,7.8784E-320,NAN},
   {-6.7903865311E-313,8.755E-321,NAN},
   {-6.7903865311E-313,9.73E-322,NAN},
   {-6.7903865311E-313,1.1E-322,NAN},
   {-6.7903865311E-313,1.0E-323,NAN},
   {-8.4879831639E-314,2.472304287230225E-309,NAN},
   {-8.4879831639E-314,2.74700476358915E-310,NAN},
   {-8.4879831639E-314,3.052227515099E-311,NAN},
   {-8.4879831639E-314,3.391363905667E-312,NAN},
   {-8.4879831639E-314,3.76818211743E-313,NAN},
   {-8.4879831639E-314,4.1868690193E-314,NAN},
   {-8.4879831639E-314,4.652076687E-315,NAN},
   {-8.4879831639E-314,5.16897407E-316,NAN},
   {-8.4879831639E-314,5.7433046E-317,NAN},
   {-8.4879831639E-314,6.38145E-318,NAN},
   {-8.4879831639E-314,7.0905E-319,NAN},
   {-8.4879831639E-314,7.8784E-320,NAN},
   {-8.4879831639E-314,8.755E-321,NAN},
   {-8.4879831639E-314,9.73E-322,NAN},
   {-8.4879831639E-314,1.1E-322,NAN},
   {-8.4879831639E-314,1.0E-323,NAN},
   {-1.0609978955E-314,2.472304287230225E-309,NAN},
   {-1.0609978955E-314,2.74700476358915E-310,NAN},
   {-1.0609978955E-314,3.052227515099E-311,NAN},
   {-1.0609978955E-314,3.391363905667E-312,NAN},
   {-1.0609978955E-314,3.76818211743E-313,NAN},
   {-1.0609978955E-314,4.1868690193E-314,NAN},
   {-1.0609978955E-314,4.652076687E-315,NAN},
   {-1.0609978955E-314,5.16897407E-316,NAN},
   {-1.0609978955E-314,5.7433046E-317,NAN},
   {-1.0609978955E-314,6.38145E-318,NAN},
   {-1.0609978955E-314,7.0905E-319,NAN},
   {-1.0609978955E-314,7.8784E-320,NAN},
   {-1.0609978955E-314,8.755E-321,NAN},
   {-1.0609978955E-314,9.73E-322,NAN},
   {-1.0609978955E-314,1.1E-322,NAN},
   {-1.0609978955E-314,1.0E-323,NAN},
   {-1.32624737E-315,2.472304287230225E-309,NAN},
   {-1.32624737E-315,2.74700476358915E-310,NAN},
   {-1.32624737E-315,3.052227515099E-311,NAN},
   {-1.32624737E-315,3.391363905667E-312,NAN},
   {-1.32624737E-315,3.76818211743E-313,NAN},
   {-1.32624737E-315,4.1868690193E-314,NAN},
   {-1.32624737E-315,4.652076687E-315,NAN},
   {-1.32624737E-315,5.16897407E-316,NAN},
   {-1.32624737E-315,5.7433046E-317,NAN},
   {-1.32624737E-315,6.38145E-318,NAN},
   {-1.32624737E-315,7.0905E-319,NAN},
   {-1.32624737E-315,7.8784E-320,NAN},
   {-1.32624737E-315,8.755E-321,NAN},
   {-1.32624737E-315,9.73E-322,NAN},
   {-1.32624737E-315,1.1E-322,NAN},
   {-1.32624737E-315,1.0E-323,NAN},
   {-1.6578092E-316,2.472304287230225E-309,NAN},
   {-1.6578092E-316,2.74700476358915E-310,NAN},
   {-1.6578092E-316,3.052227515099E-311,NAN},
   {-1.6578092E-316,3.391363905667E-312,NAN},
   {-1.6578092E-316,3.76818211743E-313,NAN},
   {-1.6578092E-316,4.1868690193E-314,NAN},
   {-1.6578092E-316,4.652076687E-315,NAN},
   {-1.6578092E-316,5.16897407E-316,NAN},
   {-1.6578092E-316,5.7433046E-317,NAN},
   {-1.6578092E-316,6.38145E-318,NAN},
   {-1.6578092E-316,7.0905E-319,NAN},
   {-1.6578092E-316,7.8784E-320,NAN},
   {-1.6578092E-316,8.755E-321,NAN},
   {-1.6578092E-316,9.73E-322,NAN},
   {-1.6578092E-316,1.1E-322,NAN},
   {-1.6578092E-316,1.0E-323,NAN},
   {-2.0722615E-317,2.472304287230225E-309,NAN},
   {-2.0722615E-317,2.74700476358915E-310,NAN},
   {-2.0722615E-317,3.052227515099E-311,NAN},
   {-2.0722615E-317,3.391363905667E-312,NAN},
   {-2.0722615E-317,3.76818211743E-313,NAN},
   {-2.0722615E-317,4.1868690193E-314,NAN},
   {-2.0722615E-317,4.652076687E-315,NAN},
   {-2.0722615E-317,5.16897407E-316,NAN},
   {-2.0722615E-317,5.7433046E-317,NAN},
   {-2.0722615E-317,6.38145E-318,NAN},
   {-2.0722615E-317,7.0905E-319,NAN},
   {-2.0722615E-317,7.8784E-320,NAN},
   {-2.0722615E-317,8.755E-321,NAN},
   {-2.0722615E-317,9.73E-322,NAN},
   {-2.0722615E-317,1.1E-322,NAN},
   {-2.0722615E-317,1.0E-323,NAN},
   {-2.590327E-318,2.472304287230225E-309,NAN},
   {-2.590327E-318,2.74700476358915E-310,NAN},
   {-2.590327E-318,3.052227515099E-311,NAN},
   {-2.590327E-318,3.391363905667E-312,NAN},
   {-2.590327E-318,3.76818211743E-313,NAN},
   {-2.590327E-318,4.1868690193E-314,NAN},
   {-2.590327E-318,4.652076687E-315,NAN},
   {-2.590327E-318,5.16897407E-316,NAN},
   {-2.590327E-318,5.7433046E-317,NAN},
   {-2.590327E-318,6.38145E-318,NAN},
   {-2.590327E-318,7.0905E-319,NAN},
   {-2.590327E-318,7.8784E-320,NAN},
   {-2.590327E-318,8.755E-321,NAN},
   {-2.590327E-318,9.73E-322,NAN},
   {-2.590327E-318,1.1E-322,NAN},
   {-2.590327E-318,1.0E-323,NAN},
   {-3.2379E-319,2.472304287230225E-309,NAN},
   {-3.2379E-319,2.74700476358915E-310,NAN},
   {-3.2379E-319,3.052227515099E-311,NAN},
   {-3.2379E-319,3.391363905667E-312,NAN},
   {-3.2379E-319,3.76818211743E-313,NAN},
   {-3.2379E-319,4.1868690193E-314,NAN},
   {-3.2379E-319,4.652076687E-315,NAN},
   {-3.2379E-319,5.16897407E-316,NAN},
   {-3.2379E-319,5.7433046E-317,NAN},
   {-3.2379E-319,6.38145E-318,NAN},
   {-3.2379E-319,7.0905E-319,NAN},
   {-3.2379E-319,7.8784E-320,NAN},
   {-3.2379E-319,8.755E-321,NAN},
   {-3.2379E-319,9.73E-322,NAN},
   {-3.2379E-319,1.1E-322,NAN},
   {-3.2379E-319,1.0E-323,NAN},
   {-4.0474E-320,2.472304287230225E-309,NAN},
   {-4.0474E-320,2.74700476358915E-310,NAN},
   {-4.0474E-320,3.052227515099E-311,NAN},
   {-4.0474E-320,3.391363905667E-312,NAN},
   {-4.0474E-320,3.76818211743E-313,NAN},
   {-4.0474E-320,4.1868690193E-314,NAN},
   {-4.0474E-320,4.652076687E-315,NAN},
   {-4.0474E-320,5.16897407E-316,NAN},
   {-4.0474E-320,5.7433046E-317,NAN},
   {-4.0474E-320,6.38145E-318,NAN},
   {-4.0474E-320,7.0905E-319,NAN},
   {-4.0474E-320,7.8784E-320,NAN},
   {-4.0474E-320,8.755E-321,NAN},
   {-4.0474E-320,9.73E-322,NAN},
   {-4.0474E-320,1.1E-322,NAN},
   {-4.0474E-320,1.0E-323,NAN},
   {-5.06E-321,2.472304287230225E-309,NAN},
   {-5.06E-321,2.74700476358915E-310,NAN},
   {-5.06E-321,3.052227515099E-311,NAN},
   {-5.06E-321,3.391363905667E-312,NAN},
   {-5.06E-321,3.76818211743E-313,NAN},
   {-5.06E-321,4.1868690193E-314,NAN},
   {-5.06E-321,4.652076687E-315,NAN},
   {-5.06E-321,5.16897407E-316,NAN},
   {-5.06E-321,5.7433046E-317,NAN},
   {-5.06E-321,6.38145E-318,NAN},
   {-5.06E-321,7.0905E-319,NAN},
   {-5.06E-321,7.8784E-320,NAN},
   {-5.06E-321,8.755E-321,NAN},
   {-5.06E-321,9.73E-322,NAN},
   {-5.06E-321,1.1E-322,NAN},
   {-5.06E-321,1.0E-323,NAN},
   {-6.32E-322,2.472304287230225E-309,NAN},
   {-6.32E-322,2.74700476358915E-310,NAN},
   {-6.32E-322,3.052227515099E-311,NAN},
   {-6.32E-322,3.391363905667E-312,NAN},
   {-6.32E-322,3.76818211743E-313,NAN},
   {-6.32E-322,4.1868690193E-314,NAN},
   {-6.32E-322,4.652076687E-315,NAN},
   {-6.32E-322,5.16897407E-316,NAN},
   {-6.32E-322,5.7433046E-317,NAN},
   {-6.32E-322,6.38145E-318,NAN},
   {-6.32E-322,7.0905E-319,NAN},
   {-6.32E-322,7.8784E-320,NAN},
   {-6.32E-322,8.755E-321,NAN},
   {-6.32E-322,9.73E-322,NAN},
   {-6.32E-322,1.1E-322,NAN},
   {-6.32E-322,1.0E-323,NAN},
   {-7.9E-323,2.472304287230225E-309,NAN},
   {-7.9E-323,2.74700476358915E-310,NAN},
   {-7.9E-323,3.052227515099E-311,NAN},
   {-7.9E-323,3.391363905667E-312,NAN},
   {-7.9E-323,3.76818211743E-313,NAN},
   {-7.9E-323,4.1868690193E-314,NAN},
   {-7.9E-323,4.652076687E-315,NAN},
   {-7.9E-323,5.16897407E-316,NAN},
   {-7.9E-323,5.7433046E-317,NAN},
   {-7.9E-323,6.38145E-318,NAN},
   {-7.9E-323,7.0905E-319,NAN},
   {-7.9E-323,7.8784E-320,NAN},
   {-7.9E-323,8.755E-321,NAN},
   {-7.9E-323,9.73E-322,NAN},
   {-7.9E-323,1.1E-322,NAN},
   {-7.9E-323,1.0E-323,NAN},
   {-1.0E-323,2.472304287230225E-309,NAN},
   {-1.0E-323,2.74700476358915E-310,NAN},
   {-1.0E-323,3.052227515099E-311,NAN},
   {-1.0E-323,3.391363905667E-312,NAN},
   {-1.0E-323,3.76818211743E-313,NAN},
   {-1.0E-323,4.1868690193E-314,NAN},
   {-1.0E-323,4.652076687E-315,NAN},
   {-1.0E-323,5.16897407E-316,NAN},
   {-1.0E-323,5.7433046E-317,NAN},
   {-1.0E-323,6.38145E-318,NAN},
   {-1.0E-323,7.0905E-319,NAN},
   {-1.0E-323,7.8784E-320,NAN},
   {-1.0E-323,8.755E-321,NAN},
   {-1.0E-323,9.73E-322,NAN},
   {-1.0E-323,1.1E-322,NAN},
   {-1.0E-323,1.0E-323,NAN},
};
/** function pow_uv__uv_extreme_pow_denom_neg_pos executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_pow_denom_neg_pos() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<272;ind++) {
		x=pow_uv__uv_extreme_pow_denom_neg_pos_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_pow_denom_neg_pos_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_pow_denom_neg_pos_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_pow_denom_neg_pos: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_pow_denom_neg_pos_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==272) {
		PRINTF("pow_uv__uv_extreme_pow_denom_neg_pos: successfully tested: 272 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_pow_denom_neg_pos: %d tests failed for pow (out of 272)\n",errors);
	}
#endif
	return errors;
}

/**
 * 289 specified tests for pow
 * (-2.781342323134002E-309,-2.781342323134002E-309); (-2.781342323134002E-309,-3.4766779039175E-310) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_pow_denom_neg_neg in the following table */
pow_uv__uv_extreme_pow_denom_neg_neg_io_table_type pow_uv__uv_extreme_pow_denom_neg_neg_io_table [289] = {
   {-2.781342323134002E-309,-2.781342323134002E-309,NAN},
   {-2.781342323134002E-309,-3.4766779039175E-310,NAN},
   {-2.781342323134002E-309,-4.345847379897E-311,NAN},
   {-2.781342323134002E-309,-5.43230922487E-312,NAN},
   {-2.781342323134002E-309,-6.7903865311E-313,NAN},
   {-2.781342323134002E-309,-8.4879831639E-314,NAN},
   {-2.781342323134002E-309,-1.0609978955E-314,NAN},
   {-2.781342323134002E-309,-1.32624737E-315,NAN},
   {-2.781342323134002E-309,-1.6578092E-316,NAN},
   {-2.781342323134002E-309,-2.0722615E-317,NAN},
   {-2.781342323134002E-309,-2.590327E-318,NAN},
   {-2.781342323134002E-309,-3.2379E-319,NAN},
   {-2.781342323134002E-309,-4.0474E-320,NAN},
   {-2.781342323134002E-309,-5.06E-321,NAN},
   {-2.781342323134002E-309,-6.32E-322,NAN},
   {-2.781342323134002E-309,-7.9E-323,NAN},
   {-2.781342323134002E-309,-1.0E-323,NAN},
   {-3.4766779039175E-310,-2.781342323134002E-309,NAN},
   {-3.4766779039175E-310,-3.4766779039175E-310,NAN},
   {-3.4766779039175E-310,-4.345847379897E-311,NAN},
   {-3.4766779039175E-310,-5.43230922487E-312,NAN},
   {-3.4766779039175E-310,-6.7903865311E-313,NAN},
   {-3.4766779039175E-310,-8.4879831639E-314,NAN},
   {-3.4766779039175E-310,-1.0609978955E-314,NAN},
   {-3.4766779039175E-310,-1.32624737E-315,NAN},
   {-3.4766779039175E-310,-1.6578092E-316,NAN},
   {-3.4766779039175E-310,-2.0722615E-317,NAN},
   {-3.4766779039175E-310,-2.590327E-318,NAN},
   {-3.4766779039175E-310,-3.2379E-319,NAN},
   {-3.4766779039175E-310,-4.0474E-320,NAN},
   {-3.4766779039175E-310,-5.06E-321,NAN},
   {-3.4766779039175E-310,-6.32E-322,NAN},
   {-3.4766779039175E-310,-7.9E-323,NAN},
   {-3.4766779039175E-310,-1.0E-323,NAN},
   {-4.345847379897E-311,-2.781342323134002E-309,NAN},
   {-4.345847379897E-311,-3.4766779039175E-310,NAN},
   {-4.345847379897E-311,-4.345847379897E-311,NAN},
   {-4.345847379897E-311,-5.43230922487E-312,NAN},
   {-4.345847379897E-311,-6.7903865311E-313,NAN},
   {-4.345847379897E-311,-8.4879831639E-314,NAN},
   {-4.345847379897E-311,-1.0609978955E-314,NAN},
   {-4.345847379897E-311,-1.32624737E-315,NAN},
   {-4.345847379897E-311,-1.6578092E-316,NAN},
   {-4.345847379897E-311,-2.0722615E-317,NAN},
   {-4.345847379897E-311,-2.590327E-318,NAN},
   {-4.345847379897E-311,-3.2379E-319,NAN},
   {-4.345847379897E-311,-4.0474E-320,NAN},
   {-4.345847379897E-311,-5.06E-321,NAN},
   {-4.345847379897E-311,-6.32E-322,NAN},
   {-4.345847379897E-311,-7.9E-323,NAN},
   {-4.345847379897E-311,-1.0E-323,NAN},
   {-5.43230922487E-312,-2.781342323134002E-309,NAN},
   {-5.43230922487E-312,-3.4766779039175E-310,NAN},
   {-5.43230922487E-312,-4.345847379897E-311,NAN},
   {-5.43230922487E-312,-5.43230922487E-312,NAN},
   {-5.43230922487E-312,-6.7903865311E-313,NAN},
   {-5.43230922487E-312,-8.4879831639E-314,NAN},
   {-5.43230922487E-312,-1.0609978955E-314,NAN},
   {-5.43230922487E-312,-1.32624737E-315,NAN},
   {-5.43230922487E-312,-1.6578092E-316,NAN},
   {-5.43230922487E-312,-2.0722615E-317,NAN},
   {-5.43230922487E-312,-2.590327E-318,NAN},
   {-5.43230922487E-312,-3.2379E-319,NAN},
   {-5.43230922487E-312,-4.0474E-320,NAN},
   {-5.43230922487E-312,-5.06E-321,NAN},
   {-5.43230922487E-312,-6.32E-322,NAN},
   {-5.43230922487E-312,-7.9E-323,NAN},
   {-5.43230922487E-312,-1.0E-323,NAN},
   {-6.7903865311E-313,-2.781342323134002E-309,NAN},
   {-6.7903865311E-313,-3.4766779039175E-310,NAN},
   {-6.7903865311E-313,-4.345847379897E-311,NAN},
   {-6.7903865311E-313,-5.43230922487E-312,NAN},
   {-6.7903865311E-313,-6.7903865311E-313,NAN},
   {-6.7903865311E-313,-8.4879831639E-314,NAN},
   {-6.7903865311E-313,-1.0609978955E-314,NAN},
   {-6.7903865311E-313,-1.32624737E-315,NAN},
   {-6.7903865311E-313,-1.6578092E-316,NAN},
   {-6.7903865311E-313,-2.0722615E-317,NAN},
   {-6.7903865311E-313,-2.590327E-318,NAN},
   {-6.7903865311E-313,-3.2379E-319,NAN},
   {-6.7903865311E-313,-4.0474E-320,NAN},
   {-6.7903865311E-313,-5.06E-321,NAN},
   {-6.7903865311E-313,-6.32E-322,NAN},
   {-6.7903865311E-313,-7.9E-323,NAN},
   {-6.7903865311E-313,-1.0E-323,NAN},
   {-8.4879831639E-314,-2.781342323134002E-309,NAN},
   {-8.4879831639E-314,-3.4766779039175E-310,NAN},
   {-8.4879831639E-314,-4.345847379897E-311,NAN},
   {-8.4879831639E-314,-5.43230922487E-312,NAN},
   {-8.4879831639E-314,-6.7903865311E-313,NAN},
   {-8.4879831639E-314,-8.4879831639E-314,NAN},
   {-8.4879831639E-314,-1.0609978955E-314,NAN},
   {-8.4879831639E-314,-1.32624737E-315,NAN},
   {-8.4879831639E-314,-1.6578092E-316,NAN},
   {-8.4879831639E-314,-2.0722615E-317,NAN},
   {-8.4879831639E-314,-2.590327E-318,NAN},
   {-8.4879831639E-314,-3.2379E-319,NAN},
   {-8.4879831639E-314,-4.0474E-320,NAN},
   {-8.4879831639E-314,-5.06E-321,NAN},
   {-8.4879831639E-314,-6.32E-322,NAN},
   {-8.4879831639E-314,-7.9E-323,NAN},
   {-8.4879831639E-314,-1.0E-323,NAN},
   {-1.0609978955E-314,-2.781342323134002E-309,NAN},
   {-1.0609978955E-314,-3.4766779039175E-310,NAN},
   {-1.0609978955E-314,-4.345847379897E-311,NAN},
   {-1.0609978955E-314,-5.43230922487E-312,NAN},
   {-1.0609978955E-314,-6.7903865311E-313,NAN},
   {-1.0609978955E-314,-8.4879831639E-314,NAN},
   {-1.0609978955E-314,-1.0609978955E-314,NAN},
   {-1.0609978955E-314,-1.32624737E-315,NAN},
   {-1.0609978955E-314,-1.6578092E-316,NAN},
   {-1.0609978955E-314,-2.0722615E-317,NAN},
   {-1.0609978955E-314,-2.590327E-318,NAN},
   {-1.0609978955E-314,-3.2379E-319,NAN},
   {-1.0609978955E-314,-4.0474E-320,NAN},
   {-1.0609978955E-314,-5.06E-321,NAN},
   {-1.0609978955E-314,-6.32E-322,NAN},
   {-1.0609978955E-314,-7.9E-323,NAN},
   {-1.0609978955E-314,-1.0E-323,NAN},
   {-1.32624737E-315,-2.781342323134002E-309,NAN},
   {-1.32624737E-315,-3.4766779039175E-310,NAN},
   {-1.32624737E-315,-4.345847379897E-311,NAN},
   {-1.32624737E-315,-5.43230922487E-312,NAN},
   {-1.32624737E-315,-6.7903865311E-313,NAN},
   {-1.32624737E-315,-8.4879831639E-314,NAN},
   {-1.32624737E-315,-1.0609978955E-314,NAN},
   {-1.32624737E-315,-1.32624737E-315,NAN},
   {-1.32624737E-315,-1.6578092E-316,NAN},
   {-1.32624737E-315,-2.0722615E-317,NAN},
   {-1.32624737E-315,-2.590327E-318,NAN},
   {-1.32624737E-315,-3.2379E-319,NAN},
   {-1.32624737E-315,-4.0474E-320,NAN},
   {-1.32624737E-315,-5.06E-321,NAN},
   {-1.32624737E-315,-6.32E-322,NAN},
   {-1.32624737E-315,-7.9E-323,NAN},
   {-1.32624737E-315,-1.0E-323,NAN},
   {-1.6578092E-316,-2.781342323134002E-309,NAN},
   {-1.6578092E-316,-3.4766779039175E-310,NAN},
   {-1.6578092E-316,-4.345847379897E-311,NAN},
   {-1.6578092E-316,-5.43230922487E-312,NAN},
   {-1.6578092E-316,-6.7903865311E-313,NAN},
   {-1.6578092E-316,-8.4879831639E-314,NAN},
   {-1.6578092E-316,-1.0609978955E-314,NAN},
   {-1.6578092E-316,-1.32624737E-315,NAN},
   {-1.6578092E-316,-1.6578092E-316,NAN},
   {-1.6578092E-316,-2.0722615E-317,NAN},
   {-1.6578092E-316,-2.590327E-318,NAN},
   {-1.6578092E-316,-3.2379E-319,NAN},
   {-1.6578092E-316,-4.0474E-320,NAN},
   {-1.6578092E-316,-5.06E-321,NAN},
   {-1.6578092E-316,-6.32E-322,NAN},
   {-1.6578092E-316,-7.9E-323,NAN},
   {-1.6578092E-316,-1.0E-323,NAN},
   {-2.0722615E-317,-2.781342323134002E-309,NAN},
   {-2.0722615E-317,-3.4766779039175E-310,NAN},
   {-2.0722615E-317,-4.345847379897E-311,NAN},
   {-2.0722615E-317,-5.43230922487E-312,NAN},
   {-2.0722615E-317,-6.7903865311E-313,NAN},
   {-2.0722615E-317,-8.4879831639E-314,NAN},
   {-2.0722615E-317,-1.0609978955E-314,NAN},
   {-2.0722615E-317,-1.32624737E-315,NAN},
   {-2.0722615E-317,-1.6578092E-316,NAN},
   {-2.0722615E-317,-2.0722615E-317,NAN},
   {-2.0722615E-317,-2.590327E-318,NAN},
   {-2.0722615E-317,-3.2379E-319,NAN},
   {-2.0722615E-317,-4.0474E-320,NAN},
   {-2.0722615E-317,-5.06E-321,NAN},
   {-2.0722615E-317,-6.32E-322,NAN},
   {-2.0722615E-317,-7.9E-323,NAN},
   {-2.0722615E-317,-1.0E-323,NAN},
   {-2.590327E-318,-2.781342323134002E-309,NAN},
   {-2.590327E-318,-3.4766779039175E-310,NAN},
   {-2.590327E-318,-4.345847379897E-311,NAN},
   {-2.590327E-318,-5.43230922487E-312,NAN},
   {-2.590327E-318,-6.7903865311E-313,NAN},
   {-2.590327E-318,-8.4879831639E-314,NAN},
   {-2.590327E-318,-1.0609978955E-314,NAN},
   {-2.590327E-318,-1.32624737E-315,NAN},
   {-2.590327E-318,-1.6578092E-316,NAN},
   {-2.590327E-318,-2.0722615E-317,NAN},
   {-2.590327E-318,-2.590327E-318,NAN},
   {-2.590327E-318,-3.2379E-319,NAN},
   {-2.590327E-318,-4.0474E-320,NAN},
   {-2.590327E-318,-5.06E-321,NAN},
   {-2.590327E-318,-6.32E-322,NAN},
   {-2.590327E-318,-7.9E-323,NAN},
   {-2.590327E-318,-1.0E-323,NAN},
   {-3.2379E-319,-2.781342323134002E-309,NAN},
   {-3.2379E-319,-3.4766779039175E-310,NAN},
   {-3.2379E-319,-4.345847379897E-311,NAN},
   {-3.2379E-319,-5.43230922487E-312,NAN},
   {-3.2379E-319,-6.7903865311E-313,NAN},
   {-3.2379E-319,-8.4879831639E-314,NAN},
   {-3.2379E-319,-1.0609978955E-314,NAN},
   {-3.2379E-319,-1.32624737E-315,NAN},
   {-3.2379E-319,-1.6578092E-316,NAN},
   {-3.2379E-319,-2.0722615E-317,NAN},
   {-3.2379E-319,-2.590327E-318,NAN},
   {-3.2379E-319,-3.2379E-319,NAN},
   {-3.2379E-319,-4.0474E-320,NAN},
   {-3.2379E-319,-5.06E-321,NAN},
   {-3.2379E-319,-6.32E-322,NAN},
   {-3.2379E-319,-7.9E-323,NAN},
   {-3.2379E-319,-1.0E-323,NAN},
   {-4.0474E-320,-2.781342323134002E-309,NAN},
   {-4.0474E-320,-3.4766779039175E-310,NAN},
   {-4.0474E-320,-4.345847379897E-311,NAN},
   {-4.0474E-320,-5.43230922487E-312,NAN},
   {-4.0474E-320,-6.7903865311E-313,NAN},
   {-4.0474E-320,-8.4879831639E-314,NAN},
   {-4.0474E-320,-1.0609978955E-314,NAN},
   {-4.0474E-320,-1.32624737E-315,NAN},
   {-4.0474E-320,-1.6578092E-316,NAN},
   {-4.0474E-320,-2.0722615E-317,NAN},
   {-4.0474E-320,-2.590327E-318,NAN},
   {-4.0474E-320,-3.2379E-319,NAN},
   {-4.0474E-320,-4.0474E-320,NAN},
   {-4.0474E-320,-5.06E-321,NAN},
   {-4.0474E-320,-6.32E-322,NAN},
   {-4.0474E-320,-7.9E-323,NAN},
   {-4.0474E-320,-1.0E-323,NAN},
   {-5.06E-321,-2.781342323134002E-309,NAN},
   {-5.06E-321,-3.4766779039175E-310,NAN},
   {-5.06E-321,-4.345847379897E-311,NAN},
   {-5.06E-321,-5.43230922487E-312,NAN},
   {-5.06E-321,-6.7903865311E-313,NAN},
   {-5.06E-321,-8.4879831639E-314,NAN},
   {-5.06E-321,-1.0609978955E-314,NAN},
   {-5.06E-321,-1.32624737E-315,NAN},
   {-5.06E-321,-1.6578092E-316,NAN},
   {-5.06E-321,-2.0722615E-317,NAN},
   {-5.06E-321,-2.590327E-318,NAN},
   {-5.06E-321,-3.2379E-319,NAN},
   {-5.06E-321,-4.0474E-320,NAN},
   {-5.06E-321,-5.06E-321,NAN},
   {-5.06E-321,-6.32E-322,NAN},
   {-5.06E-321,-7.9E-323,NAN},
   {-5.06E-321,-1.0E-323,NAN},
   {-6.32E-322,-2.781342323134002E-309,NAN},
   {-6.32E-322,-3.4766779039175E-310,NAN},
   {-6.32E-322,-4.345847379897E-311,NAN},
   {-6.32E-322,-5.43230922487E-312,NAN},
   {-6.32E-322,-6.7903865311E-313,NAN},
   {-6.32E-322,-8.4879831639E-314,NAN},
   {-6.32E-322,-1.0609978955E-314,NAN},
   {-6.32E-322,-1.32624737E-315,NAN},
   {-6.32E-322,-1.6578092E-316,NAN},
   {-6.32E-322,-2.0722615E-317,NAN},
   {-6.32E-322,-2.590327E-318,NAN},
   {-6.32E-322,-3.2379E-319,NAN},
   {-6.32E-322,-4.0474E-320,NAN},
   {-6.32E-322,-5.06E-321,NAN},
   {-6.32E-322,-6.32E-322,NAN},
   {-6.32E-322,-7.9E-323,NAN},
   {-6.32E-322,-1.0E-323,NAN},
   {-7.9E-323,-2.781342323134002E-309,NAN},
   {-7.9E-323,-3.4766779039175E-310,NAN},
   {-7.9E-323,-4.345847379897E-311,NAN},
   {-7.9E-323,-5.43230922487E-312,NAN},
   {-7.9E-323,-6.7903865311E-313,NAN},
   {-7.9E-323,-8.4879831639E-314,NAN},
   {-7.9E-323,-1.0609978955E-314,NAN},
   {-7.9E-323,-1.32624737E-315,NAN},
   {-7.9E-323,-1.6578092E-316,NAN},
   {-7.9E-323,-2.0722615E-317,NAN},
   {-7.9E-323,-2.590327E-318,NAN},
   {-7.9E-323,-3.2379E-319,NAN},
   {-7.9E-323,-4.0474E-320,NAN},
   {-7.9E-323,-5.06E-321,NAN},
   {-7.9E-323,-6.32E-322,NAN},
   {-7.9E-323,-7.9E-323,NAN},
   {-7.9E-323,-1.0E-323,NAN},
   {-1.0E-323,-2.781342323134002E-309,NAN},
   {-1.0E-323,-3.4766779039175E-310,NAN},
   {-1.0E-323,-4.345847379897E-311,NAN},
   {-1.0E-323,-5.43230922487E-312,NAN},
   {-1.0E-323,-6.7903865311E-313,NAN},
   {-1.0E-323,-8.4879831639E-314,NAN},
   {-1.0E-323,-1.0609978955E-314,NAN},
   {-1.0E-323,-1.32624737E-315,NAN},
   {-1.0E-323,-1.6578092E-316,NAN},
   {-1.0E-323,-2.0722615E-317,NAN},
   {-1.0E-323,-2.590327E-318,NAN},
   {-1.0E-323,-3.2379E-319,NAN},
   {-1.0E-323,-4.0474E-320,NAN},
   {-1.0E-323,-5.06E-321,NAN},
   {-1.0E-323,-6.32E-322,NAN},
   {-1.0E-323,-7.9E-323,NAN},
   {-1.0E-323,-1.0E-323,NAN},
};
/** function pow_uv__uv_extreme_pow_denom_neg_neg executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_pow_denom_neg_neg() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<289;ind++) {
		x=pow_uv__uv_extreme_pow_denom_neg_neg_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_pow_denom_neg_neg_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_pow_denom_neg_neg_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_pow_denom_neg_neg: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_pow_denom_neg_neg_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==289) {
		PRINTF("pow_uv__uv_extreme_pow_denom_neg_neg: successfully tested: 289 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_pow_denom_neg_neg: %d tests failed for pow (out of 289)\n",errors);
	}
#endif
	return errors;
}

/**
 * 1 specified tests for pow
 * (0,0)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_zero in the following table */
pow_uv__uv_extreme_zero_io_table_type pow_uv__uv_extreme_zero_io_table [1] = {
   {0,0,1.0},
};
/** function pow_uv__uv_extreme_zero executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_zero() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<1;ind++) {
		x=pow_uv__uv_extreme_zero_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_zero_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_zero_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_zero: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_zero_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==1) {
		PRINTF("pow_uv__uv_extreme_zero: successfully tested: 1 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_zero: %d tests failed for pow (out of 1)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (0,1) to (2,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_random_small_squares in the following table */
pow_uv__uv_extreme_random_small_squares_io_table_type pow_uv__uv_extreme_random_small_squares_io_table [50] = {
   {1.8572252532679059,1.983556299296385,3.4143499520799465},
   {0.5107784670768416,1.789432843587289,0.3005396640918111},
   {1.441304196539992,1.6643355238245088,1.8374802076571968},
   {0.8596828638930154,1.4262932323141395,0.8060224139763592},
   {0.970138883333864,1.3087789440700306,0.9610998304532545},
   {0.923843247469875,1.3451573601354962,0.8989267102216055},
   {0.8108923544584932,1.2330658290349676,0.772228169258568},
   {0.8854225201907369,1.6962234136062921,0.8134962380690934},
   {0.2692264545165255,1.7933559618027441,0.09506009348664315},
   {0.9719566258743277,1.9233679393006067,0.9467611186238037},
   {1.093414239507126,1.8029337192108683,1.1746981619485244},
   {0.20935412895759486,1.32330024592688,0.12627644952211212},
   {1.1417689828202127,1.5688904988752008,1.2312151618172913},
   {1.3140727201583082,1.058498707761136,1.335237377546305},
   {0.7085681225035243,1.373327136003979,0.6230530195955183},
   {0.8768780477890368,1.2033054407967356,0.8537651560001214},
   {1.5837285465833133,1.6781594105879707,2.163197245192796},
   {0.3519175403601942,1.8722822022278005,0.14151725462356707},
   {0.5640676792259798,1.067961928735975,0.542539303348189},
   {0.788670371677193,1.7309063729013658,0.6630340140331175},
   {0.83906281923474,1.961886156154238,0.7087505995888005},
   {0.7306623977474398,1.2209137116611797,0.681726099177309},
   {0.4415997792358093,1.4235409868403184,0.3123805717246002},
   {0.2536637026178672,1.6460696156051244,0.10456051988522727},
   {0.7842791119693622,1.9768251312792535,0.6185672544477072},
   {0.727414331007344,1.8588782605076108,0.5534384027477854},
   {0.19442769544265603,1.1960524731003952,0.1410319142229948},
   {1.5587508890829662,1.664715371617312,2.0937188324083635},
   {1.316039876512848,1.9803129705678597,1.7226222176409602},
   {1.9103368352567958,1.2170963211942203,2.1985577905776577},
   {1.264452239651414,1.8448634144942102,1.5416864840434124},
   {1.188025337695055,1.616268762253882,1.3211083779064359},
   {1.4996326984805382,1.2664435478196052,1.6706094850633293},
   {0.9512129055087124,1.6430447352794864,0.921105435391994},
   {0.5265436944079154,1.1047651495998008,0.4923233694493917},
   {0.8243699571705305,1.0607643000916052,0.8147518922217516},
   {0.7361170742274907,1.3890227652463258,0.6534106413220846},
   {1.9742625328927736,1.200508953851986,2.2627482195985253},
   {0.9679563460274934,1.3061265093920602,0.9583537534717347},
   {1.7662068616531466,1.6081599629437857,2.496217124952959},
   {0.7059601821304575,1.0564592561746844,0.6922173005897725},
   {0.32402820008777256,1.376622920111523,0.21196154503595244},
   {0.21063895105955743,1.3533112080333907,0.12148910102356039},
   {1.8922212197077255,1.4125169033817384,2.4616567898062165},
   {1.3469747309604752,1.733747561655125,1.6760100882747548},
   {1.1351399331178984,1.293852637670064,1.1782184359137238},
   {0.38425280696184116,1.5215156259626783,0.23333954615882055},
   {0.8068707651481801,1.5637010701700902,0.7149394866552228},
   {0.6243708429670862,1.2366996447694105,0.5585009979701111},
   {0.5867726364064081,1.2745800082770053,0.5068696679294812},
};
/** function pow_uv__uv_extreme_random_small_squares executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_random_small_squares() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<50;ind++) {
		x=pow_uv__uv_extreme_random_small_squares_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_random_small_squares_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_random_small_squares_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_random_small_squares: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_random_small_squares_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_extreme_random_small_squares: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_random_small_squares: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (-1,0) to (2,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_random_small_neg_sqr in the following table */
pow_uv__uv_extreme_random_small_neg_sqr_io_table_type pow_uv__uv_extreme_random_small_neg_sqr_io_table [50] = {
   {1.0365659823939994,0.9643667002526104,1.0352403272284174},
   {0.9492414221958182,0.6985265202557018,0.9642663510736005},
   {-0.4738979325907472,1.707162650819883,NAN},
   {-0.6630871083364838,1.7118860693205167,NAN},
   {1.7555726872708706,0.4343049347823438,1.2768865070297304},
   {1.624746422821255,1.2183067849911604,1.8063483795399957},
   {0.22062017194090244,1.558619400977403,0.09484025302705976},
   {0.8496756157883273,1.205176767183616,0.8217459001458525},
   {1.0061638302342164,1.4633844139028949,1.0090329205666762},
   {-0.08085445063650665,1.3118514211702217,NAN},
   {1.204176741408431,1.7947356835527715,1.3957821605542022},
   {-0.938553765805405,0.6299862253487813,NAN},
   {-0.1942222182389418,1.4531694830090065,NAN},
   {-0.07735270956498375,0.9661816887063739,NAN},
   {1.1883349808153485,0.38918610479278404,1.0694615408211152},
   {0.7135466220435569,0.9620223817589957,0.7227515377734534},
   {-0.9678549117573657,1.278817858544526,NAN},
   {0.195786341885276,0.468542117391725,0.4657686164627714},
   {-0.6911294212955279,0.6879897844356906,NAN},
   {1.2757824065133887,0.13660336677714824,1.0338307388524508},
   {1.8852819542150823,1.4303635089787246,2.476784526495175},
   {0.5483569941951097,0.31163030019249316,0.8292476415411657},
   {0.2809724023011193,0.4815185808504807,0.5426518778754605},
   {1.2989535762073026,0.010588476436398153,1.0027733499364144},
   {1.2075766594282893,0.033028154141987454,1.0062490692687185},
   {-0.7659427575678248,1.0110577238740448,NAN},
   {1.6996447171276041,1.0531895980933559,1.7482792621965428},
   {1.5639363431663855,1.8966971178332115,2.3354724798107354},
   {1.9236879368318474,1.066873151483409,2.0097202069309312},
   {1.0100616976912762,0.2803548453427127,1.0028106915387665},
   {-0.8779686737878382,0.7751551329772028,NAN},
   {0.8515527550253115,0.3655280894094155,0.9429536898147839},
   {0.6119744828433696,1.6061053930073916,0.4544344272729778},
   {0.2607430441482238,1.5088323778446704,0.13157181557399705},
   {0.31837966590665223,0.32130194620060126,0.6923016469989632},
   {-0.09255234943467383,1.5105912384946356,NAN},
   {0.9691733072253387,1.3983483108995292,0.9571598714926689},
   {0.134759561857565,1.9876118016939963,0.01861668579647912},
   {1.828363979386894,1.0678727325101156,1.904800802027663},
   {0.9261921160599093,0.3716644400007376,0.9719053560982589},
   {0.6800473679850265,0.6041916305000403,0.7921757879565061},
   {0.3947286158850102,1.8291370189667242,0.18263114677901193},
   {1.709486029533074,0.7520219868667903,1.49664920064799},
   {1.628831314583389,0.7117886091919008,1.4151777824181109},
   {-0.04628112012910979,0.017331758396567976,NAN},
   {1.4287040565879885,1.0254801221295256,1.441750882804772},
   {0.11427191347053234,0.9586191564917599,0.12500366829825663},
   {1.7580962703610172,0.8432121278000169,1.6092478460803135},
   {1.73840970300488,0.58561577374236,1.3824100414117089},
   {0.6226765152137475,0.590753657975821,0.755892108191299},
};
/** function pow_uv__uv_extreme_random_small_neg_sqr executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_random_small_neg_sqr() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<50;ind++) {
		x=pow_uv__uv_extreme_random_small_neg_sqr_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_random_small_neg_sqr_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_random_small_neg_sqr_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_random_small_neg_sqr: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_random_small_neg_sqr_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_extreme_random_small_neg_sqr: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_random_small_neg_sqr: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (-10,-10) to (-1,0)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_random_negative in the following table */
pow_uv__uv_extreme_random_negative_io_table_type pow_uv__uv_extreme_random_negative_io_table [50] = {
   {-6.751046392888837,-2.471943397673132,NAN},
   {-2.3035072237315486,-1.5120054928684095,NAN},
   {-2.871083661720335,-2.233496564728834,NAN},
   {-6.870052693077316,-8.835291717583058,NAN},
   {-4.934290904120175,-3.508711620964281,NAN},
   {-8.156438049246544,-0.8209463642782051,NAN},
   {-6.369211225097461,-4.77579137821064,NAN},
   {-6.609480940515114,-2.4976889585119144,NAN},
   {-8.738336110795467,-1.3872546495564464,NAN},
   {-4.077578953560443,-7.037108979267939,NAN},
   {-5.787760260615707,-2.947396550063698,NAN},
   {-7.4603111378963405,-0.7561291331056932,NAN},
   {-5.6661451966623755,-4.259379502260431,NAN},
   {-8.119376791486506,-4.743144699682498,NAN},
   {-1.0358640175754363,-9.228858201895541,NAN},
   {-5.534970323381049,-2.138300671275565,NAN},
   {-9.626345294823647,-1.9450239676436851,NAN},
   {-2.0032171953298317,-4.776091382651155,NAN},
   {-7.325531831816799,-1.1814390981051375,NAN},
   {-9.700566778590673,-9.763713970911503,NAN},
   {-4.170020490321409,-3.768017408348456,NAN},
   {-5.190064709847329,-5.6213306771619305,NAN},
   {-5.577845963214136,-8.681987800986393,NAN},
   {-6.412195373242668,-8.850635787104833,NAN},
   {-1.870021211325703,-7.289667848472306,NAN},
   {-7.389517675124502,-5.225856915114093,NAN},
   {-7.547845188817821,-5.942352615294961,NAN},
   {-6.279578238483703,-5.716551389286808,NAN},
   {-4.932634992638606,-5.0886135768454865,NAN},
   {-3.0346225980052157,-4.754294889047449,NAN},
   {-1.632942733744958,-4.211781293201219,NAN},
   {-9.769388107947586,-8.545596602732777,NAN},
   {-9.881460879488738,-4.235377461786677,NAN},
   {-8.999791914520294,-3.3111033244593173,NAN},
   {-2.079720053422344,-8.560738492602574,NAN},
   {-6.195597072281125,-7.696394661071296,NAN},
   {-7.903333997597655,-0.6100610925844929,NAN},
   {-7.5891675370182385,-0.15240409562266422,NAN},
   {-8.053940495356136,-9.644585312479226,NAN},
   {-9.900191447696805,-0.823734547323884,NAN},
   {-2.1961399423391157,-7.711614484594672,NAN},
   {-6.572776837708211,-8.410871087089905,NAN},
   {-9.78862723740636,-8.845551184632992,NAN},
   {-9.276602276873245,-5.587531040027008,NAN},
   {-7.408366384690703,-7.107122561019658,NAN},
   {-9.797820042502034,-1.8571200163944894,NAN},
   {-5.775049329575003,-2.6914414769178885,NAN},
   {-8.038189279387609,-0.22250306745619497,NAN},
   {-3.010234832504808,-5.169542881661812,NAN},
   {-4.652438570240024,-3.7210323005709434,NAN},
};
/** function pow_uv__uv_extreme_random_negative executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_random_negative() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<50;ind++) {
		x=pow_uv__uv_extreme_random_negative_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_random_negative_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_random_negative_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_random_negative: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_random_negative_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_extreme_random_negative: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_random_negative: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 linear tests for pow
 * range: (1000,2) to (100000,10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_big_powers in the following table */
pow_uv__uv_extreme_big_powers_io_table_type pow_uv__uv_extreme_big_powers_io_table [49] = {
   {1000.0,2.0,1000000.0},
   {1000.0,3.3333333333333335,10000000000.00001},
   {1000.0,4.666666666666667,100000000000000.2},
   {1000.0,6.0,1.0E+18},
   {1000.0,7.333333333333335,1.0000000000000103E+22},
   {1000.0,8.666666666666668,1.0000000000000081E+26},
   {1000.0,10.0,1.0E+30},
   {17500.0,2.0,3.0625E+8},
   {17500.0,3.3333333333333335,139142615385541.5},
   {17500.0,4.666666666666667,6.321850584923668E+19},
   {17500.0,6.0,2.8722900390625E+25},
   {17500.0,7.333333333333335,1.3050055450808184E+31},
   {17500.0,8.666666666666668,5.929204396250725E+36},
   {17500.0,10.0,2.6938938999176027E+42},
   {34000.0,2.0,1.156E+9},
   {34000.0,3.3333333333333335,1273297022374104.0},
   {34000.0,4.666666666666667,1.4024959404729754E+21},
   {34000.0,6.0,1.544804416E+27},
   {34000.0,7.333333333333335,1.7015526496913442E+33},
   {34000.0,8.666666666666668,1.8742058151079115E+39},
   {34000.0,10.0,2.064377754059776E+45},
   {50500.0,2.0,2.55025E+9},
   {50500.0,3.3333333333333335,4760339500340278.0},
   {50500.0,4.666666666666667,8.885729696500315E+21},
   {50500.0,6.0,1.6586252353140625E+28},
   {50500.0,7.333333333333335,3.096017733134726E+34},
   {50500.0,8.666666666666668,5.7790787212215E+40},
   {50500.0,10.0,1.0787325443468793E+47},
   {67000.0,2.0,4.489E+9},
   {67000.0,3.3333333333333335,1.221563391334346E+16},
   {67000.0,4.666666666666667,3.3241637760041625E+22},
   {67000.0,6.0,9.0458382169E+28},
   {67000.0,7.333333333333335,2.4615871707949123E+35},
   {67000.0,8.666666666666668,6.6985626474077435E+41},
   {67000.0,10.0,1.8228378045517614E+48},
   {83500.00000000001,2.0,6972250000.000003},
   {83500.00000000001,3.3333333333333335,25446120822101012.0},
   {83500.00000000001,4.666666666666667,9.286888233969843E+22},
   {83500.00000000001,6.0,3.38936899943266E+29},
   {83500.00000000001,7.333333333333335,1.2369936974469984E+36},
   {83500.00000000001,8.666666666666668,4.5145671887000056E+42},
   {83500.00000000001,10.0,1.6476492114188597E+49},
   {100000.0,2.0,1.0E+10},
   {100000.0,3.3333333333333335,46415888336127872.0},
   {100000.0,4.666666666666667,2.154434690031891E+23},
   {100000.0,6.0,1.0E+30},
   {100000.0,7.333333333333335,4.641588833612858E+36},
   {100000.0,8.666666666666668,2.1544346900319132E+43},
   {100000.0,10.0,1.00E+50},
};
/** function pow_uv__uv_extreme_big_powers executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_big_powers() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<49;ind++) {
		x=pow_uv__uv_extreme_big_powers_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_big_powers_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_big_powers_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_big_powers: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_big_powers_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("pow_uv__uv_extreme_big_powers: successfully tested: 49 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_big_powers: %d tests failed for pow (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 linear tests for pow
 * range: (1000,1) to (100000,1000)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_overflowing_powers in the following table */
pow_uv__uv_extreme_overflowing_powers_io_table_type pow_uv__uv_extreme_overflowing_powers_io_table [49] = {
   {1000.0,1.0,1000.0},
   {1000.0,167.5,INFINITY},
   {1000.0,334.0,INFINITY},
   {1000.0,500.5,INFINITY},
   {1000.0,667.0,INFINITY},
   {1000.0,833.4999999999999,INFINITY},
   {1000.0,1000.0,INFINITY},
   {17500.0,1.0,17500.0},
   {17500.0,167.5,INFINITY},
   {17500.0,334.0,INFINITY},
   {17500.0,500.5,INFINITY},
   {17500.0,667.0,INFINITY},
   {17500.0,833.4999999999999,INFINITY},
   {17500.0,1000.0,INFINITY},
   {34000.0,1.0,34000.0},
   {34000.0,167.5,INFINITY},
   {34000.0,334.0,INFINITY},
   {34000.0,500.5,INFINITY},
   {34000.0,667.0,INFINITY},
   {34000.0,833.4999999999999,INFINITY},
   {34000.0,1000.0,INFINITY},
   {50500.0,1.0,50500.0},
   {50500.0,167.5,INFINITY},
   {50500.0,334.0,INFINITY},
   {50500.0,500.5,INFINITY},
   {50500.0,667.0,INFINITY},
   {50500.0,833.4999999999999,INFINITY},
   {50500.0,1000.0,INFINITY},
   {67000.0,1.0,67000.0},
   {67000.0,167.5,INFINITY},
   {67000.0,334.0,INFINITY},
   {67000.0,500.5,INFINITY},
   {67000.0,667.0,INFINITY},
   {67000.0,833.4999999999999,INFINITY},
   {67000.0,1000.0,INFINITY},
   {83500.00000000001,1.0,83500.00000000001},
   {83500.00000000001,167.5,INFINITY},
   {83500.00000000001,334.0,INFINITY},
   {83500.00000000001,500.5,INFINITY},
   {83500.00000000001,667.0,INFINITY},
   {83500.00000000001,833.4999999999999,INFINITY},
   {83500.00000000001,1000.0,INFINITY},
   {100000.0,1.0,100000.0},
   {100000.0,167.5,INFINITY},
   {100000.0,334.0,INFINITY},
   {100000.0,500.5,INFINITY},
   {100000.0,667.0,INFINITY},
   {100000.0,833.4999999999999,INFINITY},
   {100000.0,1000.0,INFINITY},
};
/** function pow_uv__uv_extreme_overflowing_powers executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_overflowing_powers() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<49;ind++) {
		x=pow_uv__uv_extreme_overflowing_powers_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_overflowing_powers_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_overflowing_powers_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_overflowing_powers: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_overflowing_powers_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("pow_uv__uv_extreme_overflowing_powers: successfully tested: 49 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_overflowing_powers: %d tests failed for pow (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for pow
 * range: (1000,1024) to (100000,10000)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_overflowing_powers_pos in the following table */
pow_uv__uv_extreme_overflowing_powers_pos_io_table_type pow_uv__uv_extreme_overflowing_powers_pos_io_table [64] = {
   {1000.0,1024.0,INFINITY},
   {1000.0,2306.285714285714,INFINITY},
   {1000.0,3588.571428571429,INFINITY},
   {1000.0,4870.857142857143,INFINITY},
   {1000.0,6153.142857142858,INFINITY},
   {1000.0,7435.428571428572,INFINITY},
   {1000.0,8717.714285714286,INFINITY},
   {1000.0,10000.0,INFINITY},
   {15142.857142857143,1024.0,INFINITY},
   {15142.857142857143,2306.285714285714,INFINITY},
   {15142.857142857143,3588.571428571429,INFINITY},
   {15142.857142857143,4870.857142857143,INFINITY},
   {15142.857142857143,6153.142857142858,INFINITY},
   {15142.857142857143,7435.428571428572,INFINITY},
   {15142.857142857143,8717.714285714286,INFINITY},
   {15142.857142857143,10000.0,INFINITY},
   {29285.714285714286,1024.0,INFINITY},
   {29285.714285714286,2306.285714285714,INFINITY},
   {29285.714285714286,3588.571428571429,INFINITY},
   {29285.714285714286,4870.857142857143,INFINITY},
   {29285.714285714286,6153.142857142858,INFINITY},
   {29285.714285714286,7435.428571428572,INFINITY},
   {29285.714285714286,8717.714285714286,INFINITY},
   {29285.714285714286,10000.0,INFINITY},
   {43428.57142857143,1024.0,INFINITY},
   {43428.57142857143,2306.285714285714,INFINITY},
   {43428.57142857143,3588.571428571429,INFINITY},
   {43428.57142857143,4870.857142857143,INFINITY},
   {43428.57142857143,6153.142857142858,INFINITY},
   {43428.57142857143,7435.428571428572,INFINITY},
   {43428.57142857143,8717.714285714286,INFINITY},
   {43428.57142857143,10000.0,INFINITY},
   {57571.42857142857,1024.0,INFINITY},
   {57571.42857142857,2306.285714285714,INFINITY},
   {57571.42857142857,3588.571428571429,INFINITY},
   {57571.42857142857,4870.857142857143,INFINITY},
   {57571.42857142857,6153.142857142858,INFINITY},
   {57571.42857142857,7435.428571428572,INFINITY},
   {57571.42857142857,8717.714285714286,INFINITY},
   {57571.42857142857,10000.0,INFINITY},
   {71714.28571428572,1024.0,INFINITY},
   {71714.28571428572,2306.285714285714,INFINITY},
   {71714.28571428572,3588.571428571429,INFINITY},
   {71714.28571428572,4870.857142857143,INFINITY},
   {71714.28571428572,6153.142857142858,INFINITY},
   {71714.28571428572,7435.428571428572,INFINITY},
   {71714.28571428572,8717.714285714286,INFINITY},
   {71714.28571428572,10000.0,INFINITY},
   {85857.14285714286,1024.0,INFINITY},
   {85857.14285714286,2306.285714285714,INFINITY},
   {85857.14285714286,3588.571428571429,INFINITY},
   {85857.14285714286,4870.857142857143,INFINITY},
   {85857.14285714286,6153.142857142858,INFINITY},
   {85857.14285714286,7435.428571428572,INFINITY},
   {85857.14285714286,8717.714285714286,INFINITY},
   {85857.14285714286,10000.0,INFINITY},
   {100000.0,1024.0,INFINITY},
   {100000.0,2306.285714285714,INFINITY},
   {100000.0,3588.571428571429,INFINITY},
   {100000.0,4870.857142857143,INFINITY},
   {100000.0,6153.142857142858,INFINITY},
   {100000.0,7435.428571428572,INFINITY},
   {100000.0,8717.714285714286,INFINITY},
   {100000.0,10000.0,INFINITY},
};
/** function pow_uv__uv_extreme_overflowing_powers_pos executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_overflowing_powers_pos() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<64;ind++) {
		x=pow_uv__uv_extreme_overflowing_powers_pos_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_overflowing_powers_pos_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_overflowing_powers_pos_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_overflowing_powers_pos: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_overflowing_powers_pos_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("pow_uv__uv_extreme_overflowing_powers_pos: successfully tested: 64 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_overflowing_powers_pos: %d tests failed for pow (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for pow
 * range: (-100000,1024) to (1000,10000)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_overflowing_powers_neg in the following table */
pow_uv__uv_extreme_overflowing_powers_neg_io_table_type pow_uv__uv_extreme_overflowing_powers_neg_io_table [64] = {
   {-100000.0,1024.0,INFINITY},
   {-100000.0,2306.285714285714,NAN},
   {-100000.0,3588.571428571429,NAN},
   {-100000.0,4870.857142857143,NAN},
   {-100000.0,6153.142857142858,NAN},
   {-100000.0,7435.428571428572,NAN},
   {-100000.0,8717.714285714286,NAN},
   {-100000.0,10000.0,INFINITY},
   {-85571.42857142857,1024.0,INFINITY},
   {-85571.42857142857,2306.285714285714,NAN},
   {-85571.42857142857,3588.571428571429,NAN},
   {-85571.42857142857,4870.857142857143,NAN},
   {-85571.42857142857,6153.142857142858,NAN},
   {-85571.42857142857,7435.428571428572,NAN},
   {-85571.42857142857,8717.714285714286,NAN},
   {-85571.42857142857,10000.0,INFINITY},
   {-71142.85714285713,1024.0,INFINITY},
   {-71142.85714285713,2306.285714285714,NAN},
   {-71142.85714285713,3588.571428571429,NAN},
   {-71142.85714285713,4870.857142857143,NAN},
   {-71142.85714285713,6153.142857142858,NAN},
   {-71142.85714285713,7435.428571428572,NAN},
   {-71142.85714285713,8717.714285714286,NAN},
   {-71142.85714285713,10000.0,INFINITY},
   {-56714.28571428571,1024.0,INFINITY},
   {-56714.28571428571,2306.285714285714,NAN},
   {-56714.28571428571,3588.571428571429,NAN},
   {-56714.28571428571,4870.857142857143,NAN},
   {-56714.28571428571,6153.142857142858,NAN},
   {-56714.28571428571,7435.428571428572,NAN},
   {-56714.28571428571,8717.714285714286,NAN},
   {-56714.28571428571,10000.0,INFINITY},
   {-42285.71428571429,1024.0,INFINITY},
   {-42285.71428571429,2306.285714285714,NAN},
   {-42285.71428571429,3588.571428571429,NAN},
   {-42285.71428571429,4870.857142857143,NAN},
   {-42285.71428571429,6153.142857142858,NAN},
   {-42285.71428571429,7435.428571428572,NAN},
   {-42285.71428571429,8717.714285714286,NAN},
   {-42285.71428571429,10000.0,INFINITY},
   {-27857.142857142855,1024.0,INFINITY},
   {-27857.142857142855,2306.285714285714,NAN},
   {-27857.142857142855,3588.571428571429,NAN},
   {-27857.142857142855,4870.857142857143,NAN},
   {-27857.142857142855,6153.142857142858,NAN},
   {-27857.142857142855,7435.428571428572,NAN},
   {-27857.142857142855,8717.714285714286,NAN},
   {-27857.142857142855,10000.0,INFINITY},
   {-13428.571428571435,1024.0,INFINITY},
   {-13428.571428571435,2306.285714285714,NAN},
   {-13428.571428571435,3588.571428571429,NAN},
   {-13428.571428571435,4870.857142857143,NAN},
   {-13428.571428571435,6153.142857142858,NAN},
   {-13428.571428571435,7435.428571428572,NAN},
   {-13428.571428571435,8717.714285714286,NAN},
   {-13428.571428571435,10000.0,INFINITY},
   {1000.0,1024.0,INFINITY},
   {1000.0,2306.285714285714,INFINITY},
   {1000.0,3588.571428571429,INFINITY},
   {1000.0,4870.857142857143,INFINITY},
   {1000.0,6153.142857142858,INFINITY},
   {1000.0,7435.428571428572,INFINITY},
   {1000.0,8717.714285714286,INFINITY},
   {1000.0,10000.0,INFINITY},
};
/** function pow_uv__uv_extreme_overflowing_powers_neg executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_overflowing_powers_neg() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<64;ind++) {
		x=pow_uv__uv_extreme_overflowing_powers_neg_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_overflowing_powers_neg_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_overflowing_powers_neg_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_overflowing_powers_neg: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_overflowing_powers_neg_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("pow_uv__uv_extreme_overflowing_powers_neg: successfully tested: 64 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_overflowing_powers_neg: %d tests failed for pow (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for pow
 * range: (0.00001,2) to (0.001,256)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_undeflowing_powers in the following table */
pow_uv__uv_extreme_undeflowing_powers_io_table_type pow_uv__uv_extreme_undeflowing_powers_io_table [64] = {
   {0.000010,2.0,1.0000000000000002E-10},
   {0.000010,38.285714285714285,3.727593720314995E-192},
   {0.000010,74.57142857142857,0.0},
   {0.000010,110.85714285714285,0.0},
   {0.000010,147.14285714285714,0.0},
   {0.000010,183.42857142857142,0.0},
   {0.000010,219.7142857142857,0.0},
   {0.000010,256.0,0.0},
   {0.00015142857142857143,2.0,2.2930612244897958E-8},
   {0.00015142857142857143,38.285714285714285,5.708580987163609E-147},
   {0.00015142857142857143,74.57142857142857,1.4211524986323304E-285},
   {0.00015142857142857143,110.85714285714285,0.0},
   {0.00015142857142857143,147.14285714285714,0.0},
   {0.00015142857142857143,183.42857142857142,0.0},
   {0.00015142857142857143,219.7142857142857,0.0},
   {0.00015142857142857143,256.0,0.0},
   {0.0002928571428571429,2.0,8.576530612244899E-8},
   {0.0002928571428571429,38.285714285714285,5.289252394074338E-136},
   {0.0002928571428571429,74.57142857142857,3.261947301660522E-264},
   {0.0002928571428571429,110.85714285714285,0.0},
   {0.0002928571428571429,147.14285714285714,0.0},
   {0.0002928571428571429,183.42857142857142,0.0},
   {0.0002928571428571429,219.7142857142857,0.0},
   {0.0002928571428571429,256.0,0.0},
   {0.00043428571428571436,2.0,1.8860408163265313E-7},
   {0.00043428571428571436,38.285714285714285,1.8829340003712882E-129},
   {0.00043428571428571436,74.57142857142857,1.8798323021766453E-251},
   {0.00043428571428571436,110.85714285714285,0.0},
   {0.00043428571428571436,147.14285714285714,0.0},
   {0.00043428571428571436,183.42857142857142,0.0},
   {0.00043428571428571436,219.7142857142857,0.0},
   {0.00043428571428571436,256.0,0.0},
   {0.0005757142857142857,2.0,3.3144693877551026E-7},
   {0.0005757142857142857,38.285714285714285,9.166659334154696E-125},
   {0.0005757142857142857,74.57142857142857,2.535176328943485E-242},
   {0.0005757142857142857,110.85714285714285,0.0},
   {0.0005757142857142857,147.14285714285714,0.0},
   {0.0005757142857142857,183.42857142857142,0.0},
   {0.0005757142857142857,219.7142857142857,0.0},
   {0.0005757142857142857,256.0,0.0},
   {0.0007171428571428573,2.0,5.142938775510206E-7},
   {0.0007171428571428573,38.285714285714285,4.117345892510152E-121},
   {0.0007171428571428573,74.57142857142857,3.296274355684594E-235},
   {0.0007171428571428573,110.85714285714285,0.0},
   {0.0007171428571428573,147.14285714285714,0.0},
   {0.0007171428571428573,183.42857142857142,0.0},
   {0.0007171428571428573,219.7142857142857,0.0},
   {0.0007171428571428573,256.0,0.0},
   {0.0008585714285714287,2.0,7.371448979591839E-7},
   {0.0008585714285714287,38.285714285714285,4.049864930033057E-118},
   {0.0008585714285714287,74.57142857142857,2.2249907714100208E-229},
   {0.0008585714285714287,110.85714285714285,0.0},
   {0.0008585714285714287,147.14285714285714,0.0},
   {0.0008585714285714287,183.42857142857142,0.0},
   {0.0008585714285714287,219.7142857142857,0.0},
   {0.0008585714285714287,256.0,0.0},
   {0.001,2.0,0.0000010},
   {0.001,38.285714285714285,1.3894954943731485E-115},
   {0.001,74.57142857142857,1.9306977288832803E-224},
   {0.001,110.85714285714285,0.0},
   {0.001,147.14285714285714,0.0},
   {0.001,183.42857142857142,0.0},
   {0.001,219.7142857142857,0.0},
   {0.001,256.0,0.0},
};
/** function pow_uv__uv_extreme_undeflowing_powers executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_undeflowing_powers() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<64;ind++) {
		x=pow_uv__uv_extreme_undeflowing_powers_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_undeflowing_powers_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_undeflowing_powers_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_undeflowing_powers: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_undeflowing_powers_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("pow_uv__uv_extreme_undeflowing_powers: successfully tested: 64 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_undeflowing_powers: %d tests failed for pow (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for pow
 * range: (0.000000001,2) to (0.00001,1028)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_undeflowing_powers2 in the following table */
pow_uv__uv_extreme_undeflowing_powers2_io_table_type pow_uv__uv_extreme_undeflowing_powers2_io_table [64] = {
   {1.0E-9,2.0,1.0E-18},
   {1.0E-9,148.57142857142858,0.0},
   {1.0E-9,295.14285714285717,0.0},
   {1.0E-9,441.7142857142857,0.0},
   {1.0E-9,588.2857142857143,0.0},
   {1.0E-9,734.8571428571429,0.0},
   {1.0E-9,881.4285714285714,0.0},
   {1.0E-9,1028.0,0.0},
   {0.0000014294285714285716,2.0,2.0432660408163268E-12},
   {0.0000014294285714285716,148.57142857142858,0.0},
   {0.0000014294285714285716,295.14285714285717,0.0},
   {0.0000014294285714285716,441.7142857142857,0.0},
   {0.0000014294285714285716,588.2857142857143,0.0},
   {0.0000014294285714285716,734.8571428571429,0.0},
   {0.0000014294285714285716,881.4285714285714,0.0},
   {0.0000014294285714285716,1028.0,0.0},
   {0.000002857857142857143,2.0,8.167347448979593E-12},
   {0.000002857857142857143,148.57142857142858,0.0},
   {0.000002857857142857143,295.14285714285717,0.0},
   {0.000002857857142857143,441.7142857142857,0.0},
   {0.000002857857142857143,588.2857142857143,0.0},
   {0.000002857857142857143,734.8571428571429,0.0},
   {0.000002857857142857143,881.4285714285714,0.0},
   {0.000002857857142857143,1028.0,0.0},
   {0.000004286285714285715,2.0,1.83722452244898E-11},
   {0.000004286285714285715,148.57142857142858,0.0},
   {0.000004286285714285715,295.14285714285717,0.0},
   {0.000004286285714285715,441.7142857142857,0.0},
   {0.000004286285714285715,588.2857142857143,0.0},
   {0.000004286285714285715,734.8571428571429,0.0},
   {0.000004286285714285715,881.4285714285714,0.0},
   {0.000004286285714285715,1028.0,0.0},
   {0.000005714714285714286,2.0,3.265795936734694E-11},
   {0.000005714714285714286,148.57142857142858,0.0},
   {0.000005714714285714286,295.14285714285717,0.0},
   {0.000005714714285714286,441.7142857142857,0.0},
   {0.000005714714285714286,588.2857142857143,0.0},
   {0.000005714714285714286,734.8571428571429,0.0},
   {0.000005714714285714286,881.4285714285714,0.0},
   {0.000005714714285714286,1028.0,0.0},
   {0.0000071431428571428565,2.0,5.102448987755101E-11},
   {0.0000071431428571428565,148.57142857142858,0.0},
   {0.0000071431428571428565,295.14285714285717,0.0},
   {0.0000071431428571428565,441.7142857142857,0.0},
   {0.0000071431428571428565,588.2857142857143,0.0},
   {0.0000071431428571428565,734.8571428571429,0.0},
   {0.0000071431428571428565,881.4285714285714,0.0},
   {0.0000071431428571428565,1028.0,0.0},
   {0.00000857157142857143,2.0,7.347183675510208E-11},
   {0.00000857157142857143,148.57142857142858,0.0},
   {0.00000857157142857143,295.14285714285717,0.0},
   {0.00000857157142857143,441.7142857142857,0.0},
   {0.00000857157142857143,588.2857142857143,0.0},
   {0.00000857157142857143,734.8571428571429,0.0},
   {0.00000857157142857143,881.4285714285714,0.0},
   {0.00000857157142857143,1028.0,0.0},
   {0.000010,2.0,1.0000000000000002E-10},
   {0.000010,148.57142857142858,0.0},
   {0.000010,295.14285714285717,0.0},
   {0.000010,441.7142857142857,0.0},
   {0.000010,588.2857142857143,0.0},
   {0.000010,734.8571428571429,0.0},
   {0.000010,881.4285714285714,0.0},
   {0.000010,1028.0,0.0},
};
/** function pow_uv__uv_extreme_undeflowing_powers2 executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_undeflowing_powers2() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<64;ind++) {
		x=pow_uv__uv_extreme_undeflowing_powers2_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_undeflowing_powers2_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_undeflowing_powers2_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_undeflowing_powers2: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_undeflowing_powers2_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("pow_uv__uv_extreme_undeflowing_powers2: successfully tested: 64 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_undeflowing_powers2: %d tests failed for pow (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for pow
 * range: (-0.0001,1) to (-0.000000001,99)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_undeflowing_powers3 in the following table */
pow_uv__uv_extreme_undeflowing_powers3_io_table_type pow_uv__uv_extreme_undeflowing_powers3_io_table [64] = {
   {-0.00010,1.0,-0.00010},
   {-0.00010,15.0,-1.0000000000000007E-60},
   {-0.00010,29.0,-1.0000000000000014E-116},
   {-0.00010,43.0,-1.000000000000002E-172},
   {-0.00010,57.0,-1.0000000000000028E-228},
   {-0.00010,71.0,-1.0000000000000033E-284},
   {-0.00010,85.0,0.0},
   {-0.00010,99.0,0.0},
   {-0.00008571442857142858,1.0,-0.00008571442857142858},
   {-0.00008571442857142858,15.0,-9.90396308405909E-62},
   {-0.00008571442857142858,29.0,-1.1443637483818139E-118},
   {-0.00008571442857142858,43.0,-1.322267033404324E-175},
   {-0.00008571442857142858,57.0,-1.5278272403334872E-232},
   {-0.00008571442857142858,71.0,-1.7653439262531082E-289},
   {-0.00008571442857142858,85.0,0.0},
   {-0.00008571442857142858,99.0,0.0},
   {-0.00007142885714285715,1.0,-0.00007142885714285715},
   {-0.00007142885714285715,15.0,-6.428438929551513E-63},
   {-0.00007142885714285715,29.0,-5.785452648125684E-121},
   {-0.00007142885714285715,43.0,-5.206779236843378E-179},
   {-0.00007142885714285715,57.0,-4.6859859841746923E-237},
   {-0.00007142885714285715,71.0,-4.21728359222582E-295},
   {-0.00007142885714285715,85.0,0.0},
   {-0.00007142885714285715,99.0,0.0},
   {-0.00005714328571428572,1.0,-0.00005714328571428572},
   {-0.00005714328571428572,15.0,-2.26192461971933E-64},
   {-0.00005714328571428572,29.0,-8.953463073288712E-124},
   {-0.00005714328571428572,43.0,-3.5440836668859338E-183},
   {-0.00005714328571428572,57.0,-1.4028682460711838E-242},
   {-0.00005714328571428572,71.0,-5.553027244314717E-302},
   {-0.00005714328571428572,85.0,0.0},
   {-0.00005714328571428572,99.0,0.0},
   {-0.00004285771428571428,1.0,-0.00004285771428571428},
   {-0.00004285771428571428,15.0,-3.022978642657761E-66},
   {-0.00004285771428571428,29.0,-2.132264873726842E-127},
   {-0.00004285771428571428,43.0,-1.5039978872401415E-188},
   {-0.00004285771428571428,57.0,-1.0608483367590235E-249},
   {-0.00004285771428571428,71.0,-7.4827179157115E-311},
   {-0.00004285771428571428,85.0,0.0},
   {-0.00004285771428571428,99.0,0.0},
   {-0.00002857214285714286,1.0,-0.00002857214285714286},
   {-0.00002857214285714286,15.0,-6.904658335016831E-69},
   {-0.00002857214285714286,29.0,-1.6685590213405755E-132},
   {-0.00002857214285714286,43.0,-4.032189679216375E-196},
   {-0.00002857214285714286,57.0,-9.744068625223934E-260},
   {-0.00002857214285714286,71.0,-2.5E-323},
   {-0.00002857214285714286,85.0,0.0},
   {-0.00002857214285714286,99.0,0.0},
   {-0.000014286571428571418,1.0,-0.000014286571428571418},
   {-0.000014286571428571418,15.0,-2.1082409906687087E-73},
   {-0.000014286571428571418,29.0,-3.1110893869518306E-141},
   {-0.000014286571428571418,43.0,-4.590972861472678E-209},
   {-0.000014286571428571418,57.0,-6.774807533071042E-277},
   {-0.000014286571428571418,71.0,0.0},
   {-0.000014286571428571418,85.0,0.0},
   {-0.000014286571428571418,99.0,0.0},
   {-1.0000000000021927E-9,1.0,-1.0000000000021927E-9},
   {-1.0000000000021927E-9,15.0,-1.0000000000328907E-135},
   {-1.0000000000021927E-9,29.0,-1.0000000000635886E-261},
   {-1.0000000000021927E-9,43.0,0.0},
   {-1.0000000000021927E-9,57.0,0.0},
   {-1.0000000000021927E-9,71.0,0.0},
   {-1.0000000000021927E-9,85.0,0.0},
   {-1.0000000000021927E-9,99.0,0.0},
};
/** function pow_uv__uv_extreme_undeflowing_powers3 executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_undeflowing_powers3() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<64;ind++) {
		x=pow_uv__uv_extreme_undeflowing_powers3_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_undeflowing_powers3_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_undeflowing_powers3_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_undeflowing_powers3: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_undeflowing_powers3_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("pow_uv__uv_extreme_undeflowing_powers3: successfully tested: 64 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_undeflowing_powers3: %d tests failed for pow (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for pow
 * range: (0.000000001,1000) to (0.00001,100000)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_undeflowing_powers4 in the following table */
pow_uv__uv_extreme_undeflowing_powers4_io_table_type pow_uv__uv_extreme_undeflowing_powers4_io_table [64] = {
   {1.0E-9,1000.0,0.0},
   {1.0E-9,15142.857142857143,0.0},
   {1.0E-9,29285.714285714286,0.0},
   {1.0E-9,43428.57142857143,0.0},
   {1.0E-9,57571.42857142857,0.0},
   {1.0E-9,71714.28571428572,0.0},
   {1.0E-9,85857.14285714286,0.0},
   {1.0E-9,100000.0,0.0},
   {0.0000014294285714285716,1000.0,0.0},
   {0.0000014294285714285716,15142.857142857143,0.0},
   {0.0000014294285714285716,29285.714285714286,0.0},
   {0.0000014294285714285716,43428.57142857143,0.0},
   {0.0000014294285714285716,57571.42857142857,0.0},
   {0.0000014294285714285716,71714.28571428572,0.0},
   {0.0000014294285714285716,85857.14285714286,0.0},
   {0.0000014294285714285716,100000.0,0.0},
   {0.000002857857142857143,1000.0,0.0},
   {0.000002857857142857143,15142.857142857143,0.0},
   {0.000002857857142857143,29285.714285714286,0.0},
   {0.000002857857142857143,43428.57142857143,0.0},
   {0.000002857857142857143,57571.42857142857,0.0},
   {0.000002857857142857143,71714.28571428572,0.0},
   {0.000002857857142857143,85857.14285714286,0.0},
   {0.000002857857142857143,100000.0,0.0},
   {0.000004286285714285715,1000.0,0.0},
   {0.000004286285714285715,15142.857142857143,0.0},
   {0.000004286285714285715,29285.714285714286,0.0},
   {0.000004286285714285715,43428.57142857143,0.0},
   {0.000004286285714285715,57571.42857142857,0.0},
   {0.000004286285714285715,71714.28571428572,0.0},
   {0.000004286285714285715,85857.14285714286,0.0},
   {0.000004286285714285715,100000.0,0.0},
   {0.000005714714285714286,1000.0,0.0},
   {0.000005714714285714286,15142.857142857143,0.0},
   {0.000005714714285714286,29285.714285714286,0.0},
   {0.000005714714285714286,43428.57142857143,0.0},
   {0.000005714714285714286,57571.42857142857,0.0},
   {0.000005714714285714286,71714.28571428572,0.0},
   {0.000005714714285714286,85857.14285714286,0.0},
   {0.000005714714285714286,100000.0,0.0},
   {0.0000071431428571428565,1000.0,0.0},
   {0.0000071431428571428565,15142.857142857143,0.0},
   {0.0000071431428571428565,29285.714285714286,0.0},
   {0.0000071431428571428565,43428.57142857143,0.0},
   {0.0000071431428571428565,57571.42857142857,0.0},
   {0.0000071431428571428565,71714.28571428572,0.0},
   {0.0000071431428571428565,85857.14285714286,0.0},
   {0.0000071431428571428565,100000.0,0.0},
   {0.00000857157142857143,1000.0,0.0},
   {0.00000857157142857143,15142.857142857143,0.0},
   {0.00000857157142857143,29285.714285714286,0.0},
   {0.00000857157142857143,43428.57142857143,0.0},
   {0.00000857157142857143,57571.42857142857,0.0},
   {0.00000857157142857143,71714.28571428572,0.0},
   {0.00000857157142857143,85857.14285714286,0.0},
   {0.00000857157142857143,100000.0,0.0},
   {0.000010,1000.0,0.0},
   {0.000010,15142.857142857143,0.0},
   {0.000010,29285.714285714286,0.0},
   {0.000010,43428.57142857143,0.0},
   {0.000010,57571.42857142857,0.0},
   {0.000010,71714.28571428572,0.0},
   {0.000010,85857.14285714286,0.0},
   {0.000010,100000.0,0.0},
};
/** function pow_uv__uv_extreme_undeflowing_powers4 executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_undeflowing_powers4() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<64;ind++) {
		x=pow_uv__uv_extreme_undeflowing_powers4_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_undeflowing_powers4_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_undeflowing_powers4_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_undeflowing_powers4: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_undeflowing_powers4_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("pow_uv__uv_extreme_undeflowing_powers4: successfully tested: 64 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_undeflowing_powers4: %d tests failed for pow (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 54 approximation tests with factors 2.0,2.0
 * range: (0,-2) to (1,-2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_roots_of_minus_two in the following table */
pow_uv__uv_extreme_roots_of_minus_two_io_table_type pow_uv__uv_extreme_roots_of_minus_two_io_table [54] = {
   {0.5,-2.0,4.0},
   {0.75,-2.0,1.7777777777777777},
   {0.875,-2.0,1.3061224489795917},
   {0.9375,-2.0,1.1377777777777778},
   {0.96875,-2.0,1.065556711758585},
   {0.984375,-2.0,1.0319979843789369},
   {0.9921875,-2.0,1.0158100316200633},
   {0.99609375,-2.0,1.0078585159554019},
   {0.998046875,-2.0,1.0039177239670498},
   {0.9990234375,-2.0,1.0019559897527923},
   {0.99951171875,-2.0,1.000977278221683},
   {0.999755859375,-2.0,1.0004884601221598},
   {0.9998779296875,-2.0,1.0002441853357606},
   {0.99993896484375,-2.0,1.0001220814892804},
   {0.999969482421875,-2.0,1.0000610379503314},
   {0.9999847412109375,-2.0,1.0000305182766311},
   {0.9999923706054688,-2.0,1.0000152589636873},
   {0.9999961853027344,-2.0,1.0000076294381872},
   {0.9999980926513672,-2.0,1.0000038147081796},
   {0.9999990463256836,-2.0,1.0000019073513613},
   {0.9999995231628418,-2.0,1.0000009536749985},
   {0.9999997615814209,-2.0,1.0000004768373287},
   {0.9999998807907104,-2.0,1.0000002384186217},
   {0.9999999403953552,-2.0,1.0000001192093002},
   {0.9999999701976776,-2.0,1.0000000596046474},
   {0.9999999850988388,-2.0,1.000000029802323},
   {0.9999999925494194,-2.0,1.0000000149011614},
   {0.9999999962747097,-2.0,1.0000000074505806},
   {0.9999999981373549,-2.0,1.0000000037252903},
   {0.9999999990686774,-2.0,1.0000000018626451},
   {0.9999999995343387,-2.0,1.0000000009313226},
   {0.9999999997671694,-2.0,1.0000000004656613},
   {0.9999999998835847,-2.0,1.0000000002328306},
   {0.9999999999417923,-2.0,1.0000000001164153},
   {0.9999999999708962,-2.0,1.0000000000582077},
   {0.9999999999854481,-2.0,1.0000000000291038},
   {0.999999999992724,-2.0,1.000000000014552},
   {0.999999999996362,-2.0,1.000000000007276},
   {0.999999999998181,-2.0,1.000000000003638},
   {0.9999999999990905,-2.0,1.000000000001819},
   {0.9999999999995453,-2.0,1.0000000000009095},
   {0.9999999999997726,-2.0,1.0000000000004547},
   {0.9999999999998863,-2.0,1.0000000000002274},
   {0.9999999999999432,-2.0,1.0000000000001137},
   {0.9999999999999716,-2.0,1.0000000000000568},
   {0.9999999999999858,-2.0,1.0000000000000284},
   {0.9999999999999929,-2.0,1.0000000000000142},
   {0.9999999999999964,-2.0,1.000000000000007},
   {0.9999999999999982,-2.0,1.0000000000000036},
   {0.9999999999999991,-2.0,1.0000000000000018},
   {0.9999999999999996,-2.0,1.0000000000000009},
   {0.9999999999999998,-2.0,1.0000000000000004},
   {0.9999999999999999,-2.0,1.0000000000000002},
   {1.0,-2.0,1.0},
};
/** function pow_uv__uv_extreme_roots_of_minus_two executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_roots_of_minus_two() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<54;ind++) {
		x=pow_uv__uv_extreme_roots_of_minus_two_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_roots_of_minus_two_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_roots_of_minus_two_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_roots_of_minus_two: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_roots_of_minus_two_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==54) {
		PRINTF("pow_uv__uv_extreme_roots_of_minus_two: successfully tested: 54 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_roots_of_minus_two: %d tests failed for pow (out of 54)\n",errors);
	}
#endif
	return errors;
}

/**
 * 54 approximation tests with factors 2.0,2.0
 * range: (0,0) to (1,-10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_roots_to_minus_ten in the following table */
pow_uv__uv_extreme_roots_to_minus_ten_io_table_type pow_uv__uv_extreme_roots_to_minus_ten_io_table [54] = {
   {0.5,-5.0,32.0},
   {0.75,-7.5,8.650486337816945},
   {0.875,-8.75,3.2168405604482855},
   {0.9375,-9.375,1.8313412752201228},
   {0.96875,-9.6875,1.3601099130786314},
   {0.984375,-9.84375,1.167684687823281},
   {0.9921875,-9.921875,1.080927021682377},
   {0.99609375,-9.9609375,1.039756037719188},
   {0.998046875,-9.98046875,1.0197037705474332},
   {0.9990234375,-9.990234375,1.0098086475934056},
   {0.99951171875,-9.9951171875,1.0048935547330125},
   {0.999755859375,-9.99755859375,1.0024440901330596},
   {0.9998779296875,-9.998779296875,1.0012213738864735},
   {0.99993896484375,-9.9993896484375,1.0006105192267136},
   {0.999969482421875,-9.99969482421875,1.0003052176940346},
   {0.9999847412109375,-9.999847412109375,1.0001525983684125},
   {0.9999923706054688,-9.999923706054688,1.0000762965647083},
   {0.9999961853027344,-9.999961853027344,1.0000381476274989},
   {0.9999980926513672,-9.999980926513672,1.000019073650038},
   {0.9999990463256836,-9.999990463256836,1.0000095367840913},
   {0.9999995231628418,-9.999995231628418,1.0000047683818138},
   {0.9999997615814209,-9.999997615814209,1.000002384188349},
   {0.9999998807907104,-9.999998807907104,1.000001192093535},
   {0.9999999403953552,-9.999999403953552,1.0000005960466076},
   {0.9999999701976776,-9.999999701976776,1.0000002980232638},
   {0.9999999850988388,-9.999999850988388,1.000000149011622},
   {0.9999999925494194,-9.999999925494194,1.0000000745058084},
   {0.9999999962747097,-9.999999962747097,1.0000000372529037},
   {0.9999999981373549,-9.999999981373549,1.0000000186264517},
   {0.9999999990686774,-9.999999990686774,1.0000000093132257},
   {0.9999999995343387,-9.999999995343387,1.0000000046566129},
   {0.9999999997671694,-9.999999997671694,1.0000000023283064},
   {0.9999999998835847,-9.999999998835847,1.0000000011641532},
   {0.9999999999417923,-9.999999999417923,1.0000000005820766},
   {0.9999999999708962,-9.999999999708962,1.0000000002910383},
   {0.9999999999854481,-9.99999999985448,1.0000000001455192},
   {0.999999999992724,-9.99999999992724,1.0000000000727596},
   {0.999999999996362,-9.99999999996362,1.0000000000363798},
   {0.999999999998181,-9.99999999998181,1.00000000001819},
   {0.9999999999990905,-9.999999999990905,1.000000000009095},
   {0.9999999999995453,-9.999999999995453,1.0000000000045475},
   {0.9999999999997726,-9.999999999997726,1.0000000000022737},
   {0.9999999999998863,-9.999999999998863,1.0000000000011369},
   {0.9999999999999432,-9.999999999999432,1.0000000000005684},
   {0.9999999999999716,-9.999999999999716,1.0000000000002842},
   {0.9999999999999858,-9.999999999999858,1.000000000000142},
   {0.9999999999999929,-9.999999999999929,1.000000000000071},
   {0.9999999999999964,-9.999999999999964,1.0000000000000355},
   {0.9999999999999982,-9.999999999999982,1.0000000000000178},
   {0.9999999999999991,-9.999999999999991,1.0000000000000089},
   {0.9999999999999996,-9.999999999999996,1.0000000000000044},
   {0.9999999999999998,-9.999999999999998,1.0000000000000022},
   {0.9999999999999999,-10.0,1.000000000000001},
   {1.0,-10.0,1.0},
};
/** function pow_uv__uv_extreme_roots_to_minus_ten executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_roots_to_minus_ten() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<54;ind++) {
		x=pow_uv__uv_extreme_roots_to_minus_ten_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_roots_to_minus_ten_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_roots_to_minus_ten_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_roots_to_minus_ten: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_roots_to_minus_ten_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==54) {
		PRINTF("pow_uv__uv_extreme_roots_to_minus_ten: successfully tested: 54 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_roots_to_minus_ten: %d tests failed for pow (out of 54)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 (1 omitted out of input range) specified tests for pow
 * (2,21); (2,22); (2,23); (2,24); (2,25); (2,24); (2,27); (2,28); (2,29); (2,30); (2,31) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_big_powers2 in the following table */
pow_uv__uv_extreme_big_powers2_io_table_type pow_uv__uv_extreme_big_powers2_io_table [20] = {
   {2.0,21.0,2097152.0},
   {2.0,22.0,4194304.0},
   {2.0,23.0,8388608.0},
   {2.0,24.0,16777216.0},
   {2.0,25.0,33554432.0},
   {2.0,27.0,134217728.0},
   {2.0,28.0,268435456.0},
   {2.0,29.0,536870912.0},
   {2.0,30.0,1073741824.0},
   {2.0,31.0,2147483648.0},
   {2.0,32.0,4294967296.0},
   {2.0,40.0,1099511627776.0},
   {2.0,50.0,1125899906842624.0},
   {2.0,60.0,1.15292150460684698E+18},
   {2.0,70.0,1.1805916207174113E+21},
   {2.0,80.0,1.2089258196146292E+24},
   {2.0,90.0,1.2379400392853803E+27},
   {2.0,100.0,1.2676506002282294E+30},
   {2.0,200.0,1.6069380442589903E+60},
   {2.0,300.0,2.037035976334486E+90},
};
/** function pow_uv__uv_extreme_big_powers2 executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_big_powers2() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<20;ind++) {
		x=pow_uv__uv_extreme_big_powers2_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_big_powers2_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_big_powers2_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_big_powers2: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_big_powers2_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("pow_uv__uv_extreme_big_powers2: successfully tested: 20 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_big_powers2: %d tests failed for pow (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 18 specified tests for pow
 * (-1.0,Infinity); (-1.0,-Infinity); (-0.5,Infinity); (-0.5,-Infinity); (0.5,Infinity) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_extreme_coverage_dw in the following table */
pow_uv__uv_extreme_coverage_dw_io_table_type pow_uv__uv_extreme_coverage_dw_io_table [18] = {
   {-1.0,INFINITY,1.0},
   {-1.0,-INFINITY,1.0},
   {-0.5,INFINITY,0.0},
   {-0.5,-INFINITY,INFINITY},
   {0.5,INFINITY,0.0},
   {0.5,-INFINITY,INFINITY},
   {0.5,NAN,NAN},
   {-0.5,NAN,NAN},
   {0.0,INFINITY,0.0},
   {0.0,-INFINITY,INFINITY},
   {-0.0,INFINITY,0.0},
   {-0.0,-INFINITY,INFINITY},
   {1.00616383,1463.0,8022.498881768666},
   {1.00616383,-1463.0,0.00012464944087091435},
   {1.00616383,41.0,1.2865205978119534},
   {1.00616383,-41.0,0.7772903144347222},
   {-99.2,299.0,-INFINITY},
   {-99.2,29.0,-7.922066660532979E+57},
};
/** function pow_uv__uv_extreme_coverage_dw executes the tests and returns the number of failing tests */
int pow_uv__uv_extreme_coverage_dw() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<18;ind++) {
		x=pow_uv__uv_extreme_coverage_dw_io_table[ind].pow_x;
		y=pow_uv__uv_extreme_coverage_dw_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_extreme_coverage_dw_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME,10.0,&max_dif_below_pow_uv__uv_extreme,&max_dif_above_pow_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_extreme_coverage_dw: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_extreme_coverage_dw_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==18) {
		PRINTF("pow_uv__uv_extreme_coverage_dw: successfully tested: 18 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_extreme_coverage_dw: %d tests failed for pow (out of 18)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (25 functions) of pow_uv__uv_extreme
*/
int pow_uv__uv_extreme_main_test() {
	int errors=0;
	int index=0;
	errors+=pow_uv__uv_extreme_nan();                    /* 1. 5 tests */
	errors+=pow_uv__uv_extreme_pos_infinity();           /* 2. 5 tests */
	errors+=pow_uv__uv_extreme_neg_infinity();           /* 3. 5 tests */
	errors+=pow_uv__uv_extreme_infinity();               /* 4. 6 tests */
	errors+=pow_uv__uv_extreme_extreme_values();         /* 5. 9 tests */
	errors+=pow_uv__uv_extreme_pow_denom_pos_pos();      /* 6. 256 tests */
	errors+=pow_uv__uv_extreme_pow_denom_pos_neg();      /* 7. 272 tests */
	errors+=pow_uv__uv_extreme_pow_denom_neg_pos();      /* 8. 272 tests */
	errors+=pow_uv__uv_extreme_pow_denom_neg_neg();      /* 9. 289 tests */
	errors+=pow_uv__uv_extreme_zero();                   /* 10. 1 tests */
	errors+=pow_uv__uv_extreme_random_small_squares();   /* 11. 50 tests */
	errors+=pow_uv__uv_extreme_random_small_neg_sqr();   /* 12. 50 tests */
	errors+=pow_uv__uv_extreme_random_negative();        /* 13. 50 tests */
	errors+=pow_uv__uv_extreme_big_powers();             /* 14. 49 tests */
	errors+=pow_uv__uv_extreme_overflowing_powers();     /* 15. 49 tests */
	errors+=pow_uv__uv_extreme_overflowing_powers_pos(); /* 16. 64 tests */
	errors+=pow_uv__uv_extreme_overflowing_powers_neg(); /* 17. 64 tests */
	errors+=pow_uv__uv_extreme_undeflowing_powers();     /* 18. 64 tests */
	errors+=pow_uv__uv_extreme_undeflowing_powers2();    /* 19. 64 tests */
	errors+=pow_uv__uv_extreme_undeflowing_powers3();    /* 20. 64 tests */
	errors+=pow_uv__uv_extreme_undeflowing_powers4();    /* 21. 64 tests */
	errors+=pow_uv__uv_extreme_roots_of_minus_two();     /* 22. 54 tests */
	errors+=pow_uv__uv_extreme_roots_to_minus_ten();     /* 23. 54 tests */
	errors+=pow_uv__uv_extreme_big_powers2();            /* 24. 20 tests */
	errors+=pow_uv__uv_extreme_coverage_dw();            /* 25. 18 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of pow_uv__uv_extreme: successfully tested ALL 1898 cases for pow\n");
	} else {
		PRINTF("SUMMARY of pow_uv__uv_extreme: %d tests failed in pow_uv__uv_extreme (out of 1898 in pow_uv__uv_extreme)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.17g. ABS_TOLERANCE_POW_UV__UV_EXTREME=%.17g\n",pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value,ABS_TOLERANCE_POW_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.17g. REL_TOLERANCE_POW_UV__UV_EXTREME=%.17g\n",pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value,REL_TOLERANCE_POW_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.17g. ABS_REL_TOLERANCE_POW_UV__UV_EXTREME=%.17g\n",pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value,ABS_REL_TOLERANCE_POW_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.17g. ULP_TOLERANCE_POW_UV__UV_EXTREME=%.17g\n",pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value,ULP_TOLERANCE_POW_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.17g. EXAKT_TOLERANCE_POW_UV__UV_EXTREME=%.17g\n",pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value,EXAKT_TOLERANCE_POW_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.17g. EQUAL_TOLERANCE_POW_UV__UV_EXTREME=%.17g\n",pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value,EQUAL_TOLERANCE_POW_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.17g. BINHEX_TOLERANCE_POW_UV__UV_EXTREME=%.17g\n",pow_uv__uv_extreme_all_deviation_results_double[index].max_diff_value,BINHEX_TOLERANCE_POW_UV__UV_EXTREME);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls pow_uv__uv_extreme_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = pow_uv__uv_extreme_main_test();
	return result;
}
#endif /* NO_MAIN */

