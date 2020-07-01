/**
 * this file (powf_uv__uv_extreme.c) contains test cases for powf
 * all test cases for powf have been specified and split into separate files
 * this file contains 1215 test cases in 27 functions for the following purpose:
 * 5 specified tests for powf
 * (NaN,0); (NaN,1); (0,NaN); (1,NaN); (NaN,NaN)
 * returns the number of failing tests
 * 5 specified tests for powf
 * (Infinity,0); (Infinity,1); (0,Infinity); (1,Infinity); (Infinity,Infinity)
 * returns the number of failing tests
 * 5 specified tests for powf
 * (-Infinity,0); (-Infinity,1); (0,-Infinity); (1,-Infinity); (-Infinity,-Infinity) ...
 * returns the number of failing tests
 * 6 specified tests for powf
 * (-Infinity,Infinity); (Infinity,-Infinity); (NaN,-Infinity); (NaN,Infinity); (-Infinity,NaN) ...
 * returns the number of failing tests
 * 9 specified tests for powf
 * (NaN,NaN); (NaN,Infinity); (NaN,-Infinity); (Infinity,NaN); (Infinity,Infinity); (Infinity,-Infinity) ...
 * returns the number of failing tests
 * 100 specified tests for powf
 * (2.350989E-39,2.350989E-39); (2.350989E-39,4.70197E-40); (2.350989E-39,9.404E-41) ...
 * returns the number of failing tests
 * 110 specified tests for powf
 * (2.350989E-39,-2.938736E-39); (2.350989E-39,-7.34684E-40); (2.350989E-39,-1.83671E-40) ...
 * returns the number of failing tests
 * 110 specified tests for powf
 * (-2.938736E-39,2.350989E-39); (-2.938736E-39,4.70197E-40); (-2.938736E-39,9.404E-41) ...
 * returns the number of failing tests
 * 121 specified tests for powf
 * (-2.938736E-39,-2.938736E-39); (-2.938736E-39,-7.34684E-40); (-2.938736E-39,-1.83671E-40) ...
 * returns the number of failing tests
 * 1 specified tests for powf
 * (0,0)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (0,1) to (2,2)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (-1,0) to (2,2)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (-10,-10) to (-1,0)
 * returns the number of failing tests
 * 49 linear tests for powf
 * range: (1000,2) to (100000,10)
 * returns the number of failing tests
 * 49 linear tests for powf
 * range: (1000,1) to (100000,1000)
 * returns the number of failing tests
 * 64 linear tests for powf
 * range: (1000,1024) to (100000,10000)
 * returns the number of failing tests
 * 64 linear tests for powf
 * range: (-100000,1024) to (1000,10000)
 * returns the number of failing tests
 * 64 linear tests for powf
 * range: (0.00001,2) to (0.001,256)
 * returns the number of failing tests
 * 64 linear tests for powf
 * range: (0.000000001,2) to (0.00001,1028)
 * returns the number of failing tests
 * 64 linear tests for powf
 * range: (-0.0001,1) to (-0.000000001,99)
 * returns the number of failing tests
 * 64 linear tests for powf
 * range: (0.000000001,1000) to (0.00001,100000)
 * returns the number of failing tests
 * 25 approximation tests with factors 2.0,2.0
 * range: (0,-2) to (1,-2)
 * returns the number of failing tests
 * 25 approximation tests with factors 2.0,2.0
 * range: (0,0) to (1,-10)
 * returns the number of failing tests
 * 20 (1 omitted out of input range) specified tests for powf
 * (2,21); (2,22); (2,23); (2,24); (2,25); (2,24); (2,27); (2,28); (2,29); (2,30); (2,31) ...
 * returns the number of failing tests
 * 18 specified tests for powf
 * (-1.0,Infinity); (-1.0,-Infinity); (-0.5,Infinity); (-0.5,-Infinity); (0.5,Infinity) ...
 * returns the number of failing tests
 * 15 specified tests for powf
 * (0,-1); (1000,-15); (0.001,-15); (-0.001,-15); (-8,-8); (7,-4); (-1000,-15); (0.001,15) ...
 * returns the number of failing tests
 * 8 specified tests for powf
 * (-1.0,Infinity); (-1.0,-Infinity); (-0.5,Infinity); (-0.5,-Infinity); (0.5,Infinity) ...
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

#define ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME 1.0E-5

float max_dif_below_powf_uv__uv_extreme=0.0;
float max_dif_above_powf_uv__uv_extreme=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of overflowing_powers */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_overflowing_powers_io_table_type;
/* type for expected input & output values of big_powers2 */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_big_powers2_io_table_type;
/* type for expected input & output values of roots_of_minus_two */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_roots_of_minus_two_io_table_type;
/* type for expected input & output values of big_powers */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_big_powers_io_table_type;
/* type for expected input & output values of zero */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_zero_io_table_type;
/* type for expected input & output values of overflowing_powers_pos */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_overflowing_powers_pos_io_table_type;
/* type for expected input & output values of random_small_squares */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_random_small_squares_io_table_type;
/* type for expected input & output values of undeflowing_powers2 */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_undeflowing_powers2_io_table_type;
/* type for expected input & output values of powf_denom_neg_pos */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_powf_denom_neg_pos_io_table_type;
/* type for expected input & output values of powf_denom_pos_pos */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_powf_denom_pos_pos_io_table_type;
/* type for expected input & output values of coverage_dw */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_coverage_dw_io_table_type;
/* type for expected input & output values of nan */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_nan_io_table_type;
/* type for expected input & output values of random_negative */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_random_negative_io_table_type;
/* type for expected input & output values of powf_denom_pos_neg */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_powf_denom_pos_neg_io_table_type;
/* type for expected input & output values of extreme_values */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_extreme_values_io_table_type;
/* type for expected input & output values of undeflowing_powers */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_undeflowing_powers_io_table_type;
/* type for expected input & output values of pos_infinity */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_pos_infinity_io_table_type;
/* type for expected input & output values of neg_infinity */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_neg_infinity_io_table_type;
/* type for expected input & output values of random_small_neg_sqr */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_random_small_neg_sqr_io_table_type;
/* type for expected input & output values of powf_denom_neg_neg */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_powf_denom_neg_neg_io_table_type;
/* type for expected input & output values of undeflowing_powers3 */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_undeflowing_powers3_io_table_type;
/* type for expected input & output values of undeflowing_powers4 */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_undeflowing_powers4_io_table_type;
/* type for expected input & output values of overflowing_powers_neg */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_overflowing_powers_neg_io_table_type;
/* type for expected input & output values of roots_to_minus_ten */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_roots_to_minus_ten_io_table_type;
/* type for expected input & output values of coverage_values */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_coverage_values_io_table_type;
/* type for expected input & output values of infinity */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_extreme_infinity_io_table_type;
/**
 * 5 specified tests for powf
 * (NaN,0); (NaN,1); (0,NaN); (1,NaN); (NaN,NaN)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_nan in the following table */
powf_uv__uv_extreme_nan_io_table_type powf_uv__uv_extreme_nan_io_table [5] = {
   {NAN,0,1.00000000},
   {NAN,1.00000000,NAN},
   {0,NAN,NAN},
   {1.00000000,NAN,1.00000000},
   {NAN,NAN,NAN},
};
/** function powf_uv__uv_extreme_nan executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_nan() {
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
	for (ind=0;ind<5;ind++) {
		x=powf_uv__uv_extreme_nan_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_nan_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_nan_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_nan: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_nan_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==5) {
		PRINTF("powf_uv__uv_extreme_nan: successfully tested: 5 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_nan: %d tests failed for powf (out of 5)\n",errors);
	}
#endif
	return errors;
}

/**
 * 5 specified tests for powf
 * (Infinity,0); (Infinity,1); (0,Infinity); (1,Infinity); (Infinity,Infinity)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_pos_infinity in the following table */
powf_uv__uv_extreme_pos_infinity_io_table_type powf_uv__uv_extreme_pos_infinity_io_table [5] = {
   {INFINITY,0,1.00000000},
   {INFINITY,1.00000000,INFINITY},
   {0,INFINITY,0.0},
   {1.00000000,INFINITY,1.00000000},
   {INFINITY,INFINITY,INFINITY},
};
/** function powf_uv__uv_extreme_pos_infinity executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_pos_infinity() {
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
	for (ind=0;ind<5;ind++) {
		x=powf_uv__uv_extreme_pos_infinity_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_pos_infinity_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_pos_infinity_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_pos_infinity: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_pos_infinity_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==5) {
		PRINTF("powf_uv__uv_extreme_pos_infinity: successfully tested: 5 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_pos_infinity: %d tests failed for powf (out of 5)\n",errors);
	}
#endif
	return errors;
}

/**
 * 5 specified tests for powf
 * (-Infinity,0); (-Infinity,1); (0,-Infinity); (1,-Infinity); (-Infinity,-Infinity) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_neg_infinity in the following table */
powf_uv__uv_extreme_neg_infinity_io_table_type powf_uv__uv_extreme_neg_infinity_io_table [5] = {
   {-INFINITY,0,1.00000000},
   {-INFINITY,1.00000000,-INFINITY},
   {0,-INFINITY,INFINITY},
   {1.00000000,-INFINITY,1.00000000},
   {-INFINITY,-INFINITY,0.0},
};
/** function powf_uv__uv_extreme_neg_infinity executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_neg_infinity() {
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
	for (ind=0;ind<5;ind++) {
		x=powf_uv__uv_extreme_neg_infinity_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_neg_infinity_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_neg_infinity_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_neg_infinity: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_neg_infinity_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==5) {
		PRINTF("powf_uv__uv_extreme_neg_infinity: successfully tested: 5 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_neg_infinity: %d tests failed for powf (out of 5)\n",errors);
	}
#endif
	return errors;
}

/**
 * 6 specified tests for powf
 * (-Infinity,Infinity); (Infinity,-Infinity); (NaN,-Infinity); (NaN,Infinity); (-Infinity,NaN) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_infinity in the following table */
powf_uv__uv_extreme_infinity_io_table_type powf_uv__uv_extreme_infinity_io_table [6] = {
   {-INFINITY,INFINITY,INFINITY},
   {INFINITY,-INFINITY,0.0},
   {NAN,-INFINITY,NAN},
   {NAN,INFINITY,NAN},
   {-INFINITY,NAN,NAN},
   {INFINITY,NAN,NAN},
};
/** function powf_uv__uv_extreme_infinity executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_infinity() {
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
	for (ind=0;ind<6;ind++) {
		x=powf_uv__uv_extreme_infinity_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_infinity_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_infinity_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_infinity: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_infinity_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==6) {
		PRINTF("powf_uv__uv_extreme_infinity: successfully tested: 6 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_infinity: %d tests failed for powf (out of 6)\n",errors);
	}
#endif
	return errors;
}

/**
 * 9 specified tests for powf
 * (NaN,NaN); (NaN,Infinity); (NaN,-Infinity); (Infinity,NaN); (Infinity,Infinity); (Infinity,-Infinity) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_extreme_values in the following table */
powf_uv__uv_extreme_extreme_values_io_table_type powf_uv__uv_extreme_extreme_values_io_table [9] = {
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
/** function powf_uv__uv_extreme_extreme_values executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_extreme_values() {
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
	for (ind=0;ind<9;ind++) {
		x=powf_uv__uv_extreme_extreme_values_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_extreme_values_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_extreme_values_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_extreme_values: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_extreme_values_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==9) {
		PRINTF("powf_uv__uv_extreme_extreme_values: successfully tested: 9 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_extreme_values: %d tests failed for powf (out of 9)\n",errors);
	}
#endif
	return errors;
}

/**
 * 100 specified tests for powf
 * (2.350989E-39,2.350989E-39); (2.350989E-39,4.70197E-40); (2.350989E-39,9.404E-41) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_powf_denom_pos_pos in the following table */
powf_uv__uv_extreme_powf_denom_pos_pos_io_table_type powf_uv__uv_extreme_powf_denom_pos_pos_io_table [100] = {
   {2.35098926e-39,2.35098926e-39,1.00000000},
   {2.35098926e-39,4.70197292e-40,1.00000000},
   {2.35098926e-39,9.40397386e-41,1.00000000},
   {2.35098926e-39,1.88082280e-41,1.00000000},
   {2.35098926e-39,3.76108508e-42,1.00000000},
   {2.35098926e-39,7.52497275e-43,1.00000000},
   {2.35098926e-39,1.49938936e-43,1.00000000},
   {2.35098926e-39,2.94272678e-44,1.00000000},
   {2.35098926e-39,5.60519386e-45,1.00000000},
   {2.35098926e-39,1.40129846e-45,1.00000000},
   {4.70197292e-40,2.35098926e-39,1.00000000},
   {4.70197292e-40,4.70197292e-40,1.00000000},
   {4.70197292e-40,9.40397386e-41,1.00000000},
   {4.70197292e-40,1.88082280e-41,1.00000000},
   {4.70197292e-40,3.76108508e-42,1.00000000},
   {4.70197292e-40,7.52497275e-43,1.00000000},
   {4.70197292e-40,1.49938936e-43,1.00000000},
   {4.70197292e-40,2.94272678e-44,1.00000000},
   {4.70197292e-40,5.60519386e-45,1.00000000},
   {4.70197292e-40,1.40129846e-45,1.00000000},
   {9.40397386e-41,2.35098926e-39,1.00000000},
   {9.40397386e-41,4.70197292e-40,1.00000000},
   {9.40397386e-41,9.40397386e-41,1.00000000},
   {9.40397386e-41,1.88082280e-41,1.00000000},
   {9.40397386e-41,3.76108508e-42,1.00000000},
   {9.40397386e-41,7.52497275e-43,1.00000000},
   {9.40397386e-41,1.49938936e-43,1.00000000},
   {9.40397386e-41,2.94272678e-44,1.00000000},
   {9.40397386e-41,5.60519386e-45,1.00000000},
   {9.40397386e-41,1.40129846e-45,1.00000000},
   {1.88082280e-41,2.35098926e-39,1.00000000},
   {1.88082280e-41,4.70197292e-40,1.00000000},
   {1.88082280e-41,9.40397386e-41,1.00000000},
   {1.88082280e-41,1.88082280e-41,1.00000000},
   {1.88082280e-41,3.76108508e-42,1.00000000},
   {1.88082280e-41,7.52497275e-43,1.00000000},
   {1.88082280e-41,1.49938936e-43,1.00000000},
   {1.88082280e-41,2.94272678e-44,1.00000000},
   {1.88082280e-41,5.60519386e-45,1.00000000},
   {1.88082280e-41,1.40129846e-45,1.00000000},
   {3.76108508e-42,2.35098926e-39,1.00000000},
   {3.76108508e-42,4.70197292e-40,1.00000000},
   {3.76108508e-42,9.40397386e-41,1.00000000},
   {3.76108508e-42,1.88082280e-41,1.00000000},
   {3.76108508e-42,3.76108508e-42,1.00000000},
   {3.76108508e-42,7.52497275e-43,1.00000000},
   {3.76108508e-42,1.49938936e-43,1.00000000},
   {3.76108508e-42,2.94272678e-44,1.00000000},
   {3.76108508e-42,5.60519386e-45,1.00000000},
   {3.76108508e-42,1.40129846e-45,1.00000000},
   {7.52497275e-43,2.35098926e-39,1.00000000},
   {7.52497275e-43,4.70197292e-40,1.00000000},
   {7.52497275e-43,9.40397386e-41,1.00000000},
   {7.52497275e-43,1.88082280e-41,1.00000000},
   {7.52497275e-43,3.76108508e-42,1.00000000},
   {7.52497275e-43,7.52497275e-43,1.00000000},
   {7.52497275e-43,1.49938936e-43,1.00000000},
   {7.52497275e-43,2.94272678e-44,1.00000000},
   {7.52497275e-43,5.60519386e-45,1.00000000},
   {7.52497275e-43,1.40129846e-45,1.00000000},
   {1.49938936e-43,2.35098926e-39,1.00000000},
   {1.49938936e-43,4.70197292e-40,1.00000000},
   {1.49938936e-43,9.40397386e-41,1.00000000},
   {1.49938936e-43,1.88082280e-41,1.00000000},
   {1.49938936e-43,3.76108508e-42,1.00000000},
   {1.49938936e-43,7.52497275e-43,1.00000000},
   {1.49938936e-43,1.49938936e-43,1.00000000},
   {1.49938936e-43,2.94272678e-44,1.00000000},
   {1.49938936e-43,5.60519386e-45,1.00000000},
   {1.49938936e-43,1.40129846e-45,1.00000000},
   {2.94272678e-44,2.35098926e-39,1.00000000},
   {2.94272678e-44,4.70197292e-40,1.00000000},
   {2.94272678e-44,9.40397386e-41,1.00000000},
   {2.94272678e-44,1.88082280e-41,1.00000000},
   {2.94272678e-44,3.76108508e-42,1.00000000},
   {2.94272678e-44,7.52497275e-43,1.00000000},
   {2.94272678e-44,1.49938936e-43,1.00000000},
   {2.94272678e-44,2.94272678e-44,1.00000000},
   {2.94272678e-44,5.60519386e-45,1.00000000},
   {2.94272678e-44,1.40129846e-45,1.00000000},
   {5.60519386e-45,2.35098926e-39,1.00000000},
   {5.60519386e-45,4.70197292e-40,1.00000000},
   {5.60519386e-45,9.40397386e-41,1.00000000},
   {5.60519386e-45,1.88082280e-41,1.00000000},
   {5.60519386e-45,3.76108508e-42,1.00000000},
   {5.60519386e-45,7.52497275e-43,1.00000000},
   {5.60519386e-45,1.49938936e-43,1.00000000},
   {5.60519386e-45,2.94272678e-44,1.00000000},
   {5.60519386e-45,5.60519386e-45,1.00000000},
   {5.60519386e-45,1.40129846e-45,1.00000000},
   {1.40129846e-45,2.35098926e-39,1.00000000},
   {1.40129846e-45,4.70197292e-40,1.00000000},
   {1.40129846e-45,9.40397386e-41,1.00000000},
   {1.40129846e-45,1.88082280e-41,1.00000000},
   {1.40129846e-45,3.76108508e-42,1.00000000},
   {1.40129846e-45,7.52497275e-43,1.00000000},
   {1.40129846e-45,1.49938936e-43,1.00000000},
   {1.40129846e-45,2.94272678e-44,1.00000000},
   {1.40129846e-45,5.60519386e-45,1.00000000},
   {1.40129846e-45,1.40129846e-45,1.00000000},
};
/** function powf_uv__uv_extreme_powf_denom_pos_pos executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_powf_denom_pos_pos() {
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
	for (ind=0;ind<100;ind++) {
		x=powf_uv__uv_extreme_powf_denom_pos_pos_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_powf_denom_pos_pos_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_powf_denom_pos_pos_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_powf_denom_pos_pos: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_powf_denom_pos_pos_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==100) {
		PRINTF("powf_uv__uv_extreme_powf_denom_pos_pos: successfully tested: 100 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_powf_denom_pos_pos: %d tests failed for powf (out of 100)\n",errors);
	}
#endif
	return errors;
}

/**
 * 110 specified tests for powf
 * (2.350989E-39,-2.938736E-39); (2.350989E-39,-7.34684E-40); (2.350989E-39,-1.83671E-40) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_powf_denom_pos_neg in the following table */
powf_uv__uv_extreme_powf_denom_pos_neg_io_table_type powf_uv__uv_extreme_powf_denom_pos_neg_io_table [110] = {
   {2.35098926e-39,-2.93873588e-39,1.00000000},
   {2.35098926e-39,-7.34683969e-40,1.00000000},
   {2.35098926e-39,-1.83670992e-40,1.00000000},
   {2.35098926e-39,-4.59177481e-41,1.00000000},
   {2.35098926e-39,-1.14794370e-41,1.00000000},
   {2.35098926e-39,-2.86985925e-42,1.00000000},
   {2.35098926e-39,-7.17464814e-43,1.00000000},
   {2.35098926e-39,-1.79366203e-43,1.00000000},
   {2.35098926e-39,-4.48415509e-44,1.00000000},
   {2.35098926e-39,-1.12103877e-44,1.00000000},
   {2.35098926e-39,-2.80259693e-45,1.00000000},
   {4.70197292e-40,-2.93873588e-39,1.00000000},
   {4.70197292e-40,-7.34683969e-40,1.00000000},
   {4.70197292e-40,-1.83670992e-40,1.00000000},
   {4.70197292e-40,-4.59177481e-41,1.00000000},
   {4.70197292e-40,-1.14794370e-41,1.00000000},
   {4.70197292e-40,-2.86985925e-42,1.00000000},
   {4.70197292e-40,-7.17464814e-43,1.00000000},
   {4.70197292e-40,-1.79366203e-43,1.00000000},
   {4.70197292e-40,-4.48415509e-44,1.00000000},
   {4.70197292e-40,-1.12103877e-44,1.00000000},
   {4.70197292e-40,-2.80259693e-45,1.00000000},
   {9.40397386e-41,-2.93873588e-39,1.00000000},
   {9.40397386e-41,-7.34683969e-40,1.00000000},
   {9.40397386e-41,-1.83670992e-40,1.00000000},
   {9.40397386e-41,-4.59177481e-41,1.00000000},
   {9.40397386e-41,-1.14794370e-41,1.00000000},
   {9.40397386e-41,-2.86985925e-42,1.00000000},
   {9.40397386e-41,-7.17464814e-43,1.00000000},
   {9.40397386e-41,-1.79366203e-43,1.00000000},
   {9.40397386e-41,-4.48415509e-44,1.00000000},
   {9.40397386e-41,-1.12103877e-44,1.00000000},
   {9.40397386e-41,-2.80259693e-45,1.00000000},
   {1.88082280e-41,-2.93873588e-39,1.00000000},
   {1.88082280e-41,-7.34683969e-40,1.00000000},
   {1.88082280e-41,-1.83670992e-40,1.00000000},
   {1.88082280e-41,-4.59177481e-41,1.00000000},
   {1.88082280e-41,-1.14794370e-41,1.00000000},
   {1.88082280e-41,-2.86985925e-42,1.00000000},
   {1.88082280e-41,-7.17464814e-43,1.00000000},
   {1.88082280e-41,-1.79366203e-43,1.00000000},
   {1.88082280e-41,-4.48415509e-44,1.00000000},
   {1.88082280e-41,-1.12103877e-44,1.00000000},
   {1.88082280e-41,-2.80259693e-45,1.00000000},
   {3.76108508e-42,-2.93873588e-39,1.00000000},
   {3.76108508e-42,-7.34683969e-40,1.00000000},
   {3.76108508e-42,-1.83670992e-40,1.00000000},
   {3.76108508e-42,-4.59177481e-41,1.00000000},
   {3.76108508e-42,-1.14794370e-41,1.00000000},
   {3.76108508e-42,-2.86985925e-42,1.00000000},
   {3.76108508e-42,-7.17464814e-43,1.00000000},
   {3.76108508e-42,-1.79366203e-43,1.00000000},
   {3.76108508e-42,-4.48415509e-44,1.00000000},
   {3.76108508e-42,-1.12103877e-44,1.00000000},
   {3.76108508e-42,-2.80259693e-45,1.00000000},
   {7.52497275e-43,-2.93873588e-39,1.00000000},
   {7.52497275e-43,-7.34683969e-40,1.00000000},
   {7.52497275e-43,-1.83670992e-40,1.00000000},
   {7.52497275e-43,-4.59177481e-41,1.00000000},
   {7.52497275e-43,-1.14794370e-41,1.00000000},
   {7.52497275e-43,-2.86985925e-42,1.00000000},
   {7.52497275e-43,-7.17464814e-43,1.00000000},
   {7.52497275e-43,-1.79366203e-43,1.00000000},
   {7.52497275e-43,-4.48415509e-44,1.00000000},
   {7.52497275e-43,-1.12103877e-44,1.00000000},
   {7.52497275e-43,-2.80259693e-45,1.00000000},
   {1.49938936e-43,-2.93873588e-39,1.00000000},
   {1.49938936e-43,-7.34683969e-40,1.00000000},
   {1.49938936e-43,-1.83670992e-40,1.00000000},
   {1.49938936e-43,-4.59177481e-41,1.00000000},
   {1.49938936e-43,-1.14794370e-41,1.00000000},
   {1.49938936e-43,-2.86985925e-42,1.00000000},
   {1.49938936e-43,-7.17464814e-43,1.00000000},
   {1.49938936e-43,-1.79366203e-43,1.00000000},
   {1.49938936e-43,-4.48415509e-44,1.00000000},
   {1.49938936e-43,-1.12103877e-44,1.00000000},
   {1.49938936e-43,-2.80259693e-45,1.00000000},
   {2.94272678e-44,-2.93873588e-39,1.00000000},
   {2.94272678e-44,-7.34683969e-40,1.00000000},
   {2.94272678e-44,-1.83670992e-40,1.00000000},
   {2.94272678e-44,-4.59177481e-41,1.00000000},
   {2.94272678e-44,-1.14794370e-41,1.00000000},
   {2.94272678e-44,-2.86985925e-42,1.00000000},
   {2.94272678e-44,-7.17464814e-43,1.00000000},
   {2.94272678e-44,-1.79366203e-43,1.00000000},
   {2.94272678e-44,-4.48415509e-44,1.00000000},
   {2.94272678e-44,-1.12103877e-44,1.00000000},
   {2.94272678e-44,-2.80259693e-45,1.00000000},
   {5.60519386e-45,-2.93873588e-39,1.00000000},
   {5.60519386e-45,-7.34683969e-40,1.00000000},
   {5.60519386e-45,-1.83670992e-40,1.00000000},
   {5.60519386e-45,-4.59177481e-41,1.00000000},
   {5.60519386e-45,-1.14794370e-41,1.00000000},
   {5.60519386e-45,-2.86985925e-42,1.00000000},
   {5.60519386e-45,-7.17464814e-43,1.00000000},
   {5.60519386e-45,-1.79366203e-43,1.00000000},
   {5.60519386e-45,-4.48415509e-44,1.00000000},
   {5.60519386e-45,-1.12103877e-44,1.00000000},
   {5.60519386e-45,-2.80259693e-45,1.00000000},
   {1.40129846e-45,-2.93873588e-39,1.00000000},
   {1.40129846e-45,-7.34683969e-40,1.00000000},
   {1.40129846e-45,-1.83670992e-40,1.00000000},
   {1.40129846e-45,-4.59177481e-41,1.00000000},
   {1.40129846e-45,-1.14794370e-41,1.00000000},
   {1.40129846e-45,-2.86985925e-42,1.00000000},
   {1.40129846e-45,-7.17464814e-43,1.00000000},
   {1.40129846e-45,-1.79366203e-43,1.00000000},
   {1.40129846e-45,-4.48415509e-44,1.00000000},
   {1.40129846e-45,-1.12103877e-44,1.00000000},
   {1.40129846e-45,-2.80259693e-45,1.00000000},
};
/** function powf_uv__uv_extreme_powf_denom_pos_neg executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_powf_denom_pos_neg() {
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
	for (ind=0;ind<110;ind++) {
		x=powf_uv__uv_extreme_powf_denom_pos_neg_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_powf_denom_pos_neg_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_powf_denom_pos_neg_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_powf_denom_pos_neg: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_powf_denom_pos_neg_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==110) {
		PRINTF("powf_uv__uv_extreme_powf_denom_pos_neg: successfully tested: 110 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_powf_denom_pos_neg: %d tests failed for powf (out of 110)\n",errors);
	}
#endif
	return errors;
}

/**
 * 110 specified tests for powf
 * (-2.938736E-39,2.350989E-39); (-2.938736E-39,4.70197E-40); (-2.938736E-39,9.404E-41) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_powf_denom_neg_pos in the following table */
powf_uv__uv_extreme_powf_denom_neg_pos_io_table_type powf_uv__uv_extreme_powf_denom_neg_pos_io_table [110] = {
   {-2.93873588e-39,2.35098926e-39,NAN},
   {-2.93873588e-39,4.70197292e-40,NAN},
   {-2.93873588e-39,9.40397386e-41,NAN},
   {-2.93873588e-39,1.88082280e-41,NAN},
   {-2.93873588e-39,3.76108508e-42,NAN},
   {-2.93873588e-39,7.52497275e-43,NAN},
   {-2.93873588e-39,1.49938936e-43,NAN},
   {-2.93873588e-39,2.94272678e-44,NAN},
   {-2.93873588e-39,5.60519386e-45,NAN},
   {-2.93873588e-39,1.40129846e-45,NAN},
   {-7.34683969e-40,2.35098926e-39,NAN},
   {-7.34683969e-40,4.70197292e-40,NAN},
   {-7.34683969e-40,9.40397386e-41,NAN},
   {-7.34683969e-40,1.88082280e-41,NAN},
   {-7.34683969e-40,3.76108508e-42,NAN},
   {-7.34683969e-40,7.52497275e-43,NAN},
   {-7.34683969e-40,1.49938936e-43,NAN},
   {-7.34683969e-40,2.94272678e-44,NAN},
   {-7.34683969e-40,5.60519386e-45,NAN},
   {-7.34683969e-40,1.40129846e-45,NAN},
   {-1.83670992e-40,2.35098926e-39,NAN},
   {-1.83670992e-40,4.70197292e-40,NAN},
   {-1.83670992e-40,9.40397386e-41,NAN},
   {-1.83670992e-40,1.88082280e-41,NAN},
   {-1.83670992e-40,3.76108508e-42,NAN},
   {-1.83670992e-40,7.52497275e-43,NAN},
   {-1.83670992e-40,1.49938936e-43,NAN},
   {-1.83670992e-40,2.94272678e-44,NAN},
   {-1.83670992e-40,5.60519386e-45,NAN},
   {-1.83670992e-40,1.40129846e-45,NAN},
   {-4.59177481e-41,2.35098926e-39,NAN},
   {-4.59177481e-41,4.70197292e-40,NAN},
   {-4.59177481e-41,9.40397386e-41,NAN},
   {-4.59177481e-41,1.88082280e-41,NAN},
   {-4.59177481e-41,3.76108508e-42,NAN},
   {-4.59177481e-41,7.52497275e-43,NAN},
   {-4.59177481e-41,1.49938936e-43,NAN},
   {-4.59177481e-41,2.94272678e-44,NAN},
   {-4.59177481e-41,5.60519386e-45,NAN},
   {-4.59177481e-41,1.40129846e-45,NAN},
   {-1.14794370e-41,2.35098926e-39,NAN},
   {-1.14794370e-41,4.70197292e-40,NAN},
   {-1.14794370e-41,9.40397386e-41,NAN},
   {-1.14794370e-41,1.88082280e-41,NAN},
   {-1.14794370e-41,3.76108508e-42,NAN},
   {-1.14794370e-41,7.52497275e-43,NAN},
   {-1.14794370e-41,1.49938936e-43,NAN},
   {-1.14794370e-41,2.94272678e-44,NAN},
   {-1.14794370e-41,5.60519386e-45,NAN},
   {-1.14794370e-41,1.40129846e-45,NAN},
   {-2.86985925e-42,2.35098926e-39,NAN},
   {-2.86985925e-42,4.70197292e-40,NAN},
   {-2.86985925e-42,9.40397386e-41,NAN},
   {-2.86985925e-42,1.88082280e-41,NAN},
   {-2.86985925e-42,3.76108508e-42,NAN},
   {-2.86985925e-42,7.52497275e-43,NAN},
   {-2.86985925e-42,1.49938936e-43,NAN},
   {-2.86985925e-42,2.94272678e-44,NAN},
   {-2.86985925e-42,5.60519386e-45,NAN},
   {-2.86985925e-42,1.40129846e-45,NAN},
   {-7.17464814e-43,2.35098926e-39,NAN},
   {-7.17464814e-43,4.70197292e-40,NAN},
   {-7.17464814e-43,9.40397386e-41,NAN},
   {-7.17464814e-43,1.88082280e-41,NAN},
   {-7.17464814e-43,3.76108508e-42,NAN},
   {-7.17464814e-43,7.52497275e-43,NAN},
   {-7.17464814e-43,1.49938936e-43,NAN},
   {-7.17464814e-43,2.94272678e-44,NAN},
   {-7.17464814e-43,5.60519386e-45,NAN},
   {-7.17464814e-43,1.40129846e-45,NAN},
   {-1.79366203e-43,2.35098926e-39,NAN},
   {-1.79366203e-43,4.70197292e-40,NAN},
   {-1.79366203e-43,9.40397386e-41,NAN},
   {-1.79366203e-43,1.88082280e-41,NAN},
   {-1.79366203e-43,3.76108508e-42,NAN},
   {-1.79366203e-43,7.52497275e-43,NAN},
   {-1.79366203e-43,1.49938936e-43,NAN},
   {-1.79366203e-43,2.94272678e-44,NAN},
   {-1.79366203e-43,5.60519386e-45,NAN},
   {-1.79366203e-43,1.40129846e-45,NAN},
   {-4.48415509e-44,2.35098926e-39,NAN},
   {-4.48415509e-44,4.70197292e-40,NAN},
   {-4.48415509e-44,9.40397386e-41,NAN},
   {-4.48415509e-44,1.88082280e-41,NAN},
   {-4.48415509e-44,3.76108508e-42,NAN},
   {-4.48415509e-44,7.52497275e-43,NAN},
   {-4.48415509e-44,1.49938936e-43,NAN},
   {-4.48415509e-44,2.94272678e-44,NAN},
   {-4.48415509e-44,5.60519386e-45,NAN},
   {-4.48415509e-44,1.40129846e-45,NAN},
   {-1.12103877e-44,2.35098926e-39,NAN},
   {-1.12103877e-44,4.70197292e-40,NAN},
   {-1.12103877e-44,9.40397386e-41,NAN},
   {-1.12103877e-44,1.88082280e-41,NAN},
   {-1.12103877e-44,3.76108508e-42,NAN},
   {-1.12103877e-44,7.52497275e-43,NAN},
   {-1.12103877e-44,1.49938936e-43,NAN},
   {-1.12103877e-44,2.94272678e-44,NAN},
   {-1.12103877e-44,5.60519386e-45,NAN},
   {-1.12103877e-44,1.40129846e-45,NAN},
   {-2.80259693e-45,2.35098926e-39,NAN},
   {-2.80259693e-45,4.70197292e-40,NAN},
   {-2.80259693e-45,9.40397386e-41,NAN},
   {-2.80259693e-45,1.88082280e-41,NAN},
   {-2.80259693e-45,3.76108508e-42,NAN},
   {-2.80259693e-45,7.52497275e-43,NAN},
   {-2.80259693e-45,1.49938936e-43,NAN},
   {-2.80259693e-45,2.94272678e-44,NAN},
   {-2.80259693e-45,5.60519386e-45,NAN},
   {-2.80259693e-45,1.40129846e-45,NAN},
};
/** function powf_uv__uv_extreme_powf_denom_neg_pos executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_powf_denom_neg_pos() {
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
	for (ind=0;ind<110;ind++) {
		x=powf_uv__uv_extreme_powf_denom_neg_pos_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_powf_denom_neg_pos_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_powf_denom_neg_pos_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_powf_denom_neg_pos: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_powf_denom_neg_pos_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==110) {
		PRINTF("powf_uv__uv_extreme_powf_denom_neg_pos: successfully tested: 110 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_powf_denom_neg_pos: %d tests failed for powf (out of 110)\n",errors);
	}
#endif
	return errors;
}

/**
 * 121 specified tests for powf
 * (-2.938736E-39,-2.938736E-39); (-2.938736E-39,-7.34684E-40); (-2.938736E-39,-1.83671E-40) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_powf_denom_neg_neg in the following table */
powf_uv__uv_extreme_powf_denom_neg_neg_io_table_type powf_uv__uv_extreme_powf_denom_neg_neg_io_table [121] = {
   {-2.93873588e-39,-2.93873588e-39,NAN},
   {-2.93873588e-39,-7.34683969e-40,NAN},
   {-2.93873588e-39,-1.83670992e-40,NAN},
   {-2.93873588e-39,-4.59177481e-41,NAN},
   {-2.93873588e-39,-1.14794370e-41,NAN},
   {-2.93873588e-39,-2.86985925e-42,NAN},
   {-2.93873588e-39,-7.17464814e-43,NAN},
   {-2.93873588e-39,-1.79366203e-43,NAN},
   {-2.93873588e-39,-4.48415509e-44,NAN},
   {-2.93873588e-39,-1.12103877e-44,NAN},
   {-2.93873588e-39,-2.80259693e-45,NAN},
   {-7.34683969e-40,-2.93873588e-39,NAN},
   {-7.34683969e-40,-7.34683969e-40,NAN},
   {-7.34683969e-40,-1.83670992e-40,NAN},
   {-7.34683969e-40,-4.59177481e-41,NAN},
   {-7.34683969e-40,-1.14794370e-41,NAN},
   {-7.34683969e-40,-2.86985925e-42,NAN},
   {-7.34683969e-40,-7.17464814e-43,NAN},
   {-7.34683969e-40,-1.79366203e-43,NAN},
   {-7.34683969e-40,-4.48415509e-44,NAN},
   {-7.34683969e-40,-1.12103877e-44,NAN},
   {-7.34683969e-40,-2.80259693e-45,NAN},
   {-1.83670992e-40,-2.93873588e-39,NAN},
   {-1.83670992e-40,-7.34683969e-40,NAN},
   {-1.83670992e-40,-1.83670992e-40,NAN},
   {-1.83670992e-40,-4.59177481e-41,NAN},
   {-1.83670992e-40,-1.14794370e-41,NAN},
   {-1.83670992e-40,-2.86985925e-42,NAN},
   {-1.83670992e-40,-7.17464814e-43,NAN},
   {-1.83670992e-40,-1.79366203e-43,NAN},
   {-1.83670992e-40,-4.48415509e-44,NAN},
   {-1.83670992e-40,-1.12103877e-44,NAN},
   {-1.83670992e-40,-2.80259693e-45,NAN},
   {-4.59177481e-41,-2.93873588e-39,NAN},
   {-4.59177481e-41,-7.34683969e-40,NAN},
   {-4.59177481e-41,-1.83670992e-40,NAN},
   {-4.59177481e-41,-4.59177481e-41,NAN},
   {-4.59177481e-41,-1.14794370e-41,NAN},
   {-4.59177481e-41,-2.86985925e-42,NAN},
   {-4.59177481e-41,-7.17464814e-43,NAN},
   {-4.59177481e-41,-1.79366203e-43,NAN},
   {-4.59177481e-41,-4.48415509e-44,NAN},
   {-4.59177481e-41,-1.12103877e-44,NAN},
   {-4.59177481e-41,-2.80259693e-45,NAN},
   {-1.14794370e-41,-2.93873588e-39,NAN},
   {-1.14794370e-41,-7.34683969e-40,NAN},
   {-1.14794370e-41,-1.83670992e-40,NAN},
   {-1.14794370e-41,-4.59177481e-41,NAN},
   {-1.14794370e-41,-1.14794370e-41,NAN},
   {-1.14794370e-41,-2.86985925e-42,NAN},
   {-1.14794370e-41,-7.17464814e-43,NAN},
   {-1.14794370e-41,-1.79366203e-43,NAN},
   {-1.14794370e-41,-4.48415509e-44,NAN},
   {-1.14794370e-41,-1.12103877e-44,NAN},
   {-1.14794370e-41,-2.80259693e-45,NAN},
   {-2.86985925e-42,-2.93873588e-39,NAN},
   {-2.86985925e-42,-7.34683969e-40,NAN},
   {-2.86985925e-42,-1.83670992e-40,NAN},
   {-2.86985925e-42,-4.59177481e-41,NAN},
   {-2.86985925e-42,-1.14794370e-41,NAN},
   {-2.86985925e-42,-2.86985925e-42,NAN},
   {-2.86985925e-42,-7.17464814e-43,NAN},
   {-2.86985925e-42,-1.79366203e-43,NAN},
   {-2.86985925e-42,-4.48415509e-44,NAN},
   {-2.86985925e-42,-1.12103877e-44,NAN},
   {-2.86985925e-42,-2.80259693e-45,NAN},
   {-7.17464814e-43,-2.93873588e-39,NAN},
   {-7.17464814e-43,-7.34683969e-40,NAN},
   {-7.17464814e-43,-1.83670992e-40,NAN},
   {-7.17464814e-43,-4.59177481e-41,NAN},
   {-7.17464814e-43,-1.14794370e-41,NAN},
   {-7.17464814e-43,-2.86985925e-42,NAN},
   {-7.17464814e-43,-7.17464814e-43,NAN},
   {-7.17464814e-43,-1.79366203e-43,NAN},
   {-7.17464814e-43,-4.48415509e-44,NAN},
   {-7.17464814e-43,-1.12103877e-44,NAN},
   {-7.17464814e-43,-2.80259693e-45,NAN},
   {-1.79366203e-43,-2.93873588e-39,NAN},
   {-1.79366203e-43,-7.34683969e-40,NAN},
   {-1.79366203e-43,-1.83670992e-40,NAN},
   {-1.79366203e-43,-4.59177481e-41,NAN},
   {-1.79366203e-43,-1.14794370e-41,NAN},
   {-1.79366203e-43,-2.86985925e-42,NAN},
   {-1.79366203e-43,-7.17464814e-43,NAN},
   {-1.79366203e-43,-1.79366203e-43,NAN},
   {-1.79366203e-43,-4.48415509e-44,NAN},
   {-1.79366203e-43,-1.12103877e-44,NAN},
   {-1.79366203e-43,-2.80259693e-45,NAN},
   {-4.48415509e-44,-2.93873588e-39,NAN},
   {-4.48415509e-44,-7.34683969e-40,NAN},
   {-4.48415509e-44,-1.83670992e-40,NAN},
   {-4.48415509e-44,-4.59177481e-41,NAN},
   {-4.48415509e-44,-1.14794370e-41,NAN},
   {-4.48415509e-44,-2.86985925e-42,NAN},
   {-4.48415509e-44,-7.17464814e-43,NAN},
   {-4.48415509e-44,-1.79366203e-43,NAN},
   {-4.48415509e-44,-4.48415509e-44,NAN},
   {-4.48415509e-44,-1.12103877e-44,NAN},
   {-4.48415509e-44,-2.80259693e-45,NAN},
   {-1.12103877e-44,-2.93873588e-39,NAN},
   {-1.12103877e-44,-7.34683969e-40,NAN},
   {-1.12103877e-44,-1.83670992e-40,NAN},
   {-1.12103877e-44,-4.59177481e-41,NAN},
   {-1.12103877e-44,-1.14794370e-41,NAN},
   {-1.12103877e-44,-2.86985925e-42,NAN},
   {-1.12103877e-44,-7.17464814e-43,NAN},
   {-1.12103877e-44,-1.79366203e-43,NAN},
   {-1.12103877e-44,-4.48415509e-44,NAN},
   {-1.12103877e-44,-1.12103877e-44,NAN},
   {-1.12103877e-44,-2.80259693e-45,NAN},
   {-2.80259693e-45,-2.93873588e-39,NAN},
   {-2.80259693e-45,-7.34683969e-40,NAN},
   {-2.80259693e-45,-1.83670992e-40,NAN},
   {-2.80259693e-45,-4.59177481e-41,NAN},
   {-2.80259693e-45,-1.14794370e-41,NAN},
   {-2.80259693e-45,-2.86985925e-42,NAN},
   {-2.80259693e-45,-7.17464814e-43,NAN},
   {-2.80259693e-45,-1.79366203e-43,NAN},
   {-2.80259693e-45,-4.48415509e-44,NAN},
   {-2.80259693e-45,-1.12103877e-44,NAN},
   {-2.80259693e-45,-2.80259693e-45,NAN},
};
/** function powf_uv__uv_extreme_powf_denom_neg_neg executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_powf_denom_neg_neg() {
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
	for (ind=0;ind<121;ind++) {
		x=powf_uv__uv_extreme_powf_denom_neg_neg_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_powf_denom_neg_neg_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_powf_denom_neg_neg_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_powf_denom_neg_neg: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_powf_denom_neg_neg_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==121) {
		PRINTF("powf_uv__uv_extreme_powf_denom_neg_neg: successfully tested: 121 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_powf_denom_neg_neg: %d tests failed for powf (out of 121)\n",errors);
	}
#endif
	return errors;
}

/**
 * 1 specified tests for powf
 * (0,0)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_zero in the following table */
powf_uv__uv_extreme_zero_io_table_type powf_uv__uv_extreme_zero_io_table [1] = {
   {0,0,1.00000000},
};
/** function powf_uv__uv_extreme_zero executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_zero() {
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
	for (ind=0;ind<1;ind++) {
		x=powf_uv__uv_extreme_zero_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_zero_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_zero_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_zero: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_zero_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==1) {
		PRINTF("powf_uv__uv_extreme_zero: successfully tested: 1 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_zero: %d tests failed for powf (out of 1)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (0,1) to (2,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_random_small_squares in the following table */
powf_uv__uv_extreme_random_small_squares_io_table_type powf_uv__uv_extreme_random_small_squares_io_table [50] = {
   {0.126683116,1.18274248,0.0868455023},
   {1.17779779,1.85797882,1.35533893},
   {0.511047244,1.93090332,0.273568749},
   {1.31731033,1.08507037,1.34855914},
   {1.73886549,1.62062621,2.45121479},
   {1.37087822,1.31880927,1.51591682},
   {1.93188357,1.41076303,2.53192806},
   {0.306750894,1.95084810,0.0997234136},
   {0.0630822182,1.94464636,0.00463706767},
   {1.96869230,1.81973243,3.43024349},
   {0.371038795,1.50435650,0.225036591},
   {1.01899743,1.23497462,1.02351344},
   {1.63760769,1.11481512,1.73302329},
   {1.31083500,1.26018167,1.40647459},
   {1.80331016,1.75226402,2.80997849},
   {1.94569683,1.65088964,3.00075960},
   {1.58361626,1.55969131,2.04829407},
   {0.395405173,1.27584398,0.306118041},
   {0.316141725,1.16734302,0.260729730},
   {1.69972372,1.05974770,1.75445771},
   {1.48125386,1.94261265,2.14519620},
   {0.985858202,1.68677950,0.976261914},
   {0.245508552,1.41146660,0.137752473},
   {1.52195919,1.91641808,2.23645639},
   {1.30880749,1.69239902,1.57688653},
   {1.87136281,1.25835824,2.20025110},
   {1.40349269,1.63109589,1.73825657},
   {1.14717078,1.50677490,1.22983289},
   {1.38936770,1.31503558,1.54102504},
   {1.09859633,1.77419233,1.18155718},
   {0.186675787,1.75579309,0.0525026619},
   {1.04321682,1.92320192,1.08477092},
   {1.24446094,1.05460358,1.25941133},
   {1.27611232,1.85480523,1.57182169},
   {1.09079719,1.19366670,1.10931218},
   {1.35224819,1.86294174,1.75448823},
   {1.27170372,1.42973495,1.41008151},
   {0.898119330,1.04661083,0.893632412},
   {1.95596039,1.89387083,3.56285644},
   {0.739534855,1.62785745,0.611904085},
   {0.243610740,1.53612411,0.114258781},
   {1.04271162,1.45283365,1.06264842},
   {0.127993703,1.68866754,0.0310697947},
   {0.794999242,1.15437317,0.767336786},
   {0.767265916,1.77613783,0.624666274},
   {1.77029836,1.58133054,2.46742201},
   {1.64540195,1.67618608,2.30415726},
   {1.63448787,1.50880504,2.09870505},
   {0.0496439934,1.85332227,0.00382843008},
   {0.0811768770,1.14384151,0.0565670393},
};
/** function powf_uv__uv_extreme_random_small_squares executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_random_small_squares() {
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
	for (ind=0;ind<50;ind++) {
		x=powf_uv__uv_extreme_random_small_squares_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_random_small_squares_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_random_small_squares_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_random_small_squares: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_random_small_squares_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_extreme_random_small_squares: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_random_small_squares: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (-1,0) to (2,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_random_small_neg_sqr in the following table */
powf_uv__uv_extreme_random_small_neg_sqr_io_table_type powf_uv__uv_extreme_random_small_neg_sqr_io_table [50] = {
   {1.32151020,0.319813013,1.09325111},
   {1.86840487,0.0919786692,1.05917943},
   {-0.128576756,0.905229211,NAN},
   {1.08035254,0.514031887,1.04052794},
   {-0.325699449,0.644036531,NAN},
   {1.37480319,0.696115971,1.24804842},
   {0.0176184773,0.0326462984,0.876470625},
   {0.662456214,1.73526776,0.489395320},
   {-0.0443988442,0.750385046,NAN},
   {-0.336321712,0.270821571,NAN},
   {-0.661638439,0.842899323,NAN},
   {-0.649422944,0.505520463,NAN},
   {-0.826814950,0.821877122,NAN},
   {1.58394325,1.14675009,1.69453847},
   {-0.404279768,0.331364632,NAN},
   {-0.318739653,0.260280967,NAN},
   {0.911618233,1.08519256,0.904460013},
   {1.76392317,1.57148254,2.43971157},
   {1.51570487,1.75479949,2.07463884},
   {0.257031918,0.509936571,0.500185192},
   {-0.795697570,0.454303980,NAN},
   {0.169042945,1.86061573,0.0366099514},
   {-0.913020253,1.15855992,NAN},
   {0.373479128,0.752445459,0.476599693},
   {-0.231235445,1.68488920,NAN},
   {-0.289992273,1.78352523,NAN},
   {0.426600814,0.311239004,0.767093539},
   {-0.226466477,0.158192515,NAN},
   {0.230282962,1.38992989,0.129893854},
   {0.770467699,1.00735068,0.768992305},
   {-0.934100091,1.80044687,NAN},
   {0.0908114910,0.699205995,0.186864078},
   {1.68843627,1.27666295,1.95173681},
   {1.61983013,0.448141456,1.24128628},
   {-0.838999331,1.70947194,NAN},
   {0.437428713,1.32655573,0.333920836},
   {0.304397404,0.787319779,0.392015129},
   {1.11732185,0.715989232,1.08266759},
   {-0.122429848,1.77307796,NAN},
   {-0.670994520,0.0438935757,NAN},
   {1.50379527,0.960852981,1.47996795},
   {1.89757323,0.491370201,1.36993027},
   {0.0382755995,0.201706290,0.517805278},
   {-0.279361963,1.28173971,NAN},
   {-0.0993004441,0.780115366,NAN},
   {1.35593545,0.632802725,1.21249855},
   {0.0540355444,0.466869473,0.256051153},
   {-0.647589028,1.84625638,NAN},
   {1.15278566,0.528723001,1.07807231},
   {0.223414898,1.09132493,0.194836289},
};
/** function powf_uv__uv_extreme_random_small_neg_sqr executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_random_small_neg_sqr() {
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
	for (ind=0;ind<50;ind++) {
		x=powf_uv__uv_extreme_random_small_neg_sqr_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_random_small_neg_sqr_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_random_small_neg_sqr_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_random_small_neg_sqr: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_random_small_neg_sqr_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_extreme_random_small_neg_sqr: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_random_small_neg_sqr: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (-10,-10) to (-1,0)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_random_negative in the following table */
powf_uv__uv_extreme_random_negative_io_table_type powf_uv__uv_extreme_random_negative_io_table [50] = {
   {-9.44044304,-8.71731567,NAN},
   {-4.38524246,-0.510536194,NAN},
   {-8.03208542,-1.22950745,NAN},
   {-2.99037695,-3.52409935,NAN},
   {-3.07160997,-7.31084585,NAN},
   {-6.06729221,-0.452881813,NAN},
   {-3.16355610,-8.41022301,NAN},
   {-5.90000629,-7.15753508,NAN},
   {-5.34923410,-3.97318745,NAN},
   {-8.62454796,-8.20225143,NAN},
   {-6.33191586,-1.43622303,NAN},
   {-1.67166710,-3.19694757,NAN},
   {-8.49753094,-4.26469612,NAN},
   {-2.63607121,-9.81319809,NAN},
   {-5.28005981,-8.33786869,NAN},
   {-3.65412045,-7.05916405,NAN},
   {-9.73518276,-6.37324238,NAN},
   {-2.21667862,-3.67695904,NAN},
   {-9.45322514,-6.46047497,NAN},
   {-4.10403156,-3.85084152,NAN},
   {-1.92759609,-1.74963474,NAN},
   {-7.59354019,-9.56560135,NAN},
   {-3.44678879,-5.73586655,NAN},
   {-2.90551949,-2.67961979,NAN},
   {-4.98009968,-4.94741249,NAN},
   {-2.27542210,-7.29304695,NAN},
   {-1.84710312,-0.940237999,NAN},
   {-5.68713236,-9.48397446,NAN},
   {-9.40098858,-5.81243610,NAN},
   {-8.89851856,-7.60864019,NAN},
   {-3.20236397,-3.67655039,NAN},
   {-6.52129316,-5.73864937,NAN},
   {-7.13426304,-0.0876598358,NAN},
   {-5.77718735,-9.53071499,NAN},
   {-8.51584721,-6.95834637,NAN},
   {-4.69494629,-9.95308971,NAN},
   {-3.15152025,-3.35586023,NAN},
   {-1.60092640,-3.35692215,NAN},
   {-2.52781677,-8.34805584,NAN},
   {-6.31261492,-6.87833786,NAN},
   {-6.40085888,-8.03805256,NAN},
   {-5.60083866,-7.05485725,NAN},
   {-1.14521790,-1.61652470,NAN},
   {-8.73426628,-5.77339745,NAN},
   {-4.53784561,-3.89771366,NAN},
   {-3.85936260,-5.64085197,NAN},
   {-9.68976212,-1.84167099,NAN},
   {-6.06036711,-3.95515013,NAN},
   {-4.40942192,-9.17086506,NAN},
   {-9.25641060,-8.49656677,NAN},
};
/** function powf_uv__uv_extreme_random_negative executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_random_negative() {
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
	for (ind=0;ind<50;ind++) {
		x=powf_uv__uv_extreme_random_negative_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_random_negative_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_random_negative_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_random_negative: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_random_negative_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_extreme_random_negative: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_random_negative: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 linear tests for powf
 * range: (1000,2) to (100000,10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_big_powers in the following table */
powf_uv__uv_extreme_big_powers_io_table_type powf_uv__uv_extreme_big_powers_io_table [49] = {
   {1000.00000,2.00000000,1000000.00},
   {1000.00000,3.33333325,9.99999488e+09},
   {1000.00000,4.66666651,9.99998913e+13},
   {1000.00000,6.00000000,9.99999984e+17},
   {1000.00000,7.33333349,1.00000110e+22},
   {1000.00000,8.66666698,1.00000224e+26},
   {1000.00000,10.0000000,1.00000002e+30},
   {17500.0000,2.00000000,306249984.0},
   {17500.0000,3.33333325,1.39142504e+14},
   {17500.0000,4.66666651,6.32184090e+19},
   {17500.0000,6.00000000,2.87228995e+25},
   {17500.0000,7.33333349,1.30500762e+31},
   {17500.0000,8.66666698,5.92922272e+36},
   {17500.0000,10.0000000,INFINITY},
   {34000.0000,2.00000000,1.15600000e+09},
   {34000.0000,3.33333325,1.27329594e+15},
   {34000.0000,4.66666651,1.40249354e+21},
   {34000.0000,6.00000000,1.54480443e+27},
   {34000.0000,7.33333349,1.70155555e+33},
   {34000.0000,8.66666698,INFINITY},
   {34000.0000,10.0000000,INFINITY},
   {50500.0000,2.00000000,2.55024998e+09},
   {50500.0000,3.33333325,4.76033559e+15},
   {50500.0000,4.66666651,8.88571465e+21},
   {50500.0000,6.00000000,1.65862521e+28},
   {50500.0000,7.33333349,3.09602317e+34},
   {50500.0000,8.66666698,INFINITY},
   {50500.0000,10.0000000,INFINITY},
   {67000.0000,2.00000000,4.48899994e+09},
   {67000.0000,3.33333325,1.22156236e+16},
   {67000.0000,4.66666651,3.32415800e+22},
   {67000.0000,6.00000000,9.04583845e+28},
   {67000.0000,7.33333349,2.46159148e+35},
   {67000.0000,8.66666698,INFINITY},
   {67000.0000,10.0000000,INFINITY},
   {83500.0000,2.00000000,6.97225011e+09},
   {83500.0000,3.33333325,2.54460985e+16},
   {83500.0000,4.66666651,9.28687189e+22},
   {83500.0000,6.00000000,3.38936898e+29},
   {83500.0000,7.33333349,1.23699596e+36},
   {83500.0000,8.66666698,INFINITY},
   {83500.0000,10.0000000,INFINITY},
   {100000.000,2.00000000,1.00000000e+10},
   {100000.000,3.33333325,4.64158447e+16},
   {100000.000,4.66666651,2.15443073e+23},
   {100000.000,6.00000000,1.00000002e+30},
   {100000.000,7.33333349,4.64159743e+36},
   {100000.000,8.66666698,INFINITY},
   {100000.000,10.0000000,INFINITY},
};
/** function powf_uv__uv_extreme_big_powers executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_big_powers() {
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
		x=powf_uv__uv_extreme_big_powers_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_big_powers_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_big_powers_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_big_powers: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_big_powers_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("powf_uv__uv_extreme_big_powers: successfully tested: 49 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_big_powers: %d tests failed for powf (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 linear tests for powf
 * range: (1000,1) to (100000,1000)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_overflowing_powers in the following table */
powf_uv__uv_extreme_overflowing_powers_io_table_type powf_uv__uv_extreme_overflowing_powers_io_table [49] = {
   {1000.00000,1.00000000,1000.00000},
   {1000.00000,167.500000,INFINITY},
   {1000.00000,334.000000,INFINITY},
   {1000.00000,500.500000,INFINITY},
   {1000.00000,667.000000,INFINITY},
   {1000.00000,833.500000,INFINITY},
   {1000.00000,1000.00000,INFINITY},
   {17500.0000,1.00000000,17500.0000},
   {17500.0000,167.500000,INFINITY},
   {17500.0000,334.000000,INFINITY},
   {17500.0000,500.500000,INFINITY},
   {17500.0000,667.000000,INFINITY},
   {17500.0000,833.500000,INFINITY},
   {17500.0000,1000.00000,INFINITY},
   {34000.0000,1.00000000,34000.0000},
   {34000.0000,167.500000,INFINITY},
   {34000.0000,334.000000,INFINITY},
   {34000.0000,500.500000,INFINITY},
   {34000.0000,667.000000,INFINITY},
   {34000.0000,833.500000,INFINITY},
   {34000.0000,1000.00000,INFINITY},
   {50500.0000,1.00000000,50500.0000},
   {50500.0000,167.500000,INFINITY},
   {50500.0000,334.000000,INFINITY},
   {50500.0000,500.500000,INFINITY},
   {50500.0000,667.000000,INFINITY},
   {50500.0000,833.500000,INFINITY},
   {50500.0000,1000.00000,INFINITY},
   {67000.0000,1.00000000,67000.0000},
   {67000.0000,167.500000,INFINITY},
   {67000.0000,334.000000,INFINITY},
   {67000.0000,500.500000,INFINITY},
   {67000.0000,667.000000,INFINITY},
   {67000.0000,833.500000,INFINITY},
   {67000.0000,1000.00000,INFINITY},
   {83500.0000,1.00000000,83500.0000},
   {83500.0000,167.500000,INFINITY},
   {83500.0000,334.000000,INFINITY},
   {83500.0000,500.500000,INFINITY},
   {83500.0000,667.000000,INFINITY},
   {83500.0000,833.500000,INFINITY},
   {83500.0000,1000.00000,INFINITY},
   {100000.000,1.00000000,100000.000},
   {100000.000,167.500000,INFINITY},
   {100000.000,334.000000,INFINITY},
   {100000.000,500.500000,INFINITY},
   {100000.000,667.000000,INFINITY},
   {100000.000,833.500000,INFINITY},
   {100000.000,1000.00000,INFINITY},
};
/** function powf_uv__uv_extreme_overflowing_powers executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_overflowing_powers() {
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
		x=powf_uv__uv_extreme_overflowing_powers_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_overflowing_powers_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_overflowing_powers_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_overflowing_powers: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_overflowing_powers_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("powf_uv__uv_extreme_overflowing_powers: successfully tested: 49 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_overflowing_powers: %d tests failed for powf (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for powf
 * range: (1000,1024) to (100000,10000)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_overflowing_powers_pos in the following table */
powf_uv__uv_extreme_overflowing_powers_pos_io_table_type powf_uv__uv_extreme_overflowing_powers_pos_io_table [64] = {
   {1000.00000,1024.00000,INFINITY},
   {1000.00000,2306.28564,INFINITY},
   {1000.00000,3588.57153,INFINITY},
   {1000.00000,4870.85693,INFINITY},
   {1000.00000,6153.14307,INFINITY},
   {1000.00000,7435.42871,INFINITY},
   {1000.00000,8717.71387,INFINITY},
   {1000.00000,10000.0000,INFINITY},
   {15142.8574,1024.00000,INFINITY},
   {15142.8574,2306.28564,INFINITY},
   {15142.8574,3588.57153,INFINITY},
   {15142.8574,4870.85693,INFINITY},
   {15142.8574,6153.14307,INFINITY},
   {15142.8574,7435.42871,INFINITY},
   {15142.8574,8717.71387,INFINITY},
   {15142.8574,10000.0000,INFINITY},
   {29285.7148,1024.00000,INFINITY},
   {29285.7148,2306.28564,INFINITY},
   {29285.7148,3588.57153,INFINITY},
   {29285.7148,4870.85693,INFINITY},
   {29285.7148,6153.14307,INFINITY},
   {29285.7148,7435.42871,INFINITY},
   {29285.7148,8717.71387,INFINITY},
   {29285.7148,10000.0000,INFINITY},
   {43428.5703,1024.00000,INFINITY},
   {43428.5703,2306.28564,INFINITY},
   {43428.5703,3588.57153,INFINITY},
   {43428.5703,4870.85693,INFINITY},
   {43428.5703,6153.14307,INFINITY},
   {43428.5703,7435.42871,INFINITY},
   {43428.5703,8717.71387,INFINITY},
   {43428.5703,10000.0000,INFINITY},
   {57571.4297,1024.00000,INFINITY},
   {57571.4297,2306.28564,INFINITY},
   {57571.4297,3588.57153,INFINITY},
   {57571.4297,4870.85693,INFINITY},
   {57571.4297,6153.14307,INFINITY},
   {57571.4297,7435.42871,INFINITY},
   {57571.4297,8717.71387,INFINITY},
   {57571.4297,10000.0000,INFINITY},
   {71714.2891,1024.00000,INFINITY},
   {71714.2891,2306.28564,INFINITY},
   {71714.2891,3588.57153,INFINITY},
   {71714.2891,4870.85693,INFINITY},
   {71714.2891,6153.14307,INFINITY},
   {71714.2891,7435.42871,INFINITY},
   {71714.2891,8717.71387,INFINITY},
   {71714.2891,10000.0000,INFINITY},
   {85857.1406,1024.00000,INFINITY},
   {85857.1406,2306.28564,INFINITY},
   {85857.1406,3588.57153,INFINITY},
   {85857.1406,4870.85693,INFINITY},
   {85857.1406,6153.14307,INFINITY},
   {85857.1406,7435.42871,INFINITY},
   {85857.1406,8717.71387,INFINITY},
   {85857.1406,10000.0000,INFINITY},
   {100000.000,1024.00000,INFINITY},
   {100000.000,2306.28564,INFINITY},
   {100000.000,3588.57153,INFINITY},
   {100000.000,4870.85693,INFINITY},
   {100000.000,6153.14307,INFINITY},
   {100000.000,7435.42871,INFINITY},
   {100000.000,8717.71387,INFINITY},
   {100000.000,10000.0000,INFINITY},
};
/** function powf_uv__uv_extreme_overflowing_powers_pos executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_overflowing_powers_pos() {
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
	for (ind=0;ind<64;ind++) {
		x=powf_uv__uv_extreme_overflowing_powers_pos_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_overflowing_powers_pos_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_overflowing_powers_pos_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_overflowing_powers_pos: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_overflowing_powers_pos_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("powf_uv__uv_extreme_overflowing_powers_pos: successfully tested: 64 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_overflowing_powers_pos: %d tests failed for powf (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for powf
 * range: (-100000,1024) to (1000,10000)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_overflowing_powers_neg in the following table */
powf_uv__uv_extreme_overflowing_powers_neg_io_table_type powf_uv__uv_extreme_overflowing_powers_neg_io_table [64] = {
   {-100000.000,1024.00000,INFINITY},
   {-100000.000,2306.28564,NAN},
   {-100000.000,3588.57153,NAN},
   {-100000.000,4870.85693,NAN},
   {-100000.000,6153.14307,NAN},
   {-100000.000,7435.42871,NAN},
   {-100000.000,8717.71387,NAN},
   {-100000.000,10000.0000,INFINITY},
   {-85571.4297,1024.00000,INFINITY},
   {-85571.4297,2306.28564,NAN},
   {-85571.4297,3588.57153,NAN},
   {-85571.4297,4870.85693,NAN},
   {-85571.4297,6153.14307,NAN},
   {-85571.4297,7435.42871,NAN},
   {-85571.4297,8717.71387,NAN},
   {-85571.4297,10000.0000,INFINITY},
   {-71142.8594,1024.00000,INFINITY},
   {-71142.8594,2306.28564,NAN},
   {-71142.8594,3588.57153,NAN},
   {-71142.8594,4870.85693,NAN},
   {-71142.8594,6153.14307,NAN},
   {-71142.8594,7435.42871,NAN},
   {-71142.8594,8717.71387,NAN},
   {-71142.8594,10000.0000,INFINITY},
   {-56714.2852,1024.00000,INFINITY},
   {-56714.2852,2306.28564,NAN},
   {-56714.2852,3588.57153,NAN},
   {-56714.2852,4870.85693,NAN},
   {-56714.2852,6153.14307,NAN},
   {-56714.2852,7435.42871,NAN},
   {-56714.2852,8717.71387,NAN},
   {-56714.2852,10000.0000,INFINITY},
   {-42285.7148,1024.00000,INFINITY},
   {-42285.7148,2306.28564,NAN},
   {-42285.7148,3588.57153,NAN},
   {-42285.7148,4870.85693,NAN},
   {-42285.7148,6153.14307,NAN},
   {-42285.7148,7435.42871,NAN},
   {-42285.7148,8717.71387,NAN},
   {-42285.7148,10000.0000,INFINITY},
   {-27857.1426,1024.00000,INFINITY},
   {-27857.1426,2306.28564,NAN},
   {-27857.1426,3588.57153,NAN},
   {-27857.1426,4870.85693,NAN},
   {-27857.1426,6153.14307,NAN},
   {-27857.1426,7435.42871,NAN},
   {-27857.1426,8717.71387,NAN},
   {-27857.1426,10000.0000,INFINITY},
   {-13428.5713,1024.00000,INFINITY},
   {-13428.5713,2306.28564,NAN},
   {-13428.5713,3588.57153,NAN},
   {-13428.5713,4870.85693,NAN},
   {-13428.5713,6153.14307,NAN},
   {-13428.5713,7435.42871,NAN},
   {-13428.5713,8717.71387,NAN},
   {-13428.5713,10000.0000,INFINITY},
   {1000.00000,1024.00000,INFINITY},
   {1000.00000,2306.28564,INFINITY},
   {1000.00000,3588.57153,INFINITY},
   {1000.00000,4870.85693,INFINITY},
   {1000.00000,6153.14307,INFINITY},
   {1000.00000,7435.42871,INFINITY},
   {1000.00000,8717.71387,INFINITY},
   {1000.00000,10000.0000,INFINITY},
};
/** function powf_uv__uv_extreme_overflowing_powers_neg executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_overflowing_powers_neg() {
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
	for (ind=0;ind<64;ind++) {
		x=powf_uv__uv_extreme_overflowing_powers_neg_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_overflowing_powers_neg_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_overflowing_powers_neg_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_overflowing_powers_neg: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_overflowing_powers_neg_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("powf_uv__uv_extreme_overflowing_powers_neg: successfully tested: 64 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_overflowing_powers_neg: %d tests failed for powf (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for powf
 * range: (0.00001,2) to (0.001,256)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_undeflowing_powers in the following table */
powf_uv__uv_extreme_undeflowing_powers_io_table_type powf_uv__uv_extreme_undeflowing_powers_io_table [64] = {
   {9.99999975e-06,2.00000000,9.99999944e-11},
   {9.99999975e-06,38.2857132,0.0},
   {9.99999975e-06,74.5714264,0.0},
   {9.99999975e-06,110.857140,0.0},
   {9.99999975e-06,147.142853,0.0},
   {9.99999975e-06,183.428574,0.0},
   {9.99999975e-06,219.714279,0.0},
   {9.99999975e-06,256.000000,0.0},
   {0.000151428583,2.00000000,2.29306156e-08},
   {0.000151428583,38.2857132,0.0},
   {0.000151428583,74.5714264,0.0},
   {0.000151428583,110.857140,0.0},
   {0.000151428583,147.142853,0.0},
   {0.000151428583,183.428574,0.0},
   {0.000151428583,219.714279,0.0},
   {0.000151428583,256.000000,0.0},
   {0.000292857148,2.00000000,8.57653077e-08},
   {0.000292857148,38.2857132,0.0},
   {0.000292857148,74.5714264,0.0},
   {0.000292857148,110.857140,0.0},
   {0.000292857148,147.142853,0.0},
   {0.000292857148,183.428574,0.0},
   {0.000292857148,219.714279,0.0},
   {0.000292857148,256.000000,0.0},
   {0.000434285728,2.00000000,1.88604091e-07},
   {0.000434285728,38.2857132,0.0},
   {0.000434285728,74.5714264,0.0},
   {0.000434285728,110.857140,0.0},
   {0.000434285728,147.142853,0.0},
   {0.000434285728,183.428574,0.0},
   {0.000434285728,219.714279,0.0},
   {0.000434285728,256.000000,0.0},
   {0.000575714337,2.00000000,3.31447012e-07},
   {0.000575714337,38.2857132,0.0},
   {0.000575714337,74.5714264,0.0},
   {0.000575714337,110.857140,0.0},
   {0.000575714337,147.142853,0.0},
   {0.000575714337,183.428574,0.0},
   {0.000575714337,219.714279,0.0},
   {0.000575714337,256.000000,0.0},
   {0.000717142888,2.00000000,5.14293902e-07},
   {0.000717142888,38.2857132,0.0},
   {0.000717142888,74.5714264,0.0},
   {0.000717142888,110.857140,0.0},
   {0.000717142888,147.142853,0.0},
   {0.000717142888,183.428574,0.0},
   {0.000717142888,219.714279,0.0},
   {0.000717142888,256.000000,0.0},
   {0.000858571497,2.00000000,7.37145001e-07},
   {0.000858571497,38.2857132,0.0},
   {0.000858571497,74.5714264,0.0},
   {0.000858571497,110.857140,0.0},
   {0.000858571497,147.142853,0.0},
   {0.000858571497,183.428574,0.0},
   {0.000858571497,219.714279,0.0},
   {0.000858571497,256.000000,0.0},
   {0.00100000005,2.00000000,1.00000011e-06},
   {0.00100000005,38.2857132,0.0},
   {0.00100000005,74.5714264,0.0},
   {0.00100000005,110.857140,0.0},
   {0.00100000005,147.142853,0.0},
   {0.00100000005,183.428574,0.0},
   {0.00100000005,219.714279,0.0},
   {0.00100000005,256.000000,0.0},
};
/** function powf_uv__uv_extreme_undeflowing_powers executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_undeflowing_powers() {
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
	for (ind=0;ind<64;ind++) {
		x=powf_uv__uv_extreme_undeflowing_powers_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_undeflowing_powers_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_undeflowing_powers_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_undeflowing_powers: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_undeflowing_powers_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("powf_uv__uv_extreme_undeflowing_powers: successfully tested: 64 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_undeflowing_powers: %d tests failed for powf (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for powf
 * range: (0.000000001,2) to (0.00001,1028)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_undeflowing_powers2 in the following table */
powf_uv__uv_extreme_undeflowing_powers2_io_table_type powf_uv__uv_extreme_undeflowing_powers2_io_table [64] = {
   {9.99999972e-10,2.00000000,9.99999942e-19},
   {9.99999972e-10,148.571426,0.0},
   {9.99999972e-10,295.142853,0.0},
   {9.99999972e-10,441.714294,0.0},
   {9.99999972e-10,588.285706,0.0},
   {9.99999972e-10,734.857117,0.0},
   {9.99999972e-10,881.428589,0.0},
   {9.99999972e-10,1028.00000,0.0},
   {1.42942849e-06,2.00000000,2.04326573e-12},
   {1.42942849e-06,148.571426,0.0},
   {1.42942849e-06,295.142853,0.0},
   {1.42942849e-06,441.714294,0.0},
   {1.42942849e-06,588.285706,0.0},
   {1.42942849e-06,734.857117,0.0},
   {1.42942849e-06,881.428589,0.0},
   {1.42942849e-06,1028.00000,0.0},
   {2.85785700e-06,2.00000000,8.16734701e-12},
   {2.85785700e-06,148.571426,0.0},
   {2.85785700e-06,295.142853,0.0},
   {2.85785700e-06,441.714294,0.0},
   {2.85785700e-06,588.285706,0.0},
   {2.85785700e-06,734.857117,0.0},
   {2.85785700e-06,881.428589,0.0},
   {2.85785700e-06,1028.00000,0.0},
   {4.28628573e-06,2.00000000,1.83722447e-11},
   {4.28628573e-06,148.571426,0.0},
   {4.28628573e-06,295.142853,0.0},
   {4.28628573e-06,441.714294,0.0},
   {4.28628573e-06,588.285706,0.0},
   {4.28628573e-06,734.857117,0.0},
   {4.28628573e-06,881.428589,0.0},
   {4.28628573e-06,1028.00000,0.0},
   {5.71471401e-06,2.00000000,3.26579562e-11},
   {5.71471401e-06,148.571426,0.0},
   {5.71471401e-06,295.142853,0.0},
   {5.71471401e-06,441.714294,0.0},
   {5.71471401e-06,588.285706,0.0},
   {5.71471401e-06,734.857117,0.0},
   {5.71471401e-06,881.428589,0.0},
   {5.71471401e-06,1028.00000,0.0},
   {7.14314274e-06,2.00000000,5.10244867e-11},
   {7.14314274e-06,148.571426,0.0},
   {7.14314274e-06,295.142853,0.0},
   {7.14314274e-06,441.714294,0.0},
   {7.14314274e-06,588.285706,0.0},
   {7.14314274e-06,734.857117,0.0},
   {7.14314274e-06,881.428589,0.0},
   {7.14314274e-06,1028.00000,0.0},
   {8.57157102e-06,2.00000000,7.34718328e-11},
   {8.57157102e-06,148.571426,0.0},
   {8.57157102e-06,295.142853,0.0},
   {8.57157102e-06,441.714294,0.0},
   {8.57157102e-06,588.285706,0.0},
   {8.57157102e-06,734.857117,0.0},
   {8.57157102e-06,881.428589,0.0},
   {8.57157102e-06,1028.00000,0.0},
   {9.99999975e-06,2.00000000,9.99999944e-11},
   {9.99999975e-06,148.571426,0.0},
   {9.99999975e-06,295.142853,0.0},
   {9.99999975e-06,441.714294,0.0},
   {9.99999975e-06,588.285706,0.0},
   {9.99999975e-06,734.857117,0.0},
   {9.99999975e-06,881.428589,0.0},
   {9.99999975e-06,1028.00000,0.0},
};
/** function powf_uv__uv_extreme_undeflowing_powers2 executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_undeflowing_powers2() {
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
	for (ind=0;ind<64;ind++) {
		x=powf_uv__uv_extreme_undeflowing_powers2_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_undeflowing_powers2_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_undeflowing_powers2_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_undeflowing_powers2: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_undeflowing_powers2_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("powf_uv__uv_extreme_undeflowing_powers2: successfully tested: 64 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_undeflowing_powers2: %d tests failed for powf (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for powf
 * range: (-0.0001,1) to (-0.000000001,99)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_undeflowing_powers3 in the following table */
powf_uv__uv_extreme_undeflowing_powers3_io_table_type powf_uv__uv_extreme_undeflowing_powers3_io_table [64] = {
   {-9.99999975e-05,1.00000000,-9.99999975e-05},
   {-9.99999975e-05,15.0000000,0.0},
   {-9.99999975e-05,29.0000000,0.0},
   {-9.99999975e-05,43.0000000,0.0},
   {-9.99999975e-05,57.0000000,0.0},
   {-9.99999975e-05,71.0000000,0.0},
   {-9.99999975e-05,85.0000000,0.0},
   {-9.99999975e-05,99.0000000,0.0},
   {-8.57144259e-05,1.00000000,-8.57144259e-05},
   {-8.57144259e-05,15.0000000,0.0},
   {-8.57144259e-05,29.0000000,0.0},
   {-8.57144259e-05,43.0000000,0.0},
   {-8.57144259e-05,57.0000000,0.0},
   {-8.57144259e-05,71.0000000,0.0},
   {-8.57144259e-05,85.0000000,0.0},
   {-8.57144259e-05,99.0000000,0.0},
   {-7.14288544e-05,1.00000000,-7.14288544e-05},
   {-7.14288544e-05,15.0000000,0.0},
   {-7.14288544e-05,29.0000000,0.0},
   {-7.14288544e-05,43.0000000,0.0},
   {-7.14288544e-05,57.0000000,0.0},
   {-7.14288544e-05,71.0000000,0.0},
   {-7.14288544e-05,85.0000000,0.0},
   {-7.14288544e-05,99.0000000,0.0},
   {-5.71432829e-05,1.00000000,-5.71432829e-05},
   {-5.71432829e-05,15.0000000,0.0},
   {-5.71432829e-05,29.0000000,0.0},
   {-5.71432829e-05,43.0000000,0.0},
   {-5.71432829e-05,57.0000000,0.0},
   {-5.71432829e-05,71.0000000,0.0},
   {-5.71432829e-05,85.0000000,0.0},
   {-5.71432829e-05,99.0000000,0.0},
   {-4.28577150e-05,1.00000000,-4.28577150e-05},
   {-4.28577150e-05,15.0000000,0.0},
   {-4.28577150e-05,29.0000000,0.0},
   {-4.28577150e-05,43.0000000,0.0},
   {-4.28577150e-05,57.0000000,0.0},
   {-4.28577150e-05,71.0000000,0.0},
   {-4.28577150e-05,85.0000000,0.0},
   {-4.28577150e-05,99.0000000,0.0},
   {-2.85721417e-05,1.00000000,-2.85721417e-05},
   {-2.85721417e-05,15.0000000,0.0},
   {-2.85721417e-05,29.0000000,0.0},
   {-2.85721417e-05,43.0000000,0.0},
   {-2.85721417e-05,57.0000000,0.0},
   {-2.85721417e-05,71.0000000,0.0},
   {-2.85721417e-05,85.0000000,0.0},
   {-2.85721417e-05,99.0000000,0.0},
   {-1.42865711e-05,1.00000000,-1.42865711e-05},
   {-1.42865711e-05,15.0000000,0.0},
   {-1.42865711e-05,29.0000000,0.0},
   {-1.42865711e-05,43.0000000,0.0},
   {-1.42865711e-05,57.0000000,0.0},
   {-1.42865711e-05,71.0000000,0.0},
   {-1.42865711e-05,85.0000000,0.0},
   {-1.42865711e-05,99.0000000,0.0},
   {-9.99999972e-10,1.00000000,-9.99999972e-10},
   {-9.99999972e-10,15.0000000,0.0},
   {-9.99999972e-10,29.0000000,0.0},
   {-9.99999972e-10,43.0000000,0.0},
   {-9.99999972e-10,57.0000000,0.0},
   {-9.99999972e-10,71.0000000,0.0},
   {-9.99999972e-10,85.0000000,0.0},
   {-9.99999972e-10,99.0000000,0.0},
};
/** function powf_uv__uv_extreme_undeflowing_powers3 executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_undeflowing_powers3() {
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
	for (ind=0;ind<64;ind++) {
		x=powf_uv__uv_extreme_undeflowing_powers3_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_undeflowing_powers3_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_undeflowing_powers3_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_undeflowing_powers3: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_undeflowing_powers3_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("powf_uv__uv_extreme_undeflowing_powers3: successfully tested: 64 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_undeflowing_powers3: %d tests failed for powf (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for powf
 * range: (0.000000001,1000) to (0.00001,100000)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_undeflowing_powers4 in the following table */
powf_uv__uv_extreme_undeflowing_powers4_io_table_type powf_uv__uv_extreme_undeflowing_powers4_io_table [64] = {
   {9.99999972e-10,1000.00000,0.0},
   {9.99999972e-10,15142.8574,0.0},
   {9.99999972e-10,29285.7148,0.0},
   {9.99999972e-10,43428.5703,0.0},
   {9.99999972e-10,57571.4297,0.0},
   {9.99999972e-10,71714.2891,0.0},
   {9.99999972e-10,85857.1406,0.0},
   {9.99999972e-10,100000.000,0.0},
   {1.42942849e-06,1000.00000,0.0},
   {1.42942849e-06,15142.8574,0.0},
   {1.42942849e-06,29285.7148,0.0},
   {1.42942849e-06,43428.5703,0.0},
   {1.42942849e-06,57571.4297,0.0},
   {1.42942849e-06,71714.2891,0.0},
   {1.42942849e-06,85857.1406,0.0},
   {1.42942849e-06,100000.000,0.0},
   {2.85785700e-06,1000.00000,0.0},
   {2.85785700e-06,15142.8574,0.0},
   {2.85785700e-06,29285.7148,0.0},
   {2.85785700e-06,43428.5703,0.0},
   {2.85785700e-06,57571.4297,0.0},
   {2.85785700e-06,71714.2891,0.0},
   {2.85785700e-06,85857.1406,0.0},
   {2.85785700e-06,100000.000,0.0},
   {4.28628573e-06,1000.00000,0.0},
   {4.28628573e-06,15142.8574,0.0},
   {4.28628573e-06,29285.7148,0.0},
   {4.28628573e-06,43428.5703,0.0},
   {4.28628573e-06,57571.4297,0.0},
   {4.28628573e-06,71714.2891,0.0},
   {4.28628573e-06,85857.1406,0.0},
   {4.28628573e-06,100000.000,0.0},
   {5.71471401e-06,1000.00000,0.0},
   {5.71471401e-06,15142.8574,0.0},
   {5.71471401e-06,29285.7148,0.0},
   {5.71471401e-06,43428.5703,0.0},
   {5.71471401e-06,57571.4297,0.0},
   {5.71471401e-06,71714.2891,0.0},
   {5.71471401e-06,85857.1406,0.0},
   {5.71471401e-06,100000.000,0.0},
   {7.14314274e-06,1000.00000,0.0},
   {7.14314274e-06,15142.8574,0.0},
   {7.14314274e-06,29285.7148,0.0},
   {7.14314274e-06,43428.5703,0.0},
   {7.14314274e-06,57571.4297,0.0},
   {7.14314274e-06,71714.2891,0.0},
   {7.14314274e-06,85857.1406,0.0},
   {7.14314274e-06,100000.000,0.0},
   {8.57157102e-06,1000.00000,0.0},
   {8.57157102e-06,15142.8574,0.0},
   {8.57157102e-06,29285.7148,0.0},
   {8.57157102e-06,43428.5703,0.0},
   {8.57157102e-06,57571.4297,0.0},
   {8.57157102e-06,71714.2891,0.0},
   {8.57157102e-06,85857.1406,0.0},
   {8.57157102e-06,100000.000,0.0},
   {9.99999975e-06,1000.00000,0.0},
   {9.99999975e-06,15142.8574,0.0},
   {9.99999975e-06,29285.7148,0.0},
   {9.99999975e-06,43428.5703,0.0},
   {9.99999975e-06,57571.4297,0.0},
   {9.99999975e-06,71714.2891,0.0},
   {9.99999975e-06,85857.1406,0.0},
   {9.99999975e-06,100000.000,0.0},
};
/** function powf_uv__uv_extreme_undeflowing_powers4 executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_undeflowing_powers4() {
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
	for (ind=0;ind<64;ind++) {
		x=powf_uv__uv_extreme_undeflowing_powers4_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_undeflowing_powers4_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_undeflowing_powers4_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_undeflowing_powers4: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_undeflowing_powers4_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("powf_uv__uv_extreme_undeflowing_powers4: successfully tested: 64 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_undeflowing_powers4: %d tests failed for powf (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 25 approximation tests with factors 2.0,2.0
 * range: (0,-2) to (1,-2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_roots_of_minus_two in the following table */
powf_uv__uv_extreme_roots_of_minus_two_io_table_type powf_uv__uv_extreme_roots_of_minus_two_io_table [25] = {
   {0.500000000,-2.00000000,4.00000000},
   {0.750000000,-2.00000000,1.77777779},
   {0.875000000,-2.00000000,1.30612242},
   {0.937500000,-2.00000000,1.13777781},
   {0.968750000,-2.00000000,1.06555676},
   {0.984375000,-2.00000000,1.03199804},
   {0.992187500,-2.00000000,1.01581001},
   {0.996093750,-2.00000000,1.00785851},
   {0.998046875,-2.00000000,1.00391769},
   {0.999023438,-2.00000000,1.00195599},
   {0.999511719,-2.00000000,1.00097728},
   {0.999755859,-2.00000000,1.00048852},
   {0.999877930,-2.00000000,1.00024414},
   {0.999938965,-2.00000000,1.00012207},
   {0.999969482,-2.00000000,1.00006104},
   {0.999984741,-2.00000000,1.00003052},
   {0.999992371,-2.00000000,1.00001526},
   {0.999996185,-2.00000000,1.00000763},
   {0.999998093,-2.00000000,1.00000381},
   {0.999999046,-2.00000000,1.00000191},
   {0.999999523,-2.00000000,1.00000095},
   {0.999999762,-2.00000000,1.00000048},
   {0.999999881,-2.00000000,1.00000024},
   {0.999999940,-2.00000000,1.00000012},
   {1.00000000,-2.00000000,1.00000000},
};
/** function powf_uv__uv_extreme_roots_of_minus_two executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_roots_of_minus_two() {
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
	for (ind=0;ind<25;ind++) {
		x=powf_uv__uv_extreme_roots_of_minus_two_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_roots_of_minus_two_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_roots_of_minus_two_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_roots_of_minus_two: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_roots_of_minus_two_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==25) {
		PRINTF("powf_uv__uv_extreme_roots_of_minus_two: successfully tested: 25 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_roots_of_minus_two: %d tests failed for powf (out of 25)\n",errors);
	}
#endif
	return errors;
}

/**
 * 25 approximation tests with factors 2.0,2.0
 * range: (0,0) to (1,-10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_roots_to_minus_ten in the following table */
powf_uv__uv_extreme_roots_to_minus_ten_io_table_type powf_uv__uv_extreme_roots_to_minus_ten_io_table [25] = {
   {0.500000000,-5.00000000,32.0000000},
   {0.750000000,-7.50000000,8.65048599},
   {0.875000000,-8.75000000,3.21684051},
   {0.937500000,-9.37500000,1.83134127},
   {0.968750000,-9.68750000,1.36010993},
   {0.984375000,-9.84375000,1.16768467},
   {0.992187500,-9.92187500,1.08092701},
   {0.996093750,-9.96093750,1.03975606},
   {0.998046875,-9.98046875,1.01970375},
   {0.999023438,-9.99023438,1.00980866},
   {0.999511719,-9.99511719,1.00489354},
   {0.999755859,-9.99755859,1.00244415},
   {0.999877930,-9.99877930,1.00122142},
   {0.999938965,-9.99938965,1.00061047},
   {0.999969482,-9.99969482,1.00030518},
   {0.999984741,-9.99984741,1.00015259},
   {0.999992371,-9.99992371,1.00007629},
   {0.999996185,-9.99996185,1.00003815},
   {0.999998093,-9.99998093,1.00001907},
   {0.999999046,-9.99999046,1.00000954},
   {0.999999523,-9.99999523,1.00000477},
   {0.999999762,-9.99999809,1.00000238},
   {0.999999881,-9.99999905,1.00000119},
   {0.999999940,-10.0000000,1.00000060},
   {1.00000000,-10.0000000,1.00000000},
};
/** function powf_uv__uv_extreme_roots_to_minus_ten executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_roots_to_minus_ten() {
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
	for (ind=0;ind<25;ind++) {
		x=powf_uv__uv_extreme_roots_to_minus_ten_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_roots_to_minus_ten_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_roots_to_minus_ten_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_roots_to_minus_ten: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_roots_to_minus_ten_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==25) {
		PRINTF("powf_uv__uv_extreme_roots_to_minus_ten: successfully tested: 25 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_roots_to_minus_ten: %d tests failed for powf (out of 25)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 (1 omitted out of input range) specified tests for powf
 * (2,21); (2,22); (2,23); (2,24); (2,25); (2,24); (2,27); (2,28); (2,29); (2,30); (2,31) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_big_powers2 in the following table */
powf_uv__uv_extreme_big_powers2_io_table_type powf_uv__uv_extreme_big_powers2_io_table [20] = {
   {2.00000000,21.0000000,2097152.00},
   {2.00000000,22.0000000,4194304.00},
   {2.00000000,23.0000000,8388608.00},
   {2.00000000,24.0000000,16777216.0},
   {2.00000000,25.0000000,33554432.0},
   {2.00000000,27.0000000,134217728.0},
   {2.00000000,28.0000000,268435456.0},
   {2.00000000,29.0000000,536870912.0},
   {2.00000000,30.0000000,1.07374182e+09},
   {2.00000000,31.0000000,2.14748365e+09},
   {2.00000000,32.0000000,4.29496730e+09},
   {2.00000000,40.0000000,1.09951163e+12},
   {2.00000000,50.0000000,1.12589991e+15},
   {2.00000000,60.0000000,1.15292150e+18},
   {2.00000000,70.0000000,1.18059162e+21},
   {2.00000000,80.0000000,1.20892582e+24},
   {2.00000000,90.0000000,1.23794004e+27},
   {2.00000000,100.000000,1.26765060e+30},
   {2.00000000,200.000000,INFINITY},
   {2.00000000,300.000000,INFINITY},
};
/** function powf_uv__uv_extreme_big_powers2 executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_big_powers2() {
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
	for (ind=0;ind<20;ind++) {
		x=powf_uv__uv_extreme_big_powers2_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_big_powers2_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_big_powers2_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_big_powers2: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_big_powers2_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("powf_uv__uv_extreme_big_powers2: successfully tested: 20 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_big_powers2: %d tests failed for powf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 18 specified tests for powf
 * (-1.0,Infinity); (-1.0,-Infinity); (-0.5,Infinity); (-0.5,-Infinity); (0.5,Infinity) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_coverage_dw in the following table */
powf_uv__uv_extreme_coverage_dw_io_table_type powf_uv__uv_extreme_coverage_dw_io_table [18] = {
   {-1.00000000,INFINITY,1.00000000},
   {-1.00000000,-INFINITY,1.00000000},
   {-0.500000000,INFINITY,0.0},
   {-0.500000000,-INFINITY,0.0},
   {0.500000000,INFINITY,0.0},
   {0.500000000,-INFINITY,0.0},
   {0.500000000,NAN,NAN},
   {-0.500000000,NAN,NAN},
   {0.0,INFINITY,0.0},
   {0.0,-INFINITY,INFINITY},
   {-0.0,INFINITY,0.0},
   {-0.0,-INFINITY,INFINITY},
   {1.00616384,1463.00000,8022.56348},
   {1.00616384,-1463.00000,0.000124648446},
   {1.00616384,41.0000000,1.28652084},
   {1.00616384,-41.0000000,0.777290165},
   {-99.1999969,299.000000,-INFINITY},
   {-99.1999969,29.0000000,-INFINITY},
};
/** function powf_uv__uv_extreme_coverage_dw executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_coverage_dw() {
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
	for (ind=0;ind<18;ind++) {
		x=powf_uv__uv_extreme_coverage_dw_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_coverage_dw_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_coverage_dw_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_coverage_dw: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_coverage_dw_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==18) {
		PRINTF("powf_uv__uv_extreme_coverage_dw: successfully tested: 18 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_coverage_dw: %d tests failed for powf (out of 18)\n",errors);
	}
#endif
	return errors;
}

/**
 * 15 specified tests for powf
 * (0,-1); (1000,-15); (0.001,-15); (-0.001,-15); (-8,-8); (7,-4); (-1000,-15); (0.001,15) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_coverage_values in the following table */
powf_uv__uv_extreme_coverage_values_io_table_type powf_uv__uv_extreme_coverage_values_io_table [15] = {
   {0,-1.00000000,INFINITY},
   {1000.00000,-15.0000000,1.40129846e-45},
   {0.00100000005,-15.0000000,INFINITY},
   {-0.00100000005,-15.0000000,-INFINITY},
   {-8.00000000,-8.00000000,5.96046448e-08},
   {7.00000000,-4.00000000,0.000416493131},
   {-1000.00000,-15.0000000,-1.40129846e-45},
   {0.00100000005,15.0000000,1.40129846e-45},
   {1.17549435e-38,15.0000000,0.0},
   {-0.00100000005,15.0000000,-1.40129846e-45},
   {-1000.00000,15.0000000,-INFINITY},
   {-1.00000000,5.00000000,-1.00000000},
   {-1.00000000,3.29999995,NAN},
   {-1.17549435e-38,15.0000000,0.0},
   {-1.17549435e-38,8.00000000,0.0},
};
/** function powf_uv__uv_extreme_coverage_values executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_coverage_values() {
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
	for (ind=0;ind<15;ind++) {
		x=powf_uv__uv_extreme_coverage_values_io_table[ind].powf_x;
		y=powf_uv__uv_extreme_coverage_values_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_coverage_values_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_coverage_values: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_coverage_values_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==15) {
		PRINTF("powf_uv__uv_extreme_coverage_values: successfully tested: 15 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_coverage_values: %d tests failed for powf (out of 15)\n",errors);
	}
#endif
	return errors;
}

/**
 * 8 specified tests for powf
 * (-1.0,Infinity); (-1.0,-Infinity); (-0.5,Infinity); (-0.5,-Infinity); (0.5,Infinity) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_extreme_coverage_dw in the following table */
powf_uv__uv_extreme_coverage_dw_io_table_type powf_uv__uv_extreme_coverage_dw_io_table8 [8] = {
   {-1.00000000,INFINITY,1.00000000},
   {-1.00000000,-INFINITY,1.00000000},
   {-0.500000000,INFINITY,0.0},
   {-0.500000000,-INFINITY,0.0},
   {0.500000000,INFINITY,0.0},
   {0.500000000,-INFINITY,0.0},
   {0.500000000,NAN,NAN},
   {-0.500000000,NAN,NAN},
};
/** function powf_uv__uv_extreme_coverage_dw executes the tests and returns the number of failing tests */
int powf_uv__uv_extreme_coverage_dw8() {
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
	for (ind=0;ind<8;ind++) {
		x=powf_uv__uv_extreme_coverage_dw_io_table8[ind].powf_x;
		y=powf_uv__uv_extreme_coverage_dw_io_table8[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_extreme_coverage_dw_io_table8[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME,10.0,&max_dif_below_powf_uv__uv_extreme,&max_dif_above_powf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_extreme_coverage_dw: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_extreme_coverage_dw_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==8) {
		PRINTF("powf_uv__uv_extreme_coverage_dw: successfully tested: 8 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_extreme_coverage_dw: %d tests failed for powf (out of 8)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (27 functions) of powf_uv__uv_extreme
*/
int powf_uv__uv_extreme_main_test() {
	int errors=0;
	int index=0;
	errors+=powf_uv__uv_extreme_nan();                    /* 1. 5 tests */
	errors+=powf_uv__uv_extreme_pos_infinity();           /* 2. 5 tests */
	errors+=powf_uv__uv_extreme_neg_infinity();           /* 3. 5 tests */
	errors+=powf_uv__uv_extreme_infinity();               /* 4. 6 tests */
	errors+=powf_uv__uv_extreme_extreme_values();         /* 5. 9 tests */
	errors+=powf_uv__uv_extreme_powf_denom_pos_pos();     /* 6. 100 tests */
	errors+=powf_uv__uv_extreme_powf_denom_pos_neg();     /* 7. 110 tests */
	errors+=powf_uv__uv_extreme_powf_denom_neg_pos();     /* 8. 110 tests */
	errors+=powf_uv__uv_extreme_powf_denom_neg_neg();     /* 9. 121 tests */
	errors+=powf_uv__uv_extreme_zero();                   /* 10. 1 tests */
	errors+=powf_uv__uv_extreme_random_small_squares();   /* 11. 50 tests */
	errors+=powf_uv__uv_extreme_random_small_neg_sqr();   /* 12. 50 tests */
	errors+=powf_uv__uv_extreme_random_negative();        /* 13. 50 tests */
	errors+=powf_uv__uv_extreme_big_powers();             /* 14. 49 tests */
	errors+=powf_uv__uv_extreme_overflowing_powers();     /* 15. 49 tests */
	errors+=powf_uv__uv_extreme_overflowing_powers_pos(); /* 16. 64 tests */
	errors+=powf_uv__uv_extreme_overflowing_powers_neg(); /* 17. 64 tests */
	errors+=powf_uv__uv_extreme_undeflowing_powers();     /* 18. 64 tests */
	errors+=powf_uv__uv_extreme_undeflowing_powers2();    /* 19. 64 tests */
	errors+=powf_uv__uv_extreme_undeflowing_powers3();    /* 20. 64 tests */
	errors+=powf_uv__uv_extreme_undeflowing_powers4();    /* 21. 64 tests */
	errors+=powf_uv__uv_extreme_roots_of_minus_two();     /* 22. 25 tests */
	errors+=powf_uv__uv_extreme_roots_to_minus_ten();     /* 23. 25 tests */
	errors+=powf_uv__uv_extreme_big_powers2();            /* 24. 20 tests */
	errors+=powf_uv__uv_extreme_coverage_dw();            /* 25. 8 tests */
	errors+=powf_uv__uv_extreme_coverage_values();        /* 26. 15 tests */
	errors+=powf_uv__uv_extreme_coverage_dw8();            /* 27. 8 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of powf_uv__uv_extreme: successfully tested ALL 1205 cases for powf\n");
	} else {
		PRINTF("SUMMARY of powf_uv__uv_extreme: %d tests failed in powf_uv__uv_extreme (out of 1205 in powf_uv__uv_extreme)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.9g. ABS_TOLERANCE_POWF_UV__UV_EXTREME=%.9g\n",powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,ABS_TOLERANCE_POWF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.9g. REL_TOLERANCE_POWF_UV__UV_EXTREME=%.9g\n",powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,REL_TOLERANCE_POWF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.9g. ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME=%.9g\n",powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,ABS_REL_TOLERANCE_POWF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.9g. ULP_TOLERANCE_POWF_UV__UV_EXTREME=%.9g\n",powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,ULP_TOLERANCE_POWF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.9g. EXAKT_TOLERANCE_POWF_UV__UV_EXTREME=%.9g\n",powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,EXAKT_TOLERANCE_POWF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.9g. EQUAL_TOLERANCE_POWF_UV__UV_EXTREME=%.9g\n",powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,EQUAL_TOLERANCE_POWF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.9g. BINHEX_TOLERANCE_POWF_UV__UV_EXTREME=%.9g\n",powf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,BINHEX_TOLERANCE_POWF_UV__UV_EXTREME);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls powf_uv__uv_extreme_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = powf_uv__uv_extreme_main_test();
	return result;
}
#endif /* NO_MAIN */

