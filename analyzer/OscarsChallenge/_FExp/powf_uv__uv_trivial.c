/**
 * this file (powf_uv__uv_trivial.c) contains test cases for powf
 * all test cases for powf have been specified and split into separate files
 * this file contains 35 test cases in 7 functions for the following purpose:
 * 2 specified tests for powf
 * (0.0,0.0); (1.0,1.0)
 * returns the number of failing tests
 * 20 linear tests for powf
 * 1. constant 0.0
 * 2. range from from -10 to 10
 * returns the number of failing tests
 * 2 specified tests for powf
 * (-0.0,10); (-0.0,5)
 * returns the number of failing tests
 * 4 linear tests for powf
 * range: (4,-1) to (9,1)
 * returns the number of failing tests
 * 3 (6 omitted out of range or duplicate)) linear tests for powf
 * range: (-1,0) to (-1,2)
 * returns the number of failing tests
 * 1 specified tests for powf
 * (10,2)
 * returns the number of failing tests
 * 3 specified tests for powf
 * (0,0); (1,1); (2,2)
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

#define ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL 1.0E-5

float max_dif_below_powf_uv__uv_trivial=0.0;
float max_dif_above_powf_uv__uv_trivial=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of zero */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_trivial_zero_io_table_type;
/* type for expected input & output values of nine */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_trivial_nine_io_table_type;
/* type for expected input & output values of hundred */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_trivial_hundred_io_table_type;
/* type for expected input & output values of special_values */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_trivial_special_values_io_table_type;
/* type for expected input & output values of four */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_trivial_four_io_table_type;
/* type for expected input & output values of negative_zero */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_trivial_negative_zero_io_table_type;
/* type for expected input & output values of zero_one */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_trivial_zero_one_io_table_type;
/**
 * 2 specified tests for powf
 * (0.0,0.0); (1.0,1.0)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_trivial_zero_one in the following table */
powf_uv__uv_trivial_zero_one_io_table_type powf_uv__uv_trivial_zero_one_io_table [2] = {
   {0.0,0.0,1.00000000},
   {1.00000000,1.00000000,1.00000000},
};
/** function powf_uv__uv_trivial_zero_one executes the tests and returns the number of failing tests */
int powf_uv__uv_trivial_zero_one() {
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
	for (ind=0;ind<2;ind++) {
		x=powf_uv__uv_trivial_zero_one_io_table[ind].powf_x;
		y=powf_uv__uv_trivial_zero_one_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_trivial_zero_one_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,10.0,&max_dif_below_powf_uv__uv_trivial,&max_dif_above_powf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_trivial_zero_one: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_trivial_zero_one_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==2) {
		PRINTF("powf_uv__uv_trivial_zero_one: successfully tested: 2 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_trivial_zero_one: %d tests failed for powf (out of 2)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for powf
 * 1. constant 0.0
 * 2. range from from -10 to 10
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_trivial_zero in the following table */
powf_uv__uv_trivial_zero_io_table_type powf_uv__uv_trivial_zero_io_table [20] = {
   {0.0,-10.0000000,INFINITY},
   {0.0,-8.94736862,INFINITY},
   {0.0,-7.89473677,INFINITY},
   {0.0,-6.84210539,INFINITY},
   {0.0,-5.78947353,INFINITY},
   {0.0,-4.73684216,INFINITY},
   {0.0,-3.68421054,INFINITY},
   {0.0,-2.63157892,INFINITY},
   {0.0,-1.57894742,INFINITY},
   {0.0,-0.526315808,INFINITY},
   {0.0,0.526315808,0.0},
   {0.0,1.57894742,0.0},
   {0.0,2.63157892,0.0},
   {0.0,3.68421054,0.0},
   {0.0,4.73684216,0.0},
   {0.0,5.78947353,0.0},
   {0.0,6.84210539,0.0},
   {0.0,7.89473677,0.0},
   {0.0,8.94736862,0.0},
   {0.0,10.0000000,0.0},
};
/** function powf_uv__uv_trivial_zero executes the tests and returns the number of failing tests */
int powf_uv__uv_trivial_zero() {
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
		x=powf_uv__uv_trivial_zero_io_table[ind].powf_x;
		y=powf_uv__uv_trivial_zero_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_trivial_zero_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,10.0,&max_dif_below_powf_uv__uv_trivial,&max_dif_above_powf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_trivial_zero: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_trivial_zero_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("powf_uv__uv_trivial_zero: successfully tested: 20 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_trivial_zero: %d tests failed for powf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 2 specified tests for powf
 * (-0.0,10); (-0.0,5)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_trivial_negative_zero in the following table */
powf_uv__uv_trivial_negative_zero_io_table_type powf_uv__uv_trivial_negative_zero_io_table [2] = {
   {-0.0,10,0.0},
   {-0.0,5,0.0},
};
/** function powf_uv__uv_trivial_negative_zero executes the tests and returns the number of failing tests */
int powf_uv__uv_trivial_negative_zero() {
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
	for (ind=0;ind<2;ind++) {
		x=powf_uv__uv_trivial_negative_zero_io_table[ind].powf_x;
		y=powf_uv__uv_trivial_negative_zero_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_trivial_negative_zero_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,10.0,&max_dif_below_powf_uv__uv_trivial,&max_dif_above_powf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_trivial_negative_zero: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_trivial_negative_zero_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==2) {
		PRINTF("powf_uv__uv_trivial_negative_zero: successfully tested: 2 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_trivial_negative_zero: %d tests failed for powf (out of 2)\n",errors);
	}
#endif
	return errors;
}

/**
 * 4 linear tests for powf
 * range: (4,-1) to (9,1)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_trivial_four in the following table */
powf_uv__uv_trivial_four_io_table_type powf_uv__uv_trivial_four_io_table [4] = {
   {4.00000000,-1.00000000,0.250000000},
   {4.00000000,1.00000000,4.00000000},
   {9.00000000,-1.00000000,0.111111112},
   {9.00000000,1.00000000,9.00000000},
};
/** function powf_uv__uv_trivial_four executes the tests and returns the number of failing tests */
int powf_uv__uv_trivial_four() {
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
	for (ind=0;ind<4;ind++) {
		x=powf_uv__uv_trivial_four_io_table[ind].powf_x;
		y=powf_uv__uv_trivial_four_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_trivial_four_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,10.0,&max_dif_below_powf_uv__uv_trivial,&max_dif_above_powf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_trivial_four: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_trivial_four_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==4) {
		PRINTF("powf_uv__uv_trivial_four: successfully tested: 4 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_trivial_four: %d tests failed for powf (out of 4)\n",errors);
	}
#endif
	return errors;
}

/**
 * 3 (6 omitted out of range or duplicate)) linear tests for powf
 * range: (-1,0) to (-1,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_trivial_nine in the following table */
powf_uv__uv_trivial_nine_io_table_type powf_uv__uv_trivial_nine_io_table [3] = {
   {-1.00000000,0.0,1.00000000},
   {-1.00000000,1.00000000,-1.00000000},
   {-1.00000000,2.00000000,1.00000000},
};
/** function powf_uv__uv_trivial_nine executes the tests and returns the number of failing tests */
int powf_uv__uv_trivial_nine() {
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
	for (ind=0;ind<3;ind++) {
		x=powf_uv__uv_trivial_nine_io_table[ind].powf_x;
		y=powf_uv__uv_trivial_nine_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_trivial_nine_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,10.0,&max_dif_below_powf_uv__uv_trivial,&max_dif_above_powf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_trivial_nine: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_trivial_nine_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==3) {
		PRINTF("powf_uv__uv_trivial_nine: successfully tested: 3 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_trivial_nine: %d tests failed for powf (out of 3)\n",errors);
	}
#endif
	return errors;
}

/**
 * 1 specified tests for powf
 * (10,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_trivial_hundred in the following table */
powf_uv__uv_trivial_hundred_io_table_type powf_uv__uv_trivial_hundred_io_table [1] = {
   {10.0000000,2.00000000,100.000000},
};
/** function powf_uv__uv_trivial_hundred executes the tests and returns the number of failing tests */
int powf_uv__uv_trivial_hundred() {
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
		x=powf_uv__uv_trivial_hundred_io_table[ind].powf_x;
		y=powf_uv__uv_trivial_hundred_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_trivial_hundred_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,10.0,&max_dif_below_powf_uv__uv_trivial,&max_dif_above_powf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_trivial_hundred: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_trivial_hundred_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==1) {
		PRINTF("powf_uv__uv_trivial_hundred: successfully tested: 1 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_trivial_hundred: %d tests failed for powf (out of 1)\n",errors);
	}
#endif
	return errors;
}

/**
 * 3 specified tests for powf
 * (0,0); (1,1); (2,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_trivial_special_values in the following table */
powf_uv__uv_trivial_special_values_io_table_type powf_uv__uv_trivial_special_values_io_table [3] = {
   {0,0,1.00000000},
   {1.00000000,1.00000000,1.00000000},
   {2.00000000,2.00000000,4.00000000},
};
/** function powf_uv__uv_trivial_special_values executes the tests and returns the number of failing tests */
int powf_uv__uv_trivial_special_values() {
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
	for (ind=0;ind<3;ind++) {
		x=powf_uv__uv_trivial_special_values_io_table[ind].powf_x;
		y=powf_uv__uv_trivial_special_values_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_trivial_special_values_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL,10.0,&max_dif_below_powf_uv__uv_trivial,&max_dif_above_powf_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_trivial_special_values: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_trivial_special_values_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==3) {
		PRINTF("powf_uv__uv_trivial_special_values: successfully tested: 3 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_trivial_special_values: %d tests failed for powf (out of 3)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (7 functions) of powf_uv__uv_trivial
*/
int powf_uv__uv_trivial_main_test() {
	int errors=0;
	int index=0;
	errors+=powf_uv__uv_trivial_zero_one();       /* 1. 2 tests */
	errors+=powf_uv__uv_trivial_zero();           /* 2. 20 tests */
	errors+=powf_uv__uv_trivial_negative_zero();  /* 3. 2 tests */
	errors+=powf_uv__uv_trivial_four();           /* 4. 4 tests */
	errors+=powf_uv__uv_trivial_nine();           /* 5. 3 tests */
	errors+=powf_uv__uv_trivial_hundred();        /* 6. 1 tests */
	errors+=powf_uv__uv_trivial_special_values(); /* 7. 3 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of powf_uv__uv_trivial: successfully tested ALL 35 cases for powf\n");
	} else {
		PRINTF("SUMMARY of powf_uv__uv_trivial: %d tests failed in powf_uv__uv_trivial (out of 35 in powf_uv__uv_trivial)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.9g. ABS_TOLERANCE_POWF_UV__UV_TRIVIAL=%.9g\n",powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,ABS_TOLERANCE_POWF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.9g. REL_TOLERANCE_POWF_UV__UV_TRIVIAL=%.9g\n",powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,REL_TOLERANCE_POWF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.9g. ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL=%.9g\n",powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,ABS_REL_TOLERANCE_POWF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.9g. ULP_TOLERANCE_POWF_UV__UV_TRIVIAL=%.9g\n",powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,ULP_TOLERANCE_POWF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.9g. EXAKT_TOLERANCE_POWF_UV__UV_TRIVIAL=%.9g\n",powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,EXAKT_TOLERANCE_POWF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.9g. EQUAL_TOLERANCE_POWF_UV__UV_TRIVIAL=%.9g\n",powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,EQUAL_TOLERANCE_POWF_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.9g. BINHEX_TOLERANCE_POWF_UV__UV_TRIVIAL=%.9g\n",powf_uv__uv_trivial_all_deviation_results_float[index].max_diff_value,BINHEX_TOLERANCE_POWF_UV__UV_TRIVIAL);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls powf_uv__uv_trivial_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = powf_uv__uv_trivial_main_test();
	return result;
}
#endif /* NO_MAIN */

