/**
 * this file (pow_uv__uv_trivial.c) contains test cases for pow
 * all test cases for pow have been specified and split into separate files
 * this file contains 35 test cases in 7 functions for the following purpose:
 * 2 specified tests for pow
 * (0.0,0.0); (1.0,1.0)
 * returns the number of failing tests
 * 20 linear tests for pow
 * 1. constant 0.0
 * 2. range from from -10 to 10
 * returns the number of failing tests
 * 2 specified tests for pow
 * (-0.0,10); (-0.0,5)
 * returns the number of failing tests
 * 4 linear tests for pow
 * range: (4,-1) to (9,1)
 * returns the number of failing tests
 * 3 (6 omitted out of range or duplicate)) linear tests for pow
 * range: (-1,0) to (-1,2)
 * returns the number of failing tests
 * 1 specified tests for pow
 * (10,2)
 * returns the number of failing tests
 * 3 specified tests for pow
 * (0,0); (1,1); (2,2)
 * returns the number of failing tests
 * this has been generated using the following settings:
 *  - Input-Outputs: variables for inputs and outputs
 *  - Casts: no casts for inputs and outputs
 *  - Input Type: double ( d8 )
 *  - Input Type: double ( d8 )
 *  - Output Type: double ( d8 )
 * Reference values have been computed from Java reference implementation.
 * They are compared using the following method:
 * - COMPARE_ABS_REL_TOLERANCE <= 1.0E-13
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

#define ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL 1.0E-13

double max_dif_below_pow_uv__uv_trivial=0.0;
double max_dif_above_pow_uv__uv_trivial=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of zero */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_trivial_zero_io_table_type;
/* type for expected input & output values of nine */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_trivial_nine_io_table_type;
/* type for expected input & output values of hundred */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_trivial_hundred_io_table_type;
/* type for expected input & output values of special_values */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_trivial_special_values_io_table_type;
/* type for expected input & output values of four */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_trivial_four_io_table_type;
/* type for expected input & output values of negative_zero */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_trivial_negative_zero_io_table_type;
/* type for expected input & output values of zero_one */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_trivial_zero_one_io_table_type;
/**
 * 2 specified tests for pow
 * (0.0,0.0); (1.0,1.0)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_trivial_zero_one in the following table */
pow_uv__uv_trivial_zero_one_io_table_type pow_uv__uv_trivial_zero_one_io_table [2] = {
   {0.0,0.0,1.0},
   {1.0,1.0,1.0},
};
/** function pow_uv__uv_trivial_zero_one executes the tests and returns the number of failing tests */
int pow_uv__uv_trivial_zero_one() {
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
	for (ind=0;ind<2;ind++) {
		x=pow_uv__uv_trivial_zero_one_io_table[ind].pow_x;
		y=pow_uv__uv_trivial_zero_one_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_trivial_zero_one_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,10.0,&max_dif_below_pow_uv__uv_trivial,&max_dif_above_pow_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_trivial_zero_one: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_trivial_zero_one_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==2) {
		PRINTF("pow_uv__uv_trivial_zero_one: successfully tested: 2 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_trivial_zero_one: %d tests failed for pow (out of 2)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for pow
 * 1. constant 0.0
 * 2. range from from -10 to 10
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_trivial_zero in the following table */
pow_uv__uv_trivial_zero_io_table_type pow_uv__uv_trivial_zero_io_table [20] = {
   {0.0,-10.0,INFINITY},
   {0.0,-8.94736842105263,INFINITY},
   {0.0,-7.894736842105264,INFINITY},
   {0.0,-6.842105263157894,INFINITY},
   {0.0,-5.789473684210527,INFINITY},
   {0.0,-4.7368421052631575,INFINITY},
   {0.0,-3.6842105263157894,INFINITY},
   {0.0,-2.6315789473684212,INFINITY},
   {0.0,-1.578947368421053,INFINITY},
   {0.0,-0.526315789473685,INFINITY},
   {0.0,0.5263157894736832,0.0},
   {0.0,1.5789473684210513,0.0},
   {0.0,2.6315789473684212,0.0},
   {0.0,3.6842105263157894,0.0},
   {0.0,4.7368421052631575,0.0},
   {0.0,5.789473684210526,0.0},
   {0.0,6.842105263157894,0.0},
   {0.0,7.894736842105264,0.0},
   {0.0,8.94736842105263,0.0},
   {0.0,10.0,0.0},
};
/** function pow_uv__uv_trivial_zero executes the tests and returns the number of failing tests */
int pow_uv__uv_trivial_zero() {
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
		x=pow_uv__uv_trivial_zero_io_table[ind].pow_x;
		y=pow_uv__uv_trivial_zero_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_trivial_zero_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,10.0,&max_dif_below_pow_uv__uv_trivial,&max_dif_above_pow_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_trivial_zero: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_trivial_zero_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("pow_uv__uv_trivial_zero: successfully tested: 20 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_trivial_zero: %d tests failed for pow (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 2 specified tests for pow
 * (-0.0,10); (-0.0,5)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_trivial_negative_zero in the following table */
pow_uv__uv_trivial_negative_zero_io_table_type pow_uv__uv_trivial_negative_zero_io_table [2] = {
   {-0.0,10,0.0},
   {-0.0,5,0.0},
};
/** function pow_uv__uv_trivial_negative_zero executes the tests and returns the number of failing tests */
int pow_uv__uv_trivial_negative_zero() {
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
	for (ind=0;ind<2;ind++) {
		x=pow_uv__uv_trivial_negative_zero_io_table[ind].pow_x;
		y=pow_uv__uv_trivial_negative_zero_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_trivial_negative_zero_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,10.0,&max_dif_below_pow_uv__uv_trivial,&max_dif_above_pow_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_trivial_negative_zero: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_trivial_negative_zero_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==2) {
		PRINTF("pow_uv__uv_trivial_negative_zero: successfully tested: 2 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_trivial_negative_zero: %d tests failed for pow (out of 2)\n",errors);
	}
#endif
	return errors;
}

/**
 * 4 linear tests for pow
 * range: (4,-1) to (9,1)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_trivial_four in the following table */
pow_uv__uv_trivial_four_io_table_type pow_uv__uv_trivial_four_io_table [4] = {
   {4.0,-1.0,0.25},
   {4.0,1.0,4.0},
   {9.0,-1.0,0.1111111111111111},
   {9.0,1.0,9.0},
};
/** function pow_uv__uv_trivial_four executes the tests and returns the number of failing tests */
int pow_uv__uv_trivial_four() {
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
	for (ind=0;ind<4;ind++) {
		x=pow_uv__uv_trivial_four_io_table[ind].pow_x;
		y=pow_uv__uv_trivial_four_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_trivial_four_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,10.0,&max_dif_below_pow_uv__uv_trivial,&max_dif_above_pow_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_trivial_four: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_trivial_four_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==4) {
		PRINTF("pow_uv__uv_trivial_four: successfully tested: 4 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_trivial_four: %d tests failed for pow (out of 4)\n",errors);
	}
#endif
	return errors;
}

/**
 * 3 (6 omitted out of range or duplicate)) linear tests for pow
 * range: (-1,0) to (-1,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_trivial_nine in the following table */
pow_uv__uv_trivial_nine_io_table_type pow_uv__uv_trivial_nine_io_table [3] = {
   {-1.0,0.0,1.0},
   {-1.0,1.0,-1.0},
   {-1.0,2.0,1.0},
};
/** function pow_uv__uv_trivial_nine executes the tests and returns the number of failing tests */
int pow_uv__uv_trivial_nine() {
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
	for (ind=0;ind<3;ind++) {
		x=pow_uv__uv_trivial_nine_io_table[ind].pow_x;
		y=pow_uv__uv_trivial_nine_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_trivial_nine_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,10.0,&max_dif_below_pow_uv__uv_trivial,&max_dif_above_pow_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_trivial_nine: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_trivial_nine_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==3) {
		PRINTF("pow_uv__uv_trivial_nine: successfully tested: 3 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_trivial_nine: %d tests failed for pow (out of 3)\n",errors);
	}
#endif
	return errors;
}

/**
 * 1 specified tests for pow
 * (10,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_trivial_hundred in the following table */
pow_uv__uv_trivial_hundred_io_table_type pow_uv__uv_trivial_hundred_io_table [1] = {
   {10.0,2.0,100.0},
};
/** function pow_uv__uv_trivial_hundred executes the tests and returns the number of failing tests */
int pow_uv__uv_trivial_hundred() {
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
		x=pow_uv__uv_trivial_hundred_io_table[ind].pow_x;
		y=pow_uv__uv_trivial_hundred_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_trivial_hundred_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,10.0,&max_dif_below_pow_uv__uv_trivial,&max_dif_above_pow_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_trivial_hundred: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_trivial_hundred_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==1) {
		PRINTF("pow_uv__uv_trivial_hundred: successfully tested: 1 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_trivial_hundred: %d tests failed for pow (out of 1)\n",errors);
	}
#endif
	return errors;
}

/**
 * 3 specified tests for pow
 * (0,0); (1,1); (2,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_trivial_special_values in the following table */
pow_uv__uv_trivial_special_values_io_table_type pow_uv__uv_trivial_special_values_io_table [3] = {
   {0,0,1.0},
   {1.0,1.0,1.0},
   {2.0,2.0,4.0},
};
/** function pow_uv__uv_trivial_special_values executes the tests and returns the number of failing tests */
int pow_uv__uv_trivial_special_values() {
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
	for (ind=0;ind<3;ind++) {
		x=pow_uv__uv_trivial_special_values_io_table[ind].pow_x;
		y=pow_uv__uv_trivial_special_values_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_trivial_special_values_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL,10.0,&max_dif_below_pow_uv__uv_trivial,&max_dif_above_pow_uv__uv_trivial)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_trivial_special_values: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_trivial_special_values_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==3) {
		PRINTF("pow_uv__uv_trivial_special_values: successfully tested: 3 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_trivial_special_values: %d tests failed for pow (out of 3)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (7 functions) of pow_uv__uv_trivial
*/
int pow_uv__uv_trivial_main_test() {
	int errors=0;
	int index=0;
	errors+=pow_uv__uv_trivial_zero_one();       /* 1. 2 tests */
	errors+=pow_uv__uv_trivial_zero();           /* 2. 20 tests */
	errors+=pow_uv__uv_trivial_negative_zero();  /* 3. 2 tests */
	errors+=pow_uv__uv_trivial_four();           /* 4. 4 tests */
	errors+=pow_uv__uv_trivial_nine();           /* 5. 3 tests */
	errors+=pow_uv__uv_trivial_hundred();        /* 6. 1 tests */
	errors+=pow_uv__uv_trivial_special_values(); /* 7. 3 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of pow_uv__uv_trivial: successfully tested ALL 35 cases for pow\n");
	} else {
		PRINTF("SUMMARY of pow_uv__uv_trivial: %d tests failed in pow_uv__uv_trivial (out of 35 in pow_uv__uv_trivial)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.17g. ABS_TOLERANCE_POW_UV__UV_TRIVIAL=%.17g\n",pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value,ABS_TOLERANCE_POW_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.17g. REL_TOLERANCE_POW_UV__UV_TRIVIAL=%.17g\n",pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value,REL_TOLERANCE_POW_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.17g. ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL=%.17g\n",pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value,ABS_REL_TOLERANCE_POW_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.17g. ULP_TOLERANCE_POW_UV__UV_TRIVIAL=%.17g\n",pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value,ULP_TOLERANCE_POW_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.17g. EXAKT_TOLERANCE_POW_UV__UV_TRIVIAL=%.17g\n",pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value,EXAKT_TOLERANCE_POW_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.17g. EQUAL_TOLERANCE_POW_UV__UV_TRIVIAL=%.17g\n",pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value,EQUAL_TOLERANCE_POW_UV__UV_TRIVIAL);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.17g. BINHEX_TOLERANCE_POW_UV__UV_TRIVIAL=%.17g\n",pow_uv__uv_trivial_all_deviation_results_double[index].max_diff_value,BINHEX_TOLERANCE_POW_UV__UV_TRIVIAL);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls pow_uv__uv_trivial_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = pow_uv__uv_trivial_main_test();
	return result;
}
#endif /* NO_MAIN */

