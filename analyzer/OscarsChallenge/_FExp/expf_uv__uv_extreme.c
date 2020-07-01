/**
 * this file (expf_uv__uv_extreme.c) contains test cases for expf
 * all test cases for expf have been specified and split into separate files
 * this file contains 748 test cases in 26 functions for the following purpose:
 * 3 specified tests for expf
 * NaN; Infinity; -Infinity
 * returns the number of failing tests
 * 1 specified tests for expf
 * NaN
 * returns the number of failing tests
 * 2 specified tests for expf
 * Infinity; -Infinity
 * returns the number of failing tests
 * 23 specified tests for expf
 * 5.877472E-39; 2.938736E-39; 1.469368E-39; 7.34684E-40; 3.67342E-40; 1.83671E-40; 9.18355E-41 ...
 * returns the number of failing tests
 * 23 specified tests for expf
 * -5.877472E-39; -2.938736E-39; -1.469368E-39; -7.34684E-40; -3.67342E-40; -1.83671E-40 ...
 * returns the number of failing tests
 * 80 specified tests for expf
 * 9.795785E-39; 8.163154E-39; 6.802627E-39; 5.668856E-39; 4.724046E-39; 3.936706E-39 ...
 * returns the number of failing tests
 * 57 specified tests for expf
 * -9.042265E-39; -6.955589E-39; -5.350453E-39; -4.115733E-39; -3.165948E-39; -2.435345E-39 ...
 * returns the number of failing tests
 * 20 linear tests for expf
 * range from from -340282346638528859811704183484516925440 to 340282346638528859811704183484516925440
 * returns the number of failing tests
 * 50 random tests for expf
 * range from from -340282346638528859811704183484516925440 to 340282346638528859811704183484516925440
 * returns the number of failing tests
 * 25 tests for approximation from 0.0 to 340282346638528859811704183484516925440
 * returns the number of failing tests
 * 25 tests for approximation from 0.0 to -340282346638528859811704183484516925440
 * returns the number of failing tests
 * 2 specified tests for expf
 * 3.141592653589793; -1.5707963267948966
 * returns the number of failing tests
 * 20 linear tests for expf
 * range from from -1.5697963267948967 to -1.5717963267948964
 * returns the number of failing tests
 * 50 random tests for expf
 * range from from -1.5697963267948967 to -1.5717963267948964
 * returns the number of failing tests
 * 20 linear tests for expf
 * range from from -1001 to -999
 * returns the number of failing tests
 * 50 random tests for expf
 * range from from -1001 to -999
 * returns the number of failing tests
 * 20 linear tests for expf
 * range from from 9999.9 to 10000.01
 * returns the number of failing tests
 * 38 (12 omitted out of input range) random tests for expf
 * range from from 9999.9 to 10000.01
 * returns the number of failing tests
 * 60 tests for approximation from -1.00000000 to 0.0
 * returns the number of failing tests
 * 27 tests for approximation from -3.14159274 to 2.356194490192345
 * returns the number of failing tests
 * 1 specified tests for expf
 * -720
 * returns the number of failing tests
 * 2 specified tests for expf
 * -87; -88
 * returns the number of failing tests
 * 53 linear tests for expf
 * range from from 42.8457 to 42.8459
 * returns the number of failing tests
 * 0 linear tests for expf
 * range from from -13.18320 to -13.18340
 * returns the number of failing tests
 * 100 linear tests for expf
 * range from from 1.38628 to 1.38630
 * returns the number of failing tests
 * 0 linear tests for expf
 * range from from -1.38610 to -1.38630
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

#define ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME 1.0E-6

float max_dif_below_expf_uv__uv_extreme=0.0;
float max_dif_above_expf_uv__uv_extreme=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of expf_infinity */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_infinity_io_table_type;
/* type for expected input & output values of manual_values */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_manual_values_io_table_type;
/* type for expected input & output values of ten_thousand_rnd */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_ten_thousand_rnd_io_table_type;
/* type for expected input & output values of coverage_floats2 */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_coverage_floats2_io_table_type;
/* type for expected input & output values of coverage_floats3 */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_coverage_floats3_io_table_type;
/* type for expected input & output values of ten_thousand */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_ten_thousand_io_table_type;
/* type for expected input & output values of coverage_floats4 */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_coverage_floats4_io_table_type;
/* type for expected input & output values of expf_extreme_ramp_down */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_extreme_ramp_down_io_table_type;
/* type for expected input & output values of expf_denormalized_pos_float_2_0 */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_denormalized_pos_float_2_0_io_table_type;
/* type for expected input & output values of expf_denormalized_pos_float_1_2 */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_denormalized_pos_float_1_2_io_table_type;
/* type for expected input & output values of expf_extreme_ramp_up */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_extreme_ramp_up_io_table_type;
/* type for expected input & output values of mi_thousand_rnd */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_mi_thousand_rnd_io_table_type;
/* type for expected input & output values of expf_extreme_range_rnd */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_extreme_range_rnd_io_table_type;
/* type for expected input & output values of extreme_values */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_extreme_values_io_table_type;
/* type for expected input & output values of expf_extreme_range */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_extreme_range_io_table_type;
/* type for expected input & output values of coverage_floats */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_coverage_floats_io_table_type;
/* type for expected input & output values of approx_zero */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_approx_zero_io_table_type;
/* type for expected input & output values of mi_thousand */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_mi_thousand_io_table_type;
/* type for expected input & output values of expf_denormalized_neg_float_1_3 */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_denormalized_neg_float_1_3_io_table_type;
/* type for expected input & output values of expf_denormalized_neg_float_2_0 */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_denormalized_neg_float_2_0_io_table_type;
/* type for expected input & output values of pi_half */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_pi_half_io_table_type;
/* type for expected input & output values of expf_nan */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_expf_nan_io_table_type;
/* type for expected input & output values of coverage_values */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_coverage_values_io_table_type;
/* type for expected input & output values of approx_pi_34 */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_approx_pi_34_io_table_type;
/* type for expected input & output values of pi_half_rnd */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_pi_half_rnd_io_table_type;
/* type for expected input & output values of corner_values */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_extreme_corner_values_io_table_type;
/**
 * 3 specified tests for expf
 * NaN; Infinity; -Infinity
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_extreme_values in the following table */
expf_uv__uv_extreme_extreme_values_io_table_type expf_uv__uv_extreme_extreme_values_io_table [3] = {
   {NAN,NAN},
   {INFINITY,INFINITY},
   {-INFINITY,0.0},
};
/** function expf_uv__uv_extreme_extreme_values executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_extreme_values() {
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
	for (ind=0;ind<3;ind++) {
		x=expf_uv__uv_extreme_extreme_values_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_extreme_values_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_extreme_values: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_extreme_values_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==3) {
		PRINTF("expf_uv__uv_extreme_extreme_values: successfully tested: 3 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_extreme_values: %d tests failed for expf (out of 3)\n",errors);
	}
#endif
	return errors;
}

/**
 * 1 specified tests for expf
 * NaN
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_nan in the following table */
expf_uv__uv_extreme_expf_nan_io_table_type expf_uv__uv_extreme_expf_nan_io_table [1] = {
   {NAN,NAN},
};
/** function expf_uv__uv_extreme_expf_nan executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_nan() {
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
	for (ind=0;ind<1;ind++) {
		x=expf_uv__uv_extreme_expf_nan_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_nan_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_nan: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_nan_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==1) {
		PRINTF("expf_uv__uv_extreme_expf_nan: successfully tested: 1 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_nan: %d tests failed for expf (out of 1)\n",errors);
	}
#endif
	return errors;
}

/**
 * 2 specified tests for expf
 * Infinity; -Infinity
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_infinity in the following table */
expf_uv__uv_extreme_expf_infinity_io_table_type expf_uv__uv_extreme_expf_infinity_io_table [2] = {
   {INFINITY,INFINITY},
   {-INFINITY,0.0},
};
/** function expf_uv__uv_extreme_expf_infinity executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_infinity() {
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
		x=expf_uv__uv_extreme_expf_infinity_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_infinity_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_infinity: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_infinity_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==2) {
		PRINTF("expf_uv__uv_extreme_expf_infinity: successfully tested: 2 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_infinity: %d tests failed for expf (out of 2)\n",errors);
	}
#endif
	return errors;
}

/**
 * 23 specified tests for expf
 * 5.877472E-39; 2.938736E-39; 1.469368E-39; 7.34684E-40; 3.67342E-40; 1.83671E-40; 9.18355E-41 ...
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_denormalized_pos_float_2_0 in the following table */
expf_uv__uv_extreme_expf_denormalized_pos_float_2_0_io_table_type expf_uv__uv_extreme_expf_denormalized_pos_float_2_0_io_table [23] = {
   {5.87747175e-39,1.00000000},
   {2.93873588e-39,1.00000000},
   {1.46936794e-39,1.00000000},
   {7.34683969e-40,1.00000000},
   {3.67341985e-40,1.00000000},
   {1.83670992e-40,1.00000000},
   {9.18354962e-41,1.00000000},
   {4.59177481e-41,1.00000000},
   {2.29588740e-41,1.00000000},
   {1.14794370e-41,1.00000000},
   {5.73971851e-42,1.00000000},
   {2.86985925e-42,1.00000000},
   {1.43492963e-42,1.00000000},
   {7.17464814e-43,1.00000000},
   {3.58732407e-43,1.00000000},
   {1.79366203e-43,1.00000000},
   {8.96831017e-44,1.00000000},
   {4.48415509e-44,1.00000000},
   {2.24207754e-44,1.00000000},
   {1.12103877e-44,1.00000000},
   {5.60519386e-45,1.00000000},
   {2.80259693e-45,1.00000000},
   {1.40129846e-45,1.00000000},
};
/** function expf_uv__uv_extreme_expf_denormalized_pos_float_2_0 executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_denormalized_pos_float_2_0() {
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
	for (ind=0;ind<23;ind++) {
		x=expf_uv__uv_extreme_expf_denormalized_pos_float_2_0_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_denormalized_pos_float_2_0_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_denormalized_pos_float_2_0: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_denormalized_pos_float_2_0_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==23) {
		PRINTF("expf_uv__uv_extreme_expf_denormalized_pos_float_2_0: successfully tested: 23 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_denormalized_pos_float_2_0: %d tests failed for expf (out of 23)\n",errors);
	}
#endif
	return errors;
}

/**
 * 23 specified tests for expf
 * -5.877472E-39; -2.938736E-39; -1.469368E-39; -7.34684E-40; -3.67342E-40; -1.83671E-40 ...
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_denormalized_neg_float_2_0 in the following table */
expf_uv__uv_extreme_expf_denormalized_neg_float_2_0_io_table_type expf_uv__uv_extreme_expf_denormalized_neg_float_2_0_io_table [23] = {
   {-5.87747175e-39,1.00000000},
   {-2.93873588e-39,1.00000000},
   {-1.46936794e-39,1.00000000},
   {-7.34683969e-40,1.00000000},
   {-3.67341985e-40,1.00000000},
   {-1.83670992e-40,1.00000000},
   {-9.18354962e-41,1.00000000},
   {-4.59177481e-41,1.00000000},
   {-2.29588740e-41,1.00000000},
   {-1.14794370e-41,1.00000000},
   {-5.73971851e-42,1.00000000},
   {-2.86985925e-42,1.00000000},
   {-1.43492963e-42,1.00000000},
   {-7.17464814e-43,1.00000000},
   {-3.58732407e-43,1.00000000},
   {-1.79366203e-43,1.00000000},
   {-8.96831017e-44,1.00000000},
   {-4.48415509e-44,1.00000000},
   {-2.24207754e-44,1.00000000},
   {-1.12103877e-44,1.00000000},
   {-5.60519386e-45,1.00000000},
   {-2.80259693e-45,1.00000000},
   {-1.40129846e-45,1.00000000},
};
/** function expf_uv__uv_extreme_expf_denormalized_neg_float_2_0 executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_denormalized_neg_float_2_0() {
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
	for (ind=0;ind<23;ind++) {
		x=expf_uv__uv_extreme_expf_denormalized_neg_float_2_0_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_denormalized_neg_float_2_0_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_denormalized_neg_float_2_0: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_denormalized_neg_float_2_0_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==23) {
		PRINTF("expf_uv__uv_extreme_expf_denormalized_neg_float_2_0: successfully tested: 23 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_denormalized_neg_float_2_0: %d tests failed for expf (out of 23)\n",errors);
	}
#endif
	return errors;
}

/**
 * 80 specified tests for expf
 * 9.795785E-39; 8.163154E-39; 6.802627E-39; 5.668856E-39; 4.724046E-39; 3.936706E-39 ...
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_denormalized_pos_float_1_2 in the following table */
expf_uv__uv_extreme_expf_denormalized_pos_float_1_2_io_table_type expf_uv__uv_extreme_expf_denormalized_pos_float_1_2_io_table [80] = {
   {9.79578532e-39,1.00000000},
   {8.16315350e-39,1.00000000},
   {6.80262722e-39,1.00000000},
   {5.66885625e-39,1.00000000},
   {4.72404617e-39,1.00000000},
   {3.93670561e-39,1.00000000},
   {3.28058824e-39,1.00000000},
   {2.73382400e-39,1.00000000},
   {2.27818620e-39,1.00000000},
   {1.89848897e-39,1.00000000},
   {1.58207437e-39,1.00000000},
   {1.31839484e-39,1.00000000},
   {1.09866284e-39,1.00000000},
   {9.15552365e-40,1.00000000},
   {7.62960771e-40,1.00000000},
   {6.35799942e-40,1.00000000},
   {5.29833752e-40,1.00000000},
   {4.41528127e-40,1.00000000},
   {3.67940339e-40,1.00000000},
   {3.06616716e-40,1.00000000},
   {2.55514163e-40,1.00000000},
   {2.12928703e-40,1.00000000},
   {1.77440819e-40,1.00000000},
   {1.47867817e-40,1.00000000},
   {1.23223180e-40,1.00000000},
   {1.02685750e-40,1.00000000},
   {8.55716920e-41,1.00000000},
   {7.13092763e-41,1.00000000},
   {5.94248640e-41,1.00000000},
   {4.95204864e-41,1.00000000},
   {4.12668385e-41,1.00000000},
   {3.43892656e-41,1.00000000},
   {2.86579549e-41,1.00000000},
   {2.38809284e-41,1.00000000},
   {1.99012408e-41,1.00000000},
   {1.65843673e-41,1.00000000},
   {1.38196055e-41,1.00000000},
   {1.15158708e-41,1.00000000},
   {9.59609188e-42,1.00000000},
   {7.99721034e-42,1.00000000},
   {6.66457550e-42,1.00000000},
   {5.55334581e-42,1.00000000},
   {4.62708753e-42,1.00000000},
   {3.85637337e-42,1.00000000},
   {3.21317738e-42,1.00000000},
   {2.67788137e-42,1.00000000},
   {2.23086716e-42,1.00000000},
   {1.85952306e-42,1.00000000},
   {1.54983610e-42,1.00000000},
   {1.29199718e-42,1.00000000},
   {1.07619722e-42,1.00000000},
   {8.96831017e-43,1.00000000},
   {7.46892081e-43,1.00000000},
   {6.22176518e-43,1.00000000},
   {5.18480432e-43,1.00000000},
   {4.31599927e-43,1.00000000},
   {3.60133705e-43,1.00000000},
   {2.99877871e-43,1.00000000},
   {2.49431127e-43,1.00000000},
   {2.07392173e-43,1.00000000},
   {1.72359711e-43,1.00000000},
   {1.42932443e-43,1.00000000},
   {1.19110369e-43,1.00000000},
   {9.94921910e-44,1.00000000},
   {8.26766094e-44,1.00000000},
   {6.86636248e-44,1.00000000},
   {5.74532370e-44,1.00000000},
   {4.76441478e-44,1.00000000},
   {3.92363570e-44,1.00000000},
   {3.22298647e-44,1.00000000},
   {2.66246708e-44,1.00000000},
   {2.24207754e-44,1.00000000},
   {1.82168800e-44,1.00000000},
   {1.54142831e-44,1.00000000},
   {1.26116862e-44,1.00000000},
   {9.80908925e-45,1.00000000},
   {8.40779079e-45,1.00000000},
   {7.00649232e-45,1.00000000},
   {5.60519386e-45,1.00000000},
   {4.20389539e-45,1.00000000},
};
/** function expf_uv__uv_extreme_expf_denormalized_pos_float_1_2 executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_denormalized_pos_float_1_2() {
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
	for (ind=0;ind<80;ind++) {
		x=expf_uv__uv_extreme_expf_denormalized_pos_float_1_2_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_denormalized_pos_float_1_2_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_denormalized_pos_float_1_2: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_denormalized_pos_float_1_2_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==80) {
		PRINTF("expf_uv__uv_extreme_expf_denormalized_pos_float_1_2: successfully tested: 80 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_denormalized_pos_float_1_2: %d tests failed for expf (out of 80)\n",errors);
	}
#endif
	return errors;
}

/**
 * 57 specified tests for expf
 * -9.042265E-39; -6.955589E-39; -5.350453E-39; -4.115733E-39; -3.165948E-39; -2.435345E-39 ...
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_denormalized_neg_float_1_3 in the following table */
expf_uv__uv_extreme_expf_denormalized_neg_float_1_3_io_table_type expf_uv__uv_extreme_expf_denormalized_neg_float_1_3_io_table [57] = {
   {-9.04226510e-39,1.00000000},
   {-6.95558875e-39,1.00000000},
   {-5.35045321e-39,1.00000000},
   {-4.11573270e-39,1.00000000},
   {-3.16594802e-39,1.00000000},
   {-2.43534463e-39,1.00000000},
   {-1.87334267e-39,1.00000000},
   {-1.44103228e-39,1.00000000},
   {-1.10848594e-39,1.00000000},
   {-8.52681708e-40,1.00000000},
   {-6.55908575e-40,1.00000000},
   {-5.04544519e-40,1.00000000},
   {-3.88110629e-40,1.00000000},
   {-2.98546638e-40,1.00000000},
   {-2.29651799e-40,1.00000000},
   {-1.76654691e-40,1.00000000},
   {-1.35888116e-40,1.00000000},
   {-1.04529859e-40,1.00000000},
   {-8.04079072e-41,1.00000000},
   {-6.18519129e-41,1.00000000},
   {-4.75782868e-41,1.00000000},
   {-3.65991133e-41,1.00000000},
   {-2.81534874e-41,1.00000000},
   {-2.16570678e-41,1.00000000},
   {-1.66586361e-41,1.00000000},
   {-1.28148745e-41,1.00000000},
   {-9.85813470e-42,1.00000000},
   {-7.58382729e-42,1.00000000},
   {-5.83360551e-42,1.00000000},
   {-4.48695768e-42,1.00000000},
   {-3.45139812e-42,1.00000000},
   {-2.65546059e-42,1.00000000},
   {-2.04309316e-42,1.00000000},
   {-1.57225688e-42,1.00000000},
   {-1.20932057e-42,1.00000000},
   {-9.30462180e-43,1.00000000},
   {-7.16063515e-43,1.00000000},
   {-5.50710296e-43,1.00000000},
   {-4.23192136e-43,1.00000000},
   {-3.25101244e-43,1.00000000},
   {-2.49431127e-43,1.00000000},
   {-1.91977890e-43,1.00000000},
   {-1.47136339e-43,1.00000000},
   {-1.13505176e-43,1.00000000},
   {-8.68805048e-44,1.00000000},
   {-6.72623263e-44,1.00000000},
   {-5.18480432e-44,1.00000000},
   {-3.92363570e-44,1.00000000},
   {-3.08285662e-44,1.00000000},
   {-2.38220739e-44,1.00000000},
   {-1.82168800e-44,1.00000000},
   {-1.40129846e-44,1.00000000},
   {-1.12103877e-44,1.00000000},
   {-8.40779079e-45,1.00000000},
   {-7.00649232e-45,1.00000000},
   {-5.60519386e-45,1.00000000},
   {-4.20389539e-45,1.00000000},
};
/** function expf_uv__uv_extreme_expf_denormalized_neg_float_1_3 executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_denormalized_neg_float_1_3() {
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
	for (ind=0;ind<57;ind++) {
		x=expf_uv__uv_extreme_expf_denormalized_neg_float_1_3_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_denormalized_neg_float_1_3_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_denormalized_neg_float_1_3: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_denormalized_neg_float_1_3_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==57) {
		PRINTF("expf_uv__uv_extreme_expf_denormalized_neg_float_1_3: successfully tested: 57 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_denormalized_neg_float_1_3: %d tests failed for expf (out of 57)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for expf
 * range from from -340282346638528859811704183484516925440 to 340282346638528859811704183484516925440
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_extreme_range in the following table */
expf_uv__uv_extreme_expf_extreme_range_io_table_type expf_uv__uv_extreme_expf_extreme_range_io_table [20] = {
   {-3.40282347e+38,0.0},
   {-3.04463145e+38,0.0},
   {-2.68643963e+38,0.0},
   {-2.32824761e+38,0.0},
   {-1.97005559e+38,0.0},
   {-1.61186378e+38,0.0},
   {-1.25367176e+38,0.0},
   {-8.95479844e+37,0.0},
   {-5.37287926e+37,0.0},
   {-1.79095971e+37,0.0},
   {1.79095971e+37,INFINITY},
   {5.37287926e+37,INFINITY},
   {8.95479844e+37,INFINITY},
   {1.25367176e+38,INFINITY},
   {1.61186378e+38,INFINITY},
   {1.97005559e+38,INFINITY},
   {2.32824761e+38,INFINITY},
   {2.68643963e+38,INFINITY},
   {3.04463145e+38,INFINITY},
   {3.40282347e+38,INFINITY},
};
/** function expf_uv__uv_extreme_expf_extreme_range executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_extreme_range() {
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
	for (ind=0;ind<20;ind++) {
		x=expf_uv__uv_extreme_expf_extreme_range_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_extreme_range_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_extreme_range: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_extreme_range_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("expf_uv__uv_extreme_expf_extreme_range: successfully tested: 20 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_extreme_range: %d tests failed for expf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for expf
 * range from from -340282346638528859811704183484516925440 to 340282346638528859811704183484516925440
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_extreme_range_rnd in the following table */
expf_uv__uv_extreme_expf_extreme_range_rnd_io_table_type expf_uv__uv_extreme_expf_extreme_range_rnd_io_table [50] = {
   {-5.49688287e+37,0.0},
   {-1.61577899e+38,0.0},
   {1.51992301e+38,INFINITY},
   {-7.42261856e+37,0.0},
   {1.21115983e+38,INFINITY},
   {-1.89376757e+38,0.0},
   {-2.26456815e+38,0.0},
   {-3.21779029e+38,0.0},
   {-1.40751982e+38,0.0},
   {-3.19614003e+38,0.0},
   {3.11015539e+37,INFINITY},
   {-2.76709324e+38,0.0},
   {-2.69677656e+38,0.0},
   {-2.20756525e+38,0.0},
   {-3.36965929e+38,0.0},
   {2.91064686e+38,INFINITY},
   {-1.18021019e+38,0.0},
   {1.79726062e+38,INFINITY},
   {1.63774393e+38,INFINITY},
   {1.18378507e+38,INFINITY},
   {-2.48599973e+38,0.0},
   {2.66260314e+38,INFINITY},
   {-1.09901707e+38,0.0},
   {2.81925554e+38,INFINITY},
   {6.44120448e+37,INFINITY},
   {2.00776120e+38,INFINITY},
   {-1.18348864e+38,0.0},
   {-2.07865188e+38,0.0},
   {-2.45035056e+38,0.0},
   {1.62407947e+37,INFINITY},
   {-3.91385906e+37,0.0},
   {2.03464310e+38,INFINITY},
   {-3.27616144e+38,0.0},
   {-7.05027206e+37,0.0},
   {-1.62806182e+38,0.0},
   {8.43310342e+37,INFINITY},
   {-2.34669163e+38,0.0},
   {-8.27772495e+37,0.0},
   {-1.18473134e+38,0.0},
   {-7.29446720e+35,0.0},
   {2.44014972e+38,INFINITY},
   {1.07499185e+38,INFINITY},
   {2.86848866e+38,INFINITY},
   {2.07536288e+38,INFINITY},
   {1.12223607e+38,INFINITY},
   {3.37766151e+38,INFINITY},
   {-1.67296322e+38,0.0},
   {1.52102515e+38,INFINITY},
   {-2.66014937e+38,0.0},
   {1.88050247e+38,INFINITY},
};
/** function expf_uv__uv_extreme_expf_extreme_range_rnd executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_extreme_range_rnd() {
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
	for (ind=0;ind<50;ind++) {
		x=expf_uv__uv_extreme_expf_extreme_range_rnd_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_extreme_range_rnd_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_extreme_range_rnd: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_extreme_range_rnd_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("expf_uv__uv_extreme_expf_extreme_range_rnd: successfully tested: 50 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_extreme_range_rnd: %d tests failed for expf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 25 tests for approximation from 0.0 to 340282346638528859811704183484516925440
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_extreme_ramp_up in the following table */
expf_uv__uv_extreme_expf_extreme_ramp_up_io_table_type expf_uv__uv_extreme_expf_extreme_ramp_up_io_table [24] = {
   {1.00000000,2.71828175},
   {49.5000000,3.14468291e+21},
   {2450.25000,INFINITY},
   {121287.375,INFINITY},
   {6003725.00,INFINITY},
   {297184384.0,INFINITY},
   {1.47106273e+10,INFINITY},
   {7.28176067e+11,INFINITY},
   {3.60447161e+13,INFINITY},
   {1.78421344e+15,INFINITY},
   {8.83185636e+16,INFINITY},
   {4.37176901e+18,INFINITY},
   {2.16402571e+20,INFINITY},
   {1.07119277e+22,INFINITY},
   {5.30240417e+23,INFINITY},
   {2.62469014e+25,INFINITY},
   {1.29922160e+27,INFINITY},
   {6.43114677e+28,INFINITY},
   {3.18341751e+30,INFINITY},
   {1.57579167e+32,INFINITY},
   {7.80016858e+33,INFINITY},
   {3.86108348e+35,INFINITY},
   {1.91123630e+37,INFINITY},
   {INFINITY,INFINITY},
};
/** function expf_uv__uv_extreme_expf_extreme_ramp_up executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_extreme_ramp_up() {
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
	for (ind=0;ind<24;ind++) {
		x=expf_uv__uv_extreme_expf_extreme_ramp_up_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_extreme_ramp_up_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_extreme_ramp_up: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_extreme_ramp_up_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==24) {
		PRINTF("expf_uv__uv_extreme_expf_extreme_ramp_up: successfully tested: 24 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_extreme_ramp_up: %d tests failed for expf (out of 24)\n",errors);
	}
#endif
	return errors;
}

/**
 * 25 tests for approximation from 0.0 to -340282346638528859811704183484516925440
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_expf_extreme_ramp_down in the following table */
expf_uv__uv_extreme_expf_extreme_ramp_down_io_table_type expf_uv__uv_extreme_expf_extreme_ramp_down_io_table [24] = {
   {-1.00000000,0.367879450},
   {-50.5000000,1.16984589e-22},
   {-2550.25000,0.0},
   {-128787.625,0.0},
   {-6503775.00,0.0},
   {-328440640.0,0.0},
   {-1.65862523e+10,0.0},
   {-8.37605720e+11,0.0},
   {-4.22990903e+13,0.0},
   {-2.13610400e+15,0.0},
   {-1.07873249e+17,0.0},
   {-5.44759908e+18,0.0},
   {-2.75103755e+20,0.0},
   {-1.38927402e+22,0.0},
   {-7.01583360e+23,0.0},
   {-3.54299604e+25,0.0},
   {-1.78921302e+27,0.0},
   {-9.03552574e+28,0.0},
   {-4.56294060e+30,0.0},
   {-2.30428495e+32,0.0},
   {-1.16366388e+34,0.0},
   {-5.87650255e+35,0.0},
   {-2.96763369e+37,0.0},
   {-INFINITY,0.0},
};
/** function expf_uv__uv_extreme_expf_extreme_ramp_down executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_expf_extreme_ramp_down() {
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
	for (ind=0;ind<24;ind++) {
		x=expf_uv__uv_extreme_expf_extreme_ramp_down_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_expf_extreme_ramp_down_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_expf_extreme_ramp_down: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_expf_extreme_ramp_down_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==24) {
		PRINTF("expf_uv__uv_extreme_expf_extreme_ramp_down: successfully tested: 24 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_expf_extreme_ramp_down: %d tests failed for expf (out of 24)\n",errors);
	}
#endif
	return errors;
}

/**
 * 2 specified tests for expf
 * 3.141592653589793; -1.5707963267948966
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_corner_values in the following table */
expf_uv__uv_extreme_corner_values_io_table_type expf_uv__uv_extreme_corner_values_io_table [2] = {
   {3.14159274,23.1406956},
   {-1.57079637,0.207879573},
};
/** function expf_uv__uv_extreme_corner_values executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_corner_values() {
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
		x=expf_uv__uv_extreme_corner_values_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_corner_values_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_corner_values: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_corner_values_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==2) {
		PRINTF("expf_uv__uv_extreme_corner_values: successfully tested: 2 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_corner_values: %d tests failed for expf (out of 2)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for expf
 * range from from -1.5697963267948967 to -1.5717963267948964
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_pi_half in the following table */
expf_uv__uv_extreme_pi_half_io_table_type expf_uv__uv_extreme_pi_half_io_table [20] = {
   {-1.56979632,0.208087564},
   {-1.56990159,0.208065659},
   {-1.57000685,0.208043754},
   {-1.57011211,0.208021864},
   {-1.57021737,0.207999960},
   {-1.57032263,0.207978070},
   {-1.57042789,0.207956180},
   {-1.57053316,0.207934290},
   {-1.57063842,0.207912400},
   {-1.57074368,0.207890525},
   {-1.57084894,0.207868636},
   {-1.57095420,0.207846761},
   {-1.57105947,0.207824886},
   {-1.57116473,0.207803011},
   {-1.57126999,0.207781136},
   {-1.57137525,0.207759261},
   {-1.57148051,0.207737401},
   {-1.57158577,0.207715526},
   {-1.57169104,0.207693666},
   {-1.57179630,0.207671806},
};
/** function expf_uv__uv_extreme_pi_half executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_pi_half() {
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
	for (ind=0;ind<20;ind++) {
		x=expf_uv__uv_extreme_pi_half_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_pi_half_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_pi_half: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_pi_half_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("expf_uv__uv_extreme_pi_half: successfully tested: 20 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_pi_half: %d tests failed for expf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for expf
 * range from from -1.5697963267948967 to -1.5717963267948964
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_pi_half_rnd in the following table */
expf_uv__uv_extreme_pi_half_rnd_io_table_type expf_uv__uv_extreme_pi_half_rnd_io_table [50] = {
   {-1.56991100,0.208063692},
   {-1.56994927,0.208055735},
   {-1.57035625,0.207971081},
   {-1.57041633,0.207958579},
   {-1.57168341,0.207695246},
   {-1.57001781,0.208041474},
   {-1.57168031,0.207695901},
   {-1.57121491,0.207792580},
   {-1.57154727,0.207723528},
   {-1.56993318,0.208059087},
   {-1.57128477,0.207778066},
   {-1.57004118,0.208036616},
   {-1.57126939,0.207781255},
   {-1.57028043,0.207986847},
   {-1.57064676,0.207910672},
   {-1.57002592,0.208039790},
   {-1.57087398,0.207863435},
   {-1.57164156,0.207703948},
   {-1.57165647,0.207700849},
   {-1.57074213,0.207890838},
   {-1.57070971,0.207897589},
   {-1.57009315,0.208025798},
   {-1.57053304,0.207934320},
   {-1.57054925,0.207930952},
   {-1.57132292,0.207770139},
   {-1.56995392,0.208054766},
   {-1.57128155,0.207778737},
   {-1.57011199,0.208021879},
   {-1.57042551,0.207956672},
   {-1.57132149,0.207770437},
   {-1.57037985,0.207966164},
   {-1.57040071,0.207961828},
   {-1.57057881,0.207924798},
   {-1.57141089,0.207751855},
   {-1.57008386,0.208027735},
   {-1.57137215,0.207759902},
   {-1.57018065,0.208007604},
   {-1.57033205,0.207976118},
   {-1.57019186,0.208005264},
   {-1.57134748,0.207765043},
   {-1.57137132,0.207760081},
   {-1.57154644,0.207723707},
   {-1.57144189,0.207745418},
   {-1.57037103,0.207968011},
   {-1.57152569,0.207728013},
   {-1.57069600,0.207900435},
   {-1.56986761,0.208072722},
   {-1.57027674,0.207987621},
   {-1.57110298,0.207815841},
   {-1.56995988,0.208053529},
};
/** function expf_uv__uv_extreme_pi_half_rnd executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_pi_half_rnd() {
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
	for (ind=0;ind<50;ind++) {
		x=expf_uv__uv_extreme_pi_half_rnd_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_pi_half_rnd_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_pi_half_rnd: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_pi_half_rnd_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("expf_uv__uv_extreme_pi_half_rnd: successfully tested: 50 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_pi_half_rnd: %d tests failed for expf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for expf
 * range from from -1001 to -999
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_mi_thousand in the following table */
expf_uv__uv_extreme_mi_thousand_io_table_type expf_uv__uv_extreme_mi_thousand_io_table [20] = {
   {-1001.00000,0.0},
   {-1000.89471,0.0},
   {-1000.78949,0.0},
   {-1000.68420,0.0},
   {-1000.57892,0.0},
   {-1000.47369,0.0},
   {-1000.36841,0.0},
   {-1000.26318,0.0},
   {-1000.15790,0.0},
   {-1000.05261,0.0},
   {-999.947388,0.0},
   {-999.842102,0.0},
   {-999.736816,0.0},
   {-999.631592,0.0},
   {-999.526306,0.0},
   {-999.421082,0.0},
   {-999.315796,0.0},
   {-999.210510,0.0},
   {-999.105286,0.0},
   {-999.000000,0.0},
};
/** function expf_uv__uv_extreme_mi_thousand executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_mi_thousand() {
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
	for (ind=0;ind<20;ind++) {
		x=expf_uv__uv_extreme_mi_thousand_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_mi_thousand_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_mi_thousand: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_mi_thousand_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("expf_uv__uv_extreme_mi_thousand: successfully tested: 20 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_mi_thousand: %d tests failed for expf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for expf
 * range from from -1001 to -999
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_mi_thousand_rnd in the following table */
expf_uv__uv_extreme_mi_thousand_rnd_io_table_type expf_uv__uv_extreme_mi_thousand_rnd_io_table [50] = {
   {-1000.19519,0.0},
   {-999.677124,0.0},
   {-1000.52020,0.0},
   {-999.073975,0.0},
   {-1000.01288,0.0},
   {-1000.20605,0.0},
   {-1000.22229,0.0},
   {-999.158936,0.0},
   {-1000.11731,0.0},
   {-1000.59998,0.0},
   {-1000.02100,0.0},
   {-999.212280,0.0},
   {-1000.94928,0.0},
   {-999.177979,0.0},
   {-1000.10559,0.0},
   {-1000.43262,0.0},
   {-999.157166,0.0},
   {-1000.97070,0.0},
   {-1000.94421,0.0},
   {-1000.75519,0.0},
   {-999.346130,0.0},
   {-1000.51984,0.0},
   {-1000.56915,0.0},
   {-999.077515,0.0},
   {-1000.31427,0.0},
   {-999.078247,0.0},
   {-999.693298,0.0},
   {-999.475525,0.0},
   {-999.374878,0.0},
   {-999.163879,0.0},
   {-999.324463,0.0},
   {-999.680481,0.0},
   {-1000.92603,0.0},
   {-999.915527,0.0},
   {-999.382568,0.0},
   {-1000.21313,0.0},
   {-999.980286,0.0},
   {-999.479553,0.0},
   {-1000.89783,0.0},
   {-1000.18555,0.0},
   {-999.382202,0.0},
   {-1000.28821,0.0},
   {-999.319458,0.0},
   {-999.207703,0.0},
   {-1000.61487,0.0},
   {-1000.01764,0.0},
   {-999.917969,0.0},
   {-999.991577,0.0},
   {-999.865967,0.0},
   {-999.000122,0.0},
};
/** function expf_uv__uv_extreme_mi_thousand_rnd executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_mi_thousand_rnd() {
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
	for (ind=0;ind<50;ind++) {
		x=expf_uv__uv_extreme_mi_thousand_rnd_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_mi_thousand_rnd_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_mi_thousand_rnd: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_mi_thousand_rnd_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("expf_uv__uv_extreme_mi_thousand_rnd: successfully tested: 50 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_mi_thousand_rnd: %d tests failed for expf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for expf
 * range from from 9999.9 to 10000.01
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_ten_thousand in the following table */
expf_uv__uv_extreme_ten_thousand_io_table_type expf_uv__uv_extreme_ten_thousand_io_table [20] = {
   {9999.90039,INFINITY},
   {9999.90625,INFINITY},
   {9999.91211,INFINITY},
   {9999.91797,INFINITY},
   {9999.92383,INFINITY},
   {9999.92871,INFINITY},
   {9999.93457,INFINITY},
   {9999.94043,INFINITY},
   {9999.94629,INFINITY},
   {9999.95215,INFINITY},
   {9999.95801,INFINITY},
   {9999.96387,INFINITY},
   {9999.96973,INFINITY},
   {9999.97559,INFINITY},
   {9999.98145,INFINITY},
   {9999.98633,INFINITY},
   {9999.99219,INFINITY},
   {9999.99805,INFINITY},
   {10000.0039,INFINITY},
   {10000.0098,INFINITY},
};
/** function expf_uv__uv_extreme_ten_thousand executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_ten_thousand() {
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
	for (ind=0;ind<20;ind++) {
		x=expf_uv__uv_extreme_ten_thousand_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_ten_thousand_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_ten_thousand: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_ten_thousand_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("expf_uv__uv_extreme_ten_thousand: successfully tested: 20 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_ten_thousand: %d tests failed for expf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 38 (12 omitted out of input range) random tests for expf
 * range from from 9999.9 to 10000.01
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_ten_thousand_rnd in the following table */
expf_uv__uv_extreme_ten_thousand_rnd_io_table_type expf_uv__uv_extreme_ten_thousand_rnd_io_table [38] = {
   {9999.93750,INFINITY},
   {9999.94727,INFINITY},
   {9999.95508,INFINITY},
   {9999.92676,INFINITY},
   {9999.96094,INFINITY},
   {9999.96973,INFINITY},
   {9999.91797,INFINITY},
   {9999.91504,INFINITY},
   {9999.91602,INFINITY},
   {9999.92773,INFINITY},
   {9999.92188,INFINITY},
   {9999.95020,INFINITY},
   {9999.93652,INFINITY},
   {9999.93066,INFINITY},
   {9999.96289,INFINITY},
   {9999.93262,INFINITY},
   {9999.94336,INFINITY},
   {9999.94922,INFINITY},
   {9999.99414,INFINITY},
   {9999.99121,INFINITY},
   {9999.98730,INFINITY},
   {9999.99219,INFINITY},
   {9999.98438,INFINITY},
   {9999.97852,INFINITY},
   {9999.98828,INFINITY},
   {9999.91699,INFINITY},
   {9999.96875,INFINITY},
   {9999.92383,INFINITY},
   {9999.91992,INFINITY},
   {9999.93848,INFINITY},
   {9999.97656,INFINITY},
   {9999.90918,INFINITY},
   {9999.91113,INFINITY},
   {10000.0088,INFINITY},
   {9999.91406,INFINITY},
   {9999.98633,INFINITY},
   {9999.94238,INFINITY},
   {9999.99902,INFINITY},
};
/** function expf_uv__uv_extreme_ten_thousand_rnd executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_ten_thousand_rnd() {
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
	for (ind=0;ind<38;ind++) {
		x=expf_uv__uv_extreme_ten_thousand_rnd_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_ten_thousand_rnd_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_ten_thousand_rnd: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_ten_thousand_rnd_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==38) {
		PRINTF("expf_uv__uv_extreme_ten_thousand_rnd: successfully tested: 38 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_ten_thousand_rnd: %d tests failed for expf (out of 38)\n",errors);
	}
#endif
	return errors;
}

/**
 * 60 tests for approximation from -1.00000000 to 0.0
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_approx_zero in the following table */
expf_uv__uv_extreme_approx_zero_io_table_type expf_uv__uv_extreme_approx_zero_io_table [59] = {
   {-0.166666687,0.846481681},
   {-0.0277777910,0.972604454},
   {-0.00462963246,0.995381057},
   {-0.000771605643,0.999228716},
   {-0.000128600979,0.999871433},
   {-2.14335014e-05,0.999978542},
   {-3.57225144e-06,0.999996424},
   {-5.95375468e-07,0.999999404},
   {-9.92292826e-08,0.999999881},
   {-1.65382161e-08,1.00000000},
   {-2.75637024e-09,1.00000000},
   {-4.59395189e-10,1.00000000},
   {-7.65658925e-11,1.00000000},
   {-1.27609867e-11,1.00000000},
   {-2.12683170e-12,1.00000000},
   {-3.54471986e-13,1.00000000},
   {-5.90786643e-14,1.00000000},
   {-9.84644630e-15,1.00000000},
   {-1.64107467e-15,1.00000000},
   {-2.73512550e-16,1.00000000},
   {-4.55854339e-17,1.00000000},
   {-7.59757452e-18,1.00000000},
   {-1.26626256e-18,1.00000000},
   {-2.11043760e-19,1.00000000},
   {-3.51739685e-20,1.00000000},
   {-5.86232917e-21,1.00000000},
   {-9.77054996e-22,1.00000000},
   {-1.62842550e-22,1.00000000},
   {-2.71404250e-23,1.00000000},
   {-4.52340574e-24,1.00000000},
   {-7.53900956e-25,1.00000000},
   {-1.25650209e-25,1.00000000},
   {-2.09417056e-26,1.00000000},
   {-3.49028426e-27,1.00000000},
   {-5.81714236e-28,1.00000000},
   {-9.69523727e-29,1.00000000},
   {-1.61587328e-29,1.00000000},
   {-2.69312263e-30,1.00000000},
   {-4.48853772e-31,1.00000000},
   {-7.48089777e-32,1.00000000},
   {-1.24681630e-32,1.00000000},
   {-2.07802789e-33,1.00000000},
   {-3.46338105e-34,1.00000000},
   {-5.77230174e-35,1.00000000},
   {-9.62050578e-36,1.00000000},
   {-1.60341763e-36,1.00000000},
   {-2.67236361e-37,1.00000000},
   {-4.45394085e-38,1.00000000},
   {-7.42323568e-39,1.00000000},
   {-1.23720641e-39,1.00000000},
   {-2.06201069e-40,1.00000000},
   {-3.43668448e-41,1.00000000},
   {-5.72850812e-42,1.00000000},
   {-9.54284254e-43,1.00000000},
   {-1.59748025e-43,1.00000000},
   {-2.66246708e-44,1.00000000},
   {-4.20389539e-45,1.00000000},
   {-1.40129846e-45,1.00000000},
   {0.0,1.00000000},
};
/** function expf_uv__uv_extreme_approx_zero executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_approx_zero() {
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
	for (ind=0;ind<59;ind++) {
		x=expf_uv__uv_extreme_approx_zero_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_approx_zero_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_approx_zero: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_approx_zero_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==59) {
		PRINTF("expf_uv__uv_extreme_approx_zero: successfully tested: 59 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_approx_zero: %d tests failed for expf (out of 59)\n",errors);
	}
#endif
	return errors;
}

/**
 * 27 tests for approximation from -3.14159274 to 2.356194490192345
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_approx_pi_34 in the following table */
expf_uv__uv_extreme_approx_pi_34_io_table_type expf_uv__uv_extreme_approx_pi_34_io_table [26] = {
   {-0.392699003,0.675231934},
   {0.981747746,2.66911721},
   {1.66897106,5.30670452},
   {2.01258278,7.48261833},
   {2.18438864,8.88521481},
   {2.27029157,9.68222332},
   {2.31324291,10.1071482},
   {2.33471870,10.3265543},
   {2.34545660,10.4380379},
   {2.35082555,10.4942293},
   {2.35350990,10.5224380},
   {2.35485220,10.5365715},
   {2.35552335,10.5436459},
   {2.35585880,10.5471830},
   {2.35602665,10.5489531},
   {2.35611057,10.5498390},
   {2.35615253,10.5502815},
   {2.35617352,10.5505028},
   {2.35618401,10.5506134},
   {2.35618925,10.5506687},
   {2.35619187,10.5506964},
   {2.35619307,10.5507088},
   {2.35619378,10.5507164},
   {2.35619402,10.5507193},
   {2.35619426,10.5507212},
   {2.35619450,10.5507240},
};
/** function expf_uv__uv_extreme_approx_pi_34 executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_approx_pi_34() {
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
	for (ind=0;ind<26;ind++) {
		x=expf_uv__uv_extreme_approx_pi_34_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_approx_pi_34_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_approx_pi_34: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_approx_pi_34_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==26) {
		PRINTF("expf_uv__uv_extreme_approx_pi_34: successfully tested: 26 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_approx_pi_34: %d tests failed for expf (out of 26)\n",errors);
	}
#endif
	return errors;
}

/**
 * 1 specified tests for expf
 * -720
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_manual_values in the following table */
expf_uv__uv_extreme_manual_values_io_table_type expf_uv__uv_extreme_manual_values_io_table [1] = {
   {-720,0.0},
};
/** function expf_uv__uv_extreme_manual_values executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_manual_values() {
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
	for (ind=0;ind<1;ind++) {
		x=expf_uv__uv_extreme_manual_values_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_manual_values_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_manual_values: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_manual_values_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==1) {
		PRINTF("expf_uv__uv_extreme_manual_values: successfully tested: 1 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_manual_values: %d tests failed for expf (out of 1)\n",errors);
	}
#endif
	return errors;
}

/**
 * 2 specified tests for expf
 * -87; -88
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_coverage_values in the following table */
expf_uv__uv_extreme_coverage_values_io_table_type expf_uv__uv_extreme_coverage_values_io_table [2] = {
   {-87.0000000,1.64581145e-38},
   {-88.0000000,6.05460149e-39},
};
/** function expf_uv__uv_extreme_coverage_values executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_coverage_values() {
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
		x=expf_uv__uv_extreme_coverage_values_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_coverage_values_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_coverage_values: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_coverage_values_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==2) {
		PRINTF("expf_uv__uv_extreme_coverage_values: successfully tested: 2 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_coverage_values: %d tests failed for expf (out of 2)\n",errors);
	}
#endif
	return errors;
}

/**
 * 53 linear tests for expf
 * range from from 42.8457 to 42.8459
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_coverage_floats in the following table */
expf_uv__uv_extreme_coverage_floats_io_table_type expf_uv__uv_extreme_coverage_floats_io_table [53] = {
   {42.8456993,4.05182597e+18},
   {42.8457031,4.05184136e+18},
   {42.8457069,4.05185675e+18},
   {42.8457108,4.05187242e+18},
   {42.8457146,4.05188782e+18},
   {42.8457184,4.05190321e+18},
   {42.8457222,4.05191860e+18},
   {42.8457260,4.05193427e+18},
   {42.8457298,4.05194966e+18},
   {42.8457336,4.05196506e+18},
   {42.8457375,4.05198045e+18},
   {42.8457413,4.05199612e+18},
   {42.8457451,4.05201151e+18},
   {42.8457489,4.05202690e+18},
   {42.8457527,4.05204230e+18},
   {42.8457565,4.05205769e+18},
   {42.8457603,4.05207336e+18},
   {42.8457642,4.05208875e+18},
   {42.8457680,4.05210414e+18},
   {42.8457718,4.05211954e+18},
   {42.8457756,4.05213521e+18},
   {42.8457794,4.05215060e+18},
   {42.8457832,4.05216599e+18},
   {42.8457870,4.05218138e+18},
   {42.8457909,4.05219705e+18},
   {42.8457947,4.05221245e+18},
   {42.8457985,4.05222784e+18},
   {42.8458023,4.05224323e+18},
   {42.8458061,4.05225890e+18},
   {42.8458099,4.05227429e+18},
   {42.8458138,4.05228969e+18},
   {42.8458176,4.05230508e+18},
   {42.8458214,4.05232047e+18},
   {42.8458252,4.05233614e+18},
   {42.8458290,4.05235153e+18},
   {42.8458328,4.05236693e+18},
   {42.8458366,4.05238232e+18},
   {42.8458405,4.05239799e+18},
   {42.8458443,4.05241338e+18},
   {42.8458481,4.05242877e+18},
   {42.8458519,4.05244417e+18},
   {42.8458557,4.05245984e+18},
   {42.8458595,4.05247523e+18},
   {42.8458633,4.05249062e+18},
   {42.8458672,4.05250602e+18},
   {42.8458710,4.05252168e+18},
   {42.8458748,4.05253708e+18},
   {42.8458786,4.05255247e+18},
   {42.8458824,4.05256786e+18},
   {42.8458862,4.05258353e+18},
   {42.8458900,4.05259892e+18},
   {42.8458939,4.05261432e+18},
   {42.8458977,4.05262971e+18},
};
/** function expf_uv__uv_extreme_coverage_floats executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_coverage_floats() {
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
	for (ind=0;ind<53;ind++) {
		x=expf_uv__uv_extreme_coverage_floats_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_coverage_floats_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_coverage_floats: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_coverage_floats_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==53) {
		PRINTF("expf_uv__uv_extreme_coverage_floats: successfully tested: 53 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_coverage_floats: %d tests failed for expf (out of 53)\n",errors);
	}
#endif
	return errors;
}

/**
 * 100 linear tests for expf
 * range from from 1.38628 to 1.38630
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_extreme_coverage_floats3 in the following table */
expf_uv__uv_extreme_coverage_floats3_io_table_type expf_uv__uv_extreme_coverage_floats3_io_table [100] = {
   {1.38627994,3.99994230},
   {1.38628006,3.99994278},
   {1.38628018,3.99994326},
   {1.38628030,3.99994373},
   {1.38628042,3.99994421},
   {1.38628054,3.99994469},
   {1.38628066,3.99994516},
   {1.38628078,3.99994564},
   {1.38628089,3.99994612},
   {1.38628101,3.99994659},
   {1.38628113,3.99994707},
   {1.38628125,3.99994755},
   {1.38628137,3.99994802},
   {1.38628149,3.99994850},
   {1.38628161,3.99994898},
   {1.38628173,3.99994946},
   {1.38628185,3.99994993},
   {1.38628197,3.99995041},
   {1.38628209,3.99995089},
   {1.38628221,3.99995136},
   {1.38628232,3.99995184},
   {1.38628244,3.99995232},
   {1.38628256,3.99995279},
   {1.38628268,3.99995327},
   {1.38628280,3.99995375},
   {1.38628292,3.99995422},
   {1.38628304,3.99995470},
   {1.38628316,3.99995518},
   {1.38628328,3.99995565},
   {1.38628340,3.99995613},
   {1.38628352,3.99995661},
   {1.38628364,3.99995708},
   {1.38628376,3.99995756},
   {1.38628387,3.99995804},
   {1.38628399,3.99995852},
   {1.38628411,3.99995899},
   {1.38628423,3.99995947},
   {1.38628435,3.99995995},
   {1.38628447,3.99996042},
   {1.38628459,3.99996090},
   {1.38628471,3.99996138},
   {1.38628483,3.99996185},
   {1.38628495,3.99996233},
   {1.38628507,3.99996281},
   {1.38628519,3.99996328},
   {1.38628531,3.99996376},
   {1.38628542,3.99996424},
   {1.38628554,3.99996471},
   {1.38628566,3.99996519},
   {1.38628578,3.99996567},
   {1.38628590,3.99996614},
   {1.38628602,3.99996662},
   {1.38628614,3.99996710},
   {1.38628626,3.99996758},
   {1.38628638,3.99996805},
   {1.38628650,3.99996853},
   {1.38628662,3.99996901},
   {1.38628674,3.99996948},
   {1.38628685,3.99996996},
   {1.38628697,3.99997044},
   {1.38628709,3.99997091},
   {1.38628721,3.99997139},
   {1.38628733,3.99997187},
   {1.38628745,3.99997234},
   {1.38628757,3.99997282},
   {1.38628769,3.99997330},
   {1.38628781,3.99997377},
   {1.38628793,3.99997425},
   {1.38628805,3.99997473},
   {1.38628817,3.99997520},
   {1.38628829,3.99997568},
   {1.38628840,3.99997616},
   {1.38628852,3.99997663},
   {1.38628864,3.99997711},
   {1.38628876,3.99997759},
   {1.38628888,3.99997807},
   {1.38628900,3.99997854},
   {1.38628912,3.99997902},
   {1.38628924,3.99997950},
   {1.38628936,3.99997997},
   {1.38628948,3.99998045},
   {1.38628960,3.99998093},
   {1.38628972,3.99998140},
   {1.38628983,3.99998188},
   {1.38628995,3.99998236},
   {1.38629007,3.99998283},
   {1.38629019,3.99998331},
   {1.38629031,3.99998379},
   {1.38629043,3.99998426},
   {1.38629055,3.99998474},
   {1.38629067,3.99998522},
   {1.38629079,3.99998569},
   {1.38629091,3.99998617},
   {1.38629103,3.99998665},
   {1.38629115,3.99998713},
   {1.38629127,3.99998760},
   {1.38629138,3.99998808},
   {1.38629150,3.99998856},
   {1.38629162,3.99998903},
   {1.38629174,3.99998951},
};
/** function expf_uv__uv_extreme_coverage_floats3 executes the tests and returns the number of failing tests */
int expf_uv__uv_extreme_coverage_floats3() {
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
	for (ind=0;ind<100;ind++) {
		x=expf_uv__uv_extreme_coverage_floats3_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_extreme_coverage_floats3_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME,10.0,&max_dif_below_expf_uv__uv_extreme,&max_dif_above_expf_uv__uv_extreme)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_extreme_coverage_floats3: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_extreme_coverage_floats3_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==100) {
		PRINTF("expf_uv__uv_extreme_coverage_floats3: successfully tested: 100 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_extreme_coverage_floats3: %d tests failed for expf (out of 100)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (24 functions) of expf_uv__uv_extreme
*/
int expf_uv__uv_extreme_main_test() {
	int errors=0;
	int index=0;
	errors+=expf_uv__uv_extreme_extreme_values();                  /* 1. 3 tests */
	errors+=expf_uv__uv_extreme_expf_nan();                        /* 2. 1 tests */
	errors+=expf_uv__uv_extreme_expf_infinity();                   /* 3. 2 tests */
	errors+=expf_uv__uv_extreme_expf_denormalized_pos_float_2_0(); /* 4. 23 tests */
	errors+=expf_uv__uv_extreme_expf_denormalized_neg_float_2_0(); /* 5. 23 tests */
	errors+=expf_uv__uv_extreme_expf_denormalized_pos_float_1_2(); /* 6. 80 tests */
	errors+=expf_uv__uv_extreme_expf_denormalized_neg_float_1_3(); /* 7. 57 tests */
	errors+=expf_uv__uv_extreme_expf_extreme_range();              /* 8. 20 tests */
	errors+=expf_uv__uv_extreme_expf_extreme_range_rnd();          /* 9. 50 tests */
	errors+=expf_uv__uv_extreme_expf_extreme_ramp_up();            /* 10. 24 tests */
	errors+=expf_uv__uv_extreme_expf_extreme_ramp_down();          /* 11. 24 tests */
	errors+=expf_uv__uv_extreme_corner_values();                   /* 12. 2 tests */
	errors+=expf_uv__uv_extreme_pi_half();                         /* 13. 20 tests */
	errors+=expf_uv__uv_extreme_pi_half_rnd();                     /* 14. 50 tests */
	errors+=expf_uv__uv_extreme_mi_thousand();                     /* 15. 20 tests */
	errors+=expf_uv__uv_extreme_mi_thousand_rnd();                 /* 16. 50 tests */
	errors+=expf_uv__uv_extreme_ten_thousand();                    /* 17. 20 tests */
	errors+=expf_uv__uv_extreme_ten_thousand_rnd();                /* 18. 38 tests */
	errors+=expf_uv__uv_extreme_approx_zero();                     /* 19. 59 tests */
	errors+=expf_uv__uv_extreme_approx_pi_34();                    /* 20. 26 tests */
	errors+=expf_uv__uv_extreme_manual_values();                   /* 21. 1 tests */
	errors+=expf_uv__uv_extreme_coverage_values();                 /* 22. 2 tests */
	errors+=expf_uv__uv_extreme_coverage_floats();                 /* 23. 53 tests */
	errors+=expf_uv__uv_extreme_coverage_floats3();                /* 24. 100 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of expf_uv__uv_extreme: successfully tested ALL 748 cases for expf\n");
	} else {
		PRINTF("SUMMARY of expf_uv__uv_extreme: %d tests failed in expf_uv__uv_extreme (out of 748 in expf_uv__uv_extreme)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.9g. ABS_TOLERANCE_EXPF_UV__UV_EXTREME=%.9g\n",expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,ABS_TOLERANCE_EXPF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.9g. REL_TOLERANCE_EXPF_UV__UV_EXTREME=%.9g\n",expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,REL_TOLERANCE_EXPF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.9g. ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME=%.9g\n",expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,ABS_REL_TOLERANCE_EXPF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.9g. ULP_TOLERANCE_EXPF_UV__UV_EXTREME=%.9g\n",expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,ULP_TOLERANCE_EXPF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.9g. EXAKT_TOLERANCE_EXPF_UV__UV_EXTREME=%.9g\n",expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,EXAKT_TOLERANCE_EXPF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.9g. EQUAL_TOLERANCE_EXPF_UV__UV_EXTREME=%.9g\n",expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,EQUAL_TOLERANCE_EXPF_UV__UV_EXTREME);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.9g. BINHEX_TOLERANCE_EXPF_UV__UV_EXTREME=%.9g\n",expf_uv__uv_extreme_all_deviation_results_float[index].max_diff_value,BINHEX_TOLERANCE_EXPF_UV__UV_EXTREME);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls expf_uv__uv_extreme_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = expf_uv__uv_extreme_main_test();
	return result;
}
#endif /* NO_MAIN */

