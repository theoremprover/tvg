/**
 * this file (expf_uv__uv_terms.c) contains test cases for expf
 * all test cases for expf have been specified and split into separate files
 * this file contains 100 test cases in 2 functions for the following purpose:
 * 50 tests for combination with expf
 * combination term: (expf(x)=expf(x)) checked with int
 * range from from -4 to 4
 * returns the number of failing tests
 * 50 tests for combination with expf
 * combination term: ((2*expf(((3*x)+4)))-(1)) checked with Default Equality
 * range from from -4 to 4
 * returns the number of failing tests
 * this has been generated using the following settings:
 *  - Input-Outputs: variables for inputs and outputs
 *  - Casts: no casts for inputs and outputs
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
//#include <htc-aeabi.h>
#include "double_data.h"
#include "float_data.h"


/* define DEBUG/SUMMARY using compilation option -DDEBUG / -DSUMMARY */
#if ( defined(DEBUG) || defined(SUMMARY) ) && !defined(PRINTF)
#include <stdio.h>
#endif

#include "float_compares.h"

#define ABS_REL_TOLERANCE_EXPF_UV__UV_TERMS 1.0E-5

float max_dif_below_expf_uv__uv_terms=0.0;
float max_dif_above_expf_uv__uv_terms=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of expf_combine */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_terms_expf_combine_io_table_type;
/* type for expected input & output values of expf_deterministic */ 
typedef struct {
    float expf_x;
    int expf_out;
} expf_uv__uv_terms_expf_deterministic_io_table_type;
/**
 * 50 tests for combination with expf
 * combination term: (expf(x)=expf(x)) checked with int
 * range from from -4 to 4
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_terms_expf_deterministic in the following table */
expf_uv__uv_terms_expf_deterministic_io_table_type expf_uv__uv_terms_expf_deterministic_io_table [50] = {
   {-4.00000000,1.00000000},
   {-3.83673477,1.00000000},
   {-3.67346931,1.00000000},
   {-3.51020408,1.00000000},
   {-3.34693885,1.00000000},
   {-3.18367338,1.00000000},
   {-3.02040815,1.00000000},
   {-2.85714293,1.00000000},
   {-2.69387746,1.00000000},
   {-2.53061223,1.00000000},
   {-2.36734700,1.00000000},
   {-2.20408154,1.00000000},
   {-2.04081631,1.00000000},
   {-1.87755108,1.00000000},
   {-1.71428573,1.00000000},
   {-1.55102038,1.00000000},
   {-1.38775516,1.00000000},
   {-1.22448981,1.00000000},
   {-1.06122446,1.00000000},
   {-0.897959173,1.00000000},
   {-0.734693885,1.00000000},
   {-0.571428597,1.00000000},
   {-0.408163279,1.00000000},
   {-0.244897962,1.00000000},
   {-0.0816326514,1.00000000},
   {0.0816326514,1.00000000},
   {0.244897962,1.00000000},
   {0.408163279,1.00000000},
   {0.571428597,1.00000000},
   {0.734693885,1.00000000},
   {0.897959173,1.00000000},
   {1.06122446,1.00000000},
   {1.22448981,1.00000000},
   {1.38775516,1.00000000},
   {1.55102038,1.00000000},
   {1.71428573,1.00000000},
   {1.87755108,1.00000000},
   {2.04081631,1.00000000},
   {2.20408154,1.00000000},
   {2.36734700,1.00000000},
   {2.53061223,1.00000000},
   {2.69387746,1.00000000},
   {2.85714293,1.00000000},
   {3.02040815,1.00000000},
   {3.18367338,1.00000000},
   {3.34693885,1.00000000},
   {3.51020408,1.00000000},
   {3.67346931,1.00000000},
   {3.83673477,1.00000000},
   {4.00000000,1.00000000},
};
/** function expf_uv__uv_terms_expf_deterministic executes the tests and returns the number of failing tests */
int expf_uv__uv_terms_expf_deterministic() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	float x;
	/* output variable (IO_VV) */
	int res;
	int res1;
	int res2;
	/* main test starts here */
	for (ind=0;ind<50;ind++) {
		x=expf_uv__uv_terms_expf_deterministic_io_table[ind].expf_x;
		res=(expf(x)==expf(x));
		if (res!=expf_uv__uv_terms_expf_deterministic_io_table[ind].expf_out) {
			/* cast to int is only defined if value is in range, otherwise ignore deviation */
			if ((expf(x)==expf(x))>=-2147483648.0 && (expf(x)==expf(x))<=2147483647.0) {
				/* test for integer rounding errors */
				res1=((expf(x)==expf(x))+0.000001);
				res2=((expf(x)==expf(x))-0.000001);
				if ( (res1!=expf_uv__uv_terms_expf_deterministic_io_table[ind].expf_out)
				 && (res2!=expf_uv__uv_terms_expf_deterministic_io_table[ind].expf_out) ) {
					errors++;
#if defined(DEBUG)
					PRINTF("expf_uv__uv_terms_expf_deterministic: test %d (IO_VV) failed for expf(%.9g): Expected %d, found %d\n",ind+1,x,expf_uv__uv_terms_expf_deterministic_io_table[ind].expf_out,res);
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
	if (errors==0 && passes==50) {
		PRINTF("expf_uv__uv_terms_expf_deterministic: successfully tested: 50 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_terms_expf_deterministic: %d tests failed for expf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 tests for combination with expf
 * combination term: ((2*expf(((3*x)+4)))-(1)) checked with Default Equality
 * range from from -4 to 4
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_terms_expf_combine in the following table */
expf_uv__uv_terms_expf_combine_io_table_type expf_uv__uv_terms_expf_combine_io_table [50] = {
   {-4.00000000,-0.999329090},
   {-3.83673477,-0.998905063},
   {-3.67346931,-0.998213053},
   {-3.51020408,-0.997083783},
   {-3.34693885,-0.995240748},
   {-3.18367338,-0.992233038},
   {-3.02040815,-0.987324417},
   {-2.85714293,-0.979313672},
   {-2.69387746,-0.966240227},
   {-2.53061223,-0.944904625},
   {-2.36734700,-0.910085320},
   {-2.20408154,-0.853260636},
   {-2.04081631,-0.760523915},
   {-1.87755108,-0.609179139},
   {-1.71428573,-0.362186849},
   {-1.55102038,0.0409005880},
   {-1.38775516,0.698731780},
   {-1.22448981,1.77230096},
   {-1.06122446,3.52434921},
   {-0.897959173,6.38366222},
   {-0.734693885,11.0500116},
   {-0.571428597,18.6654110},
   {-0.408163279,31.0936279},
   {-0.244897962,51.3762474},
   {-0.0816326514,84.4771729},
   {0.0816326514,138.497269},
   {0.244897962,226.657242},
   {0.408163279,370.532684},
   {0.571428597,605.335266},
   {0.734693885,988.528625},
   {0.897959173,1613.89368},
   {1.06122446,2634.47998},
   {1.22448981,4300.05957},
   {1.38775516,7018.25537},
   {1.55102038,11454.3037},
   {1.71428573,18693.8574},
   {1.87755108,30508.7148},
   {2.04081631,49790.3281},
   {2.20408154,81257.5859},
   {2.36734700,132611.734},
   {2.53061223,216420.703},
   {2.69387746,353195.500},
   {2.85714293,576409.563},
   {3.02040815,940691.125},
   {3.18367338,1535192.25},
   {3.34693885,2505410.25},
   {3.51020408,4088787.00},
   {3.67346931,6672830.50},
   {3.83673477,10889955.0},
   {4.00000000,17772220.0},
};
/** function expf_uv__uv_terms_expf_combine executes the tests and returns the number of failing tests */
int expf_uv__uv_terms_expf_combine() {
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
		x=expf_uv__uv_terms_expf_combine_io_table[ind].expf_x;
		res=((2*expf(((3*x)+4)))-(1));
		if (test_compare_float(res,expf_uv__uv_terms_expf_combine_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_TERMS,ABS_REL_TOLERANCE_EXPF_UV__UV_TERMS,10.0,&max_dif_below_expf_uv__uv_terms,&max_dif_above_expf_uv__uv_terms)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_terms_expf_combine: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_terms_expf_combine_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("expf_uv__uv_terms_expf_combine: successfully tested: 50 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_terms_expf_combine: %d tests failed for expf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (2 functions) of expf_uv__uv_terms
*/
int expf_uv__uv_terms_main_test() {
	int errors=0;
	int index=0;
	errors+=expf_uv__uv_terms_expf_deterministic(); /* 1. 50 tests */
	errors+=expf_uv__uv_terms_expf_combine();       /* 2. 50 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of expf_uv__uv_terms: successfully tested ALL 100 cases for expf\n");
	} else {
		PRINTF("SUMMARY of expf_uv__uv_terms: %d tests failed in expf_uv__uv_terms (out of 100 in expf_uv__uv_terms)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.9g. ABS_TOLERANCE_EXPF_UV__UV_TERMS=%.9g\n",expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,ABS_TOLERANCE_EXPF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.9g. REL_TOLERANCE_EXPF_UV__UV_TERMS=%.9g\n",expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,REL_TOLERANCE_EXPF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.9g. ABS_REL_TOLERANCE_EXPF_UV__UV_TERMS=%.9g\n",expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,ABS_REL_TOLERANCE_EXPF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.9g. ULP_TOLERANCE_EXPF_UV__UV_TERMS=%.9g\n",expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,ULP_TOLERANCE_EXPF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.9g. EXAKT_TOLERANCE_EXPF_UV__UV_TERMS=%.9g\n",expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,EXAKT_TOLERANCE_EXPF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.9g. EQUAL_TOLERANCE_EXPF_UV__UV_TERMS=%.9g\n",expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,EQUAL_TOLERANCE_EXPF_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.9g. BINHEX_TOLERANCE_EXPF_UV__UV_TERMS=%.9g\n",expf_uv__uv_terms_all_deviation_results_float[index].max_diff_value,BINHEX_TOLERANCE_EXPF_UV__UV_TERMS);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls expf_uv__uv_terms_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = expf_uv__uv_terms_main_test();
	return result;
}
#endif /* NO_MAIN */

