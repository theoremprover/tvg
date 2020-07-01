/**
 * this file (pow_uv__uv_terms.c) contains test cases for pow
 * all test cases for pow have been specified and split into separate files
 * this file contains 118 test cases in 4 functions for the following purpose:
 * 49 tests for combination with pow
 * combination term: (pow(x,y)==pow(x,y)) checked with int
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 * 49 tests for combination with pow
 * combination term: ((2*pow(((3*x)+4),((3*y)+4)))-(1)) checked with Default Equality
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 * 100 tests for combination with pow
 * combination term: sqrt(pow(x,2)) checked with Default Equality
 * range from from (0) to (9)
 * returns the number of failing tests
 * 100 tests for combination with pow
 * combination term: pow(sqrt(x),2) checked with Default Equality
 * range from from (0) to (9)
 * returns the number of failing tests
 * this has been generated using the following settings:
 *  - Input-Outputs: variables for inputs and outputs
 *  - Casts: no casts for inputs and outputs
 *  - Input Type: double ( d8 )
 *  - Input Type: double ( d8 )
 *  - Output Type: double ( d8 )
 * Reference values have been computed from Java reference implementation.
 * They are compared using the following method:
 * - COMPARE_ABS_REL_TOLERANCE <= 1.0E-11
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

#define ABS_REL_TOLERANCE_POW_UV__UV_TERMS 1.0E-11

double max_dif_below_pow_uv__uv_terms=0.0;
double max_dif_above_pow_uv__uv_terms=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of sqrt_pow */ 
typedef struct {
    double pow_x;
    double pow_out;
} pow_uv__uv_terms_sqrt_pow_io_table_type;
/* type for expected input & output values of pow_deterministic */ 
typedef struct {
    double pow_x;
    double pow_y;
    int pow_out;
} pow_uv__uv_terms_pow_deterministic_io_table_type;
/* type for expected input & output values of pow_combine */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_terms_pow_combine_io_table_type;
/* type for expected input & output values of pow_sqrt */ 
typedef struct {
    double pow_x;
    double pow_out;
} pow_uv__uv_terms_pow_sqrt_io_table_type;
/**
 * 49 tests for combination with pow
 * combination term: (pow(x,y)==pow(x,y)) checked with int
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_terms_pow_deterministic in the following table */
pow_uv__uv_terms_pow_deterministic_io_table_type pow_uv__uv_terms_pow_deterministic_io_table [49] = {
   {0.0,0.0,1.0},
   {0.0,1.6666666666666667,1.0},
   {0.0,3.3333333333333335,1.0},
   {0.0,5.0,1.0},
   {0.0,6.666666666666667,1.0},
   {0.0,8.333333333333334,1.0},
   {0.0,10.0,1.0},
   {1.6666666666666667,0.0,1.0},
   {1.6666666666666667,1.6666666666666667,1.0},
   {1.6666666666666667,3.3333333333333335,1.0},
   {1.6666666666666667,5.0,1.0},
   {1.6666666666666667,6.666666666666667,1.0},
   {1.6666666666666667,8.333333333333334,1.0},
   {1.6666666666666667,10.0,1.0},
   {3.3333333333333335,0.0,1.0},
   {3.3333333333333335,1.6666666666666667,1.0},
   {3.3333333333333335,3.3333333333333335,1.0},
   {3.3333333333333335,5.0,1.0},
   {3.3333333333333335,6.666666666666667,1.0},
   {3.3333333333333335,8.333333333333334,1.0},
   {3.3333333333333335,10.0,1.0},
   {5.0,0.0,1.0},
   {5.0,1.6666666666666667,1.0},
   {5.0,3.3333333333333335,1.0},
   {5.0,5.0,1.0},
   {5.0,6.666666666666667,1.0},
   {5.0,8.333333333333334,1.0},
   {5.0,10.0,1.0},
   {6.666666666666667,0.0,1.0},
   {6.666666666666667,1.6666666666666667,1.0},
   {6.666666666666667,3.3333333333333335,1.0},
   {6.666666666666667,5.0,1.0},
   {6.666666666666667,6.666666666666667,1.0},
   {6.666666666666667,8.333333333333334,1.0},
   {6.666666666666667,10.0,1.0},
   {8.333333333333334,0.0,1.0},
   {8.333333333333334,1.6666666666666667,1.0},
   {8.333333333333334,3.3333333333333335,1.0},
   {8.333333333333334,5.0,1.0},
   {8.333333333333334,6.666666666666667,1.0},
   {8.333333333333334,8.333333333333334,1.0},
   {8.333333333333334,10.0,1.0},
   {10.0,0.0,1.0},
   {10.0,1.6666666666666667,1.0},
   {10.0,3.3333333333333335,1.0},
   {10.0,5.0,1.0},
   {10.0,6.666666666666667,1.0},
   {10.0,8.333333333333334,1.0},
   {10.0,10.0,1.0},
};
/** function pow_uv__uv_terms_pow_deterministic executes the tests and returns the number of failing tests */
int pow_uv__uv_terms_pow_deterministic() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	double y;
	/* output variable (IO_VV) */
	int res;
	int res1;
	int res2;
	/* main test starts here */
	for (ind=0;ind<49;ind++) {
		x=pow_uv__uv_terms_pow_deterministic_io_table[ind].pow_x;
		y=pow_uv__uv_terms_pow_deterministic_io_table[ind].pow_y;
		res=(pow(x,y)==pow(x,y));
		if (res!=pow_uv__uv_terms_pow_deterministic_io_table[ind].pow_out) {
			/* cast to int is only defined if value is in range, otherwise ignore deviation */
			if ((pow(x,y)==pow(x,y))>=-2147483648.0 && (pow(x,y)==pow(x,y))<=2147483647.0) {
				/* test for integer rounding errors */
				res1=((pow(x,y)==pow(x,y))+0.000001);
				res2=((pow(x,y)==pow(x,y))-0.000001);
				if ( (res1!=pow_uv__uv_terms_pow_deterministic_io_table[ind].pow_out)
				 && (res2!=pow_uv__uv_terms_pow_deterministic_io_table[ind].pow_out) ) {
					errors++;
#if defined(DEBUG)
					PRINTF("pow_uv__uv_terms_pow_deterministic: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %d, found %d\n",ind+1,x,y,pow_uv__uv_terms_pow_deterministic_io_table[ind].pow_out,res);
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
		PRINTF("pow_uv__uv_terms_pow_deterministic: successfully tested: 49 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_terms_pow_deterministic: %d tests failed for pow (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 tests for combination with pow
 * combination term: ((2*pow(((3*x)+4),((3*y)+4)))-(1)) checked with Default Equality
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_terms_pow_combine in the following table */
pow_uv__uv_terms_pow_combine_io_table_type pow_uv__uv_terms_pow_combine_io_table [49] = {
   {0.0,0.0,511.0},
   {0.0,1.6666666666666667,524287.0},
   {0.0,3.3333333333333335,536870911.0},
   {0.0,5.0,549755813887.0},
   {0.0,6.666666666666667,562949953421311.0},
   {0.0,8.333333333333334,5.7646075230342349E+17},
   {0.0,10.0,5.9029581035870565E+20},
   {1.6666666666666667,0.0,13121.0},
   {1.6666666666666667,1.6666666666666667,774840977.0},
   {1.6666666666666667,3.3333333333333335,45753584909921.0},
   {1.6666666666666667,5.0,2.701703435345984E+18},
   {1.6666666666666667,6.666666666666667,1.5953288615374503E+23},
   {1.6666666666666667,8.333333333333334,9.42025739449249E+27},
   {1.6666666666666667,10.0,5.5625677888738704E+32},
   {3.3333333333333335,0.0,76831.0},
   {3.3333333333333335,1.6666666666666667,41322093567.0},
   {3.3333333333333335,3.3333333333333335,22224013651116032.0},
   {3.3333333333333335,5.0,1.195260791789783E+22},
   {3.3333333333333335,6.666666666666667,6.428399400835482E+27},
   {3.3333333333333335,8.333333333333334,3.4573474793549424E+33},
   {3.3333333333333335,10.0,1.8594444507365926E+39},
   {5.0,0.0,260641.0},
   {5.0,1.6666666666666667,645375395557.0},
   {5.0,3.3333333333333335,1.59801337156576819E+18},
   {5.0,5.0,3.956839311320627E+24},
   {5.0,6.666666666666667,9.797525861921693E+30},
   {5.0,8.333333333333334,2.4259643989178443E+37},
   {5.0,10.0,6.006928022196076E+43},
   {6.666666666666667,0.0,663551.0},
   {6.666666666666667,1.6666666666666667,5283615080447.0},
   {6.666666666666667,3.3333333333333335,4.2071440246337176E+19},
   {6.666666666666667,5.0,3.349990598200503E+26},
   {6.666666666666667,6.666666666666667,2.667471553700568E+33},
   {6.666666666666667,8.333333333333334,2.1240073012813434E+40},
   {6.666666666666667,10.0,1.6912671513358054E+47},
   {8.333333333333334,0.0,1414561.0},
   {8.333333333333334,1.6666666666666667,29014291951737.0},
   {8.333333333333334,3.3333333333333335,5.951164653515989E+20},
   {8.333333333333334,5.0,1.2206522493179984E+28},
   {8.333333333333334,6.666666666666667,2.5036980162946612E+35},
   {8.333333333333334,8.333333333333334,5.1353723063224226E+42},
   {8.333333333333334,10.0,1.0533238654545285E+50},
   {10.0,0.0,2672671.0},
   {10.0,1.6666666666666667,121433985532927.0},
   {10.0,3.3333333333333335,5.51740462069845E+21},
   {10.0,5.0,2.5068561832099323E+29},
   {10.0,6.666666666666667,1.1390007359116496E+37},
   {10.0,8.333333333333334,5.1750981372457825E+44},
   {10.0,10.0,2.351327781073723E+52},
};
/** function pow_uv__uv_terms_pow_combine executes the tests and returns the number of failing tests */
int pow_uv__uv_terms_pow_combine() {
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
		x=pow_uv__uv_terms_pow_combine_io_table[ind].pow_x;
		y=pow_uv__uv_terms_pow_combine_io_table[ind].pow_y;
		res=((2*pow(((3*x)+4),((3*y)+4)))-(1));
		if (test_compare_double(res,pow_uv__uv_terms_pow_combine_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TERMS,ABS_REL_TOLERANCE_POW_UV__UV_TERMS,10.0,&max_dif_below_pow_uv__uv_terms,&max_dif_above_pow_uv__uv_terms)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_terms_pow_combine: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_terms_pow_combine_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("pow_uv__uv_terms_pow_combine: successfully tested: 49 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_terms_pow_combine: %d tests failed for pow (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 100 tests for combination with pow
 * combination term: sqrt(pow(x,2)) checked with Default Equality
 * range from from (0) to (9)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_terms_sqrt_pow in the following table */
pow_uv__uv_terms_sqrt_pow_io_table_type pow_uv__uv_terms_sqrt_pow_io_table [10] = {
   {0.0,0.0},
   {1.0,1.0},
   {2.0,2.0},
   {3.0,3.0},
   {4.0,4.0},
   {5.0,5.0},
   {6.0,6.0},
   {7.0,7.0},
   {8.0,8.0},
   {9.0,9.0},
};
/** function pow_uv__uv_terms_sqrt_pow executes the tests and returns the number of failing tests */
int pow_uv__uv_terms_sqrt_pow() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<10;ind++) {
		x=pow_uv__uv_terms_sqrt_pow_io_table[ind].pow_x;
		res=sqrt(pow(x,2));
		if (test_compare_double(res,pow_uv__uv_terms_sqrt_pow_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TERMS,ABS_REL_TOLERANCE_POW_UV__UV_TERMS,10.0,&max_dif_below_pow_uv__uv_terms,&max_dif_above_pow_uv__uv_terms)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_terms_sqrt_pow: test %d (IO_VV) failed for pow(%.17g): Expected %.17g, found %.17g\n",ind+1,x,pow_uv__uv_terms_sqrt_pow_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==10) {
		PRINTF("pow_uv__uv_terms_sqrt_pow: successfully tested: 10 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_terms_sqrt_pow: %d tests failed for pow (out of 10)\n",errors);
	}
#endif
	return errors;
}

/**
 * 100 tests for combination with pow
 * combination term: pow(sqrt(x),2) checked with Default Equality
 * range from from (0) to (9)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_terms_pow_sqrt in the following table */
pow_uv__uv_terms_pow_sqrt_io_table_type pow_uv__uv_terms_pow_sqrt_io_table [10] = {
   {0.0,0.0},
   {1.0,1.0},
   {2.0,2.0000000000000004},
   {3.0,2.9999999999999996},
   {4.0,4.0},
   {5.0,5.000000000000001},
   {6.0,5.999999999999999},
   {7.0,7.000000000000001},
   {8.0,8.000000000000002},
   {9.0,9.0},
};
/** function pow_uv__uv_terms_pow_sqrt executes the tests and returns the number of failing tests */
int pow_uv__uv_terms_pow_sqrt() {
	/* result: number of found errors */
	int errors=0;
	int passes=0;
	/* index for test loop */
	int ind=0;
	/* input variable (IO_VV) */
	double x;
	/* output variable (IO_VV) */
	double res;
	/* main test starts here */
	for (ind=0;ind<10;ind++) {
		x=pow_uv__uv_terms_pow_sqrt_io_table[ind].pow_x;
		res=pow(sqrt(x),2);
		if (test_compare_double(res,pow_uv__uv_terms_pow_sqrt_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_TERMS,ABS_REL_TOLERANCE_POW_UV__UV_TERMS,10.0,&max_dif_below_pow_uv__uv_terms,&max_dif_above_pow_uv__uv_terms)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_terms_pow_sqrt: test %d (IO_VV) failed for pow(%.17g): Expected %.17g, found %.17g\n",ind+1,x,pow_uv__uv_terms_pow_sqrt_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==10) {
		PRINTF("pow_uv__uv_terms_pow_sqrt: successfully tested: 10 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_terms_pow_sqrt: %d tests failed for pow (out of 10)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (4 functions) of pow_uv__uv_terms
*/
int pow_uv__uv_terms_main_test() {
	int errors=0;
	int index=0;
	errors+=pow_uv__uv_terms_pow_deterministic(); /* 1. 49 tests */
	errors+=pow_uv__uv_terms_pow_combine();       /* 2. 49 tests */
	errors+=pow_uv__uv_terms_sqrt_pow();          /* 3. 10 tests */
	errors+=pow_uv__uv_terms_pow_sqrt();          /* 4. 10 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of pow_uv__uv_terms: successfully tested ALL 118 cases for pow\n");
	} else {
		PRINTF("SUMMARY of pow_uv__uv_terms: %d tests failed in pow_uv__uv_terms (out of 118 in pow_uv__uv_terms)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.17g. ABS_TOLERANCE_POW_UV__UV_TERMS=%.17g\n",pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value,ABS_TOLERANCE_POW_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.17g. REL_TOLERANCE_POW_UV__UV_TERMS=%.17g\n",pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value,REL_TOLERANCE_POW_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.17g. ABS_REL_TOLERANCE_POW_UV__UV_TERMS=%.17g\n",pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value,ABS_REL_TOLERANCE_POW_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.17g. ULP_TOLERANCE_POW_UV__UV_TERMS=%.17g\n",pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value,ULP_TOLERANCE_POW_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.17g. EXAKT_TOLERANCE_POW_UV__UV_TERMS=%.17g\n",pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value,EXAKT_TOLERANCE_POW_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.17g. EQUAL_TOLERANCE_POW_UV__UV_TERMS=%.17g\n",pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value,EQUAL_TOLERANCE_POW_UV__UV_TERMS);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.17g. BINHEX_TOLERANCE_POW_UV__UV_TERMS=%.17g\n",pow_uv__uv_terms_all_deviation_results_double[index].max_diff_value,BINHEX_TOLERANCE_POW_UV__UV_TERMS);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls pow_uv__uv_terms_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = pow_uv__uv_terms_main_test();
	return result;
}
#endif /* NO_MAIN */

