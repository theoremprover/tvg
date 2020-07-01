/**
 * this file (powf_uv__uv_normal.c) contains test cases for powf
 * all test cases for powf have been specified and split into separate files
 * this file contains 542 test cases in 14 functions for the following purpose:
 * 3 specified tests for powf
 * (0,0); (1,1); (10.0,10.0)
 * returns the number of failing tests
 * 49 linear tests for powf
 * range: (0,0) to (10,10)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (1,2) to (100,2)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (-100,1) to (2,2)
 * returns the number of failing tests
 * 64 linear tests for powf
 * range: (-15,1) to (-1,15)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (1,0.5) to (100,0.5)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (1,2) to (3,10)
 * returns the number of failing tests
 * 50 random tests for powf
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 * 7 (42 omitted out of range or duplicate)) linear tests for powf
 * range: (0,2) to (20,2)
 * returns the number of failing tests
 * 49 linear tests for powf
 * range: (0,2) to (10,4)
 * returns the number of failing tests
 * 25 approximation tests with factors 2.0,2.0
 * range: (0,2) to (1,2)
 * returns the number of failing tests
 * 24 approximation tests with factors 2.0,2.0
 * range: (0,1) to (0,10)
 * returns the number of failing tests
 * 21 specified tests for powf
 * (2,0); (2,1); (2,2); (2,3); (2,4); (2,5); (2,6); (2,7); (2,8); (2,9); (2,10); (2,11) ...
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

#define ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL 1.0E-5

float max_dif_below_powf_uv__uv_normal=0.0;
float max_dif_above_powf_uv__uv_normal=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of linear_squares */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_linear_squares_io_table_type;
/* type for expected input & output values of random_squares */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_random_squares_io_table_type;
/* type for expected input & output values of normal_values */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_normal_values_io_table_type;
/* type for expected input & output values of powf_normal_range_rnd */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_powf_normal_range_rnd_io_table_type;
/* type for expected input & output values of random_squareroots */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_random_squareroots_io_table_type;
/* type for expected input & output values of random_numbers */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_random_numbers_io_table_type;
/* type for expected input & output values of powf_normal_range */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_powf_normal_range_io_table_type;
/* type for expected input & output values of linear_powers */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_linear_powers_io_table_type;
/* type for expected input & output values of negative_pow */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_negative_pow_io_table_type;
/* type for expected input & output values of roots_to_ten */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_roots_to_ten_io_table_type;
/* type for expected input & output values of negative_squares */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_negative_squares_io_table_type;
/* type for expected input & output values of roots_of_two */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_roots_of_two_io_table_type;
/* type for expected input & output values of two_powers */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_two_powers_io_table_type;
/* type for expected input & output values of random_exponent */ 
typedef struct {
    float powf_x;
    float powf_y;
    float powf_out;
} powf_uv__uv_normal_random_exponent_io_table_type;
/**
 * 3 specified tests for powf
 * (0,0); (1,1); (10.0,10.0)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_normal_values in the following table */
powf_uv__uv_normal_normal_values_io_table_type powf_uv__uv_normal_normal_values_io_table [3] = {
   {0,0,1.00000000},
   {1.00000000,1.00000000,1.00000000},
   {10.0000000,10.0000000,1.00000000e+10},
};
/** function powf_uv__uv_normal_normal_values executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_normal_values() {
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
		x=powf_uv__uv_normal_normal_values_io_table[ind].powf_x;
		y=powf_uv__uv_normal_normal_values_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_normal_values_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_normal_values: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_normal_values_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==3) {
		PRINTF("powf_uv__uv_normal_normal_values: successfully tested: 3 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_normal_values: %d tests failed for powf (out of 3)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 linear tests for powf
 * range: (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_powf_normal_range in the following table */
powf_uv__uv_normal_powf_normal_range_io_table_type powf_uv__uv_normal_powf_normal_range_io_table [49] = {
   {0.0,0.0,1.00000000},
   {0.0,1.66666663,0.0},
   {0.0,3.33333325,0.0},
   {0.0,5.00000000,0.0},
   {0.0,6.66666651,0.0},
   {0.0,8.33333302,0.0},
   {0.0,10.0000000,0.0},
   {1.66666663,0.0,1.00000000},
   {1.66666663,1.66666663,2.34286833},
   {1.66666663,3.33333325,5.48903227},
   {1.66666663,5.00000000,12.8600807},
   {1.66666663,6.66666651,30.1294746},
   {1.66666663,8.33333302,70.5893860},
   {1.66666663,10.0000000,165.381683},
   {3.33333325,0.0,1.00000000},
   {3.33333325,1.66666663,7.43814325},
   {3.33333325,3.33333325,55.3259735},
   {3.33333325,5.00000000,411.522583},
   {3.33333325,6.66666651,3060.96338},
   {3.33333325,8.33333302,22767.8809},
   {3.33333325,10.0000000,169350.844},
   {5.00000000,0.0,1.00000000},
   {5.00000000,1.66666663,14.6200876},
   {5.00000000,3.33333325,213.746964},
   {5.00000000,5.00000000,3125.00000},
   {5.00000000,6.66666651,45687.7656},
   {5.00000000,8.33333302,667959.000},
   {5.00000000,10.0000000,9765625.00},
   {6.66666651,0.0,1.00000000},
   {6.66666651,1.66666663,23.6146317},
   {6.66666651,3.33333325,557.650879},
   {6.66666651,5.00000000,13168.7227},
   {6.66666651,6.66666651,310974.469},
   {6.66666651,8.33333302,7343546.00},
   {6.66666651,10.0000000,173415264.0},
   {8.33333302,0.0,1.00000000},
   {8.33333302,1.66666663,34.2529411},
   {8.33333302,3.33333325,1173.26392},
   {8.33333302,5.00000000,40187.7500},
   {8.33333302,6.66666651,1376548.25},
   {8.33333302,8.33333302,47150812.0},
   {8.33333302,10.0000000,1.61505523e+09},
   {10.0000000,0.0,1.00000000},
   {10.0000000,1.66666663,46.4158859},
   {10.0000000,3.33333325,2154.43433},
   {10.0000000,5.00000000,100000.000},
   {10.0000000,6.66666651,4641587.00},
   {10.0000000,8.33333302,215443312.0},
   {10.0000000,10.0000000,1.00000000e+10},
};
/** function powf_uv__uv_normal_powf_normal_range executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_powf_normal_range() {
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
		x=powf_uv__uv_normal_powf_normal_range_io_table[ind].powf_x;
		y=powf_uv__uv_normal_powf_normal_range_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_powf_normal_range_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_powf_normal_range: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_powf_normal_range_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("powf_uv__uv_normal_powf_normal_range: successfully tested: 49 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_powf_normal_range: %d tests failed for powf (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_powf_normal_range_rnd in the following table */
powf_uv__uv_normal_powf_normal_range_rnd_io_table_type powf_uv__uv_normal_powf_normal_range_rnd_io_table [50] = {
   {9.20948982,4.73520517,36800.4609},
   {0.0504839420,8.57575417,7.56068558e-12},
   {7.90652561,5.80486870,163188.281},
   {0.257995725,1.46375299,0.137640446},
   {2.01527309,5.77319431,57.1450729},
   {1.74766719,1.98697925,3.03221846},
   {2.87855148,2.88549781,21.1322422},
   {5.90354395,1.70667470,20.7032585},
   {5.91730833,9.16092968,11840649.0},
   {7.70602751,6.99273586,1589907.38},
   {0.594409704,0.0319790840,0.983502507},
   {6.95055008,9.21349335,57271436.0},
   {8.23439789,6.49838018,891505.188},
   {4.05059814,5.51133060,2229.66162},
   {3.08181167,5.71120167,618.970825},
   {0.0472849607,0.686232448,0.123183332},
   {4.74711800,6.41148949,21723.2109},
   {3.05451751,6.16461039,976.072998},
   {7.44369078,3.77670527,1961.04492},
   {6.26922798,5.42352152,21072.0898},
   {1.06193841,0.119863153,1.00722933},
   {0.411164761,6.54161167,0.00298567000},
   {2.96429038,5.46457958,379.182648},
   {0.723628998,8.75950050,0.0588071309},
   {5.63025856,5.48930836,13178.9707},
   {2.81879616,7.58727026,2598.69141},
   {1.05316877,4.68209982,1.27449393},
   {5.19865704,0.659555197,2.96599030},
   {6.61501884,4.04096222,2068.87915},
   {9.39118862,7.30522108,12762588.0},
   {1.47469103,5.34307718,7.96861172},
   {1.63333297,8.79569817,74.8412399},
   {6.13549042,6.21882963,79341.5078},
   {5.88628054,3.12191367,253.149460},
   {8.55471992,1.11520767,10.9547644},
   {4.81919479,9.61386585,3681502.00},
   {7.07119560,5.11012745,21928.8945},
   {9.03583622,0.979982615,8.64634132},
   {6.61285353,8.61855316,11764281.0},
   {1.19189918,6.65735006,3.21775651},
   {9.56882954,7.76790905,41612336.0},
   {9.36267090,4.51199627,24151.9434},
   {4.73770428,4.85059977,1891.95032},
   {6.83075953,2.72504210,187.915741},
   {6.67142582,6.30303001,156702.156},
   {4.78798103,9.49143600,2855112.25},
   {7.20436049,8.41217899,16377451.0},
   {1.85644567,1.32011235,2.26303220},
   {1.07691526,7.99257278,1.80806172},
   {4.05985737,7.16593456,22937.6230},
};
/** function powf_uv__uv_normal_powf_normal_range_rnd executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_powf_normal_range_rnd() {
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
		x=powf_uv__uv_normal_powf_normal_range_rnd_io_table[ind].powf_x;
		y=powf_uv__uv_normal_powf_normal_range_rnd_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_powf_normal_range_rnd_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_powf_normal_range_rnd: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_powf_normal_range_rnd_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_normal_powf_normal_range_rnd: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_powf_normal_range_rnd: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (1,2) to (100,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_random_squares in the following table */
powf_uv__uv_normal_random_squares_io_table_type powf_uv__uv_normal_random_squares_io_table [50] = {
   {73.5401535,1.99999988,5408.15137},
   {3.98857880,2.00000000,15.9087610},
   {79.5426865,1.99999988,6327.03564},
   {85.8929825,2.00000000,7377.60449},
   {93.5511856,2.00000000,8751.82422},
   {42.8628654,2.00000000,1837.22522},
   {18.7506065,1.99999988,351.585114},
   {35.8439941,2.00000000,1284.79187},
   {51.2996101,2.00000000,2631.64990},
   {83.8680496,2.00000000,7033.84961},
   {93.7208557,2.00000000,8783.59863},
   {44.7571793,1.99999988,2003.20422},
   {94.5984039,2.00000000,8948.85840},
   {69.4069977,2.00000000,4817.33154},
   {36.2889252,2.00000000,1316.88611},
   {32.4276962,2.00000000,1051.55554},
   {19.4729786,2.00000000,379.196899},
   {71.5200424,2.00000000,5115.11670},
   {84.8316345,2.00000000,7196.40625},
   {84.4313965,1.99999988,7128.65674},
   {82.9431152,1.99999988,6879.55664},
   {56.3025208,1.99999988,3169.97241},
   {93.0863266,2.00000000,8665.06445},
   {87.6053162,2.00000000,7674.69141},
   {24.6181374,2.00000000,606.052673},
   {14.6553564,2.00000000,214.779465},
   {63.4840050,2.00000000,4030.21899},
   {35.6302528,2.00000000,1269.51489},
   {29.4277706,2.00000000,865.993713},
   {73.4505081,1.99999988,5394.97461},
   {65.2033310,2.00000000,4251.47461},
   {55.3965569,1.99999988,3068.77710},
   {77.3528671,2.00000000,5983.46582},
   {46.6024399,1.99999988,2171.78638},
   {25.6158276,2.00000000,656.170593},
   {17.0399837,2.00000000,290.361053},
   {59.1562309,2.00000000,3499.45972},
   {85.2894897,2.00000000,7274.29688},
   {39.3504295,1.99999988,1548.45557},
   {24.5846405,2.00000000,604.404541},
   {13.8431273,2.00000000,191.632172},
   {27.6658916,2.00000000,765.401550},
   {17.8569450,2.00000000,318.870483},
   {91.0233536,2.00000000,8285.25098},
   {40.9041824,2.00000000,1673.15210},
   {75.8454819,1.99999988,5752.53418},
   {40.0928764,2.00000000,1607.43872},
   {78.4031754,1.99999988,6147.05469},
   {86.3106308,2.00000000,7449.52490},
   {38.7224159,1.99999988,1499.42480},
};
/** function powf_uv__uv_normal_random_squares executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_random_squares() {
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
		x=powf_uv__uv_normal_random_squares_io_table[ind].powf_x;
		y=powf_uv__uv_normal_random_squares_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_random_squares_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_random_squares: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_random_squares_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_normal_random_squares: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_random_squares: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (-100,1) to (2,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_negative_squares in the following table */
powf_uv__uv_normal_negative_squares_io_table_type powf_uv__uv_normal_negative_squares_io_table [50] = {
   {-40.4512825,1.57581866,NAN},
   {-74.1755753,1.70842218,NAN},
   {-96.1442642,1.93439412,NAN},
   {-43.0014420,1.56167221,NAN},
   {-37.4116402,1.88773680,NAN},
   {-38.5837212,1.43947792,NAN},
   {-37.5288086,1.53711569,NAN},
   {-7.43009949,1.64386761,NAN},
   {-21.2625961,1.53426278,NAN},
   {-38.7618561,1.52240205,NAN},
   {-28.5780640,1.11201191,NAN},
   {-86.1948242,1.15127313,NAN},
   {-14.8882828,1.61854827,NAN},
   {-42.5060654,1.10772336,NAN},
   {-21.8345642,1.80984664,NAN},
   {-87.1067276,1.28436434,NAN},
   {-44.6493568,1.02275479,NAN},
   {-78.0363770,1.59273767,NAN},
   {-20.6486435,1.89709723,NAN},
   {-11.2323151,1.27362764,NAN},
   {-30.0547256,1.30963767,NAN},
   {-52.0037918,1.78052449,NAN},
   {-8.04722595,1.47438526,NAN},
   {-86.9552231,1.43669724,NAN},
   {-14.2224884,1.92931080,NAN},
   {-72.8624954,1.34512949,NAN},
   {-69.2048569,1.12394500,NAN},
   {-22.4233932,1.11680603,NAN},
   {-27.3526993,1.12856126,NAN},
   {-12.4977188,1.59232092,NAN},
   {-61.8462868,1.26484275,NAN},
   {-52.9492111,1.78346491,NAN},
   {-6.10019684,1.19934046,NAN},
   {-39.9321899,1.12740374,NAN},
   {-95.7313004,1.81205201,NAN},
   {-77.2683563,1.90148139,NAN},
   {-95.2148514,1.34856880,NAN},
   {0.822235107,1.31988811,0.772332370},
   {-39.0081749,1.45911217,NAN},
   {-74.2132416,1.65586901,NAN},
   {-28.1068649,1.52314365,NAN},
   {-78.8554459,1.38938856,NAN},
   {-5.66870880,1.53398895,NAN},
   {-79.3198776,1.25790310,NAN},
   {-56.0989647,1.98205423,NAN},
   {-78.0765762,1.52794147,NAN},
   {-14.5979538,1.91388512,NAN},
   {-47.1625252,1.35151303,NAN},
   {-78.0428772,1.27176011,NAN},
   {-9.25906372,1.72100449,NAN},
};
/** function powf_uv__uv_normal_negative_squares executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_negative_squares() {
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
		x=powf_uv__uv_normal_negative_squares_io_table[ind].powf_x;
		y=powf_uv__uv_normal_negative_squares_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_negative_squares_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_negative_squares: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_negative_squares_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_normal_negative_squares: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_negative_squares: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for powf
 * range: (-15,1) to (-1,15)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_negative_pow in the following table */
powf_uv__uv_normal_negative_pow_io_table_type powf_uv__uv_normal_negative_pow_io_table [64] = {
   {-15.0000000,1.00000000,-15.0000000},
   {-15.0000000,3.00000000,-3375.00000},
   {-15.0000000,5.00000000,-759375.000},
   {-15.0000000,7.00000000,-170859376.0},
   {-15.0000000,9.00000000,-3.84433603e+10},
   {-15.0000000,11.0000000,-8.64975585e+12},
   {-15.0000000,13.0000000,-1.94619504e+15},
   {-15.0000000,15.0000000,-4.37893906e+17},
   {-13.0000000,1.00000000,-13.0000000},
   {-13.0000000,3.00000000,-2197.00000},
   {-13.0000000,5.00000000,-371293.000},
   {-13.0000000,7.00000000,-62748516.0},
   {-13.0000000,9.00000000,-1.06044989e+10},
   {-13.0000000,11.0000000,-1.79216043e+12},
   {-13.0000000,13.0000000,-3.02875121e+14},
   {-13.0000000,15.0000000,-5.11858912e+16},
   {-11.0000000,1.00000000,-11.0000000},
   {-11.0000000,3.00000000,-1331.00000},
   {-11.0000000,5.00000000,-161051.000},
   {-11.0000000,7.00000000,-19487172.0},
   {-11.0000000,9.00000000,-2.35794765e+09},
   {-11.0000000,11.0000000,-2.85311664e+11},
   {-11.0000000,13.0000000,-3.45227122e+13},
   {-11.0000000,15.0000000,-4.17724815e+15},
   {-9.00000000,1.00000000,-9.00000000},
   {-9.00000000,3.00000000,-729.000000},
   {-9.00000000,5.00000000,-59049.0000},
   {-9.00000000,7.00000000,-4782969.00},
   {-9.00000000,9.00000000,-387420480.0},
   {-9.00000000,11.0000000,-3.13810596e+10},
   {-9.00000000,13.0000000,-2.54186593e+12},
   {-9.00000000,15.0000000,-2.05891136e+14},
   {-7.00000000,1.00000000,-7.00000000},
   {-7.00000000,3.00000000,-343.000000},
   {-7.00000000,5.00000000,-16807.0000},
   {-7.00000000,7.00000000,-823543.000},
   {-7.00000000,9.00000000,-40353608.0},
   {-7.00000000,11.0000000,-1.97732672e+09},
   {-7.00000000,13.0000000,-9.68890122e+10},
   {-7.00000000,15.0000000,-4.74756153e+12},
   {-5.00000000,1.00000000,-5.00000000},
   {-5.00000000,3.00000000,-125.000000},
   {-5.00000000,5.00000000,-3125.00000},
   {-5.00000000,7.00000000,-78125.0000},
   {-5.00000000,9.00000000,-1953125.00},
   {-5.00000000,11.0000000,-48828124.0},
   {-5.00000000,13.0000000,-1.22070310e+09},
   {-5.00000000,15.0000000,-3.05175777e+10},
   {-3.00000000,1.00000000,-3.00000000},
   {-3.00000000,3.00000000,-27.0000000},
   {-3.00000000,5.00000000,-243.000000},
   {-3.00000000,7.00000000,-2187.00000},
   {-3.00000000,9.00000000,-19683.0000},
   {-3.00000000,11.0000000,-177147.000},
   {-3.00000000,13.0000000,-1594323.00},
   {-3.00000000,15.0000000,-14348907.0},
   {-1.00000000,1.00000000,-1.00000000},
   {-1.00000000,3.00000000,-1.00000000},
   {-1.00000000,5.00000000,-1.00000000},
   {-1.00000000,7.00000000,-1.00000000},
   {-1.00000000,9.00000000,-1.00000000},
   {-1.00000000,11.0000000,-1.00000000},
   {-1.00000000,13.0000000,-1.00000000},
   {-1.00000000,15.0000000,-1.00000000},
};
/** function powf_uv__uv_normal_negative_pow executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_negative_pow() {
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
		x=powf_uv__uv_normal_negative_pow_io_table[ind].powf_x;
		y=powf_uv__uv_normal_negative_pow_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_negative_pow_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_negative_pow: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_negative_pow_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("powf_uv__uv_normal_negative_pow: successfully tested: 64 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_negative_pow: %d tests failed for powf (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (1,0.5) to (100,0.5)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_random_squareroots in the following table */
powf_uv__uv_normal_random_squareroots_io_table_type powf_uv__uv_normal_random_squareroots_io_table [50] = {
   {72.0627975,0.500000000,8.48898125},
   {43.9018288,0.500000000,6.62584543},
   {58.5498047,0.500000000,7.65178442},
   {12.6013680,0.500000000,3.54984045},
   {58.1322937,0.499999970,7.62445259},
   {64.4734116,0.499999970,8.02953243},
   {20.5311680,0.500000000,4.53113317},
   {28.8471184,0.499999970,5.37095070},
   {17.4368458,0.500000000,4.17574501},
   {94.1780624,0.500000000,9.70453835},
   {17.9992504,0.500000000,4.24255228},
   {8.46576786,0.500000000,2.90959930},
   {79.1625671,0.499999970,8.89733410},
   {25.1851234,0.499999970,5.01847792},
   {32.8230820,0.500000000,5.72914314},
   {40.2406769,0.499999970,6.34355307},
   {3.79402757,0.500000000,1.94782639},
   {15.5292683,0.500000000,3.94071937},
   {91.2404175,0.500000000,9.55198479},
   {19.7115269,0.499999970,4.43976593},
   {66.4542847,0.499999970,8.15194893},
   {91.9680786,0.499999970,9.58999729},
   {59.3519821,0.499999970,7.70402288},
   {50.8708916,0.500000000,7.13238335},
   {70.3398132,0.500000000,8.38688374},
   {16.5984936,0.499999970,4.07412434},
   {50.0808105,0.500000000,7.07677984},
   {70.4293442,0.500000000,8.39221954},
   {60.2474251,0.500000000,7.76192141},
   {54.6685867,0.500000000,7.39382076},
   {82.5250702,0.500000000,9.08433056},
   {69.6063919,0.499999970,8.34304333},
   {37.5760880,0.500000000,6.12993383},
   {11.0238380,0.500000000,3.32021666},
   {42.4402657,0.500000000,6.51461935},
   {34.7045441,0.500000000,5.89105606},
   {58.1661072,0.500000000,7.62667084},
   {15.9740334,0.499999970,3.99675250},
   {37.4880257,0.500000000,6.12274647},
   {65.8670807,0.499999970,8.11585236},
   {88.9111557,0.499999970,9.42926979},
   {60.0570564,0.500000000,7.74964857},
   {90.8841324,0.500000000,9.53331661},
   {24.4076729,0.500000000,4.94041204},
   {16.2459698,0.500000000,4.03062916},
   {60.2102737,0.500000000,7.75952816},
   {39.1667633,0.500000000,6.25833559},
   {46.9309273,0.500000000,6.85061502},
   {81.3729248,0.500000000,9.02069378},
   {71.2379684,0.500000000,8.44025898},
};
/** function powf_uv__uv_normal_random_squareroots executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_random_squareroots() {
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
		x=powf_uv__uv_normal_random_squareroots_io_table[ind].powf_x;
		y=powf_uv__uv_normal_random_squareroots_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_random_squareroots_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_random_squareroots: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_random_squareroots_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_normal_random_squareroots: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_random_squareroots: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (1,2) to (3,10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_random_exponent in the following table */
powf_uv__uv_normal_random_exponent_io_table_type powf_uv__uv_normal_random_exponent_io_table [50] = {
   {1.96947706,5.53059959,42.4558067},
   {1.64924550,4.74944448,10.7642679},
   {2.40293932,9.47819996,4062.15430},
   {1.91282976,8.51552391,250.392563},
   {1.72962642,4.07815981,9.34130478},
   {1.78237128,2.55347919,4.37440109},
   {1.58332372,2.86615133,3.73247600},
   {2.98571181,2.84573269,22.4832611},
   {2.80158472,2.32688141,10.9914627},
   {1.79137254,2.52265835,4.35212708},
   {2.38314533,6.35073709,248.418457},
   {2.81051850,4.55474281,110.689606},
   {2.47858357,2.95016432,14.5534277},
   {1.08783150,2.38530874,1.22239280},
   {2.51983094,8.33619118,2217.73047},
   {1.91260314,7.10277557,100.073296},
   {1.91744256,4.59092093,19.8589382},
   {1.40477061,3.60970998,3.41045833},
   {1.17075467,3.55964851,1.75272810},
   {2.12057090,5.86060953,81.8865128},
   {2.45021963,2.64063501,10.6597834},
   {1.71217191,3.08789015,5.26221514},
   {2.20970750,5.91046762,108.437531},
   {2.12949562,5.11761808,47.8624611},
   {2.93691850,9.15248680,19159.0684},
   {1.56621408,9.11957359,59.8355331},
   {1.30110860,5.14401102,3.87284517},
   {1.95247722,7.79192829,183.749039},
   {2.53921103,5.91906977,248.565247},
   {1.44258404,5.02561760,6.30641317},
   {1.54538226,4.19616413,6.21191883},
   {2.22593069,6.12908363,134.873886},
   {2.68768001,3.90927649,47.7040901},
   {1.82491422,7.94685364,119.139442},
   {1.54275012,5.59396172,11.3062429},
   {2.58594942,9.52715111,8532.83496},
   {1.47019792,4.67299509,6.05544853},
   {2.60260987,7.86080170,1842.67334},
   {1.97900891,2.49675226,5.49739075},
   {1.14754891,4.72776937,1.91683388},
   {1.18661022,4.98632193,2.34705925},
   {2.67803121,6.57528734,650.145752},
   {2.13570738,8.19137001,500.493439},
   {2.49553633,3.96420288,37.5351601},
   {1.39498234,6.92405844,10.0231028},
   {2.59118986,8.43456554,3073.87646},
   {2.08844686,7.40388489,233.311340},
   {2.53521657,6.25903893,337.867188},
   {1.41724586,3.71032667,3.64679337},
   {2.44083929,3.70009923,27.1602306},
};
/** function powf_uv__uv_normal_random_exponent executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_random_exponent() {
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
		x=powf_uv__uv_normal_random_exponent_io_table[ind].powf_x;
		y=powf_uv__uv_normal_random_exponent_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_random_exponent_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_random_exponent: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_random_exponent_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_normal_random_exponent: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_random_exponent: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for powf
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_random_numbers in the following table */
powf_uv__uv_normal_random_numbers_io_table_type powf_uv__uv_normal_random_numbers_io_table [50] = {
   {3.90437317,9.47439957,402340.344},
   {0.265465975,7.39462757,5.50503501e-05},
   {4.88577795,1.39022887,9.07351494},
   {2.07011104,2.55589843,6.42166519},
   {8.24124718,7.90178013,17297160.0},
   {9.51084709,5.43693590,208216.469},
   {2.05523443,3.50650501,12.5040283},
   {4.15928793,4.15681171,374.235840},
   {0.0665348768,0.585047007,0.204846501},
   {6.76712132,5.94481707,86417.0938},
   {3.60982656,0.0675523281,1.09058499},
   {3.88771725,4.72105503,608.108398},
   {0.442318916,8.53479767,0.000947162043},
   {6.72237825,2.14195204,59.2262802},
   {3.20156097,1.51881099,5.85529804},
   {5.46628952,9.99154186,23479390.0},
   {0.461173654,0.474168658,0.692811489},
   {0.542004704,9.59406090,0.00280549261},
   {4.04616642,3.72762084,183.159271},
   {4.07922029,3.18108559,87.5584793},
   {8.62033272,3.47905207,1797.78113},
   {9.90071583,6.74411774,5186719.50},
   {1.15276754,3.70100856,1.69241440},
   {9.83433056,7.24934673,15730817.0},
   {5.20308685,0.428497195,2.02728987},
   {5.27439022,4.62477732,2187.18677},
   {5.93568993,2.58806896,100.414589},
   {0.104153752,0.621685982,0.245076835},
   {8.18918991,0.109130740,1.25794792},
   {9.06085968,9.50443649,1.25128384e+09},
   {0.000699162483,6.79610014,3.59283178e-22},
   {1.73032224,6.06822538,27.8617401},
   {4.83522320,9.30372334,2331428.00},
   {9.10857868,3.28777599,1427.11414},
   {5.92336559,4.81723976,5268.02588},
   {7.22974300,8.05716610,8357869.50},
   {7.99746323,8.74286556,78413376.0},
   {1.05435729,9.11860752,1.62037957},
   {4.17512035,8.19241142,121555.828},
   {4.61955261,6.05456877,10564.9121},
   {6.51399374,6.41006184,164744.813},
   {8.50609779,3.82229519,3578.52393},
   {7.00298595,9.93198586,248508544.0},
   {8.76887417,9.96355247,2.48353562e+09},
   {4.51938534,2.36435604,35.3867645},
   {8.55376625,4.57696247,18469.2539},
   {2.03183293,2.66763687,6.62725019},
   {3.28892279,9.17902279,55724.6680},
   {5.43445539,8.98093987,4003059.75},
   {6.35156250,4.51717377,4234.00195},
};
/** function powf_uv__uv_normal_random_numbers executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_random_numbers() {
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
		x=powf_uv__uv_normal_random_numbers_io_table[ind].powf_x;
		y=powf_uv__uv_normal_random_numbers_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_random_numbers_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_random_numbers: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_random_numbers_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("powf_uv__uv_normal_random_numbers: successfully tested: 50 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_random_numbers: %d tests failed for powf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 7 (42 omitted out of range or duplicate)) linear tests for powf
 * range: (0,2) to (20,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_linear_squares in the following table */
powf_uv__uv_normal_linear_squares_io_table_type powf_uv__uv_normal_linear_squares_io_table [7] = {
   {0.0,2.00000000,0.0},
   {3.33333325,2.00000000,11.1111107},
   {6.66666651,2.00000000,44.4444427},
   {10.0000000,2.00000000,100.000000},
   {13.3333330,2.00000000,177.777771},
   {16.6666660,2.00000000,277.777771},
   {20.0000000,2.00000000,400.000000},
};
/** function powf_uv__uv_normal_linear_squares executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_linear_squares() {
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
	for (ind=0;ind<7;ind++) {
		x=powf_uv__uv_normal_linear_squares_io_table[ind].powf_x;
		y=powf_uv__uv_normal_linear_squares_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_linear_squares_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_linear_squares: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_linear_squares_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==7) {
		PRINTF("powf_uv__uv_normal_linear_squares: successfully tested: 7 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_linear_squares: %d tests failed for powf (out of 7)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 linear tests for powf
 * range: (0,2) to (10,4)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_linear_powers in the following table */
powf_uv__uv_normal_linear_powers_io_table_type powf_uv__uv_normal_linear_powers_io_table [49] = {
   {0.0,2.00000000,0.0},
   {0.0,2.33333325,0.0},
   {0.0,2.66666675,0.0},
   {0.0,3.00000000,0.0},
   {0.0,3.33333325,0.0},
   {0.0,3.66666675,0.0},
   {0.0,4.00000000,0.0},
   {1.66666663,2.00000000,2.77777767},
   {1.66666663,2.33333325,3.29341936},
   {1.66666663,2.66666675,3.90478086},
   {1.66666663,3.00000000,4.62962914},
   {1.66666663,3.33333325,5.48903227},
   {1.66666663,3.66666675,6.50796795},
   {1.66666663,4.00000000,7.71604872},
   {3.33333325,2.00000000,11.1111107},
   {3.33333325,2.33333325,16.5977936},
   {3.33333325,2.66666675,24.7938137},
   {3.33333325,3.00000000,37.0370331},
   {3.33333325,3.33333325,55.3259735},
   {3.33333325,3.66666675,82.6460419},
   {3.33333325,4.00000000,123.456779},
   {5.00000000,2.00000000,25.0000000},
   {5.00000000,2.33333325,42.7493935},
   {5.00000000,2.66666675,73.1004562},
   {5.00000000,3.00000000,125.000000},
   {5.00000000,3.33333325,213.746964},
   {5.00000000,3.66666675,365.502258},
   {5.00000000,4.00000000,625.000000},
   {6.66666651,2.00000000,44.4444427},
   {6.66666651,2.33333325,83.6476288},
   {6.66666651,2.66666675,157.430908},
   {6.66666651,3.00000000,296.296265},
   {6.66666651,3.33333325,557.650879},
   {6.66666651,3.66666675,1049.53943},
   {6.66666651,4.00000000,1975.30847},
   {8.33333302,2.00000000,69.4444427},
   {8.33333302,2.33333325,140.791672},
   {8.33333302,2.66666675,285.441223},
   {8.33333302,3.00000000,578.703613},
   {8.33333302,3.33333325,1173.26392},
   {8.33333302,3.66666675,2378.67676},
   {8.33333302,4.00000000,4822.53027},
   {10.0000000,2.00000000,100.000000},
   {10.0000000,2.33333325,215.443436},
   {10.0000000,2.66666675,464.158966},
   {10.0000000,3.00000000,1000.00000},
   {10.0000000,3.33333325,2154.43433},
   {10.0000000,3.66666675,4641.58984},
   {10.0000000,4.00000000,10000.0000},
};
/** function powf_uv__uv_normal_linear_powers executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_linear_powers() {
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
		x=powf_uv__uv_normal_linear_powers_io_table[ind].powf_x;
		y=powf_uv__uv_normal_linear_powers_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_linear_powers_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_linear_powers: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_linear_powers_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("powf_uv__uv_normal_linear_powers: successfully tested: 49 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_linear_powers: %d tests failed for powf (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 25 approximation tests with factors 2.0,2.0
 * range: (0,2) to (1,2)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_roots_of_two in the following table */
powf_uv__uv_normal_roots_of_two_io_table_type powf_uv__uv_normal_roots_of_two_io_table [25] = {
   {0.500000000,2.00000000,0.250000000},
   {0.750000000,2.00000000,0.562500000},
   {0.875000000,2.00000000,0.765625000},
   {0.937500000,2.00000000,0.878906250},
   {0.968750000,2.00000000,0.938476563},
   {0.984375000,2.00000000,0.968994141},
   {0.992187500,2.00000000,0.984436035},
   {0.996093750,2.00000000,0.992202759},
   {0.998046875,2.00000000,0.996097565},
   {0.999023438,2.00000000,0.998047829},
   {0.999511719,2.00000000,0.999023676},
   {0.999755859,2.00000000,0.999511778},
   {0.999877930,2.00000000,0.999755859},
   {0.999938965,2.00000000,0.999877930},
   {0.999969482,2.00000000,0.999938965},
   {0.999984741,2.00000000,0.999969482},
   {0.999992371,2.00000000,0.999984741},
   {0.999996185,2.00000000,0.999992371},
   {0.999998093,2.00000000,0.999996185},
   {0.999999046,2.00000000,0.999998093},
   {0.999999523,2.00000000,0.999999046},
   {0.999999762,2.00000000,0.999999523},
   {0.999999881,2.00000000,0.999999762},
   {0.999999940,2.00000000,0.999999881},
   {1.00000000,2.00000000,1.00000000},
};
/** function powf_uv__uv_normal_roots_of_two executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_roots_of_two() {
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
		x=powf_uv__uv_normal_roots_of_two_io_table[ind].powf_x;
		y=powf_uv__uv_normal_roots_of_two_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_roots_of_two_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_roots_of_two: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_roots_of_two_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==25) {
		PRINTF("powf_uv__uv_normal_roots_of_two: successfully tested: 25 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_roots_of_two: %d tests failed for powf (out of 25)\n",errors);
	}
#endif
	return errors;
}

/**
 * 24 approximation tests with factors 2.0,2.0
 * range: (0,1) to (0,10)
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_roots_to_ten in the following table */
powf_uv__uv_normal_roots_to_ten_io_table_type powf_uv__uv_normal_roots_to_ten_io_table [24] = {
   {0.0,5.50000000,0.0},
   {0.0,7.75000000,0.0},
   {0.0,8.87500000,0.0},
   {0.0,9.43750000,0.0},
   {0.0,9.71875000,0.0},
   {0.0,9.85937500,0.0},
   {0.0,9.92968750,0.0},
   {0.0,9.96484375,0.0},
   {0.0,9.98242188,0.0},
   {0.0,9.99121094,0.0},
   {0.0,9.99560547,0.0},
   {0.0,9.99780273,0.0},
   {0.0,9.99890137,0.0},
   {0.0,9.99945068,0.0},
   {0.0,9.99972534,0.0},
   {0.0,9.99986267,0.0},
   {0.0,9.99993134,0.0},
   {0.0,9.99996567,0.0},
   {0.0,9.99998283,0.0},
   {0.0,9.99999142,0.0},
   {0.0,9.99999619,0.0},
   {0.0,9.99999809,0.0},
   {0.0,9.99999905,0.0},
   {0.0,10.0000000,0.0},
};
/** function powf_uv__uv_normal_roots_to_ten executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_roots_to_ten() {
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
	for (ind=0;ind<24;ind++) {
		x=powf_uv__uv_normal_roots_to_ten_io_table[ind].powf_x;
		y=powf_uv__uv_normal_roots_to_ten_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_roots_to_ten_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_roots_to_ten: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_roots_to_ten_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==24) {
		PRINTF("powf_uv__uv_normal_roots_to_ten: successfully tested: 24 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_roots_to_ten: %d tests failed for powf (out of 24)\n",errors);
	}
#endif
	return errors;
}

/**
 * 21 specified tests for powf
 * (2,0); (2,1); (2,2); (2,3); (2,4); (2,5); (2,6); (2,7); (2,8); (2,9); (2,10); (2,11) ...
 * returns the number of failing tests
 */
/* store data of powf_uv__uv_normal_two_powers in the following table */
powf_uv__uv_normal_two_powers_io_table_type powf_uv__uv_normal_two_powers_io_table [21] = {
   {2.00000000,0,1.00000000},
   {2.00000000,1.00000000,2.00000000},
   {2.00000000,2.00000000,4.00000000},
   {2.00000000,3.00000000,8.00000000},
   {2.00000000,4.00000000,16.0000000},
   {2.00000000,5.00000000,32.0000000},
   {2.00000000,6.00000000,64.0000000},
   {2.00000000,7.00000000,128.000000},
   {2.00000000,8.00000000,256.000000},
   {2.00000000,9.00000000,512.000000},
   {2.00000000,10.0000000,1024.00000},
   {2.00000000,11.0000000,2048.00000},
   {2.00000000,12.0000000,4096.00000},
   {2.00000000,13.0000000,8192.00000},
   {2.00000000,14.0000000,16384.0000},
   {2.00000000,15.0000000,32768.0000},
   {2.00000000,16.0000000,65536.0000},
   {2.00000000,17.0000000,131072.000},
   {2.00000000,18.0000000,262144.000},
   {2.00000000,19.0000000,524288.000},
   {2.00000000,20.0000000,1048576.00},
};
/** function powf_uv__uv_normal_two_powers executes the tests and returns the number of failing tests */
int powf_uv__uv_normal_two_powers() {
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
	for (ind=0;ind<21;ind++) {
		x=powf_uv__uv_normal_two_powers_io_table[ind].powf_x;
		y=powf_uv__uv_normal_two_powers_io_table[ind].powf_y;
		res=powf(x,y);
		if (test_compare_float(res,powf_uv__uv_normal_two_powers_io_table[ind].powf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL,10.0,&max_dif_below_powf_uv__uv_normal,&max_dif_above_powf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("powf_uv__uv_normal_two_powers: test %d (IO_VV) failed for powf(%.9g,%.9g): Expected %.9g, found %.9g\n",ind+1,x,y,powf_uv__uv_normal_two_powers_io_table[ind].powf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==21) {
		PRINTF("powf_uv__uv_normal_two_powers: successfully tested: 21 cases for powf\n");
	} else {
		PRINTF("powf_uv__uv_normal_two_powers: %d tests failed for powf (out of 21)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (14 functions) of powf_uv__uv_normal
*/
int powf_uv__uv_normal_main_test() {
	int errors=0;
	int index=0;
	errors+=powf_uv__uv_normal_normal_values();         /* 1. 3 tests */
	errors+=powf_uv__uv_normal_powf_normal_range();     /* 2. 49 tests */
	errors+=powf_uv__uv_normal_powf_normal_range_rnd(); /* 3. 50 tests */
	errors+=powf_uv__uv_normal_random_squares();        /* 4. 50 tests */
	errors+=powf_uv__uv_normal_negative_squares();      /* 5. 50 tests */
	errors+=powf_uv__uv_normal_negative_pow();          /* 6. 64 tests */
	errors+=powf_uv__uv_normal_random_squareroots();    /* 7. 50 tests */
	errors+=powf_uv__uv_normal_random_exponent();       /* 8. 50 tests */
	errors+=powf_uv__uv_normal_random_numbers();        /* 9. 50 tests */
	errors+=powf_uv__uv_normal_linear_squares();        /* 10. 7 tests */
	errors+=powf_uv__uv_normal_linear_powers();         /* 11. 49 tests */
	errors+=powf_uv__uv_normal_roots_of_two();          /* 12. 25 tests */
	errors+=powf_uv__uv_normal_roots_to_ten();          /* 13. 24 tests */
	errors+=powf_uv__uv_normal_two_powers();            /* 14. 21 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of powf_uv__uv_normal: successfully tested ALL 542 cases for powf\n");
	} else {
		PRINTF("SUMMARY of powf_uv__uv_normal: %d tests failed in powf_uv__uv_normal (out of 542 in powf_uv__uv_normal)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.9g. ABS_TOLERANCE_POWF_UV__UV_NORMAL=%.9g\n",powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,ABS_TOLERANCE_POWF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.9g. REL_TOLERANCE_POWF_UV__UV_NORMAL=%.9g\n",powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,REL_TOLERANCE_POWF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.9g. ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL=%.9g\n",powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,ABS_REL_TOLERANCE_POWF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.9g. ULP_TOLERANCE_POWF_UV__UV_NORMAL=%.9g\n",powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,ULP_TOLERANCE_POWF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.9g. EXAKT_TOLERANCE_POWF_UV__UV_NORMAL=%.9g\n",powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,EXAKT_TOLERANCE_POWF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.9g. EQUAL_TOLERANCE_POWF_UV__UV_NORMAL=%.9g\n",powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,EQUAL_TOLERANCE_POWF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.9g. BINHEX_TOLERANCE_POWF_UV__UV_NORMAL=%.9g\n",powf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,BINHEX_TOLERANCE_POWF_UV__UV_NORMAL);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls powf_uv__uv_normal_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = powf_uv__uv_normal_main_test();
	return result;
}
#endif /* NO_MAIN */

