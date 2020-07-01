/**
 * this file (expf_uv__uv_normal.c) contains test cases for expf
 * all test cases for expf have been specified and split into separate files
 * this file contains 276 test cases in 10 functions for the following purpose:
 * 3 specified tests for expf
 * 0; 1; 4.0
 * returns the number of failing tests
 * 20 linear tests for expf
 * range from from -4 to 4
 * returns the number of failing tests
 * 50 random tests for expf
 * range from from -4 to 4
 * returns the number of failing tests
 * 12 specified tests for expf
 * 0.0; 1.0; 1.5707963267948966; 2.718281828459045; 3.141592653589793; 6.283185307179586 ...
 * returns the number of failing tests
 * 20 linear tests for expf
 * range from from 0.99 to 1.01
 * returns the number of failing tests
 * 50 random tests for expf
 * range from from 0.99 to 1.01
 * returns the number of failing tests
 * 20 linear tests for expf
 * range from from -1.001 to -0.999
 * returns the number of failing tests
 * 50 random tests for expf
 * range from from -1.001 to -0.999
 * returns the number of failing tests
 * 26 tests for approximation from 0.0 to 2.0
 * returns the number of failing tests
 * 27 tests for approximation from 1.00000000 to -1.0
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

#define ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL 1.0E-6

float max_dif_below_expf_uv__uv_normal=0.0;
float max_dif_above_expf_uv__uv_normal=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of around_one_rnd */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_around_one_rnd_io_table_type;
/* type for expected input & output values of expf_normal_range */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_expf_normal_range_io_table_type;
/* type for expected input & output values of around_minus_one */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_around_minus_one_io_table_type;
/* type for expected input & output values of approx_minus_one */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_approx_minus_one_io_table_type;
/* type for expected input & output values of approx_two */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_approx_two_io_table_type;
/* type for expected input & output values of normal_values */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_normal_values_io_table_type;
/* type for expected input & output values of base_values */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_base_values_io_table_type;
/* type for expected input & output values of around_minus_one_rnd */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_around_minus_one_rnd_io_table_type;
/* type for expected input & output values of expf_normal_range_rnd */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_expf_normal_range_rnd_io_table_type;
/* type for expected input & output values of around_one */ 
typedef struct {
    float expf_x;
    float expf_out;
} expf_uv__uv_normal_around_one_io_table_type;
/**
 * 3 specified tests for expf
 * 0; 1; 4.0
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_normal_values in the following table */
expf_uv__uv_normal_normal_values_io_table_type expf_uv__uv_normal_normal_values_io_table [3] = {
   {0,1.00000000},
   {1.00000000,2.71828175},
   {4.00000000,54.5981483},
};
/** function expf_uv__uv_normal_normal_values executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_normal_values() {
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
		x=expf_uv__uv_normal_normal_values_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_normal_values_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_normal_values: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_normal_values_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==3) {
		PRINTF("expf_uv__uv_normal_normal_values: successfully tested: 3 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_normal_values: %d tests failed for expf (out of 3)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for expf
 * range from from -4 to 4
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_expf_normal_range in the following table */
expf_uv__uv_normal_expf_normal_range_io_table_type expf_uv__uv_normal_expf_normal_range_io_table [20] = {
   {-4.00000000,0.0183156393},
   {-3.57894731,0.0279050581},
   {-3.15789485,0.0425151475},
   {-2.73684216,0.0647745728},
   {-2.31578946,0.0986882448},
   {-1.89473689,0.150357887},
   {-1.47368419,0.229079947},
   {-1.05263162,0.349018067},
   {-0.631578922,0.531751573},
   {-0.210526317,0.810157716},
   {0.210526317,1.23432755},
   {0.631578922,1.88057756},
   {1.05263162,2.86518121},
   {1.47368419,4.36528826},
   {1.89473689,6.65079832},
   {2.31578946,10.1329193},
   {2.73684216,15.4381571},
   {3.15789485,23.5210285},
   {3.57894731,35.8357964},
   {4.00000000,54.5981483},
};
/** function expf_uv__uv_normal_expf_normal_range executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_expf_normal_range() {
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
		x=expf_uv__uv_normal_expf_normal_range_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_expf_normal_range_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_expf_normal_range: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_expf_normal_range_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("expf_uv__uv_normal_expf_normal_range: successfully tested: 20 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_expf_normal_range: %d tests failed for expf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for expf
 * range from from -4 to 4
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_expf_normal_range_rnd in the following table */
expf_uv__uv_normal_expf_normal_range_rnd_io_table_type expf_uv__uv_normal_expf_normal_range_rnd_io_table [50] = {
   {0.945767879,2.57478976},
   {1.38700104,4.00282764},
   {2.47004128,11.8229351},
   {-0.0729193687,0.929675817},
   {-2.16657066,0.114569843},
   {-0.521027088,0.593910217},
   {-0.383924484,0.681182861},
   {2.45465755,11.6424456},
   {1.53335190,4.63368225},
   {-3.28608894,0.0373998359},
   {-3.61778069,0.0268421825},
   {2.48360491,11.9843893},
   {-2.71741962,0.0660449564},
   {-2.81086206,0.0601531155},
   {1.50875759,4.52111006},
   {0.702550888,2.01889610},
   {-0.124526501,0.882914841},
   {3.67977667,39.6375389},
   {1.98625803,7.28821039},
   {-1.04114103,0.353051603},
   {-3.08066511,0.0459286980},
   {-1.48265553,0.227033988},
   {2.59788275,13.4352617},
   {-2.93434000,0.0531657971},
   {-0.735799313,0.479122341},
   {3.06860495,21.5118713},
   {-2.64140797,0.0712608695},
   {-2.93958616,0.0528876111},
   {3.40412951,30.0880928},
   {3.28959942,26.8321133},
   {-2.25185966,0.105203398},
   {-0.452967167,0.635739028},
   {0.0838813782,1.08749986},
   {-2.28899384,0.101368405},
   {-0.948945522,0.387149036},
   {1.30110121,3.67333961},
   {-3.24152899,0.0391040593},
   {3.34080362,28.2418137},
   {-2.62660027,0.0723239258},
   {0.211454868,1.23547423},
   {0.451570034,1.57077646},
   {0.700060368,2.01387429},
   {-1.67658377,0.187011763},
   {-0.214679241,0.806800187},
   {-0.425147533,0.653673351},
   {3.51426840,33.5913429},
   {-2.63367033,0.0718143955},
   {2.39752102,10.9958839},
   {-3.83797503,0.0215371698},
   {-0.932633877,0.393515885},
};
/** function expf_uv__uv_normal_expf_normal_range_rnd executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_expf_normal_range_rnd() {
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
		x=expf_uv__uv_normal_expf_normal_range_rnd_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_expf_normal_range_rnd_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_expf_normal_range_rnd: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_expf_normal_range_rnd_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("expf_uv__uv_normal_expf_normal_range_rnd: successfully tested: 50 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_expf_normal_range_rnd: %d tests failed for expf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 12 specified tests for expf
 * 0.0; 1.0; 1.5707963267948966; 2.718281828459045; 3.141592653589793; 6.283185307179586 ...
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_base_values in the following table */
expf_uv__uv_normal_base_values_io_table_type expf_uv__uv_normal_base_values_io_table [12] = {
   {0.0,1.00000000},
   {1.00000000,2.71828175},
   {1.57079637,4.81047773},
   {2.71828175,15.1542606},
   {3.14159274,23.1406956},
   {6.28318548,535.491760},
   {-0.0,1.00000000},
   {-1.00000000,0.367879450},
   {-1.57079637,0.207879573},
   {-2.71828175,0.0659880415},
   {-3.14159274,0.0432139151},
   {-6.28318548,0.00186744239},
};
/** function expf_uv__uv_normal_base_values executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_base_values() {
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
	for (ind=0;ind<12;ind++) {
		x=expf_uv__uv_normal_base_values_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_base_values_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_base_values: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_base_values_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==12) {
		PRINTF("expf_uv__uv_normal_base_values: successfully tested: 12 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_base_values: %d tests failed for expf (out of 12)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for expf
 * range from from 0.99 to 1.01
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_around_one in the following table */
expf_uv__uv_normal_around_one_io_table_type expf_uv__uv_normal_around_one_io_table [20] = {
   {0.990000010,2.69123459},
   {0.991052628,2.69406891},
   {0.992105246,2.69690609},
   {0.993157923,2.69974661},
   {0.994210541,2.70258999},
   {0.995263159,2.70543623},
   {0.996315777,2.70828557},
   {0.997368395,2.71113777},
   {0.998421073,2.71399331},
   {0.999473691,2.71685147},
   {1.00052631,2.71971297},
   {1.00157893,2.72257710},
   {1.00263155,2.72544456},
   {1.00368416,2.72831488},
   {1.00473678,2.73118830},
   {1.00578952,2.73406506},
   {1.00684214,2.73694444},
   {1.00789475,2.73982692},
   {1.00894737,2.74271250},
   {1.00999999,2.74560094},
};
/** function expf_uv__uv_normal_around_one executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_around_one() {
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
		x=expf_uv__uv_normal_around_one_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_around_one_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_around_one: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_around_one_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("expf_uv__uv_normal_around_one: successfully tested: 20 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_around_one: %d tests failed for expf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for expf
 * range from from 0.99 to 1.01
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_around_one_rnd in the following table */
expf_uv__uv_normal_around_one_rnd_io_table_type expf_uv__uv_normal_around_one_rnd_io_table [50] = {
   {0.990157843,2.69165921},
   {1.00095224,2.72087145},
   {1.00905299,2.74300218},
   {0.997788012,2.71227574},
   {1.00368536,2.72831821},
   {0.994310141,2.70285916},
   {0.993813574,2.70151734},
   {0.998502493,2.71421432},
   {1.00192845,2.72352886},
   {1.00494695,2.73176241},
   {0.990689337,2.69309020},
   {1.00673568,2.73665309},
   {1.00875866,2.74219489},
   {1.00004768,2.71841145},
   {0.997359157,2.71111274},
   {1.00997090,2.74552107},
   {1.00766265,2.73919106},
   {0.998018146,2.71289992},
   {1.00402617,2.72924829},
   {1.00691116,2.73713326},
   {1.00233221,2.72462893},
   {1.00147104,2.72228336},
   {1.00394821,2.72903538},
   {0.993548989,2.70080256},
   {1.00366068,2.72825074},
   {0.997202635,2.71068835},
   {0.993939102,2.70185637},
   {0.990074158,2.69143414},
   {0.999891162,2.71798611},
   {1.00445199,2.73041058},
   {1.00057018,2.71983218},
   {1.00046611,2.71954918},
   {0.999513507,2.71695971},
   {0.994074523,2.70222235},
   {0.999045014,2.71568704},
   {1.00732970,2.73827910},
   {1.00169444,2.72289181},
   {1.00398350,2.72913170},
   {0.996288121,2.70821071},
   {0.999164581,2.71601176},
   {0.996101022,2.70770383},
   {0.991411567,2.69503593},
   {1.00050330,2.71965027},
   {0.994498789,2.70336914},
   {1.00029552,2.71908522},
   {0.992761552,2.69867682},
   {1.00948048,2.74417496},
   {1.00994408,2.74544740},
   {0.998632729,2.71456766},
   {1.00626445,2.73536372},
};
/** function expf_uv__uv_normal_around_one_rnd executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_around_one_rnd() {
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
		x=expf_uv__uv_normal_around_one_rnd_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_around_one_rnd_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_around_one_rnd: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_around_one_rnd_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("expf_uv__uv_normal_around_one_rnd: successfully tested: 50 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_around_one_rnd: %d tests failed for expf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 20 linear tests for expf
 * range from from -1.001 to -0.999
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_around_minus_one in the following table */
expf_uv__uv_normal_around_minus_one_io_table_type expf_uv__uv_normal_around_minus_one_io_table [20] = {
   {-1.00100005,0.367511719},
   {-1.00089478,0.367550403},
   {-1.00078952,0.367589116},
   {-1.00068426,0.367627800},
   {-1.00057900,0.367666513},
   {-1.00047374,0.367705196},
   {-1.00036848,0.367743909},
   {-1.00026321,0.367782623},
   {-1.00015795,0.367821336},
   {-1.00005269,0.367860049},
   {-0.999947369,0.367898792},
   {-0.999842107,0.367937535},
   {-0.999736845,0.367976248},
   {-0.999631584,0.368014991},
   {-0.999526322,0.368053734},
   {-0.999421060,0.368092477},
   {-0.999315798,0.368131220},
   {-0.999210536,0.368169993},
   {-0.999105275,0.368208736},
   {-0.999000013,0.368247509},
};
/** function expf_uv__uv_normal_around_minus_one executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_around_minus_one() {
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
		x=expf_uv__uv_normal_around_minus_one_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_around_minus_one_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_around_minus_one: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_around_minus_one_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==20) {
		PRINTF("expf_uv__uv_normal_around_minus_one: successfully tested: 20 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_around_minus_one: %d tests failed for expf (out of 20)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for expf
 * range from from -1.001 to -0.999
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_around_minus_one_rnd in the following table */
expf_uv__uv_normal_around_minus_one_rnd_io_table_type expf_uv__uv_normal_around_minus_one_rnd_io_table [50] = {
   {-1.00023055,0.367794633},
   {-1.00009894,0.367843032},
   {-1.00002527,0.367870152},
   {-1.00079882,0.367585689},
   {-1.00034547,0.367752373},
   {-1.00086105,0.367562801},
   {-1.00088942,0.367552400},
   {-0.999448180,0.368082494},
   {-0.999301255,0.368136585},
   {-0.999671519,0.368000299},
   {-1.00027370,0.367778778},
   {-0.999640107,0.368011862},
   {-0.999631524,0.368015021},
   {-1.00027990,0.367776483},
   {-1.00050092,0.367695212},
   {-0.999954462,0.367896199},
   {-1.00055027,0.367677063},
   {-1.00073683,0.367608488},
   {-1.00076926,0.367596567},
   {-0.999557912,0.368042111},
   {-0.999163687,0.368187219},
   {-1.00008643,0.367847651},
   {-1.00080132,0.367584765},
   {-1.00046587,0.367708087},
   {-1.00058973,0.367662549},
   {-0.999809921,0.367949367},
   {-1.00069833,0.367622644},
   {-0.999582231,0.368033171},
   {-1.00022340,0.367797256},
   {-1.00079072,0.367588669},
   {-0.999160588,0.368188381},
   {-0.999825001,0.367943823},
   {-0.999210775,0.368169904},
   {-1.00056124,0.367673039},
   {-0.999471605,0.368073881},
   {-0.999413252,0.368095368},
   {-0.999225318,0.368164539},
   {-0.999934077,0.367903680},
   {-1.00030923,0.367765695},
   {-1.00048971,0.367699325},
   {-1.00021410,0.367800683},
   {-1.00009787,0.367843449},
   {-1.00040436,0.367730707},
   {-0.999534845,0.368050605},
   {-1.00022781,0.367795646},
   {-1.00069606,0.367623448},
   {-1.00093699,0.367534906},
   {-0.999719262,0.367982745},
   {-1.00051844,0.367688775},
   {-1.00026536,0.367781848},
};
/** function expf_uv__uv_normal_around_minus_one_rnd executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_around_minus_one_rnd() {
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
		x=expf_uv__uv_normal_around_minus_one_rnd_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_around_minus_one_rnd_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_around_minus_one_rnd: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_around_minus_one_rnd_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("expf_uv__uv_normal_around_minus_one_rnd: successfully tested: 50 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_around_minus_one_rnd: %d tests failed for expf (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 26 tests for approximation from 0.0 to 2.0
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_approx_two in the following table */
expf_uv__uv_normal_approx_two_io_table_type expf_uv__uv_normal_approx_two_io_table [25] = {
   {1.00000000,2.71828175},
   {1.50000000,4.48168898},
   {1.75000000,5.75460291},
   {1.87500000,6.52081919},
   {1.93750000,6.94137573},
   {1.96875000,7.16171885},
   {1.98437500,7.27449942},
   {1.99218750,7.33155394},
   {1.99609375,7.36024904},
   {1.99804688,7.37463856},
   {1.99902344,7.38184357},
   {1.99951172,7.38544893},
   {1.99975586,7.38725233},
   {1.99987793,7.38815403},
   {1.99993896,7.38860512},
   {1.99996948,7.38883066},
   {1.99998474,7.38894320},
   {1.99999237,7.38899994},
   {1.99999619,7.38902807},
   {1.99999809,7.38904190},
   {1.99999905,7.38904905},
   {1.99999952,7.38905239},
   {1.99999976,7.38905430},
   {1.99999988,7.38905525},
   {2.00000000,7.38905621},
};
/** function expf_uv__uv_normal_approx_two executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_approx_two() {
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
	for (ind=0;ind<25;ind++) {
		x=expf_uv__uv_normal_approx_two_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_approx_two_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_approx_two: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_approx_two_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==25) {
		PRINTF("expf_uv__uv_normal_approx_two: successfully tested: 25 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_approx_two: %d tests failed for expf (out of 25)\n",errors);
	}
#endif
	return errors;
}

/**
 * 27 tests for approximation from 1.00000000 to -1.0
 * returns the number of failing tests
 */
/* store data of expf_uv__uv_normal_approx_minus_one in the following table */
expf_uv__uv_normal_approx_minus_one_io_table_type expf_uv__uv_normal_approx_minus_one_io_table [26] = {
   {0.0,1.00000000},
   {-0.500000000,0.606530666},
   {-0.750000000,0.472366542},
   {-0.875000000,0.416862011},
   {-0.937500000,0.391605616},
   {-0.968750000,0.379557192},
   {-0.984375000,0.373672694},
   {-0.992187500,0.370764762},
   {-0.996093750,0.369319290},
   {-0.998046875,0.368598670},
   {-0.999023438,0.368238866},
   {-0.999511719,0.368059129},
   {-0.999755859,0.367969275},
   {-0.999877930,0.367924362},
   {-0.999938965,0.367901891},
   {-0.999969482,0.367890656},
   {-0.999984741,0.367885053},
   {-0.999992371,0.367882252},
   {-0.999996185,0.367880851},
   {-0.999998093,0.367880136},
   {-0.999999046,0.367879778},
   {-0.999999523,0.367879629},
   {-0.999999762,0.367879540},
   {-0.999999881,0.367879480},
   {-0.999999940,0.367879450},
   {-1.00000000,0.367879450},
};
/** function expf_uv__uv_normal_approx_minus_one executes the tests and returns the number of failing tests */
int expf_uv__uv_normal_approx_minus_one() {
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
		x=expf_uv__uv_normal_approx_minus_one_io_table[ind].expf_x;
		res=expf(x);
		if (test_compare_float(res,expf_uv__uv_normal_approx_minus_one_io_table[ind].expf_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL,10.0,&max_dif_below_expf_uv__uv_normal,&max_dif_above_expf_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("expf_uv__uv_normal_approx_minus_one: test %d (IO_VV) failed for expf(%.9g): Expected %.9g, found %.9g\n",ind+1,x,expf_uv__uv_normal_approx_minus_one_io_table[ind].expf_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==26) {
		PRINTF("expf_uv__uv_normal_approx_minus_one: successfully tested: 26 cases for expf\n");
	} else {
		PRINTF("expf_uv__uv_normal_approx_minus_one: %d tests failed for expf (out of 26)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (10 functions) of expf_uv__uv_normal
*/
int expf_uv__uv_normal_main_test() {
	int errors=0;
	int index=0;
	errors+=expf_uv__uv_normal_normal_values();         /* 1. 3 tests */
	errors+=expf_uv__uv_normal_expf_normal_range();     /* 2. 20 tests */
	errors+=expf_uv__uv_normal_expf_normal_range_rnd(); /* 3. 50 tests */
	errors+=expf_uv__uv_normal_base_values();           /* 4. 12 tests */
	errors+=expf_uv__uv_normal_around_one();            /* 5. 20 tests */
	errors+=expf_uv__uv_normal_around_one_rnd();        /* 6. 50 tests */
	errors+=expf_uv__uv_normal_around_minus_one();      /* 7. 20 tests */
	errors+=expf_uv__uv_normal_around_minus_one_rnd();  /* 8. 50 tests */
	errors+=expf_uv__uv_normal_approx_two();            /* 9. 25 tests */
	errors+=expf_uv__uv_normal_approx_minus_one();      /* 10. 26 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of expf_uv__uv_normal: successfully tested ALL 276 cases for expf\n");
	} else {
		PRINTF("SUMMARY of expf_uv__uv_normal: %d tests failed in expf_uv__uv_normal (out of 276 in expf_uv__uv_normal)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.9g. ABS_TOLERANCE_EXPF_UV__UV_NORMAL=%.9g\n",expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,ABS_TOLERANCE_EXPF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.9g. REL_TOLERANCE_EXPF_UV__UV_NORMAL=%.9g\n",expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,REL_TOLERANCE_EXPF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.9g. ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL=%.9g\n",expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,ABS_REL_TOLERANCE_EXPF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.9g. ULP_TOLERANCE_EXPF_UV__UV_NORMAL=%.9g\n",expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,ULP_TOLERANCE_EXPF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.9g. EXAKT_TOLERANCE_EXPF_UV__UV_NORMAL=%.9g\n",expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,EXAKT_TOLERANCE_EXPF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.9g. EQUAL_TOLERANCE_EXPF_UV__UV_NORMAL=%.9g\n",expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,EQUAL_TOLERANCE_EXPF_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.9g. BINHEX_TOLERANCE_EXPF_UV__UV_NORMAL=%.9g\n",expf_uv__uv_normal_all_deviation_results_float[index].max_diff_value,BINHEX_TOLERANCE_EXPF_UV__UV_NORMAL);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls expf_uv__uv_normal_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = expf_uv__uv_normal_main_test();
	return result;
}
#endif /* NO_MAIN */

