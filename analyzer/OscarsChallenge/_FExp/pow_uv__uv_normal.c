/**
 * this file (pow_uv__uv_normal.c) contains test cases for pow
 * all test cases for pow have been specified and split into separate files
 * this file contains 607 test cases in 14 functions for the following purpose:
 * 3 specified tests for pow
 * (0,0); (1,1); (10.0,10.0)
 * returns the number of failing tests
 * 49 linear tests for pow
 * range: (0,0) to (10,10)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (1,2) to (100,2)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (-100,1) to (2,2)
 * returns the number of failing tests
 * 64 linear tests for pow
 * range: (-15,1) to (-1,15)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (1,0.5) to (100,0.5)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (1,2) to (3,10)
 * returns the number of failing tests
 * 50 random tests for pow
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 * 14 (35 omitted out of range or duplicate)) linear tests for pow
 * range: (0,2) to (20,2)
 * returns the number of failing tests
 * 49 linear tests for pow
 * range: (0,2) to (10,4)
 * returns the number of failing tests
 * 54 approximation tests with factors 2.0,2.0
 * range: (0,2) to (1,2)
 * returns the number of failing tests
 * 53 approximation tests with factors 2.0,2.0
 * range: (0,1) to (0,10)
 * returns the number of failing tests
 * 21 specified tests for pow
 * (2,0); (2,1); (2,2); (2,3); (2,4); (2,5); (2,6); (2,7); (2,8); (2,9); (2,10); (2,11) ...
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

#define ABS_REL_TOLERANCE_POW_UV__UV_NORMAL 1.0E-12

double max_dif_below_pow_uv__uv_normal=0.0;
double max_dif_above_pow_uv__uv_normal=0.0;

/** printf for debugging */
#ifndef PRINTF
#define PRINTF printf
#endif

/* type for expected input & output values of linear_squares */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_linear_squares_io_table_type;
/* type for expected input & output values of random_squares */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_random_squares_io_table_type;
/* type for expected input & output values of normal_values */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_normal_values_io_table_type;
/* type for expected input & output values of random_squareroots */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_random_squareroots_io_table_type;
/* type for expected input & output values of pow_normal_range */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_pow_normal_range_io_table_type;
/* type for expected input & output values of random_numbers */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_random_numbers_io_table_type;
/* type for expected input & output values of linear_powers */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_linear_powers_io_table_type;
/* type for expected input & output values of negative_pow */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_negative_pow_io_table_type;
/* type for expected input & output values of roots_to_ten */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_roots_to_ten_io_table_type;
/* type for expected input & output values of negative_squares */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_negative_squares_io_table_type;
/* type for expected input & output values of roots_of_two */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_roots_of_two_io_table_type;
/* type for expected input & output values of two_powers */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_two_powers_io_table_type;
/* type for expected input & output values of pow_normal_range_rnd */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_pow_normal_range_rnd_io_table_type;
/* type for expected input & output values of random_exponent */ 
typedef struct {
    double pow_x;
    double pow_y;
    double pow_out;
} pow_uv__uv_normal_random_exponent_io_table_type;
/**
 * 3 specified tests for pow
 * (0,0); (1,1); (10.0,10.0)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_normal_values in the following table */
pow_uv__uv_normal_normal_values_io_table_type pow_uv__uv_normal_normal_values_io_table [3] = {
   {0,0,1.0},
   {1.0,1.0,1.0},
   {10.0,10.0,1.0E+10},
};
/** function pow_uv__uv_normal_normal_values executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_normal_values() {
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
		x=pow_uv__uv_normal_normal_values_io_table[ind].pow_x;
		y=pow_uv__uv_normal_normal_values_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_normal_values_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_normal_values: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_normal_values_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==3) {
		PRINTF("pow_uv__uv_normal_normal_values: successfully tested: 3 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_normal_values: %d tests failed for pow (out of 3)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 linear tests for pow
 * range: (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_pow_normal_range in the following table */
pow_uv__uv_normal_pow_normal_range_io_table_type pow_uv__uv_normal_pow_normal_range_io_table [49] = {
   {0.0,0.0,1.0},
   {0.0,1.6666666666666667,0.0},
   {0.0,3.3333333333333335,0.0},
   {0.0,5.0,0.0},
   {0.0,6.666666666666667,0.0},
   {0.0,8.333333333333334,0.0},
   {0.0,10.0,0.0},
   {1.6666666666666667,0.0,1.0},
   {1.6666666666666667,1.6666666666666667,2.3428685147270816},
   {1.6666666666666667,3.3333333333333335,5.489032877299481},
   {1.6666666666666667,5.0,12.860082304526752},
   {1.6666666666666667,6.666666666666667,30.12948192807462},
   {1.6666666666666667,8.333333333333334,70.58941457432464},
   {1.6666666666666667,10.0,165.3817168792021},
   {3.3333333333333335,0.0,1.0},
   {3.3333333333333335,1.6666666666666667,7.438143889801885},
   {3.3333333333333335,3.3333333333333335,55.32598452539711},
   {3.3333333333333335,5.0,411.5226337448561},
   {3.3333333333333335,6.666666666666667,3060.964563704481},
   {3.3333333333333335,8.333333333333334,22767.89486641858},
   {3.3333333333333335,10.0,169350.87808430294},
   {5.0,0.0,1.0},
   {5.0,1.6666666666666667,14.620088691064332},
   {5.0,3.3333333333333335,213.7469933345872},
   {5.0,5.0,3125.0},
   {5.0,6.666666666666667,45687.77715957606},
   {5.0,8.333333333333334,667959.3541705854},
   {5.0,10.0,9765625.0},
   {6.666666666666667,0.0,1.0},
   {6.666666666666667,1.6666666666666667,23.614634870724696},
   {6.666666666666667,3.3333333333333335,557.6509800776468},
   {6.666666666666667,5.0,13168.724279835395},
   {6.666666666666667,6.666666666666667,310974.61558155995},
   {6.666666666666667,8.333333333333334,7343552.001022517},
   {6.666666666666667,10.0,173415299.1583262},
   {8.333333333333334,0.0,1.0},
   {8.333333333333334,1.6666666666666667,34.252945476812094},
   {8.333333333333334,3.3333333333333335,1173.264273837462},
   {8.333333333333334,5.0,40187.757201646105},
   {8.333333333333334,6.666666666666667,1376549.0562633472},
   {8.333333333333334,8.333333333333334,47150859.7703456},
   {8.333333333333334,10.0,1615055828.8984582},
   {10.0,0.0,1.0},
   {10.0,1.6666666666666667,46.4158883361278},
   {10.0,3.3333333333333335,2154.4346900318847},
   {10.0,5.0,100000.0},
   {10.0,6.666666666666667,4641588.833612782},
   {10.0,8.333333333333334,215443469.00318867},
   {10.0,10.0,1.0E+10},
};
/** function pow_uv__uv_normal_pow_normal_range executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_pow_normal_range() {
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
		x=pow_uv__uv_normal_pow_normal_range_io_table[ind].pow_x;
		y=pow_uv__uv_normal_pow_normal_range_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_pow_normal_range_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_pow_normal_range: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_pow_normal_range_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("pow_uv__uv_normal_pow_normal_range: successfully tested: 49 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_pow_normal_range: %d tests failed for pow (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_pow_normal_range_rnd in the following table */
pow_uv__uv_normal_pow_normal_range_rnd_io_table_type pow_uv__uv_normal_pow_normal_range_rnd_io_table [50] = {
   {9.730175340621008,2.5269108531867035,313.97338579791375},
   {4.288328464180036,6.7632947174348725,18895.15153262844},
   {4.836585607994803,3.2654211317151196,171.91260518112404},
   {7.049711916185037,0.04002378477223845,1.0813020550095997},
   {5.474893613146538,9.112335760869211,5349671.992497238},
   {5.154926782247015,8.164847579463167,653413.5929016174},
   {2.1023921193447794,8.491027349671466,549.7573220693213},
   {9.085218187527996,1.235364609984655,15.271918237179117},
   {5.402394944700697,5.822901383553692,18440.723513312605},
   {7.577845863683599,8.718508235881874,46593680.37174158},
   {9.768485659895962,8.92708353009064,685915285.6990407},
   {4.663423642192028,5.835336389822528,7982.112834951947},
   {5.645513108759812,9.644906359401002,17787261.81152138},
   {0.8006353777948982,9.452515196401913,0.12224067989109533},
   {7.207126447436037,6.349172247051166,279305.56240927574},
   {6.9367561407147695,1.246913205143969,11.190488796923063},
   {2.8750420644222165,9.664180465331183,27065.767633801013},
   {5.254049967434114,8.731991003474036,1955909.9739998465},
   {7.52311882462167,8.201022423955408,15394266.673704183},
   {0.606257151912386,5.369436960889983,0.06807533561310602},
   {6.859711162639499,9.403664329153978,73171051.7387315},
   {2.206020521833519,9.43676184097887,1748.080679198483},
   {6.444659861960343,1.704855810125907,23.964552476917643},
   {5.993246278976865,7.581336563372969,786535.4432649887},
   {0.07267162265728655,8.106911412758667,5.877407032700508E-10},
   {4.296028437348273,3.063616598505031,86.99113976138551},
   {4.842727611655069,8.030936551507835,317624.98646002077},
   {0.8899167676406283,0.5770668779425336,0.9349130253334497},
   {3.1335527256661155,3.8190762687029522,78.41572467647845},
   {6.36793785790689,9.229566012854102,26336626.972486738},
   {4.918070681688652,0.6278879509495672,2.718752027271824},
   {0.8529248299383707,2.7808298209273685,0.642502133806523},
   {7.375152170380344,3.61695027857289,1376.209361152804},
   {8.515355974387905,1.5526671650240986,27.81604348139389},
   {2.594865754160284,4.315338017588837,61.241489891133504},
   {1.1040326741166784,3.563639977004879,1.4228925714644745},
   {8.323822688330173,8.839608055889725,136550412.8153372},
   {5.753577939338438,3.386970601548983,374.87602547396085},
   {1.1600998743737612,4.240456783229398,1.8771106365203547},
   {5.701871725005809,5.990297766449583,33788.55341436445},
   {6.3826183950092386,5.671225796441462,36756.183562378625},
   {1.2321816333768532,9.388159307253552,7.100171187971595},
   {3.687743315126238,6.2999475067762924,3720.168068905067},
   {2.1488864469448608,4.322832031871312,27.296251793379415},
   {3.5062602179480407,3.9276366214601612,138.02247595094855},
   {2.4023350236770993,1.3864370218186683,3.370733965307025},
   {4.2867770697599745,3.1290548191204426,95.05440238916495},
   {2.0975186225579567,6.0069754414232746,85.60103036245609},
   {7.156988124686596,9.398181737831182,107872480.25179875},
   {2.0528771040459715,9.543230135327173,957.0872423028106},
};
/** function pow_uv__uv_normal_pow_normal_range_rnd executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_pow_normal_range_rnd() {
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
		x=pow_uv__uv_normal_pow_normal_range_rnd_io_table[ind].pow_x;
		y=pow_uv__uv_normal_pow_normal_range_rnd_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_pow_normal_range_rnd_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_pow_normal_range_rnd: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_pow_normal_range_rnd_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_normal_pow_normal_range_rnd: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_pow_normal_range_rnd: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (1,2) to (100,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_random_squares in the following table */
pow_uv__uv_normal_random_squares_io_table_type pow_uv__uv_normal_random_squares_io_table [50] = {
   {8.050105931127392,2.0,64.80420550237243},
   {14.065049420929343,1.9999999999999998,197.82561521318473},
   {53.770531676517315,2.0,2891.270076775352},
   {86.4294420687521,2.0,7470.048456315774},
   {34.4383257906402,1.9999999999999998,1185.998283262273},
   {46.57682322686436,2.0,2169.4004619065718},
   {89.5064066106921,2.0,8011.396824358546},
   {28.217563753923702,2.0,796.2309042067487},
   {83.26736702810798,2.0,6933.454411793645},
   {38.050717249828466,2.0,1447.8570832263936},
   {4.472077716905003,1.9999999999999998,19.99947910603826},
   {93.36636704333661,2.0,8717.278494871052},
   {41.415051160561326,2.0,1715.206462631912},
   {92.84610321622493,1.9999999999999998,8620.398882437885},
   {93.54578796254746,1.9999999999999998,8750.81444553388},
   {72.66918037606176,2.0,5280.809776528599},
   {53.1603273247948,1.9999999999999998,2826.020401279322},
   {91.69882223488914,2.0,8408.673999265799},
   {4.3162224129746525,2.0,18.62977591826473},
   {9.783954729960076,2.0,95.72577015790814},
   {74.79667767739186,2.0,5594.5429915756495},
   {1.066074817801343,1.9999999999999998,1.1365155171501669},
   {70.72600852031874,1.9999999999999998,5002.168281216194},
   {7.629268623828407,1.9999999999999998,58.20573973453257},
   {14.308675009886688,2.0,204.7381805385558},
   {90.87333397562688,2.0,8257.962827845822},
   {36.12801422876277,2.0,1305.2334121136853},
   {17.625724365918575,2.0,310.6661594233358},
   {60.49842965976141,2.0,3660.0599912970993},
   {77.23753294432892,2.0,5965.6364953262955},
   {29.51613321688919,2.0,871.2021200771495},
   {26.73195852278451,2.0,714.5976064638713},
   {38.69866316678099,1.9999999999999998,1497.5865308959703},
   {92.49070718236972,1.9999999999999998,8554.530915094849},
   {70.44230302468942,2.0,4962.118055422168},
   {91.97449290164657,2.0,8459.307344515035},
   {20.959081583513147,2.0,439.28310082435996},
   {73.57835030842769,2.0,5413.773634109701},
   {45.88145054910083,2.0,2105.107504489585},
   {92.92063795611618,2.0,8634.24495817162},
   {87.342829408377,2.0,7628.769849060846},
   {25.65667910890353,2.0,658.2651828972467},
   {48.056090740255364,2.0,2309.3878572356575},
   {87.29366827000244,1.9999999999999998,7620.184520033224},
   {30.719666808161293,2.0,943.6979288044466},
   {78.46301786544142,2.0,6156.445172552579},
   {95.98230764985799,1.9999999999999998,9212.603381791978},
   {1.679491617941189,2.0,2.8206920947347127},
   {20.19267838983233,2.0,407.7442605552016},
   {69.78355540272936,2.0,4869.744604645798},
};
/** function pow_uv__uv_normal_random_squares executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_random_squares() {
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
		x=pow_uv__uv_normal_random_squares_io_table[ind].pow_x;
		y=pow_uv__uv_normal_random_squares_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_random_squares_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_random_squares: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_random_squares_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_normal_random_squares: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_random_squares: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (-100,1) to (2,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_negative_squares in the following table */
pow_uv__uv_normal_negative_squares_io_table_type pow_uv__uv_normal_negative_squares_io_table [50] = {
   {-28.652727047023603,1.4399150224568964,NAN},
   {-11.36918523615978,1.0697577230415256,NAN},
   {-3.1050050328169334,1.9251059571052287,NAN},
   {0.03485652747083634,1.0201769759139674,0.03257405465831706},
   {-78.06393419314351,1.7907122129428903,NAN},
   {-70.7697801032804,1.8266531403522066,NAN},
   {-50.70582570479947,1.2257704351380259,NAN},
   {-53.44270936585424,1.9441088661033477,NAN},
   {-45.21808804319729,1.1930031874032159,NAN},
   {-22.69023645580836,1.4657494766589887,NAN},
   {-99.90826314522819,1.4051327691490385,NAN},
   {-36.744029221295705,1.1968926186202746,NAN},
   {-20.021717621625115,1.010455128105289,NAN},
   {-61.69750554356252,1.7390981207973062,NAN},
   {-0.5374007403195975,1.7149991171332974,NAN},
   {-48.825833325670665,1.5038189109304743,NAN},
   {-7.376991669240297,1.397698180217978,NAN},
   {-24.33750414503841,1.9195186283036072,NAN},
   {-16.44131908841534,1.8853530559455915,NAN},
   {-93.14038892072443,1.2221798501011638,NAN},
   {-75.67897044657285,1.445765525363656,NAN},
   {-42.152292442388514,1.1263519573250118,NAN},
   {-97.99548088856369,1.7302752159305645,NAN},
   {1.554060730109697,1.3623652052672286,1.8232630153811842},
   {-28.227524810329626,1.3510320522421746,NAN},
   {-9.090432293481541,1.5350637490388979,NAN},
   {-25.635078990338343,1.0136899916728324,NAN},
   {-8.936138151721977,1.0103868158064429,NAN},
   {-12.405483465869636,1.1428077153616343,NAN},
   {-87.7871098877004,1.568354046297129,NAN},
   {-92.51689691841501,1.2350947586629664,NAN},
   {-73.41498986062405,1.374487548449352,NAN},
   {-89.97460691657359,1.7547420789674768,NAN},
   {-67.69055927468683,1.4164294456035775,NAN},
   {-30.91224448471189,1.2454285940224135,NAN},
   {-19.743900170387974,1.928490912053823,NAN},
   {-30.340876816880495,1.4751960105449038,NAN},
   {-54.51137151265173,1.4840943220088145,NAN},
   {-83.4325383205133,1.8532361769523575,NAN},
   {-21.28738253932383,1.3745477852262664,NAN},
   {-53.14075320697514,1.0617369390297566,NAN},
   {-48.97122820533687,1.612316950667271,NAN},
   {-36.689092221260395,1.831283397663014,NAN},
   {-22.583969535472292,1.3979361196503808,NAN},
   {-58.99988453610297,1.05491394757667,NAN},
   {-27.32766013339048,1.1719270230797096,NAN},
   {-67.4886514183448,1.98378099864888,NAN},
   {-29.968939877139064,1.4365702754710952,NAN},
   {-63.8158841945759,1.076468899952797,NAN},
   {-70.65133859091455,1.3771008958563677,NAN},
};
/** function pow_uv__uv_normal_negative_squares executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_negative_squares() {
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
		x=pow_uv__uv_normal_negative_squares_io_table[ind].pow_x;
		y=pow_uv__uv_normal_negative_squares_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_negative_squares_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_negative_squares: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_negative_squares_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_normal_negative_squares: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_negative_squares: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 64 linear tests for pow
 * range: (-15,1) to (-1,15)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_negative_pow in the following table */
pow_uv__uv_normal_negative_pow_io_table_type pow_uv__uv_normal_negative_pow_io_table [64] = {
   {-15.0,1.0,-15.0},
   {-15.0,3.0,-3375.0},
   {-15.0,5.0,-759375.0},
   {-15.0,7.0,-170859375.0},
   {-15.0,9.0,-38443359375.0},
   {-15.0,11.0,-8649755859375.0},
   {-15.0,13.0,-1946195068359375.0},
   {-15.0,15.0,-4.3789389038085939E+17},
   {-13.0,1.0,-13.0},
   {-13.0,3.0,-2197.0},
   {-13.0,5.0,-371293.0},
   {-13.0,7.0,-62748517.0},
   {-13.0,9.0,-10604499373.0},
   {-13.0,11.0,-1792160394037.0},
   {-13.0,13.0,-302875106592253.0},
   {-13.0,15.0,-5.118589301409076E+16},
   {-11.0,1.0,-11.0},
   {-11.0,3.0,-1331.0},
   {-11.0,5.0,-161051.0},
   {-11.0,7.0,-19487171.0},
   {-11.0,9.0,-2357947691.0},
   {-11.0,11.0,-285311670611.0},
   {-11.0,13.0,-34522712143931.0},
   {-11.0,15.0,-4177248169415651.0},
   {-9.0,1.0,-9.0},
   {-9.0,3.0,-729.0},
   {-9.0,5.0,-59049.0},
   {-9.0,7.0,-4782969.0},
   {-9.0,9.0,-387420489.0},
   {-9.0,11.0,-31381059609.0},
   {-9.0,13.0,-2541865828329.0},
   {-9.0,15.0,-205891132094649.0},
   {-7.0,1.0,-7.0},
   {-7.0,3.0,-343.0},
   {-7.0,5.0,-16807.0},
   {-7.0,7.0,-823543.0},
   {-7.0,9.0,-40353607.0},
   {-7.0,11.0,-1977326743.0},
   {-7.0,13.0,-96889010407.0},
   {-7.0,15.0,-4747561509943.0},
   {-5.0,1.0,-5.0},
   {-5.0,3.0,-125.0},
   {-5.0,5.0,-3125.0},
   {-5.0,7.0,-78125.0},
   {-5.0,9.0,-1953125.0},
   {-5.0,11.0,-48828125.0},
   {-5.0,13.0,-1220703125.0},
   {-5.0,15.0,-30517578125.0},
   {-3.0,1.0,-3.0},
   {-3.0,3.0,-27.0},
   {-3.0,5.0,-243.0},
   {-3.0,7.0,-2187.0},
   {-3.0,9.0,-19683.0},
   {-3.0,11.0,-177147.0},
   {-3.0,13.0,-1594323.0},
   {-3.0,15.0,-14348907.0},
   {-1.0,1.0,-1.0},
   {-1.0,3.0,-1.0},
   {-1.0,5.0,-1.0},
   {-1.0,7.0,-1.0},
   {-1.0,9.0,-1.0},
   {-1.0,11.0,-1.0},
   {-1.0,13.0,-1.0},
   {-1.0,15.0,-1.0},
};
/** function pow_uv__uv_normal_negative_pow executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_negative_pow() {
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
		x=pow_uv__uv_normal_negative_pow_io_table[ind].pow_x;
		y=pow_uv__uv_normal_negative_pow_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_negative_pow_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_negative_pow: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_negative_pow_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==64) {
		PRINTF("pow_uv__uv_normal_negative_pow: successfully tested: 64 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_negative_pow: %d tests failed for pow (out of 64)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (1,0.5) to (100,0.5)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_random_squareroots in the following table */
pow_uv__uv_normal_random_squareroots_io_table_type pow_uv__uv_normal_random_squareroots_io_table [50] = {
   {14.42682916175177,0.5,3.7982665996151153},
   {13.253346091060052,0.5,3.6405145365813407},
   {60.772018842933974,0.5,7.795641015524892},
   {35.618429937055204,0.5,5.9681177884702645},
   {49.37683903102786,0.5,7.026865519634474},
   {41.7259111986645,0.5,6.459559675292465},
   {16.581620044353752,0.5,4.072053541439964},
   {5.083087500253637,0.49999999999999994,2.254570358239821},
   {75.33847991207294,0.5,8.679774185546128},
   {77.3457013343253,0.5,8.794640489202802},
   {86.57828959799942,0.49999999999999994,9.304745541818937},
   {97.20340065252235,0.49999999999999994,9.859178497852765},
   {42.021640971935476,0.49999999999999994,6.482410120621454},
   {70.40443990462578,0.5,8.390735361374817},
   {75.09260015891499,0.49999999999999994,8.665598661310998},
   {45.72051496170762,0.5,6.761694681195508},
   {50.77666547668751,0.49999999999999994,7.125774728174298},
   {23.007067719696508,0.5,4.796568327429154},
   {19.540693569798385,0.5,4.420485671258124},
   {21.462690191242906,0.5,4.632784280672143},
   {98.80264496987374,0.49999999999999994,9.939951960139128},
   {79.73322063027467,0.5,8.929346035980164},
   {81.90295949990472,0.5,9.050025386699462},
   {51.6418678113516,0.5,7.186227648171995},
   {22.086866346433908,0.5,4.6996666207757665},
   {97.05620335184761,0.5,9.85171068149322},
   {29.658424989858716,0.49999999999999994,5.445954919925311},
   {93.72610821653375,0.5,9.681224520510499},
   {58.212105015152,0.5,7.629685774339071},
   {9.843702549639701,0.5,3.1374675376232504},
   {26.686637530335727,0.49999999999999994,5.165911103603673},
   {15.587123942525421,0.49999999999999994,3.94805318385219},
   {98.91850491836773,0.5,9.945778245987979},
   {61.0144191927621,0.5,7.811172715589004},
   {24.39871698068392,0.5,4.939505742549948},
   {45.37210013956479,0.5,6.735881541384527},
   {83.05675556697598,0.49999999999999994,9.113547913243005},
   {84.17490677012589,0.5,9.174688374551252},
   {18.10933166397075,0.5,4.255506040880538},
   {2.2990544114152613,0.5,1.5162633054371728},
   {45.31927956797292,0.5,6.731959563750581},
   {23.810740055877222,0.5,4.879624991316159},
   {92.41127763256333,0.5,9.613078468033189},
   {99.32491916329754,0.5,9.966188798296846},
   {43.886148916256836,0.5,6.62466217374568},
   {41.35633715972167,0.5,6.430889297734931},
   {95.0644275048382,0.5,9.750098845900908},
   {94.38560815707831,0.5,9.715225584466802},
   {16.87246491855926,0.5,4.107610609412638},
   {65.21754848578381,0.5,8.075738262585274},
};
/** function pow_uv__uv_normal_random_squareroots executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_random_squareroots() {
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
		x=pow_uv__uv_normal_random_squareroots_io_table[ind].pow_x;
		y=pow_uv__uv_normal_random_squareroots_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_random_squareroots_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_random_squareroots: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_random_squareroots_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_normal_random_squareroots: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_random_squareroots: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (1,2) to (3,10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_random_exponent in the following table */
pow_uv__uv_normal_random_exponent_io_table_type pow_uv__uv_normal_random_exponent_io_table [50] = {
   {2.8113274929728873,9.650933563978136,21498.812944648944},
   {1.5183281869417122,4.543218405140807,6.667818655058319},
   {1.7517962001521608,8.64287234312482,127.17375636568322},
   {2.0620473126027608,3.2064290727628197,10.180667439083875},
   {2.9409475545620687,5.006248312298374,221.49486088871288},
   {2.132076530786085,3.341962403941956,12.555862792942708},
   {2.643983444862004,6.771632970737283,723.4044042274087},
   {2.7074078814720233,7.257138007796243,1377.5323788434373},
   {1.1510484740814033,8.780156590403479,3.4388362232285314},
   {1.5009755581987414,4.753706336916838,6.893319312534895},
   {1.303677174177619,6.11232023393311,5.057737646201228},
   {1.2808276429372891,6.319452312988068,4.778397804010071},
   {1.7752565484199203,5.316376376924728,21.142967765478787},
   {2.153595421561432,2.41050307531824,6.354671925959293},
   {2.284275058629626,7.14550637721416,365.9645046017027},
   {1.5837585305321649,2.318728555730897,2.9041865537354052},
   {2.374209215648096,9.645834363694924,4189.806278198491},
   {1.1585327538719015,5.376427606018845,2.2059640860583927},
   {1.911474812528084,5.820134366638937,43.41131488831202},
   {1.407035676350493,6.7583144461451505,10.052927797163003},
   {2.0329504717441935,5.724892918170281,58.07549560719835},
   {1.3437118159051358,3.8986223540322413,3.1638593639488093},
   {1.9171566360214718,2.7412084263329266,5.954199571250124},
   {1.8298300111914516,7.548963954642893,95.70381814749526},
   {1.4582535499772544,7.892816811957919,19.638313605524715},
   {1.3331907019542348,9.7603931130361,16.557612388186275},
   {2.689598782207507,2.4673480756699755,11.486524015528211},
   {1.2150112613448574,7.580927327127543,4.377207472858421},
   {1.6336825970503113,3.8297278774838617,6.55200495296558},
   {1.2842846902944465,6.017605928952553,4.506931069701623},
   {1.147838806688536,2.199455334176914,1.3542703573549282},
   {1.033734817693484,2.8199662981809874,1.0980782792972628},
   {2.290206071034663,3.296296380685191,15.355120002801401},
   {1.7078199048480702,2.899635073024494,4.720599776968317},
   {2.7479922895544595,3.141745625480935,23.94832027079534},
   {1.1520647681668952,9.708546858949875,3.9523129045229055},
   {1.9501745041696332,2.9364242455047282,7.108513344393665},
   {1.254328644036875,6.656110124861714,4.5189414724491686},
   {1.6293425557614079,7.816391391700821,45.41213329466715},
   {2.7124455112411563,5.653455886571443,281.83013230481134},
   {1.8024751022388805,7.424161586700572,79.36240840056585},
   {1.8490004224682477,6.130716772243949,43.302722982337315},
   {1.4080132285926283,8.613028889996913,19.052593553552658},
   {1.2428346140479571,8.381095362757446,6.184252425266718},
   {2.166675451068678,9.236116633934358,1263.0812284509454},
   {2.05819967911036,3.843295593062109,16.026000460519636},
   {1.6260363524082466,6.854379507262034,28.00056943147188},
   {1.1263090794677635,9.941700309562606,3.262602988451119},
   {2.8039689061673596,7.330600086418109,1916.2237110154704},
   {2.070363914480516,8.361493008952277,439.1594016812566},
};
/** function pow_uv__uv_normal_random_exponent executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_random_exponent() {
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
		x=pow_uv__uv_normal_random_exponent_io_table[ind].pow_x;
		y=pow_uv__uv_normal_random_exponent_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_random_exponent_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_random_exponent: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_random_exponent_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_normal_random_exponent: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_random_exponent: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 50 random tests for pow
 * range from from (0,0) to (10,10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_random_numbers in the following table */
pow_uv__uv_normal_random_numbers_io_table_type pow_uv__uv_normal_random_numbers_io_table [50] = {
   {1.8181416085085111,7.074040684992788,68.64627156320884},
   {0.3197895464397782,1.4296677869510455,0.19593877417613162},
   {9.163672287959816,1.2684498354537421,16.608750526570255},
   {5.8704694052710895,0.9290161349168979,5.177373072601071},
   {5.112432731923231,9.855053497858536,9628596.209615044},
   {8.319367203624779,2.749637383737894,338.77758532564496},
   {7.532616814696203,9.101191233688938,95774989.56808981},
   {0.5812434022413515,9.788898617092883,0.004935426702313475},
   {3.939595675107801,3.0304882447963024,63.754263307278535},
   {8.945266725067452,9.205856853485955,575746959.7014446},
   {3.061423349973973,7.36700853398821,3800.158958468114},
   {1.1142922478396033,8.975546137995178,2.641451691495838},
   {8.888099127906576,2.696944230409337,362.1468070959471},
   {8.985133669687421,9.185858076872902,574038629.8843534},
   {3.7332151976229735,5.634771998316377,1673.236812247081},
   {3.3283333485374342,5.501637411967366,746.6251087787489},
   {6.924506026930776,1.0093154122821812,7.050458583027996},
   {6.520081687557656,1.6053804222771828,20.285507821309736},
   {6.140202042986816,5.12617231402894,10973.878294899532},
   {3.7855757666174403,4.621189408517252,469.5233669904007},
   {9.120991429562723,0.016478945463216377,1.0370996329233764},
   {3.0322448094552534,2.5383996242387727,16.707451915117566},
   {4.512772214930363,1.2480020861584107,6.557627516429829},
   {8.282917296738393,5.473565300550711,106105.13138579896},
   {1.9332759550115386,7.353060567260346,127.390198746863},
   {1.9631369885707328,3.2442524795577645,8.920839578993725},
   {3.00603292817876,7.21879850414666,2821.8923926223883},
   {6.853481864521433,0.17535595664260883,1.401464237884463},
   {7.068035940256331,2.888844121571419,284.1133879973117},
   {9.386880014272148,4.318418448330833,15839.949008919111},
   {7.532857064747337,9.141407869854657,103907416.82494643},
   {2.1525526487329882,2.336702118241579,5.998101477099301},
   {7.478854625209671,7.937383320504,8629071.74120533},
   {4.962360806061711,3.193521072617914,166.60781445839893},
   {5.932503584136388,5.373036585235762,14276.9721682865},
   {0.6419691808917583,3.4786162443197357,0.21400094809339126},
   {9.52977454636192,1.125380881837782,12.642730180457473},
   {3.828382560382526,4.7171118646610815,562.5336761360958},
   {7.763868854451448,5.544349361009751,86080.43406050511},
   {8.487049411810903,8.327867811602832,54270432.42653454},
   {3.865517315884335,7.524397237903541,26204.954929072457},
   {2.6058720250118106,7.032797565691168,842.0033778533458},
   {7.512436962698591,7.079177382078594,1584192.380392033},
   {6.580096728758309,2.465995299722877,104.17342698040416},
   {0.027427910713481163,0.14644955858937214,0.5905735798367335},
   {5.171204169948545,7.49127692901703,221673.06507474117},
   {7.499988010048869,9.566400394770813,235060563.91810715},
   {1.6045579046350922,8.30900633974874,50.850973742328954},
   {2.354211362228422,5.405065742739731,102.29375266955999},
   {6.3647079451422615,5.817845171718169,47452.46197625707},
};
/** function pow_uv__uv_normal_random_numbers executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_random_numbers() {
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
		x=pow_uv__uv_normal_random_numbers_io_table[ind].pow_x;
		y=pow_uv__uv_normal_random_numbers_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_random_numbers_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_random_numbers: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_random_numbers_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==50) {
		PRINTF("pow_uv__uv_normal_random_numbers: successfully tested: 50 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_random_numbers: %d tests failed for pow (out of 50)\n",errors);
	}
#endif
	return errors;
}

/**
 * 14 (35 omitted out of range or duplicate)) linear tests for pow
 * range: (0,2) to (20,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_linear_squares in the following table */
pow_uv__uv_normal_linear_squares_io_table_type pow_uv__uv_normal_linear_squares_io_table [14] = {
   {0.0,2.0,0.0},
   {0.0,1.9999999999999998,0.0},
   {3.3333333333333335,2.0,11.111111111111112},
   {3.3333333333333335,1.9999999999999998,11.111111111111109},
   {6.666666666666667,2.0,44.44444444444445},
   {6.666666666666667,1.9999999999999998,44.44444444444443},
   {10.0,2.0,100.0},
   {10.0,1.9999999999999998,99.99999999999994},
   {13.333333333333334,2.0,177.7777777777778},
   {13.333333333333334,1.9999999999999998,177.7777777777777},
   {16.666666666666668,2.0,277.7777777777778},
   {16.666666666666668,1.9999999999999998,277.77777777777766},
   {20.0,2.0,400.0},
   {20.0,1.9999999999999998,399.9999999999997},
};
/** function pow_uv__uv_normal_linear_squares executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_linear_squares() {
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
	for (ind=0;ind<14;ind++) {
		x=pow_uv__uv_normal_linear_squares_io_table[ind].pow_x;
		y=pow_uv__uv_normal_linear_squares_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_linear_squares_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_linear_squares: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_linear_squares_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==14) {
		PRINTF("pow_uv__uv_normal_linear_squares: successfully tested: 14 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_linear_squares: %d tests failed for pow (out of 14)\n",errors);
	}
#endif
	return errors;
}

/**
 * 49 linear tests for pow
 * range: (0,2) to (10,4)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_linear_powers in the following table */
pow_uv__uv_normal_linear_powers_io_table_type pow_uv__uv_normal_linear_powers_io_table [49] = {
   {0.0,2.0,0.0},
   {0.0,2.333333333333333,0.0},
   {0.0,2.6666666666666665,0.0},
   {0.0,3.0,0.0},
   {0.0,3.333333333333333,0.0},
   {0.0,3.6666666666666665,0.0},
   {0.0,4.0,0.0},
   {1.6666666666666667,2.0,2.777777777777778},
   {1.6666666666666667,2.333333333333333,3.2934197263796876},
   {1.6666666666666667,2.6666666666666665,3.904780857878469},
   {1.6666666666666667,3.0,4.629629629629631},
   {1.6666666666666667,3.333333333333333,5.489032877299479},
   {1.6666666666666667,3.6666666666666665,6.507968096464115},
   {1.6666666666666667,4.0,7.716049382716051},
   {3.3333333333333335,2.0,11.111111111111112},
   {3.3333333333333335,2.333333333333333,16.597795357619123},
   {3.3333333333333335,2.6666666666666665,24.79381296600628},
   {3.3333333333333335,3.0,37.037037037037045},
   {3.3333333333333335,3.333333333333333,55.32598452539708},
   {3.3333333333333335,3.6666666666666665,82.64604322002093},
   {3.3333333333333335,4.0,123.45679012345681},
   {5.0,2.0,25.0},
   {5.0,2.333333333333333,42.74939866691741},
   {5.0,2.6666666666666665,73.10044345532164},
   {5.0,3.0,125.0},
   {5.0,3.333333333333333,213.74699333458702},
   {5.0,3.6666666666666665,365.5022172766082},
   {5.0,4.0,625.0},
   {6.666666666666667,2.0,44.44444444444445},
   {6.666666666666667,2.333333333333333,83.64764701164694},
   {6.666666666666667,2.6666666666666665,157.43089913816456},
   {6.666666666666667,3.0,296.29629629629636},
   {6.666666666666667,3.333333333333333,557.6509800776463},
   {6.666666666666667,3.6666666666666665,1049.5393275877639},
   {6.666666666666667,4.0,1975.308641975309},
   {8.333333333333334,2.0,69.44444444444446},
   {8.333333333333334,2.333333333333333,140.79171286049532},
   {8.333333333333334,2.6666666666666665,285.44121230676734},
   {8.333333333333334,3.0,578.7037037037038},
   {8.333333333333334,3.333333333333333,1173.264273837461},
   {8.333333333333334,3.6666666666666665,2378.6767692230615},
   {8.333333333333334,4.0,4822.5308641975325},
   {10.0,2.0,100.0},
   {10.0,2.333333333333333,215.44346900318823},
   {10.0,2.6666666666666665,464.15888336127773},
   {10.0,3.0,1000.0},
   {10.0,3.333333333333333,2154.4346900318824},
   {10.0,3.6666666666666665,4641.588833612777},
   {10.0,4.0,10000.0},
};
/** function pow_uv__uv_normal_linear_powers executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_linear_powers() {
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
		x=pow_uv__uv_normal_linear_powers_io_table[ind].pow_x;
		y=pow_uv__uv_normal_linear_powers_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_linear_powers_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_linear_powers: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_linear_powers_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==49) {
		PRINTF("pow_uv__uv_normal_linear_powers: successfully tested: 49 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_linear_powers: %d tests failed for pow (out of 49)\n",errors);
	}
#endif
	return errors;
}

/**
 * 54 approximation tests with factors 2.0,2.0
 * range: (0,2) to (1,2)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_roots_of_two in the following table */
pow_uv__uv_normal_roots_of_two_io_table_type pow_uv__uv_normal_roots_of_two_io_table [54] = {
   {0.5,2.0,0.25},
   {0.75,2.0,0.5625},
   {0.875,2.0,0.765625},
   {0.9375,2.0,0.87890625},
   {0.96875,2.0,0.9384765625},
   {0.984375,2.0,0.968994140625},
   {0.9921875,2.0,0.98443603515625},
   {0.99609375,2.0,0.9922027587890625},
   {0.998046875,2.0,0.9960975646972656},
   {0.9990234375,2.0,0.9980478286743164},
   {0.99951171875,2.0,0.9990236759185791},
   {0.999755859375,2.0,0.9995117783546448},
   {0.9998779296875,2.0,0.9997558742761612},
   {0.99993896484375,2.0,0.9998779334127903},
   {0.999969482421875,2.0,0.9999389657750726},
   {0.9999847412109375,2.0,0.9999694826547056},
   {0.9999923706054688,2.0,0.9999847412691452},
   {0.9999961853027344,2.0,0.9999923706200207},
   {0.9999980926513672,2.0,0.9999961853063724},
   {0.9999990463256836,2.0,0.9999980926522767},
   {0.9999995231628418,2.0,0.999999046325911},
   {0.9999997615814209,2.0,0.9999995231628986},
   {0.9999998807907104,2.0,0.9999997615814351},
   {0.9999999403953552,2.0,0.999999880790714},
   {0.9999999701976776,2.0,0.9999999403953561},
   {0.9999999850988388,2.0,0.9999999701976778},
   {0.9999999925494194,2.0,0.9999999850988388},
   {0.9999999962747097,2.0,0.9999999925494194},
   {0.9999999981373549,2.0,0.9999999962747097},
   {0.9999999990686774,2.0,0.9999999981373549},
   {0.9999999995343387,2.0,0.9999999990686774},
   {0.9999999997671694,2.0,0.9999999995343387},
   {0.9999999998835847,2.0,0.9999999997671694},
   {0.9999999999417923,2.0,0.9999999998835847},
   {0.9999999999708962,2.0,0.9999999999417923},
   {0.9999999999854481,2.0,0.9999999999708962},
   {0.999999999992724,2.0,0.9999999999854481},
   {0.999999999996362,2.0,0.999999999992724},
   {0.999999999998181,2.0,0.999999999996362},
   {0.9999999999990905,2.0,0.999999999998181},
   {0.9999999999995453,2.0,0.9999999999990905},
   {0.9999999999997726,2.0,0.9999999999995453},
   {0.9999999999998863,2.0,0.9999999999997726},
   {0.9999999999999432,2.0,0.9999999999998863},
   {0.9999999999999716,2.0,0.9999999999999432},
   {0.9999999999999858,2.0,0.9999999999999716},
   {0.9999999999999929,2.0,0.9999999999999858},
   {0.9999999999999964,2.0,0.9999999999999929},
   {0.9999999999999982,2.0,0.9999999999999964},
   {0.9999999999999991,2.0,0.9999999999999982},
   {0.9999999999999996,2.0,0.9999999999999991},
   {0.9999999999999998,2.0,0.9999999999999996},
   {0.9999999999999999,2.0,0.9999999999999998},
   {1.0,2.0,1.0},
};
/** function pow_uv__uv_normal_roots_of_two executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_roots_of_two() {
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
		x=pow_uv__uv_normal_roots_of_two_io_table[ind].pow_x;
		y=pow_uv__uv_normal_roots_of_two_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_roots_of_two_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_roots_of_two: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_roots_of_two_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==54) {
		PRINTF("pow_uv__uv_normal_roots_of_two: successfully tested: 54 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_roots_of_two: %d tests failed for pow (out of 54)\n",errors);
	}
#endif
	return errors;
}

/**
 * 53 approximation tests with factors 2.0,2.0
 * range: (0,1) to (0,10)
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_roots_to_ten in the following table */
pow_uv__uv_normal_roots_to_ten_io_table_type pow_uv__uv_normal_roots_to_ten_io_table [53] = {
   {0.0,5.5,0.0},
   {0.0,7.75,0.0},
   {0.0,8.875,0.0},
   {0.0,9.4375,0.0},
   {0.0,9.71875,0.0},
   {0.0,9.859375,0.0},
   {0.0,9.9296875,0.0},
   {0.0,9.96484375,0.0},
   {0.0,9.982421875,0.0},
   {0.0,9.9912109375,0.0},
   {0.0,9.99560546875,0.0},
   {0.0,9.997802734375,0.0},
   {0.0,9.9989013671875,0.0},
   {0.0,9.99945068359375,0.0},
   {0.0,9.999725341796875,0.0},
   {0.0,9.999862670898438,0.0},
   {0.0,9.999931335449219,0.0},
   {0.0,9.99996566772461,0.0},
   {0.0,9.999982833862305,0.0},
   {0.0,9.999991416931152,0.0},
   {0.0,9.999995708465576,0.0},
   {0.0,9.999997854232788,0.0},
   {0.0,9.999998927116394,0.0},
   {0.0,9.999999463558197,0.0},
   {0.0,9.999999731779099,0.0},
   {0.0,9.99999986588955,0.0},
   {0.0,9.999999932944775,0.0},
   {0.0,9.999999966472387,0.0},
   {0.0,9.999999983236194,0.0},
   {0.0,9.999999991618097,0.0},
   {0.0,9.999999995809048,0.0},
   {0.0,9.999999997904524,0.0},
   {0.0,9.999999998952262,0.0},
   {0.0,9.999999999476131,0.0},
   {0.0,9.999999999738066,0.0},
   {0.0,9.999999999869033,0.0},
   {0.0,9.999999999934516,0.0},
   {0.0,9.999999999967258,0.0},
   {0.0,9.999999999983629,0.0},
   {0.0,9.999999999991815,0.0},
   {0.0,9.999999999995907,0.0},
   {0.0,9.999999999997954,0.0},
   {0.0,9.999999999998977,0.0},
   {0.0,9.999999999999488,0.0},
   {0.0,9.999999999999744,0.0},
   {0.0,9.999999999999872,0.0},
   {0.0,9.999999999999936,0.0},
   {0.0,9.999999999999968,0.0},
   {0.0,9.999999999999984,0.0},
   {0.0,9.999999999999993,0.0},
   {0.0,9.999999999999996,0.0},
   {0.0,9.999999999999998,0.0},
   {0.0,10.0,0.0},
};
/** function pow_uv__uv_normal_roots_to_ten executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_roots_to_ten() {
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
	for (ind=0;ind<53;ind++) {
		x=pow_uv__uv_normal_roots_to_ten_io_table[ind].pow_x;
		y=pow_uv__uv_normal_roots_to_ten_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_roots_to_ten_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_roots_to_ten: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_roots_to_ten_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==53) {
		PRINTF("pow_uv__uv_normal_roots_to_ten: successfully tested: 53 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_roots_to_ten: %d tests failed for pow (out of 53)\n",errors);
	}
#endif
	return errors;
}

/**
 * 21 specified tests for pow
 * (2,0); (2,1); (2,2); (2,3); (2,4); (2,5); (2,6); (2,7); (2,8); (2,9); (2,10); (2,11) ...
 * returns the number of failing tests
 */
/* store data of pow_uv__uv_normal_two_powers in the following table */
pow_uv__uv_normal_two_powers_io_table_type pow_uv__uv_normal_two_powers_io_table [21] = {
   {2.0,0,1.0},
   {2.0,1.0,2.0},
   {2.0,2.0,4.0},
   {2.0,3.0,8.0},
   {2.0,4.0,16.0},
   {2.0,5.0,32.0},
   {2.0,6.0,64.0},
   {2.0,7.0,128.0},
   {2.0,8.0,256.0},
   {2.0,9.0,512.0},
   {2.0,10.0,1024.0},
   {2.0,11.0,2048.0},
   {2.0,12.0,4096.0},
   {2.0,13.0,8192.0},
   {2.0,14.0,16384.0},
   {2.0,15.0,32768.0},
   {2.0,16.0,65536.0},
   {2.0,17.0,131072.0},
   {2.0,18.0,262144.0},
   {2.0,19.0,524288.0},
   {2.0,20.0,1048576.0},
};
/** function pow_uv__uv_normal_two_powers executes the tests and returns the number of failing tests */
int pow_uv__uv_normal_two_powers() {
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
	for (ind=0;ind<21;ind++) {
		x=pow_uv__uv_normal_two_powers_io_table[ind].pow_x;
		y=pow_uv__uv_normal_two_powers_io_table[ind].pow_y;
		res=pow(x,y);
		if (test_compare_double(res,pow_uv__uv_normal_two_powers_io_table[ind].pow_out,COMPARE_TYPE_ABS_REL_TOLERANCE,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL,10.0,&max_dif_below_pow_uv__uv_normal,&max_dif_above_pow_uv__uv_normal)!=TEST_TRUE) {
			errors++;
#if defined(DEBUG)
			PRINTF("pow_uv__uv_normal_two_powers: test %d (IO_VV) failed for pow(%.17g,%.17g): Expected %.17g, found %.17g\n",ind+1,x,y,pow_uv__uv_normal_two_powers_io_table[ind].pow_out,res);
#endif
		} else {
			passes++;
		}
	}
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0 && passes==21) {
		PRINTF("pow_uv__uv_normal_two_powers: successfully tested: 21 cases for pow\n");
	} else {
		PRINTF("pow_uv__uv_normal_two_powers: %d tests failed for pow (out of 21)\n",errors);
	}
#endif
	return errors;
}

/**
 * main function calls all tests (14 functions) of pow_uv__uv_normal
*/
int pow_uv__uv_normal_main_test() {
	int errors=0;
	int index=0;
	errors+=pow_uv__uv_normal_normal_values();        /* 1. 3 tests */
	errors+=pow_uv__uv_normal_pow_normal_range();     /* 2. 49 tests */
	errors+=pow_uv__uv_normal_pow_normal_range_rnd(); /* 3. 50 tests */
	errors+=pow_uv__uv_normal_random_squares();       /* 4. 50 tests */
	errors+=pow_uv__uv_normal_negative_squares();     /* 5. 50 tests */
	errors+=pow_uv__uv_normal_negative_pow();         /* 6. 64 tests */
	errors+=pow_uv__uv_normal_random_squareroots();   /* 7. 50 tests */
	errors+=pow_uv__uv_normal_random_exponent();      /* 8. 50 tests */
	errors+=pow_uv__uv_normal_random_numbers();       /* 9. 50 tests */
	errors+=pow_uv__uv_normal_linear_squares();       /* 10. 14 tests */
	errors+=pow_uv__uv_normal_linear_powers();        /* 11. 49 tests */
	errors+=pow_uv__uv_normal_roots_of_two();         /* 12. 54 tests */
	errors+=pow_uv__uv_normal_roots_to_ten();         /* 13. 53 tests */
	errors+=pow_uv__uv_normal_two_powers();           /* 14. 21 tests */
#if defined(DEBUG) || defined(SUMMARY)
	if (errors==0) {
		PRINTF("SUMMARY of pow_uv__uv_normal: successfully tested ALL 607 cases for pow\n");
	} else {
		PRINTF("SUMMARY of pow_uv__uv_normal: %d tests failed in pow_uv__uv_normal (out of 607 in pow_uv__uv_normal)\n",errors);
	}

#if defined(USE_ABS_TOL) && !defined(NO_ABS_TOL)
    if (pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ABS deviation of %.17g. ABS_TOLERANCE_POW_UV__UV_NORMAL=%.17g\n",pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value,ABS_TOLERANCE_POW_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_REL_TOL) && !defined(NO_REL_TOL)
    if (pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal REL deviation of %.17g. REL_TOLERANCE_POW_UV__UV_NORMAL=%.17g\n",pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value,REL_TOLERANCE_POW_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_ABS_REL_TOL) && !defined(NO_ABS_REL_TOL)
    if (pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ABS_REL deviation of %.17g. ABS_REL_TOLERANCE_POW_UV__UV_NORMAL=%.17g\n",pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value,ABS_REL_TOLERANCE_POW_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_ULP_TOL) && !defined(NO_ULP_TOL)
    if (pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal ULP deviation of %.17g. ULP_TOLERANCE_POW_UV__UV_NORMAL=%.17g\n",pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value,ULP_TOLERANCE_POW_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_EXAKT_TOL) && !defined(NO_EXAKT_TOL)
    if (pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal EXAKT deviation of %.17g. EXAKT_TOLERANCE_POW_UV__UV_NORMAL=%.17g\n",pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value,EXAKT_TOLERANCE_POW_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_EQUAL_TOL) && !defined(NO_EQUAL_TOL)
    if (pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal EQUAL deviation of %.17g. EQUAL_TOLERANCE_POW_UV__UV_NORMAL=%.17g\n",pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value,EQUAL_TOLERANCE_POW_UV__UV_NORMAL);
	}
	index++;
#endif

#if defined(USE_BINHEX_TOL) && !defined(NO_BINHEX_TOL)
    if (pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value>0) {
		PRINTF("found maximal BINHEX deviation of %.17g. BINHEX_TOLERANCE_POW_UV__UV_NORMAL=%.17g\n",pow_uv__uv_normal_all_deviation_results_double[index].max_diff_value,BINHEX_TOLERANCE_POW_UV__UV_NORMAL);
	}
	index++;
#endif

	/* print famous last words */
	PRINTF("END_OF_TEST_EXECUTION_REACHED\n");
#endif /* defined(DEBUG) || defined(SUMMARY) */
	return errors;
}

/**
 * main function, just calls pow_uv__uv_normal_main_test
*/
#ifndef NO_MAIN
int main() {
	int result = pow_uv__uv_normal_main_test();
	return result;
}
#endif /* NO_MAIN */

