#include "defs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


// for testing 
typedef union
 {
 unsigned long long hex;
 double val;
 } _HV;

double my_atof(char *s) {
	if (strcmp(s,"NAN")==0) {
		return NAN;
	}
	if (strcmp(s,"DENORM")==0 || strcmp(s,"DEN")==0) {
		double d=1.0E-300;
		while (d/2>0) {
			d=d/2;
		}
		return d;
	}
	if (strcmp(s,"-DENORM")==0 || strcmp(s,"-DEN")==0) {
		double d=-1.0E-300;
		while (d/2<0) {
			d=d/2;
		}
		return d;
	}
	if (strcmp(s,"INF")==0 || strcmp(s,"INFINITY")==0) {
		return ((double)INFINITY);
	}
	if (strcmp(s,"-INF")==0 || strcmp(s,"-INFINITY")==0) {
		return -((double)INFINITY);
	}
	if (strncmp(s,"0x",2)==0) {
		_HV hv;
		hv.hex=atoll(&(s[2]));
		// printf("my_atof(%s): hex %x (%17.9g)\n",s,hv.hex,hv.val);
		return hv.val;
	}
	return atof(s);
}

/**
 * this file calls all test cases for expf
*/

/** some extern declarations */

	extern int expf_uv__uv_trivial_main_test();
	extern int expf_uv__uv_normal_main_test();
	extern int expf_uv__uv_extreme_main_test();
	extern int expf_uv__uv_terms_main_test();

	extern int powf_uv__uv_trivial_main_test();
	extern int powf_uv__uv_normal_main_test();
	extern int powf_uv__uv_extreme_main_test();
	extern int powf_uv__uv_terms_main_test();

unsigned long toRep(float x) {
    const union { float f; unsigned long i; } rep = {.f = x};
    return rep.i;
}

/**
 * main function calls all tests (4 test files) of expf
*/
int main() {
	int errors=0;
	printf("main is called\n");
	/* 
	errors+=expf_uv__uv_trivial_main_test();
	errors+=expf_uv__uv_normal_main_test();
	errors+=expf_uv__uv_extreme_main_test();
	errors+=expf_uv__uv_terms_main_test();
	*/
	errors+=powf_uv__uv_trivial_main_test();
	errors+=powf_uv__uv_normal_main_test();
	errors+=powf_uv__uv_extreme_main_test();
	errors+=powf_uv__uv_terms_main_test();

	printf("================ manually generated test cases ================\n");
	/*
	expf(16.0f);
	printf("==================================================\n");
	expf(5.87747175e-39);
	printf("==================================================\n");
	float x = _FInf._Float;
	_FExp(&x,1.0f,0);
	printf("==================================================\n");
	x = 16.0f;
	_FExp(&x,_FInf._Float,0);
	printf("==================================================\n");
	x = _FNan._Float;
	_FExp(&x,1.0f,0);
	printf("==================================================\n");
	x = 1.0f;
	_FExp(&x,_FNan._Float,0);
	printf("==================================================\n");
	x = 0.0f;
	_FExp(&x,0.0f,0);
	printf("==================================================\n");
	x = _FInf._Float;
	_FExp(&x,0.0f,0);
	printf("==================================================\n");
	*/
	powf(2.0f,3.0f);
	printf("==================================================\n");
	powf(50.0f,3.0f);
	return errors;

}

