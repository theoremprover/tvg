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
		double d=1.0E-200;
		while (d/2>0) {
			d=d/2;
		}
		return d;
	}
	if (strcmp(s,"-DENORM")==0 || strcmp(s,"-DEN")==0) {
		double d=-1.0E-200;
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
//
int main(int argc, char **argv) {
	char s[100];
	if (argc!=2) {
		printf("%s error: requires 2 arguments.\n",argv[0]);
		return -1;
	}
	double x=my_atof(argv[1]);
	// without that gcov does not work (!!)
	sprintf(s,"sin(%.17g / %s)=%.17g\n",x,argv[1],sin(x));
	printf(s);
	// test also cos to increase coverage of sin
	sprintf(s,"cos(%.17g / %s)=%.17g\n",x,argv[1],cos(x));
	printf(s);
	return 0;
}
//int main(int argc, char **argv)
//{
//	sin(0.0f);
//	return 0;
//}

