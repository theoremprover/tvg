/*
gcc -w -DFORBUILD -shared -fPIC tvg.c -o libtvg.so

*/

#include <stdio.h>
#include <stdlib.h>

FILE* tracefile;

void mytrace(char *text)
{
#ifndef FORBUILD
//	printf(text);
	fprintf(tracefile,text);
#endif
}

void opentrace(char* filename)
{
#ifndef FORBUILD
	tracefile = fopen(filename,"w");
	fprintf(tracefile,"");
	fclose(tracefile);

	tracefile = fopen(filename,"a");
#endif
}

void closetrace()
{
#ifndef FORBUILD
	fclose(tracefile);
#endif
}
