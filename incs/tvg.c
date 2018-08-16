/*
gcc -w -DFORBUILD -shared -fPIC tvg.c -o libtvg.so

*/

#include <stdio.h>
#include <stdlib.h>

FILE* tracefile;

void mytrace(char *text)
{
#ifndef FORBUILD
	fprintf(tracefile,text);
#endif
}

void opentrace(char* filename)
{
#ifndef FORBUILD
	tracefile = fopen(filename,"w");
	if(tracefile==NULL) { printf("tracefile==NULL!\n"); exit(1); }
	fprintf(tracefile,"");
	fclose(tracefile);

	tracefile = fopen(filename,"a");
	if(tracefile==NULL) { printf("tracefile==NULL!\n"); exit(2); }
#endif
}

void closetrace()
{
#ifndef FORBUILD
	fclose(tracefile);
#endif
}
