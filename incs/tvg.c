/*
gcc -w -DFORBUILD -shared -fPIC tvg.c -o libtvg.so

*/

#include <stdio.h>
#include <stdlib.h>

FILE* tracefile;
char buf[10];

void mytrace(char *text)
{
#ifndef FORBUILD
//	printf(text);
	if(tracefile==NULL) { printf("tracefile==NULL!\nPress [RETURN]\n"); gets(buf); }
	fprintf(tracefile,text);
#endif
}

void opentrace(char* filename)
{
#ifndef FORBUILD
	tracefile = fopen(filename,"w");
	if(tracefile==NULL) { printf("tracefile==NULL!\nPress [RETURN]\n"); gets(buf); }
	fprintf(tracefile,"");
	fclose(tracefile);

	tracefile = fopen(filename,"a");
	if(tracefile==NULL) { printf("tracefile==NULL!\nPress [RETURN]\n"); gets(buf); }
#endif
}

void closetrace()
{
#ifndef FORBUILD
	fclose(tracefile);
#endif
}
