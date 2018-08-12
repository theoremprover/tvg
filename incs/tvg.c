#include <stdio.h>
#include <stdlib.h>

FILE* tracefile;

void mytrace(char *text)
{
	fprintf(tracefile,text);
}

void opentrace(char* filename)
{
	tracefile = fopen(filename,"w");
	if(tracefile==NULL) { printf("tracefile==NULL!\n"); exit(1); }
	fprintf(tracefile,"");
	fclose(tracefile);

	tracefile = fopen(filename,"a");
	if(tracefile==NULL) { printf("tracefile==NULL!\n"); exit(2); }
}

void closetrace()
{
	fclose(tracefile);
}
