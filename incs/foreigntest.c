#include <stdio.h>
#include <stdlib.h>

#include "CovStats_stub.h"
#include "data.h"

COUNTER counter = { 280,1,0 };

int main(int argc, char *argv[])
{
	char **pargv = argv;

	hs_init(&argc,&pargv);

	int i = show_stats(&counter);
	printf("show_stats returned %i\n",i);

	hs_exit();

	return 0;
}

