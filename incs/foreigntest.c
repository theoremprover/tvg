#include <stdio.h>
#include <stdlib.h>

#include "CovStats_stub.h"

int main(void)
{
	int argc = 0;
	char *argv[] = { NULL };
	char **pargv = argv;

	hs_init(&argc,&pargv);

	char c = show_stats(7);
	printf("show_stats returned %c\n",c);

	hs_exit();
	return 0;
}

