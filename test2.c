#include <stdlib.h>
#include <stdio.h>

int x = 1;

#ifdef ALT
int g(int x)
{
	int y = x+1;
	return y;
}
#endif

int f(int a,int b)
{
	int erg = a+b;

	if(erg==2) exit(b+1);
	return( { erg++; erg; } );
}

int main(int argc, char* argv[])
{
	if(argc>1) return 99;

	switch(argc)
	{
		case 1: f(2,3); argc=3; break;
		case 2: argc++; break;
		default: f(5,6);
	}

	printf("OK\n");
	return f(1,2);
}
