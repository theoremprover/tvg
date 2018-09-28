#include <data.h>

#include <data.h>

#include <data.h>

#include <data.h>

#include <stdlib.h>

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
	return( { x++; erg; } );
}

int main(int argc, char* argv[])
{
	if(argc>1) return 99;

	return f(1,2);
}
