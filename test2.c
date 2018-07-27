#include <stdio.h>

int abs(int x)
{
	if(x>0) return x;
	else return 0-x;
}

int g_rev(int x,int y)
{
	int e = y;
	if(x>1)
	{
		e = e * 2;
	}

	return e-1;
}

int f(int x,int y)
{
	int erg = 0;

	int f = 1;
	if(x<0)
	{
		f = -1;
	}

	for(int i=1;i<=abs(x);i++)
	{
		erg = erg + f*y;
	}

	return erg;
}

int main()
{
	printf("%i",f(-2,-3));
}

int h(int x)
{
	return x-1;
}

int h2(int x,int y)
{
	return x-2*y;
}
