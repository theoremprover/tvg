#include <stdio.h>

int g(int x)
{
	if(x>5)
		return(2*x);

		else
		{
			x = x + 1;
		}

	return(x+10);
}

int f(int x)
{
	int z = 2*g(x);

	int y = 3*z;

	if(y>10)
	{
		return(g(y));
	}
	else
	{
		y = y+2;
	}

	return(y+1);
}

int main()
{
	int x;
	// [ x>5, x <- [-9,..,4], FALSE, x<=-9 ]
	// This should give full line coverage of f:
	x= 6; printf("f(%li)=%li\n",x,f(x));
	x=-2; printf("f(%li)=%li\n",x,f(x));
	x=-9; printf("f(%li)=%li\n",x,f(x));
}
