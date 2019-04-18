#include <stdio.h>

int g(int x)
{
	// [ x>5, x <- [-9,..,4], FALSE, x<=-9  ] 
	if(x>5)
		// [ x>5 , FALSE ] <=>
		// [ x>5 & x>1, x>5 & x<=2 ] <=>
		// [ x>5 & 2*x>2, x>5 & 2*x<=2 ]
		return(2*x);

		else
		{
			// [ x <- [-9,..,4] , x<=-9 ] <=>
			x = x + 1;
			// [ x <- [-8,..,5] , x<=-8 ] <=>
			// [ x<=5 & x>=-8 , x<=5 & x<=-8 ]
		}

	// [ x>-8, x<=-8 ] <=>
	// [ x+10>2, x+10<=2 ]
	return(x+10);
	// g(X) > 2
}

int f(int x)
{
	// [ x>5, x <- [-9,..,4], FALSE, x<=-9 ] <=>
	// [ g(x)>2, g(x)<=2 ]
	int z = 2*g(x);

	// [ z>5, z<=5 ]
	int y = 2*z;

	// [ y>10, y<=10 ]
	if(y>10)
	{
		// [y>10]
		y = y - 1;
	}
	else
	{
		// [y<=10]
		y = y+1;
	}

	// []
	return(y+1);
}

int test2(int x)
{
	int y = x + 99;
	return y;
}

int test(int x)
{
	if (x>0)
	{
		return test2(x);
	}
	else
	{
		return test2(x+1);
	}
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
