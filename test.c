#include <stdio.h>

int g(int z)
{
	if(z>5)
		return(2*z);

	else
	{
		z = z + 1;
	}

	return(z+10);
}

int f(int x)
{
	int z = 2*g(x+1);

	int y = 3*z;

	if(y>10)
	{
		return(g(y));
	}
	else
	{
		if(z>5)
		{
			x = x + 2;
			y = g(x);
		}
	}

	return(y+1);
}

int main()
{
	int x;
	x= 6; printf("f(%li)=%li\n",x,f(x));
	x=-2; printf("f(%li)=%li\n",x,f(x));
	x=-9; printf("f(%li)=%li\n",x,f(x));
}
