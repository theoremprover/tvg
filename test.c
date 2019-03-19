#include <stdio.h>

int g(int x)
{
	if(x>5) return(2*x);
	return(x+10);
}

int f(int x)
{
	int y = 2*x;
	if(y>10)
	{
		y = y - 1;
	}
	else
	{
		y = 2*g(y);
	}
	return(y+1);
}

int main()
{
	int x = 3;
	printf("f(%li)=%li\n",x,f(x));
}
