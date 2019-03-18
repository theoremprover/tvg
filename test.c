#include <stdio.h>

int f(int x)
{
	int y = 2*x;
	return(y+1);
}

int main()
{
	int x = 3;
	printf("f(%li)=%li\n",x,f(x));
}
