#include <stdio.h>

int g(int x)
{
	if(x>5) return(2*x);
	// 

	// [ [x>-8], [x<=-8] ] <=>
	// [ [x+10>2], [x+10<=2] ]
	return(x+10);
}

int f(int x)
{
	// [ [], [] ] <=>
	// [ [g(x)>2], [g(x)<=2] ]
	int z = 2*g(x);

	// [ [z>5], [z<=5] ]
	int y = 2*z;

	// [ [y>10], [y<=10] ]
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

int main()
{
	int x = 3;
	printf("f(%li)=%li\n",x,f(x));
}
