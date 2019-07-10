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

/*
["return 2 * x","x > 5"]
["x > 5"]
"------"
["return x + 10","x := x + 1","!(x > 5)"]
["!(x > 5)"]
*/

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

/*
["return g(y)","y > 10","y := 3 * z","z := 2 * g(x+1)"]
["3 * (2 * g(x+1)) > 10"]

<=> 3*2*g(x+1) > 10
<=> 3*2* > 10, 

"------"
["return y + 1","y := g(x)","x := x + 2","z > 5","!(y > 10)","y := 3 * z","z := 2 * g(x)"]
["!(3 * (2 * g(x+1)) > 10)","2 * g(x) > 5"]
"------"
["return y + 1","!(z > 5)","!(y > 10)","y := 3 * z","z := 2 * g(x)"]
["!(3 * (2 * g(x+1)) > 10)","!(2 * g(x) > 5)"]
*/


int main()
{
	int x;
	x= 6; printf("f(%li)=%li\n",x,f(x));
	x=-2; printf("f(%li)=%li\n",x,f(x));
	x=-9; printf("f(%li)=%li\n",x,f(x));
}
