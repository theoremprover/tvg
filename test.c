#include <stdio.h>
#include <stdlib.h>

int g(int z)
{
	if(z>5)
	{
		return(2*z);
    }

	else
	{
		z = z + 1;
	}

	return(z+10);
}

int f(int x)
{
	int z1 = 2*g(x+1);

	int y = 3*z1;

    while ( x < g(6) )
    {
        x = x + 2;
        y = y + g(x);
    }

	return(y+1);
}

int main(int argc,char* argv[])
{
    int x = atoi(argv[1]);
    int ret = f(x);
    printf("f(%i)=%i\n",x,ret);
    return 0;
}

/*

*/