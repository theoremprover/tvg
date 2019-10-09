#include <stdio.h>
#include <stdlib.h>

#ifdef TRACE
#define T(x) printf("%i\n",x);
#else
#define T(X)
#endif

int g(int z)
{
	if(z>5)
	{
	    T(1);
		return(2*z);
    }

	else
	{
	    T(2);
		z = z + 1;
	}

	return(z+10);
}

int f(int x)
{
	int z1 = 2*g(x+1);

	int y = 3*z1;

	if(y>10)
	{
		T(3);
		return(g(y));
	}
	else
	{
        T(4);
		if(z1>5)
		{
            T(5);
			x = x + 2;
			y = g(x);
		}
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

1)
x>4
x>10/12-1

*/