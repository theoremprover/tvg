#include <stdio.h>
#include <stdlib.h>

struct a {
    int b;
    double c;
};

int x = 7;

int g(int z)
{
    struct a ainst;
    ainst.b;
    struct a * ptr = & ainst;
    ptr -> c;

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

int f(int x,int y)
{
    x += y;
    if((x<<1) == (x<<2))
    {
        x += 2;
    }

	return(x+1);
}

int main(int argc,char* argv[])
{
    int x = atoi(argv[1]);
    int ret = g(x);
    printf("f(%i)=%i\n",x,ret);
    return 0;
}
