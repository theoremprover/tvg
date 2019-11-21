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
    int ret = f(x,2);
    printf("f(%i)=%i\n",x,ret);
    return 0;
}

/*

*/