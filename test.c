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
//    if((x & 1) == 1)
    if(((~x) & 1) == 1)
    {
        x = x + 2;
    }

	return(x+1);
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