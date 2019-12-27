#include <stdio.h>
#include <stdlib.h>

int x = 7;

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
    int z;
    if(x>y)
    {
        z = g(x)+g(y);
    }

	return(z+1);
}

int main(int argc,char* argv[])
{
    int x = atoi(argv[1]);
    int y = atoi(argv[2]);
    int ret = f(x,y);
    printf("f(%i,%i)=%i\n",x,y,ret);
    return 0;
}
