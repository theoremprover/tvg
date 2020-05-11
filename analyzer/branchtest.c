#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

int g(int x)
{
    if(x>=0) return x;
    else return (-x);
}

int f(int x,int y)
{
    int z = g(x);
    if(z>=0)
    {
        return 1;
    }
    else
    {
        return -1;
    }
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int x = atoi(argv[1]);
    int y = atoi(argv[2]);
    printf("f(x=%i,z=%i) =\n%i\n",x,y,f(x,y));
    return 0;
}
#endif
