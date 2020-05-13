#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

int g(int x)
{
    if(x>=0)
    {
        return x;
    }
    else
    {
        if(x>10)
            // Dead in g, but not in f => should not be reported!
            return (-x+1);
        else
            return (-x);
    }
}

int f(int x,int y)
{
    int z = y+g(x);
    if(z>=0)
    {
        return 1;
    }
    else
    {
        if(z>10)
        {
            // DEAD!
            z++;
            return -10;
        }
        else
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
