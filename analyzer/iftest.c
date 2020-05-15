#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

enum Zahl { EINS, ZWEI };

int side = -1;

int h(int x)
{
    if(x>1) return 0;
    return 1;
}

int g(int x)
{
    {
        if(x>1)
            return 2;
        else
        {
            side++;
            return 1;
        }
    }
}

int f(int x, enum Zahl z)
{
    if(x>0)
    {
        if(z==ZWEI)
            return 2;
        else
        {
            int y = z+g(x); //+2*h(x+1);
            if(y+side>1)
            {
                x = x - 1;
                y = y * 2;
            }
            return y;
        }
    }
    return -1;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int x = atoi(argv[1]);
    int z = atoi(argv[2]);
    printf("f(x=%i,z=%i) =\n%i\n",x,z,f(x,z));
    return 0;
}
#endif
