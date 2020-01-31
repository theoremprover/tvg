#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

int g(int x,int y)
{
    int erg = x;
    while(y>0)
    {
        erg *= 2; //erg = erg << 1;
        y--;
    }
    return(erg);
}

int f(int y)
{
    if(g(2,y)>5) return 1;
    else return 0;
}

void main(int argc, char* argv[])
{
#ifdef CALC
    int y = atoi(argv[1]);
    printf("f(%i) = %i\n",y,f(y));
#endif
}
