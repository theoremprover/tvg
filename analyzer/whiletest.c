#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

int solver_pragma(int x,...)
{
    return 1;
}
void solver_debug(void* x)
{
}

int g(int x,int y)
{
    int erg = x;
    while(y>0)
    {
        solver_debug(y);
        erg = erg << 1;
        y=y-1;
    }
    if(erg<=1) { erg=100; }
    return(erg);
}

int f(int y)
{
    if(g(2,y)>5) return 1;
    else return 0;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int y = atoi(argv[1]);
    printf("f(%i) =\n%i\n",y,f(y));
    return 0;
}
#endif
