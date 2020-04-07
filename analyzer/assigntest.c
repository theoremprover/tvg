#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

enum Zahl { EINS, ZWEI };

typedef enum Zahl MYENUM;

typedef struct
{
    MYENUM member1;
    int member2;
} STRUCT;

int f(STRUCT* x,STRUCT* y)
{
    if(x->member2 == 0)
    {
        (x->member2)++;
    }

    y->member2 *= 2;

    if(y->member2 > 3)
        y->member1 = ZWEI;

    return(y->member2);
}

int g(int z)
{
    STRUCT s0;
    s0.member1 = EINS;
    s0.member2 = 2;
    STRUCT s1 = { EINS, z };
    if(f(&s0,&s1)>1)
    {
        if (s1.member1 == EINS) return 1;
        else return 2;
    }
    else
    {
        if (s1.member1 == EINS) return (&s1)->member2;
        else return 4;
    }
}

int main(int argc, char* argv[])
{
#ifdef CALC
    int z = atoi(argv[1]);

    printf("g(z=%i) = %i\n",z,g(z));
#endif
    return 0;
}
