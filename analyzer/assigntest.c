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

    if(y->member1 == EINS)
        y->member1 = ZWEI;

    return(y->member2);
}

int g(int z)
{
    STRUCT s0 = { EINS,10 };
    STRUCT s1 = { ZWEI,20 };
    if(f(&s0,&s1)>1) return 1;
    else return 0;
}

int main(int argc, char* argv[])
{
#ifdef CALC
    int z = atoi(argv[1]);

    printf("g(z=%i) = %i\n",z,g(z));
#endif
    return 0;
}
