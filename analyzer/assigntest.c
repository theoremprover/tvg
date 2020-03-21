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

int f(STRUCT* x)
{
    if(x->member2 == 0)
    {
        (x->member2)++;
    }

    (x+1)->member2 *= 2;

    if((x+1)->member1 == EINS)
        (x+1)->member1 = ZWEI;

    return((x+1)->member2);
}

int g(int z)
{
    STRUCT s = { EINS,10 };
    if(f(&s)>1) return 1;
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
