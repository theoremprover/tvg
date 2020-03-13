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

STRUCT* f(STRUCT *p,int x)
{
    if(p->member1 == EINS)
    {
        p->member1 = ZWEI;
    }

    p = p+1;

    if(p->member2 > x)
    {
        p->member2 = p->member2 + 1;
    }

    return(p);
}

int main(int argc, char* argv[])
{
#ifdef CALC
    int m1 = atoi(argv[1]);
    int m2 = atoi(argv[2]);
    int y = atoi(argv[3]);
    STRUCT p = { m1, m2 };

    printf("<fun>(p_m1=%i, p_m2=%i, y=%i) = %i\n",m1,m2,y,f(&p,y));
#endif
    return 0;
}
