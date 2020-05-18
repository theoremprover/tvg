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

STRUCT* g(STRUCT *p,int x)
{
    if(p->member1 == EINS)
    {
        p->member1 = ZWEI;
    }

    if(p->member2 > x)
    {
        p->member2 = p->member2 + 1;
    }

    return(p);
}

int f(STRUCT *q,int y)
{
    STRUCT *r;
    r = f(q,y);
    if(r->member1==EINS) return 0;
    if(r->member2>0) return 1;
    return 2;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int m1 = atoi(argv[1]);
    int m2 = atoi(argv[2]);
    int y = atoi(argv[3]);
    STRUCT p = { m1, m2 };

    printf("<fun>(p_m1=%i, p_m2=%i, y=%i) =\n%i\n",m1,m2,y,f(&p,y));
    return 0;
}
#endif
