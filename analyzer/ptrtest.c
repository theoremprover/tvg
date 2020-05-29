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

/*
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
    if(q->member1==EINS)
        q->member1 = ZWEI;
    if (q->member2 < y)
    {
        q->member2++;
    }
    return 1;
}
*/

/*
int g(enum Zahl z)
{
    if(z==EINS) return 1;
    else return 2;
}

int h(int* r)
{
    return (*r);
}
*/

int f(STRUCT* p)
{
    STRUCT r = { EINS,10 };

    int* q;

    q = &(r.member2);
    *q = 2;

    if(r.member2 == p->member2)
    {
        return 0;
    }
    return 1;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int arg1 = atoi(argv[1]);
    int arg2 = atoi(argv[2]);
    STRUCT q = { arg1,arg2 };
    printf("f(p={%i,%i}) =\n%i\n",arg1,arg2,f(&q));
    return 0;
}
#endif
