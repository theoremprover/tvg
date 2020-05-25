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

int f(int* p)
{
    int a = 0;
    int* q;

    q = &a;
    *q = 2;

    if(a==*q)
    {
        return 0;
    }
    return 1;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int m0 = atoi(argv[1]);
    int m1 = atoi(argv[2]);
    int p[2] = { m0,m1 };
    printf("f(p={%i,%i}) =\n%i\n",m0,m1,f(p));
    return 0;
}
#endif
