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
*/

int h(STRUCT r)
{
    return (r.member2 + 1);
}

int f(STRUCT q,int a)
{
    STRUCT q1;
    q1.member1 = ZWEI;
    q1.member2 = q.member2 + 1;
    if(h(q1)==1)
    {
        return q.member1;
    }
    return 2;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int m0 = atoi(argv[1]);
    int m1 = atoi(argv[2]);
    int m2 = atoi(argv[3]);
    int y = atoi(argv[4]);
    STRUCT p = { m1, m2 };

    printf("<fun>({p_m1=%i, p_m2=%i}, y=%i) =\n%i\n",m1,m2,y,f(p,y));
    return 0;
}
#endif
