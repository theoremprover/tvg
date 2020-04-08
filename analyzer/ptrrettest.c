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

STRUCT r;

STRUCT* f(STRUCT* x,STRUCT* y)
{
    if(x->member1 == y->member1)
        r.member1 = EINS;
    else
        r.member1 = ZWEI;

    r.member2 = x->member2 + y->member2;

    return &r;
/*
    f_ret == &r
    (&r)->member1 ~> r.member1 ~> r_DOT_member1
    COND r_DOT_member1 == f_ret->member1
    COND r_DOT_member2 == f_ret->member2
    COND ADR_r == f_ret
*/
}

int g(int y, int z)
{
    STRUCT s0;
    s0.member1 = EINS;
    s0.member2 = 2;
    STRUCT s1 = { y, z };
    STRUCT * rer = f(&s0,&s1);

    if (rer->member1 == EINS) return 1;
    if (rer->member1 == ZWEI) return 2;
    return rer->member2;
}

int main(int argc, char* argv[])
{
#ifdef CALC
    int z = atoi(argv[1]);

    printf("g(z=%i) = %i\n",z,g(z));
#endif
    return 0;
}
