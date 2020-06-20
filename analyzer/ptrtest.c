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
/*
STRUCT* p  =>  p :: STRUCT*, p_ARROW_member1 :: MyEnum, p_ARROW_member2 :: int
STRUCT a   =>  a :: STRUCT,  a_DOT_member_1 :: MyEnum,  a_DOT_member2 :: int
int* p     =>  p :: int*,    IND_p :: int
RSTRUCT a  =>  a :: RSTRUCT, .., a_DOT_member3 :: STRUCT,
                                 a_DOT_member3_DOT_member1 :: MyEnum,
                                 a_DOT_member3_DOT_member2 :: Int
RSTRUCT* p =>
*/

STRUCT* f(STRUCT* p,STRUCT r)
{
    int* q;

    q = &(r.member2);
    *q = 2;

    if(r.member2 == p->member2)
    {
        return p;
    }
    return p;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int i = 1;
    int arg1 = atoi(argv[i++]);
    int arg2 = atoi(argv[i++]);
    int arg3 = atoi(argv[i++]);
    int arg4 = atoi(argv[i++]);
    int arg5 = atoi(argv[i++]);
    int arg6 = atoi(argv[i++]);
    STRUCT px = { arg2,arg3 };
    STRUCT qx = { arg5,arg6 };
    STRUCT* res = f(&px,qx);
    printf("f(p=%i, p={%i,%i}, q=%i, q={%i,%i}) =\n%i %i %i\n",arg1,arg2,arg3,arg4,arg5,arg6,res,res->member1,res->member2);
    return 0;
}
#endif
