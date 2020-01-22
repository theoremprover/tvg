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

    if(p->member2 > 0)
    {
        p->member2 = p->member2 + x;
    }

    return(p);
}

int main()
{
    STRUCT p[2] = { { EINS,5 }, { EINS,6 } };
    f(p,3);
    return 0;
}
