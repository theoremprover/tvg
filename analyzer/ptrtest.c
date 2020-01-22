enum Zahl { EINS, ZWEI };

typedef enum Zahl MYENUM;

typedef struct
{
    MYENUM member1;
    int member2;
} STRUCT;

STRUCT* f(STRUCT *p,int x)
{
    if(p->member2 > 0)
    {
        p->member2 = p->member2 + 1;
    }

    p = p + 1;
    p->member2 = ZWEI;
    p->member2 = (p-1)->member2 + 2;

    return(p);
}

int main()
{
    STRUCT p[2] = { { EINS,5 }, { EINS,6 } };
    f(p,3);
    return 0;
}
