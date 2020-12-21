int f(int arr[3],int n)
{
    int j=0;
    for(int i=0;solver_pragma(0,1,2) && i<(n%3);i++)
    {
        if(arr[i]>0) arr[i] *= 2;
        j += arr[i];
    }
/*
    switch(j)
    {
        case -1 : return(1); break;
        case  0 : return(2); break;
        case  8 : return(3); break;
        default : j++;
    }
    return(j);
*/
    if(j==-1) return(1);
    else { if(j==0) return(2);
    else { if(j==8) return(3);
    else return(j+1); } }
}

/*
... f ( int a[3], ...)  =>  ... f ( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2, ... )

DECL a Array 3 Int Int      DECL a0 Array 3 Int Int
DECL a_INDEX_0 Int
DECL a_INDEX_1 Int
DECL a_INDEX_2 Int

ASSN a[0] = a_INDEX_0       COND a1 = store a0 0 a_INDEX_0
ASSN a[1] = a_INDEX_1       COND a2 = store a1 1 a_INDEX_1
ASSN a[2] = a_INDEX_2       COND a3 = store a2 2 a_INDEX_2

                            DECL a4 Array 10 Int Int
ASSN a[2] = 7         =>    COND a4 = store a3 2 7

COND ... a[2] ...           COND ...a1[2]...

                            DECL a5 Array 3 Int Int
ASSN a[2] = a[2]+1    =>    COND a5 = store a4 2 (select a1 2 + 1)

COND ... a[2] ...           COND ...a5[2]...




f( int a[3] )
{
    a[1] = a[1] + 1;
    a[1] = a[1] * 2;
    return a[1];
}

f( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2 )
{
    ASSN i = 1
    ASSN a[i] = a[i] + 1
    ASSN i = i+1
    ASSN a[i] = a[i-1] * 2
    RET a[i]
}

ELIM_ASSIGNMENTS

f( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2 )
{
    ASSN a[1] = a[1] + 1
    ASSN a[1+1] = a[1+1-1] * 2
    RET a[1+1]
}

ELIM_ARRAY_ASSIGNMENTS

f( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2 )
{
    DECL a$1
    ASSN a$1[1] = a[1] + 1
    ASSN a=a$1

    DECL a$2
    ASSN a$2[1+1] = a[1+1-1] * 2
    ASSN a=a$2

    RET a[1+1]
}

ELIM_ASSIGNMENTS 2

f( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2 )
{
    DECL a$1
    ASSN a$1[1] = a[1] + 1

    DECL a$2
    ASSN a$2[1+1] = a$1[1+1-1] * 2

    RET a$2[1+1]
}

During expr2sexpr:
    ASSN a$1[1] = a[1] + 1           =>   COND a$1 = store a 1 (select a 1 + 1)
    ASSN a$2[1+1] = a$1[1+1-1] * 2   =>   COND a$2 = store a$1 (1+1) ((select a$1 1+1-1) * 2)


*/