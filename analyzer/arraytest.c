int f(int arr[3],int n)
{
    int j=0;
    for(int i=0;i<(n%3);i++)
    {
        if(arr[i]>0) arr[i] *= 2;
        j += arr[i];
    }
    switch(j)
    {
        case -1 : return(1); break;
        case  0 : return(2); break;
        case  8 : return(3); break;
        default : j++;
    }
    return(j);
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
    return a[1];
}

f( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2 )
{
    ASSN a[1] = a[1] + 1
    RET a[1]
}

ELIM_ARRAY_ASSIGNMENTS

f( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2 )
{
    ASSN a1[1] = a[1] + 1
    ASSN a1 = a
    RET a[1]
}

ELIM_ASSIGNMENTS

f( int a_INDEX_0, int a_INDEX_1, int a_INDEX_2 )
{
    ASSN a1[1] = a[1] + 1
    ASSN a1 = a
    RET a[1]
}

*/