int g(int y)
{
    return ( (y==0) ? 1 : ((y==1) ? 2 : 3) );
}

int f2(int x)
{
    if((g(x)==2) ? 0 : 1 ) return 0;
    return 1;
}

int f(int x)
{
    int r = (x==2) ? 0 : 1 ;
    return r + f2(x);
}
