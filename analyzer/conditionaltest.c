int g(int y)
{
    return ( (y==0) ? 1 : ((y==1) ? 2 : 3) );
}

int f(int x)
{
    return ( (x==0) ? 1 : ((x==1) ? 2 : 3) );
//    if((g(x)==2) ? 0 : 1 ) return 0;
//    return 1;
}
