int g(int x)
{
    return 2*x;
}

int f(int x,int y)
{
    return ( ++x >= 0 ? (y++ || y>=0 ? x++ : g(g(x)) ) : (x++, y+x) );
}

