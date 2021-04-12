int g(int* p)
{
    return (*p--);
}

int f(int x,int* y)
{
    if(++x > 0 || ( g(y),*y>0 ) ) return x+*y;
    if(x<0 && g(&x)) return x;
    return 99;
}
