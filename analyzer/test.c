int g(int y)
{
    int z = 2*y;

    return z;
}

int f(int x)
{
    if(g(x)) return 0;
    return 1;
}

int h(int x)
{
    switch(g(x))
    {
        case 1: return 1;
        case 2: return 2;
        default: return 0;
    }
}
