int g(int x)
{
    if(x>5)
        return(2*x);

    else
    {
        x = x-- + 2;
    }

    return(x+10);
}

int f(int x)
{
    int z = 2*g(x);

    int y = x>100 ? 2*z : 3*z;

    if(y>10)
    {
        return(g(y));
    }
    else
    {
        if(z>5)
        {
            y = z>10000 ? g(x++) : g(x+2);
        }
    }

    return(y+1);
}

int main()
{
    int x;
    x= 6;
    x=-2;
    x=-9;
}
