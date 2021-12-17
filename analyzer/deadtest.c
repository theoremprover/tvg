int g(int x)
{
    if(x>=0)
    {
        return x;
    }
    else
    {
        if(x>10)
            return (-x+1);
        else
            return (-x);
    }
}

int f(int x,int y)
{
    int z = y+g(x);
    if(z>=0)
    {
        return 1;
    }
    else
    {
        if(z>10)
        {
            // DEAD!
            z++;
            return -10;
        }
        else
            return -1;
    }
}
