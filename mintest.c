int g(int xg)
{
    if(xg>=0)
    {
        return(2*xg);
    }
    else
    {
        return(3*(-xg));
    }
}

int f(int x,int y)
{
    if(x>g(y+1))
    {
        short abc;
        abc = x+g(x-1);
        return(abc);
    }
	return(x);
}

int main()
{
    int ret = f(1,0);
    return 0;
}
