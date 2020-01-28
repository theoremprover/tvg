int g(int x,int y)
{
    int erg = x;
    while(y>0)
    {
        erg = erg << 1;
        y--;
    }
    return(erg);
}

int f(int y)
{
    if(g(2,y)>10) return 1;
    else return 0;
}

void main()
{
    f(1);
}
