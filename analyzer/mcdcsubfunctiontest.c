int g(int x,int y)
{
    if(x>0) return y;
    return y+1;
}

int f(int x,int y)
{
    if(g(x,y)>0) return (y>0 ? 1 :2);
    switch(y)
    {
        case 1: y++;
        case 2: return y;
        default: y+=2;
    }
    return y;
}
