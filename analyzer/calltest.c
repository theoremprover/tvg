int f1(int x)
{
    if(x>0) return x+1;
    else return x-1;
}

int f2(int x)
{
    if(x>0) return x+10;
    else return x-10;
}

int g(int z1,int z2)
{
    int r = f1(z1)+f2(z2);
    return r;
}
