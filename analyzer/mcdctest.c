int f(double x,double y,int z)
{
    if(x>0.0 || y<0.0)
    {
        return 1; //(z==1 ? 1 : -1);
    }
    return 0;

    for(int i=0;i<3;i++) z++;

    switch(z)
    {
        case 1: z++;
        case 2: return z;
        default: return z+1;
    }

    return z+10;
}
