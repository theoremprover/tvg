int f(double x,double y,int z)
{
    if(x>0.0 || y<0.0)
    {
        return (z==1 ? 1 : -1);
    }

    //for(int i=0;i<3;i++) z++;

    switch(z)
    {
        case 1: z++;
        case 2: return 3;
        default: return 4;
    }

    return -10;
}
