int f(double x,double y,int z)
{
    switch(z)
    {
        case 0:
        case 1: z++; break;
        case 2: return z;
        default: return z+1;
    }

    return ((x>0.0 && (y<0.0 || (y > 10.0 && z==0) || x<(-10.0) )) ? 1 : -1);

   int i = 3;
   for(;i>=0;--i) z++;
    return z+10;
}
