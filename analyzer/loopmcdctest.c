int f(int x,int y)
{
    int i=x,j=y;

//    for( ; solver_pragma(12,12,2) && (i<0 || j<0) ; )
    for(;i<3 || j>3;)
    {
        i++; j--;
    }

    return i+j;
}
