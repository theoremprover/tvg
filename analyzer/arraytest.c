int f(int arr[3],int n)
{
    int j=0;
    for(int i=0;i<(n%3);i++)
    {
        if(arr[i]>0) arr[i] *= 2;
        j += arr[i];
    }
    switch(j)
    {
        case -1 : return(1); break;
        case  0 : return(2); break;
        case  8 : return(3); break;
        default : j++;
    }
    return(j);
}

/*



*/