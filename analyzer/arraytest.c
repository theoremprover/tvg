int f(int arr[3],int n)
{
    int j=0;
    for(int i=0;i<n;i++)
    {
        if(arr[i]>0) arr[i] *= 2;
        j += arr[i];
    }
    return j;
}

/*



*/