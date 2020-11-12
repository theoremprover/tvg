#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

int f(int arr[3])
{
    int j=0;
    for(int i=0;i<sizeof(arr);i++)
    {
        if(arr[i]>0) arr[i] *= 2;
        j += arr[i];
    }
    return j;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int i=0;
    int a[3];
    sscanf(argv[i++],"%i",&a[0]);
    sscanf(argv[i++],"%i",&a[1]);
    sscanf(argv[i++],"%i",&a[2]);

    printf("f(a0=%i, a1=%i, a2=%i) = %i\n%i\n",a[0],a[1],a[2],f(a));
}
#endif
