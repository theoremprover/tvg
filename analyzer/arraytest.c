#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

int f(int arr[3])
{
    int j=0;
    for(int i=0;i<3;i++)
    {
        if(arr[i]>0) arr[i] *= 2;
        j += arr[i];
    }
    return j;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int i=1;
    int a[3];
    sscanf(argv[i++],"%i",&a[0]);
    sscanf(argv[i++],"%i",&a[1]);
    sscanf(argv[i++],"%i",&a[2]);

    printf("f(a0=%i, a1=%i, a2=%i) = %i\n%i\n",a[0],a[1],a[2],f(a));
}
#endif

/*
(set-option :pp.fp_real_literals true)
(declare-const i Int)
(declare-const ar (Array Int Float32))

(assert (<= 0 i))
(assert (<= i 3))
(assert (= (store ar 1 1.0) ar))
(assert (= (store ar 2 4.0) ar))
(assert (= (store ar 3 9.0) ar))

(assert (>= (select ar i) (roundTowardZero (/ 5 1))))

(check-sat)
(get-model)
(get-value (i))


int a[10];

a[2]=7       => ar1 = store ar0 2 7

a[3]=a[2]+1  => ar2 = store ar1 3 ((select ar1 2) + 1)

int y = a[2]+1;


*/
