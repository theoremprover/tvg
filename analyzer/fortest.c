#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

int solver_pragma(int x,...)
{
    return 1;
}
void solver_debug(void* x)
{
}


int f(int x,int n)
{
    int j = 1;
    for(int i=0;solver_pragma(3) && i<n;i++)
    {
        j *= x;
    }
    return j;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int x0,n0;
    int i=1;
    sscanf(argv[i++],"%i",&x0);
    sscanf(argv[i++],"%i",&n0);

    printf("f(x=%i, n=%i) = %i\n%i\n",x0,n0,f(x0,n0));
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



DECL a Array Int Int
ASSN a[i] = 7
COND a[i]>0
ASSN a[i] = a[i]+1
COND a[i]==8

*/
