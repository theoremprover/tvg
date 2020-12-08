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


--DECL a Array 10 Int Int
ASSN a[2] = 7
COND ... a[2] ...
ASSN a[2] = a[2]+1
COND ... a[2] ...



DECL a Array 10 Int Int     DECL a0 Array 10 Int Int

                            DECL a1 Array 10 Int Int
ASSN a[2] = 7         =>    COND a1 = store a0 2 7

COND ... a[2] ...           COND ...a1[2]...

                            DECL a2 Array 10 Int Int
ASSN a[2] = a[2]+1    =>    COND a2 = store a1 2 (select a1 2 + 1)

COND ... a[2] ...           COND ...a2[2]...

*/
