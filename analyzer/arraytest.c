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

/*

i::int
->
int i;
scanf("...",&i);

pi :: int*
->
int* pi;
scanf("...",&(*pi));

s :: struct S
->
struct S s;
...
scanf("...",&s.member1);
...

p_s :: struct S *
->
struct S p_s_compound;
struct S * p_s = & p_s_compound;
...
scanf("...",&(p_s->member1));
...


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
