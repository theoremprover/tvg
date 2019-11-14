
#include "stdio.h"
#include "math.h"

typedef long long int64;
typedef unsigned long long uint64;

extern int64 __divdi3 (int64 u, int64 v);
extern int64 __moddi3 (int64 u, int64 v);
extern uint64 __udivdi3 (uint64 n, uint64 d);
extern uint64 __umoddi3 (uint64 u, uint64 v);
extern uint64 __udivmoddi4(uint64 n, uint64 d, uint64* rp);
extern float __mulsf3 (float a, float b); 

int main()
{
    /*
    uint64 r;
    //uint64 d0 = 0x5Full ;
    uint64 d0 = 0xFFFFull ;
    uint64 d1 = 0ull;
    uint64 n0 = 0ull;
    uint64 n1 = 0xFFFFFFFFull;
    uint64 res = __udivmoddi4((n1 << 32) + n0, (d1 << 32) + d0,&r);
    printf("result = %llu\n", res);
    printf("remainder = %llu\n", r);
    printf("=================================\n");
    uint64 d0_1 = 0xFFFFFFull ;
    uint64 d1_1 = 0ull;
    uint64 n0_1 = 0ull;
    uint64 n1_1 = 0xFFFFFFFFull;
    uint64 res_1 = __udivmoddi4((n1_1 << 32) + n0_1, (d1_1 << 32) + d0_1,&r);
    printf("result = %llu\n", res_1);
    printf("remainder = %llu\n", r);
    printf("=================================\n");
    uint64 d0_2 = 0xFFull ;
    uint64 d1_2 = 0ull;
    uint64 n0_2 = 0ull;
    uint64 n1_2 = 0xFFFFFFFFull;
    uint64 res_2 = __udivmoddi4((n1_2 << 32) + n0_2, (d1_2 << 32) + d0_2,&r);
    printf("result = %llu\n", res_1);
    printf("remainder = %llu\n", r);
    printf("=================================\n");
    uint64 d0_3 = 0xFFFull ;
    uint64 d1_3 = 0ull;
    uint64 n0_3 = 0ull;
    uint64 n1_3 = 0xFFFFFFFFull;
    uint64 res_3 = __udivmoddi4((n1_3 << 32) + n0_3, (d1_3 << 32) + d0_3,&r);
    printf("result = %llu\n", res_1);
    printf("remainder = %llu\n", r);
    printf("=================================\n");
    uint64 d0_4 = 0xF01ull ;
    uint64 d1_4 = 0ull;
    uint64 n0_4 = 0ull;
    uint64 n1_4 = 0xFFFFFFFFull;
    uint64 res_4 = __udivmoddi4((n1_4 << 32) + n0_4, (d1_4 << 32) + d0_4,&r);
    printf("result = %llu\n", res_1);
    printf("remainder = %llu\n", r);
    */

   //__mulsf3(3.4028235e+38f,3.4028235e+38f);
   //__mulsf3(1.1754942e-38f, 1.1754942e-38f);
   //__mulsf3(1.1754945E-38f, 1.1754945E-38f);
   //__mulsf3(1.1754965E-38f, 1.1754965E-38f);
   //__mulsf3(1.4E-45, 1.4E-45);
   //__mulsf3(1.1754942E-38f, 1.1754942E-38f);
   //__mulsf3(0.500000089406967163f,2.f);
   __mulsf3(0.50000006f,2.f);
   //__mulsf3(0.5000001f,2.f);
}