#include <stdio.h>

typedef unsigned int UDItype __attribute__ ((mode (DI))) ;
typedef unsigned int USItype __attribute__ ((mode (SI))) ;

int main()
{
    UDItype answer = (UDItype)253*(UDItype)2147483584;
    printf("answer = %llu\n",answer);
    USItype high = answer >> 32;
    printf("high = %u\n",high);

    return 0;
}
