#include <stdio.h>
#include <stdlib.h>

#include "fp-bit.c"

int main(int argc, char* argv[])
{
    int x = atoi(argv[1]);
    int y = atoi(argv[2]);
    printf("f(x=%i,z=%i) =\n%i\n",x,y,_fpdiv_parts(x,y));
    return 0;
}
