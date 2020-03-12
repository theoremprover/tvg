#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

enum Zahl { EINS, ZWEI };

typedef enum Zahl MYENUM;

typedef struct
{
    MYENUM member1;
    int member2;
} STRUCT;

int f(int* x)
{
    if(*x == 0)
    {
        (*x)++;
    }

    *(x+1) *= 2;

    return(*x+1);
}

int g(int z)
{
    int is[] = { 1,2 };
    STRUCT s = { EINS,10 };
    if(f(is)==1) return 1;
    else return 0;
}

int main(int argc, char* argv[])
{
#ifdef CALC
    int z = atoi(argv[1]);

    printf("g(z=%i) = %i\n",z,g(z));
#endif
    return 0;
}
