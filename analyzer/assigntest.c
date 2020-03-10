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

int f(int x)
{
    int y = 0;

    if(x == 0)
    {
        y++;
    }

    y *= 2;

    return(y);
}

int g(int z)
{
    if(f(z)==1) return 1;
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
