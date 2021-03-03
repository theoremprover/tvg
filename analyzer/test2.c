#include <stdio.h>

short f()
{
    unsigned short x = 0xc010;
    return x;
}

int main(int argc,char *argv[])
{
   unsigned short x = f();
 printf("%hi\n",x);

 }
