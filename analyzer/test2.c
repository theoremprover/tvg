#include <stdio.h>

int main(int argc,char *argv[])
{
   float f = 3.14;

   float *fp = &f;

   printf("u=%u\n",*((unsigned long int*)fp));

 }
