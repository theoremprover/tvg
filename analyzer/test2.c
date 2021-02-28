#include <stdio.h>

int main(int argc,char *argv[])
{
    double d = 0.0/0.0;
    if(d>0.0) printf("TRUE\n"); else printf("FALSE\n");
}
