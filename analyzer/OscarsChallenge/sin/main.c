#include <stdio.h>
#include <stdlib.h>
int solver_pragma(int x, ...)
{
return 1;
}
void solver_debug(void *x)
{ }
#include "oscar.c"
int main(int argc, char *argv[])
{
int i = 1;

double x;
sscanf(argv[i++],"%lf",&(x));
unsigned int qoff;
sscanf(argv[i++],"%u",&(qoff));
int quads;
sscanf(argv[i++],"%i",&(quads));

printf("_Sinx(x = %lf, qoff = %u, quads = %i) = \n",x, qoff, quads);
double return_val = _Sinx(x,qoff,quads);
printf("%lf\n", return_val);
return 0;
}
