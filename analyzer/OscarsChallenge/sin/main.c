#include <stdio.h>
#include <stdlib.h>
int solver_pragma(int x, ...)
{
return 1;
}
void solver_debug(void *x)
{ }
#include "xdtest.c"
int main(int argc, char *argv[])
{
int i = 1;

double PTR_px;
double * px = &PTR_px;
sscanf(argv[i++],"%lf",&(*px));

printf("_Dtest(*px = %lf) = \n",*px);
short return_val = _Dtest(px);
printf("%i\n", return_val);
return 0;
}
