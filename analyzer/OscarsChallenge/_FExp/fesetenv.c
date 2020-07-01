#include "defs.h"

static struct {
 int except;
 double num, denom;
 } table[] = {
 {0x01, 0.0, 0.0},
 {0x02, 1.0, 0.0},
 {0x04, 1e+300, 1.0 / 1e+300},
 {0x08, 1.0 / 1e+300, 1e+300},
 {0x10, 2.0, 3.0}};
double _Force_raise(int except)
 {
 double ans = 0.0;
 int i;





 for (i = 0; i < sizeof (table) / sizeof (table[0]); ++i)
  if ((except & table[i].except) != 0)
   ans = table[i].num / table[i].denom;
 return (ans);
 }
 
 
int (fesetenv)(const fenv_t *penv)
 {
	// OS: since this just sets the exception (which was read before) it is safe to skip it
    //  __asm__("vmsr fpscr, %0" : : "r" (*penv));
 return (0);
 }
