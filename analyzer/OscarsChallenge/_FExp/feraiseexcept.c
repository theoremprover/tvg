#include "defs.h"

int (feraiseexcept)(int except)
 {
 if ((except &= (0x02 | 0x10 | 0x01 | 0x04 | 0x08)) != 0)
  {
  fenv_t env;

  fegetenv(&env);






  env |= except << 0;


  fesetenv(&env);
  if ((except &= (env >> 8)) != 0)
   _Force_raise(except);
  }
 return (0);
 }
