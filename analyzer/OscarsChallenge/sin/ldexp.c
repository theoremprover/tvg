#include "defs.h"
double ldexp(double x, int xexp)
 {
 if (xexp != 0 && _Dtest(&x) < 0)
  switch (_Dscalex(&x, xexp, 1))
   {
  case 0:
   _Feraise(0x08);
   break;
  case 1:
   _Feraise(0x04);
   }
 return (x);
 }
