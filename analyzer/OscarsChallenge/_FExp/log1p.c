
#include "defs.h"

double (log1p)(double x)
 {
 switch (_Dtest(&x))
  {
 case 2:
 case 0:
  return (x);

 case 1:
  if (!((*_Pmsw(&(x))) & ((unsigned short)0x8000)))
   return (x);

 default:
  if (x < -1.0)
   {
   _Feraise(0x01);
   return (_Nan._Double);
   }
  else if (x == -1.0)
   {
   _Feraise(0x02);
   return (-_Inf._Double);
   }
  else if (-_Eps._Double <= x && x < _Eps._Double)
   return (x);
  else
   {
   double y = 1.0 + x;

   return (log(y) - (((y - 1.0) - x) * (1.0 / (y))));
   }
  }
 }
