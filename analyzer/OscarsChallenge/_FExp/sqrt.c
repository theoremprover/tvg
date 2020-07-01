#include "defs.h"
double sqrt(double x)
 {
 short xexp;
 double y;
 switch (_Dunscale(&xexp, &x))
  {
 case 2:
 case 0:
  return (x);
 case 1:
  if (!((*_Pmsw(&(x))) & ((unsigned short)0x8000)))
   return (x);
 default:
  if (((*_Pmsw(&(x))) & ((unsigned short)0x8000)))
   {
   _Feraise(0x01);
   return (_Nan._Double);
   }
  if ((unsigned int)xexp & 1)
   x *= 2.0, --xexp;
  y = (-0.09977 * x + 0.71035) * x
   + 0.38660;
  y += ((x) / (y));
  y = 0.25 * y + ((x) / (y));
  y = 0.5 * (y + ((x) / (y)));
  _Dscale(&y, xexp / 2);
  return (y);
  }
 }
