#include "defs.h"
#include <stdio.h>
static const double xbig = (double)(0.347 * (53 + 1));

double _Cosh(double x, double y)
 {
 const short errx = _Dtest(&x);
 const short erry = _Dtest(&y);

 if (0 <= errx || 0 <= erry)
  {
  if (errx == 2)
   return (x);
  else if (erry == 2)
   return (y);
  else if (errx == 1)
   if (erry != 0) 
	   return (y < 0.0 ? -_Inf._Double
     : _Inf._Double);
   else
    {
    _Feraise(0x01);
    return (_Nan._Double);
    }
  else
   return (y);
  }
 else
  {
  ((*_Pmsw(&(x))) &= ~((unsigned short)0x8000));

  if (x < xbig)
   {
   _Exp(&x, 1.0, -1);
   return (y * (x + ((0.25) / (x))));
   }
  else
   {
   _Exp(&x, y, -1);
   return (x);
   }
  }
 }

double (cosh)(double x)
 {
 return (_Cosh(x, 1.0));
 }