#include "defs.h"
double (fmod)(double x, double y)
 {
 const short errx = _Dtest(&x);
 const short erry = _Dtest(&y);
 if (0 <= errx || 0 <= erry)
  {
  if (errx == 2)
   return (x);
  else if (erry == 2)
   return (y);
  else if (errx == 1 || erry == 0)
   {
   _Feraise(0x01);
   return (_Nan._Double);
   }
  else
   return (x);
  }
 else
  {
  double t;
  short neg, ychar;
  long n;
  ((*_Pmsw(&(y))) &= ~((unsigned short)0x8000));
  if (x < 0.0)
   {
   ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
   neg = 1;
   }
  else
   neg = 0;
  for (t = y, _Dunscale(&ychar, &t), n = 0; ; )
   {
   short xchar;
   t = x;
   if (n < 0 || _Dunscale(&xchar, &t) == 0
    || (n = (long)xchar - ychar) < 0)
    {
    if (neg)
     ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
    return (x);
    }
   for (; 0 <= n; --n)
    {
    t = y, _Dscale(&t, n);
    if (x < t)
     ;
    else if (x == t)
     return (0.0);
    else
     {
     _Dunscale(&xchar, &x);
     _Dscale(&t, -xchar);
     x -= t;
     _Dscale(&x, xchar);
     }
    }
   }
  }
 }
