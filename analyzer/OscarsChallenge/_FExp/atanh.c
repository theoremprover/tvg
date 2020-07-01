#include "defs.h"
#include <stdio.h> 

double (atanh)(double x)
 {
 double y;
 int neg;

// printf("atanh(%.17g\n",x);
 switch (_Dtest(&x))
  {
 case 2:// printf("DTEST=2\n");
 case 0:// printf("DTEST=0\n");
  return (x);
 default:
  if (x < 0.0)
   {
	   // printf("negating %.17g",x);
   ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
	// printf("to %.17g\n",x);
   neg = 1;
   }
  else
   neg = 0;

  // printf("atanh(%17.g) eps=%17.g\n",x,_Rteps._Double);
  
  if (1.0 < x)
   {
   _Feraise(0x01);
   return (_Nan._Double);
   }
  else if (x == 1.0)
   {
   _Feraise(0x02);
   return (neg ? -_Inf._Double : _Inf._Double);
   }
  else if (-_Rteps._Double < x && x < _Rteps._Double)
   {
   if (neg)
    ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
   return (x);
   }
  else
   {
   y = 0.5 * log1p(2.0 * ((x) / ((1.0 - x))));

   return (neg ? -y : y);
   }
  }
 }

