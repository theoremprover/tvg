#include "defs.h"

static const double c[] = {
 0.00099173235263350450,
 0.11110779924116564678,
 };
static const double s[] = {
 0.00003304120783105597,
 0.01388723295391837963,
 0.49999999999999998664,
 };

static const double c1 = (double)(46516319.0L / 67108864.0L);
static const double c23 = (double)(0.87218081361711894970L / 67108864.0L);

static const double hugexp = (int)(((unsigned short)((1 << (15 - 4)) - 1)) * 900L / 1000);
static const double invln2 = 1.4426950408889634073599246810018922;

short _Exp(double *px, double y, long eoff)
 {
 short errx = _Dtest(px);
 short erry = _Dtest(&y);

 if (0 <= errx || 0 <= erry)
  {
  if (errx == 2)
   return (2);
  else if (erry == 2)
   {
   *px = y;
   return (2);
   }
  else if (erry == 0)
   if (*px != _Inf._Double)
    {
    *px = y;
    return (0);
    }
   else
    {
    _Feraise(0x01);
    *px = _Nan._Double;
    return (2);
    }
  else if (erry == 1)
   if (*px != -_Inf._Double)
    {
    *px = y;
    return (1);
    }
   else
    {
    _Feraise(0x01);
    *px = _Nan._Double;
    return (2);
    }
  else if (errx == 0)
   {
   *px = y;
   switch (errx = _Dscale(px, eoff))
    {
   case 0:
    _Feraise(0x08);
    break;

   case 1:
    _Feraise(0x04);
    }
   return (errx);
   }
  else if (*px == _Inf._Double)
   {
   *px *= y;
   return (1);
   }
  else
   {
   *px = 0.0 * y;
   return (0);
   }
  }
 else if (*px < -hugexp)
  {
  *px = 0.0 * y;
  return (0);
  }
 else if (hugexp < *px)
  {
  *px = _Inf._Double * y;
  _Feraise(0x04);
  return (1);
  }
 else
  {
  double g = *px * invln2;
  long xexp = (long)(g + (g < 0.0 ? - 0.5 : 0.5));

  g = xexp;
  g = (double)((*px - g * c1) - g * c23);
  if (-_Eps._Double < g && g < _Eps._Double)
   *px = y;
  else
   {
   const double z = g * g;
   const double ch = z * (c[0] * z + c[1]);
   const double sh = g * ((s[0] * z + s[1]) * z + s[2]);
   const double den = (1.0 / (1.0 + (ch - sh)));

   *px = (den + (ch + sh) * den) * y;
   }

  switch (errx = _Dscale(px, (long)xexp + eoff))
   {
  case 0:
   _Feraise(0x08);
   break;

  case 1:
   _Feraise(0x04);
   }
  return (errx);
  }
 }

