#include "defs.h"
#include <stdio.h>

static const float c[] = {
 0.0999748594F,
 };
static const float s[] = {
 0.0083208258F,
 0.4999999992F,
 };

static const float c1 = (float)(1419.0L / 2048.0L);
static const float c23 = (float)(0.5654257868L / 2048.0L);

static const float hugexp = (int)(((unsigned short)((1 << (15 - 7)) - 1)) * 900L / 1000);
static const float invln2 = 1.4426950408889634073599246810018922F;

float expf(float x)
 {
   printf("expf called with x = %f ( = 0x%x )\n", x, toRep(x));
 switch (_FDtest(&x))
  {
 case 2:
  return (x);

 case 1:
  return (((*_FPmsw(&(x))) & ((unsigned short)0x8000)) ? 0.0F : x);

 case 0:
  return (1.0F);

 default:
  _FExp(&x, 1.0F, 0);
  return (x);
  }
 }

short _FExp(float *px, float y, long eoff)
 {
   printf("_FExp: *px = %f, y = %f, eoff = %li\n", *px, y, eoff);
 short errx = _FDtest(px);
 short erry = _FDtest(&y);
 printf("_Fexp: errx = %hi, erry = %hi\n", errx, erry);

 if (0 <= errx || 0 <= erry)
  {
    printf("(0 <= errx || 0 <= erry)\n");
  if (errx == 2)
   return (2);
  else if (erry == 2)
   {
   *px = y;
   return (2);
   }
  else if (erry == 0)
   if (*px != _FInf._Float)
    {
    *px = y;
    return (0);
    }
   else
    {
    _Feraise(0x01);
    *px = _FNan._Float;
    return (2);
    }
  else if (erry == 1)
   if (*px != -_FInf._Float)
    {
    *px = y;
    return (1);
    }
   else
    {
    _Feraise(0x01);
    *px = _FNan._Float;
    return (2);
    }
  else if (errx == 0)
   {
   *px = y;
   switch (errx = _FDscale(px, eoff))
    {
   case 0:
    _Feraise(0x08);
    break;

   case 1:
    _Feraise(0x04);
    }
   return (errx);
   }
  else if (*px == _FInf._Float)
   {
   *px *= y;
   return (1);
   }
  else
   {
   *px = 0.0F * y;
   return (0);
   }
  }
 else if (*px < -hugexp)
  {
  *px = 0.0F * y;
  return (0);
  }
 else if (hugexp < *px)
  {
  *px = _FInf._Float * y;
  _Feraise(0x04);
  return (1);
  }
 else
  {
  float g = *px * invln2;
  long xexp = (long)(g + (g < 0.0F ? - 0.5F : 0.5F));

  g = xexp;
  g = (float)((*px - g * c1) - g * c23);
  if (-_FEps._Float < g && g < _FEps._Float)
   *px = y;
  else
   {
   const float z = g * g;
   const float ch = z * (c[0]);
   const float sh = g * (s[0] * z + s[1]);
   const float den = (1.0F / (1.0F + (ch - sh)));

   *px = (den + (ch + sh) * den) * y;
   }

  switch (errx = _FDscale(px, (long)xexp + eoff))
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