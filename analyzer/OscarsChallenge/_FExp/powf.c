#include "defs.h"
#include <stdio.h>

static const float log2e[] = {
 (float)(2954.0F / 4096.0L * 2),
 (float)(2619.0F / 4096.0L / 4096.0L * 2),
 (float)(661.0F / 4096.0L / 4096.0L / 4096.0L * 2),
 (float)(3095.0F / 4096.0L / 4096.0L / 4096.0L
  / 4096.0L * 2),
 };

float (_FPow)(float x, float y, short *pex)
 {
  if(pex!=0)
    printf("_FPow(x = %f,y = %f,*pex = %i)\n", x, y, *pex);
  else
    printf("_FPow(x = %f,y = %f,*pex = nullptr)\n", x, y);
 float x1;
 float yi = y;
 float z;

 long zexp = 0;
 short xexp;
 short neg;

 if (y == 1.0F)
  return (x);

 short errx = _FDunscale(&xexp, &x);
 const short erry = _FDint(&yi, 0);

 static const float invln2 = 1.4426950408889634073599246810018922F;
 static const float ln2 = 0.69314718055994530941723212145817657F;
 static const float rthalf = 0.70710678118654752440084436210484904F;
 static const float maxexp = (float)(32768);







 if (pex != 0)
  *pex = 0;
 if ((erry == 0 && y == 0.0F)
  || (errx < 0 && xexp == 1
   && (x == 0.5F || (erry == 1 && x == -0.5F))))
  return (1.0F);
 else if (0 <= errx || 0 < erry)
  {
  if (errx == 2)
   return (x);
  else if (erry == 2)
   return (y);
  else if (errx == 1)
   if (!((*_FPmsw(&(x))) & ((unsigned short)0x8000)))
    return (((*_FPmsw(&(y))) & ((unsigned short)0x8000)) ? 0.0F : _FInf._Float);
   else if (!((*_FPmsw(&(y))) & ((unsigned short)0x8000)))
    return (erry == 0 && _FDint(&yi, -1) < 0
     ? -_FInf._Float
     : _FInf._Float);
   else
    return (erry == 0 && _FDint(&yi, -1) < 0
     ? -_FZero : 0.0F);
  else if (erry == 1)
   if (!((*_FPmsw(&(y))) & ((unsigned short)0x8000)))
    return (xexp <= 0 ? 0.0F : _FInf._Float);
   else
    return (xexp <= 0 ? _FInf._Float : 0.0F);
  else
   if (!((*_FPmsw(&(y))) & ((unsigned short)0x8000)))
    return (erry == 0 && _FDint(&yi, -1) < 0 && ((*_FPmsw(&(x))) & ((unsigned short)0x8000))
     ? -_FZero : 0.0F);
   else
    {
    _Feraise(0x02);
    return (erry == 0 && _FDint(&yi, -1) < 0 && ((*_FPmsw(&(x))) & ((unsigned short)0x8000))
     ? -_FInf._Float : _FInf._Float);
    }
  }
 else if (((*_FPmsw(&(x))) & ((unsigned short)0x8000)) && erry < 0)
  {
  _Feraise(0x01);
  return (_FNan._Float);
  }

 if (0.0F < x)
  neg = 0;
 else
  {
  ((*_FPmsw(&(x))) ^= ((unsigned short)0x8000));
  neg = _FDint(&yi, -1);
  }


  {
 float z, w;

 if (x < rthalf)
  {
  x *= 2.0F;
  --xexp;
  }

printf("_FPow: x = %f\n", x);
 z = ((x - 1.0F) / (x + 1.0F));
 printf("_FPow: z = ((x - 1.0F) / (x + 1.0F)) == %f\n", z);
 w = z * z;

 x -= 1.0F;
 x1 = z * (w * _FLogpoly(w) - x);

 yi = (float)xexp + (x + x1) * invln2;
  }

 z = y * yi;
 printf("_FPow: y == %f\n", y);
 printf("_FPow:%d: z = y * yi == %f\n", __LINE__, z);
 if (z < -maxexp)
  errx = 0;
 else if (-12.0F <= z && z <= 12.0F
  && -40.0F < y && y < 40.0F)
  {
  zexp = (long)(z < 0.0F ? z - 0.5F : z + 0.5F);
  z = y * (x + x1);


  z += (y * (float)xexp - (float)zexp) * ln2;
  printf("_FPow:%d: z == %f\n", __LINE__, z);






  errx = -1;
  }
 else if (maxexp < z)
  errx = 1;
 else
  {

    printf("_FPow:%d: else branch\n", __LINE__);
  float xpx1[2], xpx[4], xpy[4], xpz[4];
  int i;

  _FXp_setw(xpx, 4, x);
  _FXp_setw(xpx1, 2, x1);
  _FXp_addx(xpx, 4, xpx1, 2);

  if (xpx[0] == 0.0F)
   _FXp_setw(xpy, 4, 0.0F);
  else
   {
   memcpy_HighTecARMImpl(xpy, log2e, sizeof (xpy));
   _FXp_mulh(xpy, 4, xpx[0]);
   for (i = 1; i < 4 && xpx[i] != 0.0F; ++i)
    {
    float xpw[4];

    memcpy_HighTecARMImpl(xpw, log2e, sizeof (xpw));
    _FXp_mulh(xpw, 4, xpx[i]);
    _FXp_addx(xpy, 4, xpw, 4);
    }
   }
  _FXp_addh(xpy, 4, (float)xexp);

  _FXp_setw(xpx, 2, y);
  memcpy_HighTecARMImpl(xpz, xpy, sizeof (xpz));
  _FXp_mulh(xpz, 4, xpx[0]);

  if (xpx[1] != 0.0F)
   {
   float xpw[4];

   memcpy_HighTecARMImpl(xpw, xpy, sizeof (xpw));
   _FXp_mulh(xpw, 4, xpx[1]);
   _FXp_addx(xpz, 4, xpw, 4);
   }

  x = xpz[0];
  if (xpz[0] != 0.0F && xpz[1] != 0.0F)
   x += xpz[1] + xpz[2];
  _FDint(&x, 0);
  _FXp_addh(xpz, 4, -x);
  z = _FXp_getw(xpz, 4);
  z *= ln2;
  printf("_FPow:%d: z == %f\n", __LINE__, z);
  zexp = (long)x;
  errx = -1;
  }
  printf("_FPow:%d: errx == %i\n", __LINE__, errx);
 if (errx < 0)
  {
  if (pex != 0)
   {
   *pex = zexp;
   zexp = 0;
   }
  errx = _FExp(&z, 1.0F, zexp);
   }
 switch (errx)
  {
 case 0:
  z = 0.0F;
  _Feraise(0x08);
  break;

 case 1:
  if (z < 0.0F)
   {
   z = 0.0F;
   _Feraise(0x08);
   }
  else
   {
   z = _FInf._Float;
   _Feraise(0x04);
   }
  }

 if (neg)
  ((*_FPmsw(&(z))) ^= ((unsigned short)0x8000));
 return (z);
 }

 float (powf)(float x, float y)
 {
 return (_FPow(x, y, 0));
 }