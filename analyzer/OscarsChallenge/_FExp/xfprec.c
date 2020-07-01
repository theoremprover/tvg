#include "defs.h"

float _FXp_getw(const float *p, int n)
 {
 if (n == 0)
  return (0.0F);
 else if (n == 1 || p[0] == 0.0F || p[1] == 0.0F)
  return (p[0]);
 else if (n == 2 || p[2] == 0.0F)
  return (p[0] + p[1]);
 else
  {
  float p01 = p[0] + p[1];
  float p2 = p[2];

  if (4 <= n)
   p2 += p[3];
  if (p01 - p[0] == p[1])
   return (p01 + p2);
  else
   return (p[0] + (p[1] + p2));
  }
 }

float *_FXp_setw(float *p, int n, float x)
 {
 float x0 = x;
 short errx, xexp;

 if (n <= 0)
  ;
 else if (n == 1 || (errx = _FDunscale(&xexp, &x0)) == 0)
  p[0] = x0;
 else if (0 < errx)
  {
  p[0] = x0;
  p[1] = 0.0F;
  }
 else
  {
  _FDint(&x0, (24 / 2));
  _FDscale(&x0, xexp);

  p[0] = x0;
  p[1] = x - x0;
  if ((24 & 1) != 0 && 2 < n && p[1] != 0.0F)
   {
   x = p[1];
   _FDunscale(&xexp, &p[1]);
   _FDint(&p[1], (24 / 2));
   _FDscale(&p[1], xexp);
   p[2] = x - p[1];
   if (3 < n && p[2] != 0.0F)
    p[3] = 0.0F;
   }
  else if (2 < n)
   p[2] = 0.0F;
  }
 return (p);
 }

float *_FXp_addh(float *p, int n, float x0)
 {
 float xscaled = x0;
 short errx, xexp;

 if (n == 0)
  ;
 else if (0 < (errx = _FDunscale(&xexp, &xscaled)))
  if (errx == 2 || (errx = _FDtest(&p[0])) <= 0)
   p[0] = x0;
  else if (errx == 2 || ((*_FPmsw(&(x0))) & ((unsigned short)0x8000)) == ((*_FPmsw(&(p[0]))) & ((unsigned short)0x8000)))
   ;
  else
   {
   _Feraise(0x01);
   p[0] = _FNan._Float;
   if (1 < n)
    p[1] = 0.0F;
   }
 else if (errx < 0)
  {
  long prevexp = (2 * 128);
  int k;

  for (k = 0; k < n; )
   {
   float yscaled = p[k];
   int mybits = (24 / 2);
   short yexp;
   long diff;

   if (0 < (errx = _FDunscale(&yexp, &yscaled)))
    break;
   else if (errx == 0)
    {
    p[k] = x0;
    if (k + 1 < n)
     p[k + 1] = 0.0F;
    break;
    }
   else if ((diff = (long)yexp - xexp) <= -mybits
    && x0 != 0.0F)
    {
    int j;

    for (j = k; ++j < n && p[j] != 0.0F; )
     ;
    if (j < n - 1)
     ++j;
    else if (j == n)
     --j;
    for (; k < j; --j)
     p[j] = p[j - 1];
    p[k] = x0;
    x0 = 0.0F;
    }
   else if (mybits <= diff && x0 != 0.0F)
    {
    prevexp = yexp;
    ++k;
    }
   else
    {
    if ((p[k] += x0) == 0.0F)
     {
     {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0F; ) ; p[n - 1] = 0.0F;}
     if (p[k] == 0.0F)
      break;
     }
    x0 = p[k];
    _FDunscale(&xexp, &x0);
    if (prevexp - mybits < xexp)
     {
     _FDint(&x0, (short)(xexp - (prevexp - mybits)));
     _FDscale(&x0, xexp);
     if ((p[k] -= x0) == 0.0F)
      {
      {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0F; ) ; p[n - 1] = 0.0F;}
      }
     if (--k == 0)
      prevexp = (2 * 128);
     else
      {
      xscaled = p[k - 1];
      _FDunscale(&yexp, &xscaled);
      prevexp = yexp;
      }
     }
    else if (k + 1 == n)
     break;
    else
     {
     x0 = p[k];
     _FDunscale(&yexp, &p[k]);
     _FDint(&p[k], (24 / 2));
     _FDscale(&p[k], yexp);
     x0 -= p[k];
     prevexp = yexp;

     xscaled = x0 != 0.0F ? x0 : p[k];
     _FDunscale(&xexp, &xscaled);
     ++k;
     }
    }
   }
  }
 return (p);
 }

float *_FXp_mulh(float *p, int n, float x0)
 {
 short errx;
 int j, k;
 float buf[4];

 if (0 < n)
  {
  buf[0] = p[0] * x0;
  if (0 <= (errx = _FDtest(&buf[0])))
   {
   if (errx == 2)
    _Feraise(0x01);
   p[0] = buf[0];
   if (0 < errx && 1 < n)
    p[1] = 0.0F;
   return (p);
   }
  p[0] = 0.0F;
  }

 for (j = 1, k = 0; k < n; ++k, --j)
  {
  for (; j < 4; ++j)
   if (k + j < n && p[k + j] != 0.0F)
    {
    buf[j] = p[k + j] * x0;
    p[k + j] = 0.0F;
    }
   else
    {
    buf[j] = 0.0F;
    j = 2 * 4;
    break;
    }

  if (buf[0] == 0.0F)
   break;
  else
   {
   int i = 0;
   float y0 = buf[0];
   short xexp;

   _FDunscale(&xexp, &y0);
   _FDint(&y0, (24 / 2));
   _FDscale(&y0, xexp);
   _FXp_addh(p, n, y0);
   _FXp_addh(p, n, buf[0] - y0);

   for (; ++i < j; )
    if ((buf[i - 1] = buf[i]) == 0.0F)
     break;
   }
  }
 return (p);
 }

float *_FXp_setn(float *p, int n, long x)
 {




 _FXp_setw(p, n, (float)(x / 10000));
 _FXp_mulh(p, n, (float)10000);
 _FXp_addh(p, n, (float)(x % 10000));


 return (p);
 }

float *_FXp_movx(float *p, int n, const float *q)
 {
 memcpy_HighTecARMImpl(p, q, n * sizeof (float));
 return (p);
 }

float *_FXp_addx(float *p, int n,
 const float *q, int m)
 {
 int k;

 for (k = 0; k < m && q[k] != 0.0F; ++k)
  _FXp_addh(p, n, q[k]);
 return (p);
 }
# 301 "/home/jenkins/workspace/toolchain/arm-qualified-library-build/library-src/dinkum/source/./xxxprec.h"
float *_FXp_subx(float *p, int n,
 const float *q, int m)
 {
 int k;

 for (k = 0; k < m && q[k] != 0.0F; ++k)
  _FXp_addh(p, n, -q[k]);
 return (p);
 }

float *_FXp_ldexpx(float *p, int n, int m)
 {
 int k;

 for (k = 0; k < n; ++k)
  {
  p[k] = ldexpf(p[k], m);
  if (p[k] == 0.0F)
   break;
  }
 return (p);
 }

float *_FXp_mulx(float *p, int n,
 const float *q, int m,
 float *ptemp2)
 {
 if (n == 0 || m == 0)
  ;
 else if (q[0] == 0.0F || q[1] == 0.0F)
  _FXp_mulh(p, n, q[0]);
 else
  {
  float *px = ptemp2;
  float *pac = ptemp2 + n;
  int j;

  _FXp_movx(px, n, p);
  _FXp_mulh(p, n, q[0]);
  for (j = 1; j < m && q[j] != 0.0F; ++j)
   {
   _FXp_movx(pac, n, px);
   _FXp_mulh(pac, n, q[j]);
   _FXp_addx(p, n, pac, n);
   }
  }
 return (p);
 }
# 378 "/home/jenkins/workspace/toolchain/arm-qualified-library-build/library-src/dinkum/source/./xxxprec.h"
float *_FXp_invx(float *p, int n, float *ptemp4)
 {
 short errx;

 if (n == 0)
  ;
 else if (0 <= (errx = _FDtest(&p[0])))
  {
  if (errx == 1)
   p[0] = 0.0F;
  else if (errx == 0)
   p[0] = _FInf._Float;

  }
 else
  {
  float *pac = ptemp4;
  float *py = ptemp4 + n;
  float *ptemp2 = py + n;
  float x0 = p[0];
  int k;

  _FXp_movx(py, n, p);
  _FXp_mulh(py, n, -1.0F);

  if (1 < n)
   x0 += p[1];
  _FXp_setw(p, n, (1.0F / (x0)));

  for (k = 1; k < n; k <<= 1)
   {
   _FXp_movx(pac, n, p);
   _FXp_mulx(pac, n, py, n, ptemp2);
   _FXp_addh(pac, n, 1.0F);
   _FXp_mulx(pac, n, p, n, ptemp2);
   _FXp_addx(p, n, pac, n);
   }
  }
 return (p);
 }

float *_FXp_sqrtx(float *p, int n, float *ptemp4)
 {
 if (n == 0)
  ;
 else if (0 <= _FDtest(&p[0]) || p[0] < 0.0F)
  {
  if (p[0] < 0.0F)
   {
   _Feraise(0x01);
   p[0] = _FNan._Float;
   }
  }
 else
  {
  float *pac = ptemp4;
  float *py = ptemp4 + n;
  float *ptemp2 = py + n;
  float x0 = p[0];
  int k;

  if (1 < n)
   x0 += p[1];
  _FXp_setw(py, n, (1.0F / (sqrtf(x0))));

  for (k = 2; k < n; k <<= 1)
   {
   _FXp_movx(pac, n, py);
   _FXp_mulh(pac, n, -0.5F);
   _FXp_mulx(pac, n, p, n, ptemp2);
   _FXp_mulx(pac, n, py, n, ptemp2);
   _FXp_addh(pac, n, 1.5F);
   _FXp_mulx(py, n, pac, n, ptemp2);
   }
  _FXp_mulx(p, n, py, n, ptemp2);
  }
 return (p);
 }