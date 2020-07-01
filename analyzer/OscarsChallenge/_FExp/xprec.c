#include "defs.h"
#include <stdio.h>

double _Xp_getw(const double *p, int n) {
 if (n == 0)
  return (0.0);
 else if (n == 1 || p[0] == 0.0 || p[1] == 0.0)
  return (p[0]);
 else if (n == 2 || p[2] == 0.0)
  return (p[0] + p[1]);
 else
  {
  double p01 = p[0] + p[1];
  double p2 = p[2];
  if (4 <= n)
   p2 += p[3];
  if (p01 - p[0] == p[1])
   return (p01 + p2);
  else
   return (p[0] + (p[1] + p2));
  }
 }
double *_Xp_setw(double *p, int n, double x)
 {
 double x0 = x;
 short errx, xexp;
 if (n <= 0)
  ;
 else if (n == 1 || (errx = _Dunscale(&xexp, &x0)) == 0)
  p[0] = x0;
 else if (0 < errx)
  {
  p[0] = x0;
  p[1] = 0.0;
  }
 else
  {
  _Dint(&x0, (53 / 2));
  _Dscale(&x0, xexp);
  p[0] = x0;
  p[1] = x - x0;
  if ((53 & 1) != 0 && 2 < n && p[1] != 0.0)
   {
   x = p[1];
   _Dunscale(&xexp, &p[1]);
   _Dint(&p[1], (53 / 2));
   _Dscale(&p[1], xexp);
   p[2] = x - p[1];
   if (3 < n && p[2] != 0.0)
    p[3] = 0.0;
   }
  else if (2 < n)
   p[2] = 0.0;
  }
 return (p);
 }
double *_Xp_addh(double *p, int n, double x0)
 {
 double xscaled = x0;
 short errx, xexp;
 if (n == 0)
  ;
 else if (0 < (errx = _Dunscale(&xexp, &xscaled)))
  if (errx == 2 || (errx = _Dtest(&p[0])) <= 0)
   p[0] = x0;
  else if (errx == 2 || ((*_Pmsw(&(x0))) & ((unsigned short)0x8000)) == ((*_Pmsw(&(p[0]))) & ((unsigned short)0x8000)))
   ;
  else
   {
   _Feraise(0x01);
   p[0] = _Nan._Double;
   if (1 < n)
    p[1] = 0.0;
   }
 else if (errx < 0)
  {
  long prevexp = (2 * 1024);
  int k;
  for (k = 0; k < n; )
   {
   double yscaled = p[k];
   int mybits = (53 / 2);
   short yexp;
   long diff;
   if (0 < (errx = _Dunscale(&yexp, &yscaled)))
    break;
   else if (errx == 0)
    {
    p[k] = x0;
    if (k + 1 < n)
     p[k + 1] = 0.0;
    break;
    }
   else if ((diff = (long)yexp - xexp) <= -mybits
    && x0 != 0.0)
    {
    int j;
    for (j = k; ++j < n && p[j] != 0.0; )
     ;
    if (j < n - 1)
     ++j;
    else if (j == n)
     --j;
    for (; k < j; --j)
     p[j] = p[j - 1];
    p[k] = x0;
    x0 = 0.0;
    }
   else if (mybits <= diff && x0 != 0.0)
    {
    prevexp = yexp;
    ++k;
    }
   else
    {
    if ((p[k] += x0) == 0.0)
     {
     {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0; ) ; p[n - 1] = 0.0;}
     if (p[k] == 0.0)
      break;
     }
    x0 = p[k];
    _Dunscale(&xexp, &x0);
    if (prevexp - mybits < xexp)
     {
     _Dint(&x0, (short)(xexp - (prevexp - mybits)));
     _Dscale(&x0, xexp);
     if ((p[k] -= x0) == 0.0)
      {
      {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0; ) ; p[n - 1] = 0.0;}
      }
     if (--k == 0)
      prevexp = (2 * 1024);
     else
      {
      xscaled = p[k - 1];
      _Dunscale(&yexp, &xscaled);
      prevexp = yexp;
      }
     }
    else if (k + 1 == n)
     break;
    else
     {
     x0 = p[k];
     _Dunscale(&yexp, &p[k]);
     _Dint(&p[k], (53 / 2));
     _Dscale(&p[k], yexp);
     x0 -= p[k];
     prevexp = yexp;
     xscaled = x0 != 0.0 ? x0 : p[k];
     _Dunscale(&xexp, &xscaled);
     ++k;
     }
    }
   }
  }
 return (p);
 }
double *_Xp_mulh(double *p, int n, double x0)
 {
// printf("_Xp_mulh(%.17g[0],%17.g[1],%d,%.17g)\n",p[0],p[1],n,x0);
 short errx;
 int j, k;
 double buf[4];
 if (0 < n)
  {
  buf[0] = p[0] * x0;
  if (0 <= (errx = _Dtest(&buf[0])))
   {
   if (errx == 2)
    _Feraise(0x01);
   p[0] = buf[0];
   if (0 < errx && 1 < n)
    p[1] = 0.0;
   return (p);
   }
  p[0] = 0.0;
  }
 for (j = 1, k = 0; k < n; ++k, --j)
  {
  for (; j < 4; ++j)
   if (k + j < n && p[k + j] != 0.0)
    {
    buf[j] = p[k + j] * x0;
    p[k + j] = 0.0;
    }
   else
    {
    buf[j] = 0.0;
    j = 2 * 4;
    break;
    }
  if (buf[0] == 0.0)
   break;
  else
   {
   int i = 0;
   double y0 = buf[0];
   short xexp;
   _Dunscale(&xexp, &y0);
   _Dint(&y0, (53 / 2));
   _Dscale(&y0, xexp);
   _Xp_addh(p, n, y0);
   _Xp_addh(p, n, buf[0] - y0);
   for (; ++i < j; )
    if ((buf[i - 1] = buf[i]) == 0.0)
     break;
   }
  }
 return (p);
 }
double *_Xp_setn(double *p, int n, long x)
 {
 _Xp_setw(p, n, (double)x);
 return (p);
 }
double *_Xp_movx(double *p, int n, const double *q)
 {
	memcpy_HighTecARMImpl(p, q, n * sizeof (double));
 return (p);
 }
double *_Xp_addx(double *p, int n,
 const double *q, int m)
 {
 int k;
 for (k = 0; k < m && q[k] != 0.0; ++k)
  _Xp_addh(p, n, q[k]);
 return (p);
 }
double *_Xp_subx(double *p, int n,
 const double *q, int m)
 {
 int k;
 for (k = 0; k < m && q[k] != 0.0; ++k)
  _Xp_addh(p, n, -q[k]);
 return (p);
 }
double *_Xp_ldexpx(double *p, int n, int m)
 {
 int k;
 for (k = 0; k < n; ++k)
  {
  p[k] = ldexp(p[k], m);
  if (p[k] == 0.0)
   break;
  }
 return (p);
 }
double *_Xp_mulx(double *p, int n,
 const double *q, int m,
 double *ptemp2)
 {
 if (n == 0 || m == 0)
  ;
 else if (q[0] == 0.0 || q[1] == 0.0)
  _Xp_mulh(p, n, q[0]);
 else
  {
  double *px = ptemp2;
  double *pac = ptemp2 + n;
  int j;
  _Xp_movx(px, n, p);
  _Xp_mulh(p, n, q[0]);
  for (j = 1; j < m && q[j] != 0.0; ++j)
   {
   _Xp_movx(pac, n, px);
   _Xp_mulh(pac, n, q[j]);
   _Xp_addx(p, n, pac, n);
   }
  }
 return (p);
 }
double *_Xp_invx(double *p, int n, double *ptemp4)
 {
 short errx;
 if (n == 0)
  ;
 else if (0 <= (errx = _Dtest(&p[0])))
  {
  if (errx == 1)
   p[0] = 0.0;
  else if (errx == 0)
   p[0] = _Inf._Double;
  }
 else
  {
  double *pac = ptemp4;
  double *py = ptemp4 + n;
  double *ptemp2 = py + n;
  double x0 = p[0];
  int k;
  _Xp_movx(py, n, p);
  _Xp_mulh(py, n, -1.0);
  if (1 < n)
   x0 += p[1];
  _Xp_setw(p, n, (1.0 / (x0)));
  for (k = 1; k < n; k <<= 1)
   {
   _Xp_movx(pac, n, p);
   _Xp_mulx(pac, n, py, n, ptemp2);
   _Xp_addh(pac, n, 1.0);
   _Xp_mulx(pac, n, p, n, ptemp2);
   _Xp_addx(p, n, pac, n);
   }
  }
 return (p);
 }
double *_Xp_sqrtx(double *p, int n, double *ptemp4)
 {
 if (n == 0)
  ;
 else if (0 <= _Dtest(&p[0]) || p[0] < 0.0)
  {
  if (p[0] < 0.0)
   {
   _Feraise(0x01);
   p[0] = _Nan._Double;
   }
  }
 else
  {
  double *pac = ptemp4;
  double *py = ptemp4 + n;
  double *ptemp2 = py + n;
  double x0 = p[0];
  int k;
  if (1 < n)
   x0 += p[1];
  _Xp_setw(py, n, (1.0 / (sqrt(x0))));
  for (k = 2; k < n; k <<= 1)
   {
   _Xp_movx(pac, n, py);
   _Xp_mulh(pac, n, -0.5);
   _Xp_mulx(pac, n, p, n, ptemp2);
   _Xp_mulx(pac, n, py, n, ptemp2);
   _Xp_addh(pac, n, 1.5);
   _Xp_mulx(py, n, pac, n, ptemp2);
   }
  _Xp_mulx(p, n, py, n, ptemp2);
  }
 return (p);
 }
