#include "defs.h"

double (fma)(double x, double y, double z)
 {
 short errx, erry, errz;
 short expx, expy, expz;
 double ans;
 double xp = x, yp = y, zp = z;
 if ((errx = _Dunscale(&expx, &xp)) == 2)
  ans = x;
 else if ((erry = _Dunscale(&expy, &yp)) == 2)
  ans = y;
 else if ((errz = _Dunscale(&expz, &zp)) == 2)
  ans = z;
 else if (errx == 1 || erry == 1)
  if (errx == 0 || erry == 0
   || (errz == 1 && (((*_Pmsw(&(x))) & ((unsigned short)0x8000)) ^ ((*_Pmsw(&(y))) & ((unsigned short)0x8000))) != ((*_Pmsw(&(z))) & ((unsigned short)0x8000))))
   {
   _Feraise(0x01);
   ans = _Nan._Double;
   }
  else if (errz == 1)
   ans = z;
  else if ((((*_Pmsw(&(x))) & ((unsigned short)0x8000)) ^ ((*_Pmsw(&(y))) & ((unsigned short)0x8000))) == 0)
   ans = _Inf._Double;
  else
   ans = -_Inf._Double;
 else if (errz == 1 || errx == 0 || erry == 0)
  ans = z;
 else if (errz == 0)
  ans = x * y;
 else
  {
  long exp_prod = (long)expx + expy;
  long exp_diff = expz - exp_prod;
  double xpx[4], xpy[2], xpz[2];

  if (2 * 53 < exp_diff)
   {
   _Xp_setw(xpz, 2, zp);
   exp_prod = expz;

   xp = (xp < 0.0) ^ (yp < 0.0)
    ? -2.2250738585072014e-308 : 2.2250738585072014e-308;
   _Xp_setw(xpx, 4, xp);
   }
  else
   {
   if (exp_diff < -2 * 53)
    zp = zp < 0.0
     ? -2.2250738585072014e-308 : 2.2250738585072014e-308;
   else
    _Dscale(&zp, exp_diff);
   _Xp_setw(xpz, 2, zp);

   _Xp_setw(xpx, 4, xp);
   _Xp_setw(xpy, 2, yp);
   _Xp_mulh(xpx, 4, xpy[0]);
   if (xpy[1] != 0.0)
    {
    double xpw[4];

    _Xp_setw(xpw, 4, xp);
    _Xp_mulh(xpw, 4, xpy[1]);
    _Xp_addx(xpx, 4, xpw, 4);
    }
   }
  _Xp_addx(xpx, 4, xpz, 2);
  ans = _Xp_getw(xpx, 4);
  if (exp_prod != 0)
   _Dscale(&ans, exp_prod);

  if (_Dtest(&ans) != 1)
   ;
  else if (0.0 < ans)
   ans = 1.7976931348623157e+308 + 1.7976931348623157e+308;
  else
   ans = -(1.7976931348623157e+308 + 1.7976931348623157e+308);
  }
 return (ans);
 }
