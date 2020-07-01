#include "defs.h"

#define ROUNDING_TBD	4

short _FDscale(float *px, long lexp)
 {
 return (_FDscalex(px, lexp, 4));
 }

short _FDscalex(float *px, long lexp, int round_mode)
 {
 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (short)((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7);

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  return ((short)((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1));
 else if (xchar == 0 && 0 < (xchar = _FDnorm(ps)))
  return (0);

 if (0 < lexp && ((unsigned short)((1 << (15 - 7)) - 1)) - xchar <= lexp)
  {
  *px = ps->_Sh[1] & ((unsigned short)0x8000) ? -_FInf._Float : _FInf._Float;
  return (1);
  }
 else if (-xchar < lexp)
  {
  ps->_Sh[1] = (unsigned short)((ps->_Sh[1] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1)))))
   | (lexp + xchar) << 7);
  return ((-1));
  }
 else
  {
  unsigned short sign = (unsigned short)(ps->_Sh[1] & ((unsigned short)0x8000));

  ps->_Sh[1] = (unsigned short)(1 << 7 | (ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))));
  lexp += xchar - 1;
  if (lexp < -(16 + 1 + 7) || 0 <= lexp)
   {
   ps->_Sh[1] = sign;
   ps->_Sh[0] = 0;
   return (0);
   }
  else
   {
   short xexp = (short)lexp;
   unsigned short psx = 0;

   if (xexp <= -16)
    {
    psx = ps->_Sh[0] | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = ps->_Sh[1];
    ps->_Sh[1] = 0;
    xexp += 16;
    }
   if ((xexp = (short)-xexp) != 0)
    {
    psx = (ps->_Sh[0] << (16 - xexp)) | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> xexp
     | ps->_Sh[1] << (16 - xexp));
    ps->_Sh[1] >>= xexp;
    }

   ps->_Sh[1] |= sign;
   if (psx != 0)
    {
    int roundup = 0;

    if (round_mode == 4)
     round_mode = __builtin_flt_rounds;
    switch (round_mode)
     {
    case 0:
     break;

    case 2:
     if (!sign)
      roundup = 1;
     break;

    case 3:
     if (sign)
      roundup = 1;
     break;

    default:
     if ((0x8000 < psx
      || (0x8000 == psx && (ps->_Sh[0] & 0x0001) != 0)))
      roundup = 1;
     break;
     }

    if (roundup
     && (++ps->_Sh[0] & 0xffff) == 0)
     ++ps->_Sh[1];
    }

   if (ps->_Sh[1] == sign
    && ps->_Sh[0] == 0)
    return (0);
   else
    return ((-1));
   }
  }
 }
