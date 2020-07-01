#include "defs.h"

short _Dscale(double *px, long lexp)
 {
 return (_Dscalex(px, lexp, 4));
 }

short _Dscalex(double *px, long lexp, int round_mode)
 {
 _Dval *ps = (_Dval *)(char *)px;
 short xchar = (short)((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) >> 4);

 if (xchar == ((unsigned short)((1 << (15 - 4)) - 1)))
  return ((short)((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2] != 0
   || ps->_Sh[1] != 0 || ps->_Sh[0] != 0 ? 2 : 1));
 else if (xchar == 0 && 0 < (xchar = _Dnorm(ps)))
  return (0);

 if (0 < lexp && ((unsigned short)((1 << (15 - 4)) - 1)) - xchar <= lexp)
  {
  *px = ps->_Sh[3] & ((unsigned short)0x8000) ? -_Inf._Double : _Inf._Double;
  return (1);
  }
 else if (-xchar < lexp)
  {
  ps->_Sh[3] = (unsigned short)((ps->_Sh[3] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1)))))
   | (lexp + xchar) << 4);
  return ((-1));
  }
 else
  {
  unsigned short sign = (unsigned short)(ps->_Sh[3] & ((unsigned short)0x8000));

  ps->_Sh[3] = (unsigned short)(1 << 4 | (ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))));
  lexp += xchar - 1;
  if (lexp < -(48 + 1 + 4) || 0 <= lexp)
   {
   ps->_Sh[3] = sign;
   ps->_Sh[2] = 0;
   ps->_Sh[1] = 0;
   ps->_Sh[0] = 0;
   return (0);
   }
  else
   {
   short xexp = (short)lexp;
   unsigned short psx = 0;

   for (; xexp <= -16; xexp += 16)
    {
    psx = ps->_Sh[0] | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = ps->_Sh[1];
    ps->_Sh[1] = ps->_Sh[2];
    ps->_Sh[2] = ps->_Sh[3];
    ps->_Sh[3] = 0;
    }
   if ((xexp = (short)-xexp) != 0)
    {
    psx = (ps->_Sh[0] << (16 - xexp)) | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> xexp
     | ps->_Sh[1] << (16 - xexp));
    ps->_Sh[1] = (unsigned short)(ps->_Sh[1] >> xexp
     | ps->_Sh[2] << (16 - xexp));
    ps->_Sh[2] = (unsigned short)(ps->_Sh[2] >> xexp
     | ps->_Sh[3] << (16 - xexp));
    ps->_Sh[3] >>= xexp;
    }

   ps->_Sh[3] |= sign;
   if (psx != 0)
    {
    int roundup = 0;

    if (round_mode == 4)
     round_mode = __builtin_flt_rounds;
     //OS: used defined found somewhere instead of: round_mode = (__builtin_flt_rounds());
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
     && (++ps->_Sh[0] & 0xffff) == 0
     && (++ps->_Sh[1] & 0xffff) == 0
     && (++ps->_Sh[2] & 0xffff) == 0)
     ++ps->_Sh[3];
    }

   if (ps->_Sh[3] == sign
    && ps->_Sh[2] == 0
    && ps->_Sh[1] == 0
    && ps->_Sh[0] == 0)
    return (0);
   else
    return ((-1));
   }
  }
 }
