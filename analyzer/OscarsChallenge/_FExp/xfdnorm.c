#include "defs.h"

short _FDnorm(_Fval *ps)
 {
 short xchar;
 unsigned short sign = (unsigned short)(ps->_Sh[1] & ((unsigned short)0x8000));

 xchar = 1;
 if ((ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0])
  {
  if (ps->_Sh[1] == 0)
   ps->_Sh[1] = ps->_Sh[0], ps->_Sh[0] = 0, xchar -= 16;
  for (; ps->_Sh[1] < 1 << 7; --xchar)
   {
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] << 1
    | ps->_Sh[0] >> 15);
   ps->_Sh[0] <<= 1;
   }
  for (; 1 << (7 + 1) <= ps->_Sh[1]; ++xchar)
   {
   ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> 1
    | ps->_Sh[1] << 15);
   ps->_Sh[1] >>= 1;
   }
  ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1));
  }
 ps->_Sh[1] |= sign;
 return (xchar);
 }
