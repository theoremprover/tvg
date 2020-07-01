#include "defs.h"

short _Dnorm(_Dval *ps)
 {
 short xchar;
 unsigned short sign = (unsigned short)(ps->_Sh[3] & ((unsigned short)0x8000));

 xchar = 1;
 if ((ps->_Sh[3] &= ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2]
  || ps->_Sh[1] || ps->_Sh[0])
  {
  for (; ps->_Sh[3] == 0; xchar -= 16)
   {
   ps->_Sh[3] = ps->_Sh[2], ps->_Sh[2] = ps->_Sh[1];
   ps->_Sh[1] = ps->_Sh[0], ps->_Sh[0] = 0;
   }
  for (; ps->_Sh[3] < 1 << 4; --xchar)
   {
   ps->_Sh[3] = (unsigned short)(ps->_Sh[3] << 1
    | ps->_Sh[2] >> 15);
   ps->_Sh[2] = (unsigned short)(ps->_Sh[2] << 1
    | ps->_Sh[1] >> 15);
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] << 1
    | ps->_Sh[0] >> 15);
   ps->_Sh[0] <<= 1;
   }
  for (; 1 << (4 + 1) <= ps->_Sh[3]; ++xchar)
   {
   ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> 1
    | ps->_Sh[1] << 15);
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] >> 1
    | ps->_Sh[2] << 15);
   ps->_Sh[2] = (unsigned short)(ps->_Sh[2] >> 1
    | ps->_Sh[3] << 15);
   ps->_Sh[3] >>= 1;
   }
  ps->_Sh[3] &= ((unsigned short)((1 << 4) - 1));
  }
 ps->_Sh[3] |= sign;
 return (xchar);
 }
