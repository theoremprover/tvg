#include "defs.h"

short _Dunscale(short *pex, double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;
 short xchar = (ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) >> 4;

 if (xchar == ((unsigned short)((1 << (15 - 4)) - 1)))
  {
  *pex = 0;
  return ((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2] != 0
   || ps->_Sh[1] != 0 || ps->_Sh[0] != 0 ? 2 : 1);
  }
 else if (0 < xchar || (xchar = _Dnorm(ps)) <= 0)
  {
  ps->_Sh[3] = (ps->_Sh[3] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) | 0x3fe << 4;
  *pex = xchar - 0x3fe;
  return ((-1));
  }
 else
  {
  *pex = 0;
  return (0);
  }
 }
