#include "defs.h"

/*
typedef union
 {
 unsigned short _Sh[8];
 double _Val;
 } _Dval;
*/

short _Dtest(double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;
  
 if ((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) == ((unsigned short)((1 << (15 - 4)) - 1)) << 4)
  return ((short)((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2] != 0));
  return (0);
 }
