#include "defs.h"
//#include <stdio.h>

short _Dtest(double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;
  
 if ((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) == ((unsigned short)((1 << (15 - 4)) - 1)) << 4)
  return ((short)((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2] != 0
   || ps->_Sh[1] != 0 || ps->_Sh[0] != 0 ? 2 : 1));
 else if ((ps->_Sh[3] & ~((unsigned short)0x8000)) != 0 || ps->_Sh[2] != 0
  || ps->_Sh[1] != 0 || ps->_Sh[0] != 0)
  return ((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) == 0 ? (-2) : (-1));
 else
  return (0);
 }

unsigned short *_Plsw(double *px)
 {
 return (&((_Dval *)(char *)px)->_Sh[0]);
 }

unsigned short *_Pmsw(double *px)
 {
 return (&((_Dval *)(char *)px)->_Sh[3]);
 }
