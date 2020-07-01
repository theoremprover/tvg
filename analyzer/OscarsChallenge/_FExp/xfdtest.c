#include "defs.h"
#include <stdio.h>

short _FDtest(float *px)
 {
 
 _Fval *ps = (_Fval *)(char *)px;
 //printf("_FDtest: *px = %f ( = 0x%x )\n", ps->_Val, toRep(ps->_Val));
unsigned short B1 = ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))));
unsigned short B2 = ((unsigned short)((1 << (15 - 7)) - 1)) << 7;
unsigned short B3 = ((unsigned short)((1 << 7) - 1));
unsigned short B4 = ~((unsigned short)0x8000);
unsigned short B5 = ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))));
 /* 
 printf("_FDtest: Bitmask B1 = 0x%x\n", B1);
 printf("_FDtest: ps->_Sh[0] = %x\n", ps->_Sh[0]);
 printf("_FDtest: ps->_Sh[1] = %x\n", ps->_Sh);
 printf("_FDtest: ps->_Sh[1] & B1 = 0x%x\n",((ps->_Sh[1] & B1)));
 printf("_FDtest: Bitmask B2 = 0x%x\n", B2);
 printf("_FDtest: (ps->_Sh[1] & B1 == B2) == %i\n", (ps->_Sh[1] & B1) == B2);
 printf("_FDtest: Bitmask B3 = 0x%x\n", B3);
 printf("_FDtest: ps->_Sh[1] & B3 = 0x%x\n",((ps->_Sh[1] & B3)));
 printf("_FDtest: Bitmask B4 = 0x%x\n", B4);
 printf("_FDtest: ps->_Sh[1] & B4 = 0x%x\n",((ps->_Sh[1] & B4)));
 printf("_FDtest: Bitmask B5 = 0x%x\n", B5);
 printf("_FDtest: ps->_Sh[1] & B5 = 0x%x\n",((ps->_Sh[1] & B5)));
 */
 if ((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) == ((unsigned short)((1 << (15 - 7)) - 1)) << 7)
  return ((short)((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1));
 else if ((ps->_Sh[1] & ~((unsigned short)0x8000)) != 0 || ps->_Sh[0] != 0)
  return ((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) == 0 ? (-2) : (-1));
 else
  {
    printf("FDtest: returning 0\n");
    return (0);
  }
 }

unsigned short *_FPlsw(float *px)
 {
 return (&((_Fval *)(char *)px)->_Sh[0]);
 }

unsigned short *_FPmsw(float *px)
 {
 return (&((_Fval *)(char *)px)->_Sh[1]);
 }
