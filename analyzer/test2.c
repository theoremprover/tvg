
typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } __attribute__ ((__may_alias__)) _Dconst;


short _FDscale(float *px, long lexp)
 {
 solver_debug_Float("*px",*px);

 _Fval *ps = (_Fval *)(char *)px;
 solver_debug_UShort("ps->_Sh[0]",ps->_Sh[0]);
 solver_debug_UShort("ps->_Sh[1]",ps->_Sh[1]);
 unsigned short xchar0 = (ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7;
 solver_debug_UShort("xchar0",xchar0);
 short xchar = (short)xchar0;

 solver_debug_Short("xchar",xchar);

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
 {
  return ((short)((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1));
 }

  return 0;
}

short f(float x)
{
  float y = 0.09977F * x;

  return (_FDscale(&y, 1));
}
