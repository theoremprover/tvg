
typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


short _FDscale(float *px, long lexp)
 {
 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (short)((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7);
 //solver_debug_Short("xchar",xchar);
 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1))) return 1; else return 2;
}

short f(float x)
{
  x = 0.0F/0.0F;
  float y = 0.09977F * x;

  return (_FDscale(&y, 1));
}
