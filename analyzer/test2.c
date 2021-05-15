
typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


short _FDscale(float *px, long lexp)
 {
 _Fval *ps = (_Fval *)(char *)px;

 return(ps->_Sh[0]);
}

short f(float x)
{
  float y = 0.09977F * x;

  return (_FDscale(&y, 1));
}
