

typedef union
 {
 unsigned short _Sh[sizeof(double)/sizeof(short)];
 double _Val;
 } __attribute__ ((__may_alias__)) _Dval;

typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


short _FDtest(float *px)
 {
 _Fval *ps = (_Fval *)(char *)px;

 if ((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) == ((unsigned short)((1 << (15 - 7)) - 1)) << 7)
  return ((short)((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1));
 else if ((ps->_Sh[1] & ~((unsigned short)0x8000)) != 0 || ps->_Sh[0] != 0)
  return ((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) == 0 ? (-2) : (-1));
 else
  return (0);
 }

short _Dtest(double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;

 if ((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) == ((unsigned short)((1 << (15 - 4)) - 1)) << 4)
  return ((short)((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2] != 0
   || ps->_Sh[1] != 0 || ps->_Sh[0] != 0 ? 2 : 1));
 else
    if ((ps->_Sh[3] & ~(((_Dval *)(char *)&(*px))->_Sh[3] & ((unsigned short)0x8000))) != 0 || ps->_Sh[2] != 0
  || ps->_Sh[1] != 0 || ps->_Sh[0] != 0)
    return ((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) == 0 ? (-2) : (-1));
 else
  return (0);
 }


double (fmax)(double x, double y)
 {
 if (_Dtest(&x) == 2)
  return (y);
 else if (_Dtest(&y) == 2)
  return (x);
 else if (x < y || x == y && (((_Dval *)(char *)&(x))->_Sh[3] & ((unsigned short)0x8000)))
  return (y);
 else
  return (x);
 }



float (fmaxf)(float x, float y)
 {
 if (_FDtest(&x) == 2)
  return (y);
 else if (_FDtest(&y) == 2)
  return (x);
 else if (x < y || x == y && (((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)))
  return (y);
 else
  return (x);
 }



double (fmin)(double x, double y)
 {
 if (_Dtest(&x) == 2)
  return (y);
 else if (_Dtest(&y) == 2)
  return (x);
 else if (x < y || x == y && (((_Dval *)(char *)&(x))->_Sh[3] & ((unsigned short)0x8000)))
  return (x);
 else
  return (y);
 }



float (fminf)(float x, float y)
 {
 if (_FDtest(&x) == 2)
  return (y);
 else if (_FDtest(&y) == 2)
  return (x);
 else if (x < y || x == y && (((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)))
  return (x);
 else
  return (y);
 }


