


typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } __attribute__ ((__may_alias__)) _Dconst;


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

float (fabsf)(float x)
 {
_Dconst _FInf = {{0, ((unsigned short)((1 << (15 - 7)) - 1)) << 7}};

 switch (_FDtest(&x))
  {
 case 2:
  return (x);
 case 1:
  return (_FInf._Float);
 case 0:
  return (0.0F);
 default:
  return (x < 0.0F ? -x : x);
  }
 }

