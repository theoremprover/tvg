
typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } __attribute__ ((__may_alias__)) _Dconst;


float (sqrtf)(float x)
 {
  _Dconst _FNan = { {0, (((unsigned short)((1 << (15 - 7)) - 1)) << 7) | (1 << (7 - 1))} };

   return (_FNan._Float);
 }
