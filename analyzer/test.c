typedef long unsigned int _Sizet;
typedef _Sizet size_t;

typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


short _FDint(float *px, short xexp)
 {
 _Fval *ps = (_Fval *)(char *)px;

 ps->_Sh[0] = 0x2000;

 if (ps->_Sh[1] == 255) return 2;

  return 99;
}





float (roundf)(float x)
 {
 switch (_FDint(&x, 1))
  {
 case 2:
 case 1:
  break;

 default:
  if (_FDint(&x, 0) == 0)
   ;
  else if ((((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)))
   x -= 1.0F;
  else
   x += 1.0F;
  break;
  }
 return (x);
 }

