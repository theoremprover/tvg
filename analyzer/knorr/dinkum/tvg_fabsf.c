


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

/*
                   _Dconst _FDenorm = {{1, 0}};
                   _Dconst _FEps = {
 {0, (0x7e - (16 + 7) - 1) << 7}};
                   _Dconst _FInf = {{0, ((unsigned short)((1 << (15 - 7)) - 1)) << 7}};
                   _Dconst _FNan = {{0, (((unsigned short)((1 << (15 - 7)) - 1)) << 7) | (1 << (7 - 1))}
                      };
                   _Dconst _FSnan = {{1, ((unsigned short)((1 << (15 - 7)) - 1)) << 7}};
                   _Dconst _FRteps = {
 {0, (0x7e - (16 + 7) / 2) << 7}};

                   float _FXbig = ((16 + 7) + 1) * 347L / 1000;
                   float _FZero = 0.0F;

*/
/*

float (fabsf)(float x)
 {
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
*/

/*
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

*/

short _FDtest(float *px)
 {
 _Fval *ps = (_Fval *)(char *)px;
// NaN = 0x7f800001 => _Sh[0]=0001, _Sh[1]=7f80
// NaN = 0x7f810000 => _Sh[0]=0000, _Sh[1]=7f81

// Inf = 0x7f800000 => _Sh[0]=0000, _Sh[1]=7f80

    if (
        (ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) ==
        ((unsigned short)((1 << (15 - 7)) - 1)) << 7)
    {
        // _Sh[1]
        if((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0)
        {
//            solver_debug(ps->_Sh[0]);
//            solver_debug(ps->_Sh[1]);
            return 2;
        }
        else
        {
            return 1;
        }
    }
  return (0);
 }
