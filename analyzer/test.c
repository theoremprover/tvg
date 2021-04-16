

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

int (feraiseexcept)(int except)
 {
          return 0;
        }

void (_Feraise)(int except)
 {

int __attribute__((fardata)) _Errno;

 int errh = (1 | 2);

 if ((errh & 2) != 0)
  {
  if ((except & (0x08 | 0x10)) != 0)
   except |= 0x20;
  feraiseexcept(except);
  }

 if ((errh & 1) == 0)
  ;
 else if ((except & 0x01) != 0)
  ( _Errno) = 0x0021;
 else if ((except & (0x04 | 0x10 | 0x08)) != 0)
  ( _Errno) = 0x0022;
}


short _FDnorm(_Fval *ps)
 {
 short xchar;
 unsigned short sign = (unsigned short)(ps->_Sh[1] & ((unsigned short)0x8000));

 xchar = 1;
 ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1));
 if (ps->_Sh[1] != 0 || ps->_Sh[0])
  {
  if (ps->_Sh[1] == 0)
    ps->_Sh[1] = ps->_Sh[0], ps->_Sh[0] = 0, xchar -= 16;
  for (;solver_pragma(0,1) && (ps->_Sh[1] < 1 << 7); --xchar)
   {
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] << 1
    | ps->_Sh[0] >> 15);
   ps->_Sh[0] <<= 1;
   }
  for (;solver_pragma(0,1) && (1 << (7 + 1) <= ps->_Sh[1]); ++xchar)
   {
   ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> 1
    | ps->_Sh[1] << 15);
   ps->_Sh[1] >>= 1;
   }
  ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1));
  }
 ps->_Sh[1] |= sign;
 return (xchar);
 }



short _FDunscale(short *pex, float *px)
 {
 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7;

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  {
  *pex = 0;
  return ((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1);
  }
 else if (0 < xchar || (xchar = _FDnorm(ps)) <= 0)
  {
  ps->_Sh[1] = ps->_Sh[1] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1)))) | 0x7e << 7;
  *pex = xchar - 0x7e;
  return ((-1));
  }
 else
  {
  *pex = 0;
  return (0);
  }
 }


short ftest(_Fval *ps)
 {
 ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1));
 return (1);
 }


short _FDscale(float *px, long lexp)
 {
 _Dconst _FInf = {{0, ((unsigned short)((1 << (15 - 7)) - 1)) << 7}};

 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (short)((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7);

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  return ((short)((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1));
 else
 {

 short xchar_old = xchar;
 ftest(ps);//xchar = _FDnorm(ps);

 if(xchar_old!=0) return 99;

 if (xchar_old == 0 && 0 < xchar)
  return (0);
 }

 if (0 < lexp && ((unsigned short)((1 << (15 - 7)) - 1)) - xchar <= lexp)
  {
  *px = ps->_Sh[1] & ((unsigned short)0x8000) ? -_FInf._Float : _FInf._Float;
  return (1);
  }
 else if (-xchar < lexp)
  {
  ps->_Sh[1] = (unsigned short)(ps->_Sh[1] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))
   | (lexp + xchar) << 7);
  return ((-1));
  }
 else
  {
  unsigned short sign = (unsigned short)(ps->_Sh[1] & ((unsigned short)0x8000));

  ps->_Sh[1] = (unsigned short)(1 << 7 | ps->_Sh[1] & ((unsigned short)((1 << 7) - 1)));
  lexp += xchar - 1;
  if (lexp < -(16 + 1 + 7) || 0 <= lexp)
   {
   ps->_Sh[1] = sign;
   ps->_Sh[0] = 0;
   return (0);
   }
  else
   {
   short xexp = (short)lexp;
   unsigned short psx = 0;

   if (xexp <= -16)
    {
    psx = ps->_Sh[0] | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = ps->_Sh[1];
    ps->_Sh[1] = 0;
    xexp += 16;
    }
   if ((xexp = (short)-xexp) != 0)
    {
    psx = (ps->_Sh[0] << (16 - xexp)) | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> xexp
     | ps->_Sh[1] << (16 - xexp));
    ps->_Sh[1] >>= xexp;
    }

   ps->_Sh[1] |= sign;
   if (0x8000 < psx
    || 0x8000 == psx && (ps->_Sh[0] & 0x0001) != 0)
    {
      ps->_Sh[0]++;
      if((ps->_Sh[0] & 0xffff) == 0)
        ++ps->_Sh[1];
      else if (ps->_Sh[1] == sign && ps->_Sh[0] == 0)
        return (0);
    }
   else if (ps->_Sh[1] == sign && ps->_Sh[0] == 0)
    return (0);
   return ((-1));
   }
  }
 }



float (sqrtf)(float x)
 {
   _Dconst _FNan = {{0, (((unsigned short)((1 << (15 - 7)) - 1)) << 7) | (1 << (7 - 1))}
      };

 short xexp;
 float y;

 switch (_FDunscale(&xexp, &x))
  {
 case 2:
 case 0:
  return (x);
 case 1:
  if (!(((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)))
   return (x);
 default:
  if ((((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)))
   {
   _Feraise(0x01);
   return (_FNan._Float);
   }
  if ((unsigned int)xexp & 1)
   x *= 2.0F, --xexp;
  y = (-0.09977F * x + 0.71035F) * x
   + 0.38660F;
  y += x / y;
  y = 0.25F * y + x / y;
  _FDscale(&y, xexp / 2);
  return (y);
  }
 }
