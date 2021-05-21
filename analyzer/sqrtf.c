/*
---- Trace [1,3,1,3,3,2,1,1,2,2,1] -----------------------------------

sqrtf ( x = -Infinity = 0xff800000 = -Infinity )
    = return_val = 0x0p+0 = 0x00000000 = 0.0

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Else branch 3 "(F||F)" at line 114, col 13, len 70
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Else branch 3 "(F||F)" at line 114, col 13, len 70
	Then branch 3 "case 1" at line 233, col 2, len 89
	Else branch 2 "if(!(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000))" at line 234, col 7, len 59
	Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 237, col 9, len 55
	Then branch 1 "if((errh & 2) != 0)" at line 59, col 7, len 14
	Else branch 2 "if((0x1 & (0x8 | 0x10)) != 0)" at line 61, col 8, len 28
	Else branch 2 "if((errh & 1) == 0)" at line 66, col 7, len 14
	Then branch 1 "if((0x1 & 0x1) != 0)" at line 68, col 12, len 19

checkSolutionM ERROR for return_val : exec_val=NaN(0x400000) = 0x7fc00000 = NaN /= predicted_result=0x0p+0 = 0x00000000 = 0.0









*/

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

int (feraiseexcept)(int except)
 {
          return 0;
        }

void (_Feraise)(int except)
 {

int __attribute__((fardata)) _Errno;

 int errh = (1 | 2);

 if (solver_pragma(1) && (errh & 2) != 0) //Then branch 1 "if((errh & 2) != 0)" at line 59, col 7, len 14
  {
  if (solver_pragma(2) && (except & (0x08 | 0x10)) != 0) //Else branch 2 "if((0x1 & (0x8 | 0x10)) != 0)" at line 61, col 8, len 28
     except |= 0x20;
  feraiseexcept(except);
  }

 if (solver_pragma(2) && (errh & 1) == 0) //Else branch 2 "if((errh & 1) == 0)" at line 66, col 7, len 14
  ;
 else if (solver_pragma(1) && (except & 0x01) != 0) //Then branch 1 "if((0x1 & 0x1) != 0)" at line 68, col 12, len 19
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
  for (;solver_pragma(0,1,2) && (ps->_Sh[1] < 1 << 7); --xchar)
   {
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] << 1
    | ps->_Sh[0] >> 15);
   ps->_Sh[0] <<= 1;
   }
  for (;solver_pragma(0,1,2) && (1 << (7 + 1) <= ps->_Sh[1]); ++xchar)
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

 if (solver_pragma(1) && xchar == ((unsigned short)((1 << (15 - 7)) - 1))) //Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
  {
  *pex = 0;
  return ((solver_pragma(3) && ((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0 ))
   ? 2 : 1); //Else branch 3 "(F||F)" at line 114, col 12, len 70
  }
 else
  {
    solver_error();
    short xchar1;
    if(0>=xchar) xchar1 = _FDnorm(ps);
    if (0 < xchar || xchar1 <= 0)
    {
      if(0>=xchar) xchar = xchar1;
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
 }


short _FDscale(float *px, long lexp)
 {
 _Dconst _FInf = { { 0, ((unsigned short)((1 << (15 - 7)) - 1)) << 7 } };

 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (short)((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7);

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  return ((short)((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1));
 else
 {

 short xchar_old = xchar;
 if(xchar_old==0) xchar = _FDnorm(ps);

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
  _Dconst _FNan = {{0, (((unsigned short)((1 << (15 - 7)) - 1)) << 7) | (1 << (7 - 1))} };

 short xexp;
 float y;

/*
    switch(solver_pragma(2,3,1) && ...
    case 1: <-> if(solver_pragma(2,2,1))
    case 2: <-> if(solver_pragma(1,2)
    default: <->
*/

 switch (solver_pragma(3) && _FDunscale(&xexp, &x))
  {
 case 2:
 case 0:
  return (x);
 case 1: //Then branch 3 "case 1" at line 232, col 2, len 91
  if (solver_pragma(2) && !(((_Fval *)(char *)&x)->_Sh[1] & ((unsigned short)0x8000))) //Else branch 2 "if(!(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000))" at line 233, col 7, len 59
   return (x);
 default:
  if (solver_pragma(1) && (((_Fval *)(char *)&x)->_Sh[1] & ((unsigned short)0x8000))) //Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
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
