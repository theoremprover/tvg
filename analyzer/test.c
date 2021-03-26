

typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;



/*
C:\Data\tvg\analyzer>main.exe 10 0
_FDnorm(ps->_Sh[0] = 10, ps->_Sh[1] = 0, f2u(ps->_Val) = a) =
1.1.xchar=-15
2.1.xchar=-19
2.2.xchar=-19
-19
*/
short _FDnorm(_Fval *ps)
 {
 short xchar;

 xchar = 1;

  for (; (ps->_Sh[1] < 1 << 7); --xchar)
   {
     return 99;

   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] << 1 | ps->_Sh[0] >> 15);
   ps->_Sh[0] <<= 1;
   }
   return 100;

 }

