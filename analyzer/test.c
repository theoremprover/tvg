
typedef union
 {
 unsigned short _Sh[sizeof(double)/sizeof(short)];
 double _Val;
 } __attribute__ ((__may_alias__)) _Dval;


short _Dtest(double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;

 if ((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) == ((unsigned short)((1 << (15 - 4)) - 1)) << 4)
  return ( (short) (
    ps->_Sh[0] != 0
    ? 2 : 1)
    );
    return 0;
 }

