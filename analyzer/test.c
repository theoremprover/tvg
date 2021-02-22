typedef union
 {
 unsigned short _Sh[sizeof(double)/sizeof(short)];
 double _Val;
 } __attribute__ ((__may_alias__)) _Dval;

short f(double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;
 return (ps->_Sh[0]);
}
