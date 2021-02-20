typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;

short f(float *px)
 {
 _Fval *ps = (_Fval *)(char *)px;
 if(ps->_Sh[1] == 1) return 0;
 else return 1;
}
