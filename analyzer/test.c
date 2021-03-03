
typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


short _FDint(float *px, short xexp)
 {
     _Fval *ps = (_Fval *)(char *)px;

        ps->_Sh[0] = 0xc010;

         return (ps->_Sh[0]);
 }

