
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


/*
ASSN ( (ushort []) (*px) ) [0] = 49168
COND return_val == ( (ushort []) (*px) ) [0]

  |
  v

-- new temp array PTR_px$$$1 for *px:
ASSN PTR_px$$$1 -> PTR_px$$$1 [0] = (ushort) 49168
ASSN ( (ushort []) (*px) ) = PTR_px$$$1

COND return_val == ( (ushort []) (*px) ) [0]

  |
  v



*/