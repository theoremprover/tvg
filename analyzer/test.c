
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
--- TRACE after Initial [] -----------
<leaving out builtins...>
DECL _FDint :: short (float * px, short xexp)
DECL px :: float *
DECL PTR_px :: float
DECL xexp :: short
DECL ps$1 :: union $18 *

ASSN ps$1 = ((union*) ((Z3_Ptr (Z3_BitVector 8 True)) px))
ASSN (ps$1  -> _Sh)[0] = ((Z3_BitVector 16 True) 49168)
DECL return_val :: short
COND Nothing (return_val == (ps$1  -> _Sh)[0])
RET  (ps$1  -> _Sh)[0]

--- TRACE after elimInds [] -----------
<leaving out builtins...>
DECL _FDint :: short (float * px, short xexp)
DECL px :: float *
DECL PTR_px :: float
DECL xexp :: short
DECL ps$1 :: union $18 *
ASSN ((union *) ((Z3_Ptr (Z3_BitVector 8 True)) px))  -> _Sh)[0] = ((Z3_BitVector 16 True) 49168)
DECL return_val :: short
COND Nothing (return_val == ((union *) ((Z3_Ptr (Z3_BitVector 8 True)) px))  -> _Sh)[0])
RET  ((union *) ((Z3_Ptr (Z3_BitVector 8 True)) px))  -> _Sh)[0]

--- TRACE after 1. simplifyTraceM [] -----------
<leaving out builtins...>
DECL _FDint :: short (float * px, short xexp)
DECL px :: float *
DECL PTR_px :: float
DECL xexp :: short
DECL ps$1 :: union $18 *
ASSN ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0] = ((Z3_BitVector 16 True) 49168)
DECL return_val :: short
COND Nothing (return_val == ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0])
RET  ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0]

--- TRACE after 1. elimAssignmentsM [] -----------
<leaving out builtins...>
DECL _FDint :: short (float * px, short xexp)
DECL px :: float *
DECL PTR_px :: float
DECL xexp :: short
DECL ps$1 :: union $18 *
ASSN ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0] = ((Z3_BitVector 16 True) 49168)
DECL return_val :: short
COND Nothing (return_val == ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0])
RET  ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0]

--- TRACE after elimArrayAssignsM [] -----------
<leaving out builtins...>
DECL _FDint :: short (float * px, short xexp)
DECL px :: float *
DECL PTR_px :: float
DECL xexp :: short
DECL ps$1 :: union $18 *
DECL PTR_px$$$1 :: unsigned short [sizeof(float) / sizeof(short)]
ASSN PTR_px$$$1->PTR_px$$$1[0] = ((Z3_BitVector 16 True) 49168)
ASSN ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px)) = PTR_px$$$1
DECL return_val :: short
COND Nothing (return_val == ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0])
RET  ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0]

--- TRACE after 2. elimAssignmentsM [] -----------
<leaving out builtins...>
DECL _FDint :: short (float * px, short xexp)
DECL px :: float *
DECL PTR_px :: float
DECL xexp :: short
DECL ps$1 :: union $18 *
DECL PTR_px$$$1 :: unsigned short [sizeof(float) / sizeof(short)]
ASSN PTR_px$$$1->PTR_px$$$1[0] = ((Z3_BitVector 16 True) 49168)
DECL return_val :: short
COND Nothing (return_val == ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0])
RET  ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0]

--- TRACE after 2. simplifyTraceM [] -----------
<leaving out builtins...>
DECL _FDint :: short (float * px, short xexp)
DECL px :: float *
DECL PTR_px :: float
DECL xexp :: short
DECL ps$1 :: union $18 *
DECL PTR_px$$$1 :: unsigned short [sizeof(float) / sizeof(short)]
ASSN PTR_px$$$1->PTR_px$$$1[0] = ((Z3_BitVector 16 True) 49168)
DECL return_val :: short
COND Nothing (return_val == ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0])
RET  ((Z3_Array (Z3_BitVector 16 True) Nothing) (* px))[0]

--- TRACE after createSymbolicVarsM [] -----------
<leaving out builtins...>
DECL _FDint :: short (float * px, short xexp)
DECL px :: float *
DECL PTR_px :: float
DECL xexp :: short
DECL ps$1 :: union $18 *
DECL PTR_px$$$1 :: unsigned short [sizeof(float) / sizeof(short)]
ASSN PTR_px$$$1->PTR_px$$$1[0] = ((Z3_BitVector 16 True) 49168)
DECL return_val :: short
COND Nothing (return_val == ((Z3_Array (Z3_BitVector 16 True) Nothing) PTR_px)[0])
RET  ((Z3_Array (Z3_BitVector 16 True) Nothing) PTR_px)[0]*/