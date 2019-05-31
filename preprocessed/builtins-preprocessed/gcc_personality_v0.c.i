# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 369 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c" 2
# 12 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 1
# 71 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h"
# 1 "/toolchain/arm/include/limits.h" 1 3




# 1 "/toolchain/arm/include/yvals.h" 1 3


# 1 "/toolchain/arm/include/xkeycheck.h" 1 3
# 4 "/toolchain/arm/include/yvals.h" 2 3
# 462 "/toolchain/arm/include/yvals.h" 3
typedef long _Int32t;
typedef unsigned long _Uint32t;



  typedef int _Ptrdifft;
# 477 "/toolchain/arm/include/yvals.h" 3
typedef unsigned int _Sizet;
# 1151 "/toolchain/arm/include/yvals.h" 3
# 1 "/toolchain/arm/include/stdarg.h" 1 3
# 10 "/toolchain/arm/include/stdarg.h" 3
typedef __builtin_va_list va_list;
# 1152 "/toolchain/arm/include/yvals.h" 2 3
# 1278 "/toolchain/arm/include/yvals.h" 3
typedef long long _Longlong;
typedef unsigned long long _ULonglong;
# 1337 "/toolchain/arm/include/yvals.h" 3
typedef unsigned int _Wchart;
typedef unsigned int _Wintt;
# 1371 "/toolchain/arm/include/yvals.h" 3
typedef va_list _Va_list;
# 1394 "/toolchain/arm/include/yvals.h" 3
void _Atexit(void (*)(void));
# 1409 "/toolchain/arm/include/yvals.h" 3
typedef char _Sysch_t;
# 6 "/toolchain/arm/include/limits.h" 2 3
# 72 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2
# 1 "/toolchain/arm/include/stdint.h" 1 3
# 25 "/toolchain/arm/include/stdint.h" 3
typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int int64_t;

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef long long unsigned int uint64_t;

typedef signed char int_least8_t;
typedef short int_least16_t;
typedef int int_least32_t;
typedef long long int int_least64_t;

typedef unsigned char uint_least8_t;
typedef unsigned short uint_least16_t;
typedef unsigned int uint_least32_t;
typedef long long unsigned int uint_least64_t;

typedef signed char int_fast8_t;
typedef short int_fast16_t;
typedef int int_fast32_t;
typedef long long int int_fast64_t;

typedef unsigned char uint_fast8_t;
typedef unsigned short uint_fast16_t;
typedef unsigned int uint_fast32_t;
typedef long long unsigned int uint_fast64_t;

typedef int intptr_t;
typedef unsigned int uintptr_t;

typedef long long int intmax_t;
typedef long long unsigned int uintmax_t;
# 73 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2
# 1 "/toolchain/arm/include/stdbool.h" 1 3
# 74 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2
# 1 "/toolchain/arm/include/float.h" 1 3



# 1 "/toolchain/arm/include/ymath.h" 1 3



# 1 "/toolchain/arm/include/yvals.h" 1 3
# 5 "/toolchain/arm/include/ymath.h" 2 3
# 126 "/toolchain/arm/include/ymath.h" 3
void _Feraise(int);

typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } _Dconst;


double _Cosh(double, double);
short _Dtest(double *);
double _Sinh(double, double);
double _Divide(double, double);
short _Exp(double *, double, long);
double _Log(double, int);
double _Recip(double);
double _Sin(double, unsigned int);
double _Sinx(double, unsigned int, int);

extern const _Dconst _Denorm, _Hugeval, _Inf,
 _Nan, _Snan;


float _FCosh(float, float);
short _FDtest(float *);
float _FSinh(float, float);
float _FDivide(float, float);
short _FExp(float *, float, long);
float _FLog(float, int);
float _FRecip(float);
float _FSin(float, unsigned int);
float _FSinx(float, unsigned int, int);

extern const _Dconst _FDenorm, _FInf, _FNan, _FSnan;


long double _LCosh(long double, long double);
short _LDtest(long double *);
long double _LSinh(long double, long double);
long double _LDivide(long double, long double);
short _LExp(long double *, long double, long);
long double _LLog(long double, int);
long double _LRecip(long double);
long double _LSin(long double, unsigned int);
long double _LSinx(long double, unsigned int, int);

extern const _Dconst _LDenorm, _LInf, _LNan, _LSnan;
# 5 "/toolchain/arm/include/float.h" 2 3
# 75 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2



# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_types.h" 1
# 21 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_types.h"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_endianness.h" 1
# 22 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_types.h" 2





typedef int si_int;
typedef unsigned su_int;

typedef long long di_int;
typedef unsigned long long du_int;

typedef union
{
    di_int all;
    struct
    {

        su_int low;
        si_int high;




    }s;
} dwords;

typedef union
{
    du_int all;
    struct
    {

        su_int low;
        su_int high;




    }s;
} udwords;
# 117 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_types.h"
typedef union
{
    su_int u;
    float f;
} float_bits;

typedef union
{
    udwords u;
    double f;
} double_bits;

typedef struct
{

    udwords low;
    udwords high;




} uqwords;

typedef union
{
    uqwords u;
    long double f;
} long_double_bits;


typedef float _Complex Fcomplex;
typedef double _Complex Dcomplex;
typedef long double _Complex Lcomplex;
# 79 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2


# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_util.h" 1
# 25 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_util.h"
__attribute__((noreturn)) void compilerrt_abort_impl(const char *file, int line,
                                    const char *function);
# 82 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2

                si_int __paritysi2(si_int a);
                si_int __paritydi2(di_int a);

                di_int __divdi3(di_int a, di_int b);
                si_int __divsi3(si_int a, si_int b);
                su_int __udivsi3(su_int n, su_int d);

                su_int __udivmodsi4(su_int a, su_int b, su_int* rem);
                du_int __udivmoddi4(du_int a, du_int b, du_int* rem);
# 13 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c" 2

# 1 "/toolchain/lib/clang/7.0.1/include/unwind.h" 1 3 4
# 56 "/toolchain/lib/clang/7.0.1/include/unwind.h" 3 4
# 1 "/toolchain/arm/include/stdint.h" 1 3 4
# 57 "/toolchain/lib/clang/7.0.1/include/unwind.h" 2 3 4
# 66 "/toolchain/lib/clang/7.0.1/include/unwind.h" 3 4
#pragma GCC visibility push(default)


typedef uintptr_t _Unwind_Word;
typedef intptr_t _Unwind_Sword;
typedef uintptr_t _Unwind_Ptr;
typedef uintptr_t _Unwind_Internal_Ptr;
typedef uint64_t _Unwind_Exception_Class;

typedef intptr_t _sleb128_t;
typedef uintptr_t _uleb128_t;

struct _Unwind_Context;

struct _Unwind_Control_Block;
typedef struct _Unwind_Control_Block _Unwind_Exception;




typedef enum {
  _URC_NO_REASON = 0,


  _URC_OK = 0,

  _URC_FOREIGN_EXCEPTION_CAUGHT = 1,

  _URC_FATAL_PHASE2_ERROR = 2,
  _URC_FATAL_PHASE1_ERROR = 3,
  _URC_NORMAL_STOP = 4,

  _URC_END_OF_STACK = 5,
  _URC_HANDLER_FOUND = 6,
  _URC_INSTALL_CONTEXT = 7,
  _URC_CONTINUE_UNWIND = 8,


  _URC_FAILURE = 9

} _Unwind_Reason_Code;

typedef enum {
  _UA_SEARCH_PHASE = 1,
  _UA_CLEANUP_PHASE = 2,

  _UA_HANDLER_FRAME = 4,
  _UA_FORCE_UNWIND = 8,
  _UA_END_OF_STACK = 16
} _Unwind_Action;

typedef void (*_Unwind_Exception_Cleanup_Fn)(_Unwind_Reason_Code,
                                             _Unwind_Exception *);


typedef struct _Unwind_Control_Block _Unwind_Control_Block;
typedef uint32_t _Unwind_EHT_Header;

struct _Unwind_Control_Block {
  uint64_t exception_class;
  void (*exception_cleanup)(_Unwind_Reason_Code, _Unwind_Control_Block *);

  struct {
    uint32_t reserved1;
    uint32_t reserved2;
    uint32_t reserved3;
    uint32_t reserved4;
    uint32_t reserved5;
  } unwinder_cache;

  struct {
    uint32_t sp;
    uint32_t bitpattern[5];
  } barrier_cache;

  struct {
    uint32_t bitpattern[4];
  } cleanup_cache;

  struct {
    uint32_t fnstart;
    _Unwind_EHT_Header *ehtp;
    uint32_t additional;
    uint32_t reserved1;
  } pr_cache;
  long long int : 0;
} __attribute__((__aligned__(8)));
# 169 "/toolchain/lib/clang/7.0.1/include/unwind.h" 3 4
typedef _Unwind_Reason_Code (*_Unwind_Stop_Fn)(int, _Unwind_Action,
                                               _Unwind_Exception_Class,
                                               _Unwind_Exception *,
                                               struct _Unwind_Context *,
                                               void *);

typedef _Unwind_Reason_Code (*_Unwind_Personality_Fn)(int, _Unwind_Action,
                                                      _Unwind_Exception_Class,
                                                      _Unwind_Exception *,
                                                      struct _Unwind_Context *);
typedef _Unwind_Personality_Fn __personality_routine;

typedef _Unwind_Reason_Code (*_Unwind_Trace_Fn)(struct _Unwind_Context *,
                                                void *);


typedef enum {
  _UVRSC_CORE = 0,
  _UVRSC_VFP = 1,
  _UVRSC_WMMXD = 3,
  _UVRSC_WMMXC = 4
} _Unwind_VRS_RegClass;

typedef enum {
  _UVRSD_UINT32 = 0,
  _UVRSD_VFPX = 1,
  _UVRSD_UINT64 = 3,
  _UVRSD_FLOAT = 4,
  _UVRSD_DOUBLE = 5
} _Unwind_VRS_DataRepresentation;

typedef enum {
  _UVRSR_OK = 0,
  _UVRSR_NOT_IMPLEMENTED = 1,
  _UVRSR_FAILED = 2
} _Unwind_VRS_Result;

typedef uint32_t _Unwind_State;






_Unwind_VRS_Result _Unwind_VRS_Get(struct _Unwind_Context *__context,
  _Unwind_VRS_RegClass __regclass,
  uint32_t __regno,
  _Unwind_VRS_DataRepresentation __representation,
  void *__valuep);

_Unwind_VRS_Result _Unwind_VRS_Set(struct _Unwind_Context *__context,
  _Unwind_VRS_RegClass __regclass,
  uint32_t __regno,
  _Unwind_VRS_DataRepresentation __representation,
  void *__valuep);

static __inline__
_Unwind_Word _Unwind_GetGR(struct _Unwind_Context *__context, int __index) {
  _Unwind_Word __value;
  _Unwind_VRS_Get(__context, _UVRSC_CORE, __index, _UVRSD_UINT32, &__value);
  return __value;
}

static __inline__
void _Unwind_SetGR(struct _Unwind_Context *__context, int __index,
                   _Unwind_Word __value) {
  _Unwind_VRS_Set(__context, _UVRSC_CORE, __index, _UVRSD_UINT32, &__value);
}

static __inline__
_Unwind_Word _Unwind_GetIP(struct _Unwind_Context *__context) {
  _Unwind_Word __ip = _Unwind_GetGR(__context, 15);
  return __ip & ~(_Unwind_Word)(0x1);
}

static __inline__
void _Unwind_SetIP(struct _Unwind_Context *__context, _Unwind_Word __value) {
  _Unwind_Word __thumb_mode_bit = _Unwind_GetGR(__context, 15) & 0x1;
  _Unwind_SetGR(__context, 15, __value | __thumb_mode_bit);
}
# 258 "/toolchain/lib/clang/7.0.1/include/unwind.h" 3 4
_Unwind_Word _Unwind_GetIPInfo(struct _Unwind_Context *, int *);

_Unwind_Word _Unwind_GetCFA(struct _Unwind_Context *);

_Unwind_Word _Unwind_GetBSP(struct _Unwind_Context *);

void *_Unwind_GetLanguageSpecificData(struct _Unwind_Context *);

_Unwind_Ptr _Unwind_GetRegionStart(struct _Unwind_Context *);



_Unwind_Reason_Code _Unwind_RaiseException(_Unwind_Exception *);
_Unwind_Reason_Code _Unwind_ForcedUnwind(_Unwind_Exception *, _Unwind_Stop_Fn,
                                         void *);
void _Unwind_DeleteException(_Unwind_Exception *);
void _Unwind_Resume(_Unwind_Exception *);
_Unwind_Reason_Code _Unwind_Resume_or_Rethrow(_Unwind_Exception *);



_Unwind_Reason_Code _Unwind_Backtrace(_Unwind_Trace_Fn, void *);


typedef struct SjLj_Function_Context *_Unwind_FunctionContext_t;

void _Unwind_SjLj_Register(_Unwind_FunctionContext_t);
void _Unwind_SjLj_Unregister(_Unwind_FunctionContext_t);
_Unwind_Reason_Code _Unwind_SjLj_RaiseException(_Unwind_Exception *);
_Unwind_Reason_Code _Unwind_SjLj_ForcedUnwind(_Unwind_Exception *,
                                              _Unwind_Stop_Fn, void *);
void _Unwind_SjLj_Resume(_Unwind_Exception *);
_Unwind_Reason_Code _Unwind_SjLj_Resume_or_Rethrow(_Unwind_Exception *);

void *_Unwind_FindEnclosingFunction(void *);
# 325 "/toolchain/lib/clang/7.0.1/include/unwind.h" 3 4
_Unwind_Ptr _Unwind_GetDataRelBase(struct _Unwind_Context *);
_Unwind_Ptr _Unwind_GetTextRelBase(struct _Unwind_Context *);





#pragma GCC visibility pop
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c" 2
# 24 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/unwind-ehabi-helpers.h" 1
# 13 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/unwind-ehabi-helpers.h"
# 1 "/toolchain/arm/include/stdint.h" 1 3
# 14 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/unwind-ehabi-helpers.h" 2
# 42 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/unwind-ehabi-helpers.h"
typedef uint32_t _Unwind_State;
# 25 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c" 2
# 54 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c"
static uintptr_t readULEB128(const uint8_t** data)
{
    uintptr_t result = 0;
    uintptr_t shift = 0;
    unsigned char byte;
    const uint8_t* p = *data;
    do {
        byte = *p++;
        result |= (byte & 0x7f) << shift;
        shift += 7;
    } while (byte & 0x80);
    *data = p;
    return result;
}


static uintptr_t readEncodedPointer(const uint8_t** data, uint8_t encoding)
{
    const uint8_t* p = *data;
    uintptr_t result = 0;

    if ( encoding == 0xff )
        return 0;


    switch (encoding & 0x0F) {
        case 0x00:
            result = *((const uintptr_t*)p);
            p += sizeof(uintptr_t);
            break;
        case 0x01:
            result = readULEB128(&p);
            break;
        case 0x02:
            result = *((const uint16_t*)p);
            p += sizeof(uint16_t);
            break;
        case 0x03:
            result = *((const uint32_t*)p);
            p += sizeof(uint32_t);
            break;
        case 0x04:
            result = *((const uint64_t*)p);
            p += sizeof(uint64_t);
            break;
        case 0x0A:
            result = *((const int16_t*)p);
            p += sizeof(int16_t);
            break;
        case 0x0B:
            result = *((const int32_t*)p);
            p += sizeof(int32_t);
            break;
        case 0x0C:
            result = *((const int64_t*)p);
            p += sizeof(int64_t);
            break;
        case 0x09:
        default:

            compilerrt_abort_impl("/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c", 114, __func__);
            break;
    }


    switch ( encoding & 0x70 ) {
        case 0x00:

            break;
        case 0x10:
            result += (uintptr_t)(*data);
            break;
        case 0x20:
        case 0x30:
        case 0x40:
        case 0x50:
        default:

            compilerrt_abort_impl("/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c", 132, __func__);
            break;
    }


    if (encoding & 0x80) {
        result = *((const uintptr_t*)result);
    }

    *data = p;
    return result;
}




_Unwind_Reason_Code __gnu_unwind_frame(struct _Unwind_Exception *,
                                       struct _Unwind_Context *);


static inline _Unwind_Reason_Code
continueUnwind(struct _Unwind_Exception *exceptionObject,
               struct _Unwind_Context *context) {





    if (__gnu_unwind_frame(exceptionObject, context) != 0)
        return 9;

    return _URC_CONTINUE_UNWIND;
}
# 183 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/gcc_personality_v0.c"
                _Unwind_Reason_Code __gcc_personality_v0(
         _Unwind_State state, struct _Unwind_Exception *exceptionObject,
         struct _Unwind_Context *context)






{





    if ((state & ((_Unwind_State)3)) != ((_Unwind_State)1))



        return continueUnwind(exceptionObject, context);


    const uint8_t* lsda = (uint8_t*)_Unwind_GetLanguageSpecificData(context);
    if ( lsda == (uint8_t*) 0 )
        return continueUnwind(exceptionObject, context);

    uintptr_t pc = _Unwind_GetIP(context)-1;
    uintptr_t funcStart = _Unwind_GetRegionStart(context);
    uintptr_t pcOffset = pc - funcStart;


    uint8_t lpStartEncoding = *lsda++;
    if (lpStartEncoding != 0xff) {
        readEncodedPointer(&lsda, lpStartEncoding);
    }
    uint8_t ttypeEncoding = *lsda++;
    if (ttypeEncoding != 0xff) {
        readULEB128(&lsda);
    }

    uint8_t callSiteEncoding = *lsda++;
    uint32_t callSiteTableLength = readULEB128(&lsda);
    const uint8_t* callSiteTableStart = lsda;
    const uint8_t* callSiteTableEnd = callSiteTableStart + callSiteTableLength;
    const uint8_t* p=callSiteTableStart;
    while (p < callSiteTableEnd) {
        uintptr_t start = readEncodedPointer(&p, callSiteEncoding);
        uintptr_t length = readEncodedPointer(&p, callSiteEncoding);
        uintptr_t landingPad = readEncodedPointer(&p, callSiteEncoding);
        readULEB128(&p);
        if ( landingPad == 0 )
            continue;
        if ( (start <= pcOffset) && (pcOffset < (start+length)) ) {





            _Unwind_SetGR(context, __builtin_eh_return_data_regno(0),
                          (uintptr_t)exceptionObject);
            _Unwind_SetGR(context, __builtin_eh_return_data_regno(1), 0);
            _Unwind_SetIP(context, (funcStart + landingPad));
            return _URC_INSTALL_CONTEXT;
        }
    }


    return continueUnwind(exceptionObject, context);
}
