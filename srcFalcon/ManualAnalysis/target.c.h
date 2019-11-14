#define FLOAT_BEHAVIOR_UNSPECIFIED 0
#define FLOAT_BEHAVIOR_IEEE_754 7541
#define FLOAT_BEHAVIOR_IEEE_754_ARM 7542
#ifndef TARGET_NATIVE_HALF
#define TARGET_NATIVE_HALF FLOAT_BEHAVIOR_UNSPECIFIED
#endif
#ifndef TARGET_NATIVE_FLOAT
#define TARGET_NATIVE_FLOAT FLOAT_BEHAVIOR_IEEE_754
#endif
#ifndef TARGET_NATIVE_DOUBLE
#define TARGET_NATIVE_DOUBLE FLOAT_BEHAVIOR_IEEE_754
#endif
#define UTTC_PACKED __attribute__((packed))
#ifdef __GNUC__
#define UTTC_ATTRIBUTE_FORMAT( ... ) __attribute__((format(__VA_ARGS__)))
#else
#define UTTC_ATTRIBUTE_FORMAT( ... )
#endif
#define UTTC_LINKAGE static inline
UTTC_LINKAGE void uttc_print(const char*bo,...);UTTC_LINKAGE void uttc_print_string(const char*eD);
#ifndef __has_builtin
#ifdef __GNUC__
#define __has_builtin(x) 1
#else
#define __has_builtin(x) 0
#endif
#endif
#if !defined(__cplusplus) || __cplusplus < 2011u
#define nullptr ((void*)0)
#endif
#ifndef TARGET_NO_EXIT
#if __has_builtin(__builtin_exit)
UTTC_LINKAGE int uttc_exit(int aU){__builtin_exit(aU);return aU;}
#else
extern
#ifdef __cplusplus
"C"
#endif
void exit(int);UTTC_LINKAGE int uttc_exit(int aU){exit(aU);return aU;}
#endif
#else
UTTC_LINKAGE int uttc_exit(int aU){return aU;}
#endif
UTTC_LINKAGE void uttc_assert_implementation(const char*cE,const char*bj,int cj){uttc_print("%s:%d: Assertion Failed: %s\n",bj,cj,cE);uttc_exit(-1);}
#define uttc_assert( EX ) (void)( (EX) || ( uttc_assert_implementation( #EX , __FILE__ , __LINE__ ) , 0 ) )
#define uttc_assert_ex( EX , q ) (void)( (EX) || ( uttc_assert_implementation( q , __FILE__ , __LINE__ ) , 0 ) )
#define little 4321
#define LITTLE 4321
#define big 4321
#define BIG 4321
typedef enum uttc_endian_t{
#ifdef TARGET_ENDIANNESS
UTTC_ENDIAN_LITTLE=4321,UTTC_ENDIAN_BIG=1234
#if ( TARGET_ENDIANNESS == little ) || ( TARGET_ENDIANNESS == LITTLE )
,UTTC_ENDIAN_NATIVE=UTTC_ENDIAN_LITTLE
#elif TARGET_ENDIANNESS == big || TARGET_ENDIANNESS == BIG
,UTTC_ENDIAN_NATIVE=UTTC_ENDIAN_BIG
#else
,UTTC_ENDIAN_NATIVE=TARGET_ENDIANNESS
#endif
#elif defined(__ORDER_LITTLE_ENDIAN__) && defined(__ORDER_BIG_ENDIAN__) && defined(__BYTE_ORDER__)
UTTC_ENDIAN_LITTLE=__ORDER_LITTLE_ENDIAN__,UTTC_ENDIAN_BIG=__ORDER_BIG_ENDIAN__,UTTC_ENDIAN_NATIVE=__BYTE_ORDER__
#elif defined(_WIN32)
UTTC_ENDIAN_LITTLE=4321,UTTC_ENDIAN_BIG=1234,UTTC_ENDIAN_NATIVE=UTTC_ENDIAN_LITTLE
#else
UTTC_ENDIAN_LITTLE=0x41424344UL,UTTC_ENDIAN_BIG=0x44434241UL,UTTC_ENDIAN_NATIVE=('ABCD')
#endif
}uttc_endian_t;
#undef little
#undef LITTLE
#undef big
#undef BIG
#ifndef __INT8_TYPE__
#define __INT8_TYPE__ char
#endif
#ifndef __UINT8_TYPE__
#define __UINT8_TYPE__ unsigned __INT8_TYPE__
#endif
#ifndef __INT16_TYPE__
#define __INT16_TYPE__ short
#endif
#ifndef __UINT16_TYPE__
#define __UINT16_TYPE__ unsigned __INT16_TYPE__
#endif
#ifndef __UINT32_TYPE__
#ifndef __INT32_TYPE__
#ifdef __SIZEOF_INT__
#if __SIZEOF_INT__ == 2
#define __INT32_TYPE__ long
#else
#define __INT32_TYPE__ int
#endif
#elif defined(_MSC_VER)
#define __INT32_TYPE__ __int32
#else
#define __INT32_TYPE__ int
#endif
#endif
#endif
#ifndef __UINT32_TYPE__
#define __UINT32_TYPE__ unsigned __INT32_TYPE__
#endif
#ifndef __UINT64_TYPE__
#ifndef __INT64_TYPE__
#ifdef __SIZEOF_LONG__
#if __SIZEOF_LONG__ == 4
#define __INT64_TYPE__ long long
#else
#define __INT64_TYPE__ long
#endif
#elif defined(_MSC_VER)
#define __INT64_TYPE__ __int64
#else
#define __INT64_TYPE__ long long
#endif
#endif
#ifndef __UINT64_TYPE__
#define __UINT64_TYPE__ unsigned __INT64_TYPE__
#endif
#endif
#ifndef __UINT64_TYPE__
#define __UINT64_TYPE__ unsigned __INT64_TYPE__
#endif
#ifndef __PTRDIFF_TYPE__
#ifdef __SIZEOF_PTRDIFF_T__
#if __SIZEOF_PTRDIFF_T__ == 1
#define __PTRDIFF_TYPE__ char
#elif __SIZEOF_PTRDIFF_T__ == 2
#define __PTRDIFF_TYPE__ __INT16_TYPE__
#elif __SIZEOF_PTRDIFF_T__ == 4
#define __PTRDIFF_TYPE__ __INT32_TYPE__
#elif __SIZEOF_PTRDIFF_T__ == 8
#define __PTRDIFF_TYPE__ __INT64_TYPE__
#endif
#elif defined(_MSC_VER)
#define __PTRDIFF_TYPE__ __int64
#else
#define __PTRDIFF_TYPE__ long int
#endif
#endif
#ifndef __INTPTR_TYPE__
#define __INTPTR_TYPE__ __PTRDIFF_TYPE__
#endif
#ifndef __UINTPTR_TYPE__
#define __UINTPTR_TYPE__ unsigned __INTPTR_TYPE__
#endif
#ifndef __SIZE__TYPE__
#define __SIZE__TYPE__ __UINTPTR_TYPE__
#endif
#ifndef __WINT_TYPE__
#ifdef __SIZEOF_WINT_T__
#if __SIZEOF_WINT_T__ == 1
#define __WINT_TYPE__ char
#elif __SIZEOF_WINT_T__ == 2
#define __WINT_TYPE__ __INT16_TYPE__
#elif __SIZEOF_WINT_T__ == 4
#define __WINT_TYPE__ __INT32_TYPE__
#elif __SIZEOF_WINT_T__ == 8
#define __WINT_TYPE__ __INT64_TYPE__
#endif
#elif defined(_MSC_VER)
#define __WINT_TYPE__ __int16
#else
#define __WINT_TYPE__ int
#endif
#endif
typedef __INT8_TYPE__ uttc_int8_t;typedef __UINT8_TYPE__ uttc_uint8_t;typedef __INT16_TYPE__ uttc_int16_t;typedef __UINT16_TYPE__ uttc_uint16_t;typedef __INT32_TYPE__ uttc_int32_t;typedef __UINT32_TYPE__ uttc_uint32_t;typedef __INT64_TYPE__ uttc_int64_t;typedef __UINT64_TYPE__ uttc_uint64_t;typedef __INTPTR_TYPE__ uttc_intptr_t;typedef __UINTPTR_TYPE__ uttc_uintptr_t;typedef __PTRDIFF_TYPE__ uttc_ptrdiff_t;typedef __SIZE__TYPE__ uttc_size_t;typedef __WINT_TYPE__ uttc_wint_t;typedef long long uttc_intmax_t;typedef unsigned long long uttc_uintmax_t;
#define UTTC_ULONGLONG_MAX ((unsigned long long)-1)
#define UTTC_ULONG_MAX ((unsigned long)-1)
#define UTTC_UINT_MAX ((unsigned int)-1)
#define UTTC_USHORT_MAX ((unsigned short)-1)
#define UTTC_SIZET_MAX ((uttc_size_t)-1)
#define UTTC_UINTMAX_MAX ((uttc_uintmax_t)-1)
#define UTTC_LONGLONG_MAX ((long long)(UTTC_ULONGLONG_MAX>>1))
#define UTTC_LONG_MAX ((long)(UTTC_ULONG_MAX>>1))
#define UTTC_INT_MAX ((int)(UTTC_UINT_MAX>>1))
#define UTTC_SHORT_MAX ((short)(UTTC_USHORT_MAX>>1))
#define UTTC_INTMAX_MAX ((uttc_intmax_t)(UTTC_UINTMAX_MAX>>1))
#define UTTC_ULONGLONG_MIN ((unsigned long long)0)
#define UTTC_ULONG_MIN ((unsigned long)0)
#define UTTC_UINT_MIN ((unsigned int)0)
#define UTTC_USHORT_MIN ((unsigned short)0)
#define UTTC_SIZET_MIN ((uttc_size_t)0)
#define UTTC_UINTMAX_MIN ((uttc_uintmax_t)0)
#define UTTC_LONGLONG_MIN ((long long)(-UTTC_LONGLONG_MAX-1))
#define UTTC_LONG_MIN ((long)(-UTTC_LONG_MAX-1))
#define UTTC_INT_MIN ((int)(-UTTC_INT_MAX-1))
#define UTTC_SHORT_MIN ((short)(-UTTC_SHORT_MAX-1))
#define UTTC_INTMAX_MIN ((uttc_intmax_t)(-UTTC_INTMAX_MAX-1))
#ifndef __cplusplus
#if !defined(_BOOL) && !defined(bool)
typedef unsigned char bool;
#else
typedef _Bool bool;
#endif
#ifndef true
#define true ((bool)1)
#endif
#ifndef false
#define false ((bool)0)
#endif
#endif
UTTC_LINKAGE void uttc_memset(void*aa,uttc_uint8_t fn,uttc_size_t cc){if(!cc)return;uttc_uint8_t*bO=(uttc_uint8_t*)aa;uttc_size_t cd=(cc+7)/8;switch(cc%8){case 0:do{*bO++=fn;case 7:*bO++=fn;case 6:*bO++=fn;case 5:*bO++=fn;case 4:*bO++=fn;case 3:*bO++=fn;case 2:*bO++=fn;case 1:*bO++=fn;}while(--cd);}}UTTC_LINKAGE void uttc_memcpy(void*to,const void*bt,uttc_size_t cc){if(!cc)return;const uttc_uint8_t*bP=(const uttc_uint8_t*)bt;uttc_uint8_t*bQ=(uttc_uint8_t*)to;uttc_size_t cd=(cc+7)/8;switch(cc%8){case 0:do{*bQ++=*bP++;case 7:*bQ++=*bP++;case 6:*bQ++=*bP++;case 5:*bQ++=*bP++;case 4:*bQ++=*bP++;case 3:*bQ++=*bP++;case 2:*bQ++=*bP++;case 1:*bQ++=*bP++;}while(--cd);}}UTTC_LINKAGE void uttc_memmove(void*to,const void*bt,uttc_size_t cc){if(!cc||to==bt)return;const uttc_uint8_t*bP=(const uttc_uint8_t*)bt;uttc_uint8_t*bQ=(uttc_uint8_t*)to;uttc_size_t cd=(cc+7)/8;if((uttc_intptr_t)to<(uttc_intptr_t)bt)switch(cc%8){case 0:do{*bQ++=*bP++;case 7:*bQ++=*bP++;case 6:*bQ++=*bP++;case 5:*bQ++=*bP++;case 4:*bQ++=*bP++;case 3:*bQ++=*bP++;case 2:*bQ++=*bP++;case 1:*bQ++=*bP++;}while(--cd);}else{bP+=cc;bQ+=cc;switch(cc%8){case 0:do{*--bQ=*--bP;case 7:*--bQ=*--bP;case 6:*--bQ=*--bP;case 5:*--bQ=*--bP;case 4:*--bQ=*--bP;case 3:*--bQ=*--bP;case 2:*--bQ=*--bP;case 1:*--bQ=*--bP;}while(--cd);}}}UTTC_LINKAGE int uttc_memcmp(const void*bZ,const void*ee,uttc_size_t n){const uttc_uint8_t*dz=(const uttc_uint8_t*)bZ;const uttc_uint8_t*dH=(const uttc_uint8_t*)ee;while(n--)if(*dz!=*dH)return(int)*dz-(int)*dH;else dz++,dH++;return 0;}UTTC_LINKAGE uttc_size_t uttc_strlen(const char*eD){const char*eS=eD;while(*eS)++eS;return eS-eD;}UTTC_LINKAGE uttc_size_t uttc_strcmp(const char*ci,const char*ed){while(*ci&&*ci==*ed)ci++,ed++;return*ci<*ed?-1:(*ci>*ed?1:0);}UTTC_LINKAGE void uttc_strcpy(char*aC,const char*eA){while(*eA)*aC++=*eA++;*aC='\0';}UTTC_LINKAGE char*uttc_strcat(char*aC,const char*eA){char*aB=aC;while(*aC)aC++;while((*aC++=*eA++)){}return aB;}UTTC_LINKAGE int uttc_islower(int c){return c>='a'&&c<='z';}UTTC_LINKAGE int uttc_isupper(int c){return c>='A'&&c<='Z';}UTTC_LINKAGE uttc_size_t uttc_tolower(unsigned char c){return uttc_isupper(c)?c+('a'-'A'):c;}UTTC_LINKAGE uttc_size_t uttc_toupper(unsigned char c){return uttc_islower(c)?c-('a'-'A'):c;}
#ifndef TARGET_NO_STDARG
#include <stdarg.h>
#endif
#ifndef va_start
#if defined(__GNUC__) || defined(__clang__)
typedef __builtin_va_list uttc_va_list;
#define uttc_va_start(ck, dw) __builtin_va_start(ck, dw)
#define uttc_va_arg(ck, fa) __builtin_va_arg(ck, fa)
#define uttc_va_end(ck) __builtin_va_end(ck)
#define uttc_va_copy(aC, eA) __builtin_va_copy(aC, eA)
#else
typedef void*uttc_va_list;
#define uttc_va_start(ck, dw) (ck = ((void*)&(dw)) + ( ( sizeof(dw) + sizeof(int) - 1 ) / sizeof(int) ) * sizeof(int) )
#define uttc_va_arg(ck, fa) *(fa*)( ( ck += sizeof(fa) ) - sizeof(fa) )
#define uttc_va_end(ck) (ck = (void*)0)
#define uttc_va_copy(aC, eA) (aC = eA)
#endif
#else
typedef va_list uttc_va_list;
#define uttc_va_start(ck, dw) va_start(ck,dw)
#define uttc_va_arg(ck, fa) va_arg(ck, fa)
#define uttc_va_end(ck) va_end(ck)
#ifdef va_copy
#define uttc_va_copy(aC, eA) va_copy(aC, eA)
#else
#define uttc_va_copy(aC, eA) (aC = eA)
#endif
#endif
UTTC_LINKAGE bool uttc_isprint(unsigned char c){return c>=0x20&&c<=0x7e;} UTTC_LINKAGE uttc_uint8_t uttc_log2_8(uttc_uint8_t fm){static const uttc_uint8_t cs[256]={0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7};return cs[fm];}UTTC_LINKAGE uttc_uint8_t uttc_log2_16(uttc_uint16_t fm){return fm>>8?8+uttc_log2_8((uttc_uint8_t)(fm>>8)):uttc_log2_8((uttc_uint8_t)fm);}UTTC_LINKAGE uttc_uint8_t uttc_log2_32(uttc_uint32_t fm){return fm>>16?16+uttc_log2_16((uttc_uint16_t)(fm>>16)):uttc_log2_16((uttc_uint16_t)fm);}UTTC_LINKAGE uttc_uint8_t uttc_log2_64(uttc_uint64_t fm){return fm>>32?32+uttc_log2_32((uttc_uint32_t)(fm>>32)):uttc_log2_32((uttc_uint32_t)fm);}typedef enum{UTTC_ROUND_INDETERMINATE=-1,UTTC_ROUND_TOWARD_ZERO=0,UTTC_ROUND_TO_NEAREST=1,UTTC_ROUND_TOWARD_POS_INFINITY=2,UTTC_ROUND_TOWARD_NEG_INFINITY=3,UTTC_ROUND_AWAY_FROM_ZERO=4,UTTC_ROUND_TO_NEAREST_TIES_ZERO=5,UTTC_ROUND_TO_NEAREST_TIES_EVEN=6,UTTC_ROUND_TO_NEAREST_IEEE753=7}uttc_round_t;
#define uttc_min( ci , ed ) (ci < ed ? ci : ed)
#define uttc_max( ci , ed ) (ci > ed ? ci : ed)
#ifndef TARGET_IEEE_754_HALF_TYPE
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754
#if defined(__GNUC__) || defined(__clang__)
#define TARGET_IEEE_754_HALF_TYPE __fp16
#else
#define TARGET_IEEE_754_HALF_TYPE _Float16
#endif
#else
#define TARGET_IEEE_754_HALF_TYPE uttc_uint16_t
#endif
#endif
#ifndef TARGET_IEEE_754_HALF_ALT_TYPE
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754_ARM
#if defined(__GNUC__) || defined(__clang__)
#define TARGET_IEEE_754_HALF_ALT_TYPE __fp16
#else
#define TARGET_IEEE_754_HALF_ALT_TYPE _Float16
#endif
#else
#define TARGET_IEEE_754_HALF_ALT_TYPE uttc_uint16_t
#endif
#endif
#ifndef TARGET_IEEE_754_FLOAT_TYPE
#if TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754
#define TARGET_IEEE_754_FLOAT_TYPE float
#else
#define TARGET_IEEE_754_FLOAT_TYPE uttc_uint32_t
#endif
#endif
#ifndef TARGET_IEEE_754_DOUBLE_TYPE
#if TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754
#define TARGET_IEEE_754_DOUBLE_TYPE double
#else
#define TARGET_IEEE_754_DOUBLE_TYPE uttc_uint64_t
#endif
#endif
#if TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754
#define TARGET_IEEE_754_DOUBLE_PRM_TYPE double
#else
#define TARGET_IEEE_754_DOUBLE_PRM_TYPE uttc_uint64_t
#endif
#define TARGET_IEEE_754_DOUBLE_PRM_CAST uttc_ieee754_double
#if TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754
#define TARGET_IEEE_754_FLOAT_PRM_TYPE double
#define TARGET_IEEE_754_FLOAT_PRM_CAST uttc_double2float
#else
#define TARGET_IEEE_754_FLOAT_PRM_TYPE int
#define TARGET_IEEE_754_FLOAT_PRM_CAST uttc_ieee754_float
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754
#define TARGET_IEEE_754_HALF_PRM_TYPE double
#define TARGET_IEEE_754_HALF_PRM_CAST uttc_double2half
#else
#define TARGET_IEEE_754_HALF_PRM_TYPE int
#define TARGET_IEEE_754_HALF_PRM_CAST uttc_ieee754_half
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754_ARM
#define TARGET_IEEE_754_HALF_ALT_PRM_TYPE double
#define TARGET_IEEE_754_HALF_ALT_PRM_CAST uttc_double2half_alt
#else
#define TARGET_IEEE_754_HALF_ALT_PRM_TYPE int
#define TARGET_IEEE_754_HALF_ALT_PRM_CAST uttc_ieee754_half_alt
#endif
typedef TARGET_IEEE_754_DOUBLE_TYPE uttc_ieee754_double;typedef TARGET_IEEE_754_FLOAT_TYPE uttc_ieee754_float;typedef TARGET_IEEE_754_HALF_TYPE uttc_ieee754_half;typedef TARGET_IEEE_754_HALF_ALT_TYPE uttc_ieee754_half_alt;typedef union{unsigned char aw[sizeof(uttc_ieee754_double)];uttc_ieee754_double fn;uttc_uint64_t dQ;}uttc_ieee754_double_t;typedef union{unsigned char aw[sizeof(uttc_ieee754_float)];uttc_ieee754_float fn;uttc_uint32_t dQ;}uttc_ieee754_float_t;typedef union{unsigned char aw[sizeof(uttc_ieee754_half)];uttc_ieee754_half fn;uttc_uint16_t dQ;}uttc_ieee754_half_t;typedef union{unsigned char aw[sizeof(uttc_ieee754_half)];uttc_ieee754_half_alt fn;uttc_uint16_t dQ;}uttc_ieee754_half_alt_t;UTTC_LINKAGE uttc_uint64_t uttc_toraw(uttc_ieee754_double fm){uttc_ieee754_double_t eS={.fn=fm};return eS.dQ;}UTTC_LINKAGE uttc_uint32_t uttc_torawf(uttc_ieee754_float fm){uttc_ieee754_float_t eS={.fn=fm};return eS.dQ;}UTTC_LINKAGE uttc_uint16_t uttc_torawh(uttc_ieee754_half fm){uttc_ieee754_half_t eS={.fn=fm};return eS.dQ;}UTTC_LINKAGE uttc_uint16_t uttc_torawh2(uttc_ieee754_half_alt fm){uttc_ieee754_half_alt_t eS={.fn=fm};return eS.dQ;}
#define uttc_from_raw( fm ) ((uttc_ieee754_double_t){ .dQ = fm }).fn
#define uttc_from_rawf( fm ) ((uttc_ieee754_float_t){ .dQ = fm }).fn
#define uttc_from_rawh( fm ) ((uttc_ieee754_half_t){ .dQ = fm }).fn
#define uttc_from_rawh2( fm ) ((uttc_ieee754_half_alt_t){ .dQ = fm }).fn
#define UTTC_EXPONENT_BIAS 1023
#define UTTC_EXPONENT_BIASF 127
#define UTTC_EXPONENT_BIASH 15
#define UTTC_EXPONENT_BIASH2 UTTC_EXPONENT_BIASH
#define UTTC_EXPONENT_SHIFT 52
#define UTTC_EXPONENT_SHIFTF 23
#define UTTC_EXPONENT_SHIFTH 10
#define UTTC_EXPONENT_SHIFTH2 UTTC_EXPONENT_SHIFTH
#define UTTC_SIGNBIT_SHIFT 63
#define UTTC_SIGNBIT_SHIFTF 31
#define UTTC_SIGNBIT_SHIFTH 15
#define UTTC_SIGNBIT_SHIFTH2 UTTC_SIGNBIT_SHIFTH
#define UTTC_SIGNBIT_MASK ( 1uLL << UTTC_SIGNBIT_SHIFT )
#define UTTC_SIGNBIT_MASKF ( 1uLL << UTTC_SIGNBIT_SHIFTF )
#define UTTC_SIGNBIT_MASKH ( 1uLL << UTTC_SIGNBIT_SHIFTH )
#define UTTC_SIGNBIT_MASKH2 UTTC_SIGNBIT_MASKH
#define UTTC_EXPONENT_MASK 0x7FF0000000000000uLL
#define UTTC_EXPONENT_MASKF 0x7F800000uL
#define UTTC_EXPONENT_MASKH 0x7C00u
#define UTTC_EXPONENT_MASKH2 UTTC_EXPONENT_MASKH
#define UTTC_MANTISSA_MASK 0xFFFFFFFFFFFFFuLL
#define UTTC_MANTISSA_MASKF 0x7FFFFFuL
#define UTTC_MANTISSA_MASKH 0x3FFu
#define UTTC_MANTISSA_MASKH2 UTTC_MANTISSA_MASKH
#if TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754 && defined(__GNUC__)
#define UTTC_INFINITY (1./0.)
#define UTTC_NINFINITY (-1./0.)
#define UTTC_NAN (0./0.)
#else
#define UTTC_INFINITY uttc_from_raw(UTTC_EXPONENT_MASK)
#define UTTC_NINFINITY uttc_from_raw(UTTC_EXPONENT_MASK|UTTC_SIGNBIT_MASK)
#define UTTC_NAN uttc_from_raw(UTTC_EXPONENT_MASK|(UTTC_MANTISSA_MASK>>1))
#endif
#if TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754 && defined(__GNUC__)
#define UTTC_INFINITYF (1.f/0.f)
#define UTTC_NINFINITYF (-1.f/0.f)
#define UTTC_NANF (0.f/0.f)
#else
#define UTTC_INFINITYF uttc_from_rawf(UTTC_EXPONENT_MASKF)
#define UTTC_NINFINITYF uttc_from_rawf(UTTC_EXPONENT_MASKF|UTTC_SIGNBIT_MASKF)
#define UTTC_NANF uttc_from_rawf(UTTC_EXPONENT_MASKF|(UTTC_MANTISSA_MASKF>>1))
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754 && TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754
#define UTTC_INFINITYH ((TARGET_IEEE_754_HALF_TYPE)(1.f/0.f))
#define UTTC_NINFINITYH ((TARGET_IEEE_754_HALF_TYPE)(-1.f/0.f))
#define UTTC_NANH ((TARGET_IEEE_754_HALF_TYPE)(0.f/0.f))
#elif TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754 && TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754
#define UTTC_INFINITYH ((TARGET_IEEE_754_HALF_TYPE)(1./0.))
#define UTTC_NINFINITYH ((TARGET_IEEE_754_HALF_TYPE)(-1./0.))
#define UTTC_NANH ((TARGET_IEEE_754_HALF_TYPE)(0./0.))
#else
#define UTTC_INFINITYH uttc_from_rawh(UTTC_EXPONENT_MASKH)
#define UTTC_NINFINITYH uttc_from_rawh(UTTC_EXPONENT_MASKH|UTTC_SIGNBIT_MASKH)
#define UTTC_NANH uttc_from_rawh(UTTC_EXPONENT_MASKH|(UTTC_MANTISSA_MASKH>>1))
#endif
#define UTTC_INFINITYH2 uttc_from_rawh2(0x7FFFu)
#define UTTC_NINFINITYH2 uttc_from_rawh2(0xFFFFu)
#define UTTC_NANH2 uttc_from_rawh2(0u)
UTTC_LINKAGE bool uttc_signbit(uttc_ieee754_double fm){return uttc_toraw(fm)>>UTTC_SIGNBIT_SHIFT;}UTTC_LINKAGE bool uttc_signbitf(uttc_ieee754_float fm){return uttc_torawf(fm)>>UTTC_SIGNBIT_SHIFTF;}UTTC_LINKAGE bool uttc_signbith(uttc_ieee754_half fm){return uttc_torawh(fm)>>UTTC_SIGNBIT_SHIFTH;}UTTC_LINKAGE bool uttc_signbith2(uttc_ieee754_half_alt fm){return uttc_torawh2(fm)>>UTTC_SIGNBIT_SHIFTH;}UTTC_LINKAGE uttc_uint64_t uttc_mantissa(uttc_ieee754_double fm){return uttc_toraw(fm)&UTTC_MANTISSA_MASK;}UTTC_LINKAGE uttc_uint32_t uttc_mantissaf(uttc_ieee754_float fm){return uttc_torawf(fm)&UTTC_MANTISSA_MASKF;}UTTC_LINKAGE uttc_uint16_t uttc_mantissah(uttc_ieee754_half fm){return uttc_torawh(fm)&UTTC_MANTISSA_MASKH;}UTTC_LINKAGE uttc_uint16_t uttc_mantissah2(uttc_ieee754_half_alt fm){return uttc_torawh2(fm)&UTTC_MANTISSA_MASKH;}UTTC_LINKAGE uttc_uint16_t uttc_exponent(uttc_ieee754_double fm){return(uttc_toraw(fm)&UTTC_EXPONENT_MASK)>>UTTC_EXPONENT_SHIFT;}UTTC_LINKAGE uttc_uint8_t uttc_exponentf(uttc_ieee754_float fm){return(uttc_torawf(fm)&UTTC_EXPONENT_MASKF)>>UTTC_EXPONENT_SHIFTF;}UTTC_LINKAGE uttc_uint8_t uttc_exponenth(uttc_ieee754_half fm){return(uttc_torawh(fm)&UTTC_EXPONENT_MASKH)>>UTTC_EXPONENT_SHIFTH;}UTTC_LINKAGE uttc_uint8_t uttc_exponenth2(uttc_ieee754_half_alt fm){return(uttc_torawh2(fm)&UTTC_EXPONENT_MASKH)>>UTTC_EXPONENT_SHIFTH;}UTTC_LINKAGE uttc_ieee754_double uttc_negate(uttc_ieee754_double fm){return uttc_from_raw(uttc_toraw(fm)^UTTC_SIGNBIT_MASK);}UTTC_LINKAGE uttc_ieee754_float uttc_negatef(uttc_ieee754_float fm){return uttc_from_rawf(uttc_torawf(fm)^UTTC_SIGNBIT_MASKF);}UTTC_LINKAGE uttc_ieee754_half uttc_negateh(uttc_ieee754_half fm){return uttc_from_rawh(uttc_torawh(fm)^UTTC_SIGNBIT_MASKH);}UTTC_LINKAGE uttc_ieee754_half_alt uttc_negateh2(uttc_ieee754_half_alt fm){return uttc_from_rawh2(uttc_torawh2(fm)^UTTC_SIGNBIT_MASKH);}typedef enum{UTTC_FP_ZERO=1,UTTC_FP_SUBNORMAL=2,UTTC_FP_NORMAL=4,UTTC_FP_INFINITE=8,UTTC_FP_NAN=16}uttc_ieee754_class_t;UTTC_LINKAGE uttc_ieee754_class_t uttc_fp_classify(uttc_ieee754_double fm){if((uttc_toraw(fm)&UTTC_EXPONENT_MASK)==UTTC_EXPONENT_MASK)return uttc_mantissa(fm)?UTTC_FP_NAN:UTTC_FP_INFINITE;else if(!(uttc_toraw(fm)&UTTC_EXPONENT_MASK))return uttc_mantissa(fm)?UTTC_FP_SUBNORMAL:UTTC_FP_ZERO;return UTTC_FP_NORMAL;}UTTC_LINKAGE uttc_ieee754_class_t uttc_fp_classifyf(uttc_ieee754_float fm){if((uttc_torawf(fm)&UTTC_EXPONENT_MASKF)==UTTC_EXPONENT_MASKF)return uttc_mantissaf(fm)?UTTC_FP_NAN:UTTC_FP_INFINITE;else if(!(uttc_torawf(fm)&UTTC_EXPONENT_MASKF))return uttc_mantissaf(fm)?UTTC_FP_SUBNORMAL:UTTC_FP_ZERO;return UTTC_FP_NORMAL;}UTTC_LINKAGE uttc_ieee754_class_t uttc_fp_classifyh(uttc_ieee754_half fm){if((uttc_torawh(fm)&UTTC_EXPONENT_MASKH)==UTTC_EXPONENT_MASKH)return uttc_mantissah(fm)?UTTC_FP_NAN:UTTC_FP_INFINITE;else if(!(uttc_torawh(fm)&UTTC_EXPONENT_MASKH))return uttc_mantissah(fm)?UTTC_FP_SUBNORMAL:UTTC_FP_ZERO;return UTTC_FP_NORMAL;}UTTC_LINKAGE uttc_ieee754_class_t uttc_fp_classifyh2(uttc_ieee754_half_alt fm){if(!(uttc_torawh2(fm)&UTTC_EXPONENT_MASKH))return uttc_mantissah(fm)?UTTC_FP_SUBNORMAL:UTTC_FP_ZERO;return UTTC_FP_NORMAL;}UTTC_LINKAGE bool uttc_isnan(uttc_ieee754_double fm){return uttc_fp_classify(fm)==UTTC_FP_NAN;}UTTC_LINKAGE bool uttc_isnanf(uttc_ieee754_float fm){return uttc_fp_classifyf(fm)==UTTC_FP_NAN;}UTTC_LINKAGE bool uttc_isnanh(uttc_ieee754_half fm){return uttc_fp_classifyh(fm)==UTTC_FP_NAN;}UTTC_LINKAGE bool uttc_isnanh2(uttc_ieee754_half_alt fm){(void)fm;return false;}UTTC_LINKAGE bool uttc_isinf(uttc_ieee754_double fm){return uttc_fp_classify(fm)==UTTC_FP_INFINITE;}UTTC_LINKAGE bool uttc_isinff(uttc_ieee754_float fm){return uttc_fp_classifyf(fm)==UTTC_FP_INFINITE;}UTTC_LINKAGE bool uttc_isinfh(uttc_ieee754_half fm){return uttc_fp_classifyh(fm)==UTTC_FP_INFINITE;}UTTC_LINKAGE bool uttc_isinfh2(uttc_ieee754_half_alt fm){(void)fm;return false;}UTTC_LINKAGE uttc_ieee754_double uttc_fabs(uttc_ieee754_double fm){return uttc_from_raw(uttc_toraw(fm)&~UTTC_SIGNBIT_MASK);}UTTC_LINKAGE uttc_ieee754_float uttc_fabsf(uttc_ieee754_float fm){return uttc_from_rawf(uttc_torawf(fm)&~UTTC_SIGNBIT_MASKF);}UTTC_LINKAGE uttc_ieee754_half uttc_fabsh(uttc_ieee754_half fm){return uttc_from_rawh(uttc_torawh(fm)&~UTTC_SIGNBIT_MASKH);}UTTC_LINKAGE uttc_ieee754_half_alt uttc_fabsh2(uttc_ieee754_half_alt fm){return uttc_from_rawh2(uttc_torawh2(fm)&~UTTC_SIGNBIT_MASKH);}UTTC_LINKAGE uttc_ieee754_double uttc_nan(uttc_uint64_t dy){if(dy<1)dy=1;return uttc_from_raw(UTTC_EXPONENT_MASK|(dy&UTTC_MANTISSA_MASK));}UTTC_LINKAGE uttc_ieee754_float uttc_nanf(uttc_uint32_t dy){if(dy<1)dy=1;return uttc_from_rawf(UTTC_EXPONENT_MASKF|(dy&UTTC_MANTISSA_MASKF));}UTTC_LINKAGE uttc_ieee754_half uttc_nanh(uttc_uint16_t dy){if(dy<1)dy=1;return uttc_from_rawh(UTTC_EXPONENT_MASKH|(dy&UTTC_MANTISSA_MASKH));}UTTC_LINKAGE uttc_ieee754_half_alt uttc_nanh2(uttc_uint16_t dy){if(dy<1)dy=1;return uttc_from_rawh2(UTTC_EXPONENT_MASKH2|(dy&UTTC_MANTISSA_MASKH2));}
#define uttc_nanpayload( ... ) uttc_mantissa( __VA_ARGS__ )
#define uttc_nanpayloadf( ... ) uttc_mantissaf( __VA_ARGS__ )
#define uttc_nanpayloadh( ... ) uttc_mantissah( __VA_ARGS__ )
#define uttc_nanpayloadh2( ... ) uttc_mantissah2( __VA_ARGS__ )
#if TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE bool uttc_isless(uttc_ieee754_double bZ,uttc_ieee754_double ee){return bZ<ee;}
#else
UTTC_LINKAGE bool uttc_isless(uttc_ieee754_double bZ,uttc_ieee754_double ee){uttc_uint64_t bS=uttc_toraw(uttc_fabs(bZ)),dP=uttc_toraw(uttc_fabs(ee));return!uttc_isnan(bZ)&&!uttc_isnan(ee)&&(uttc_signbit(bZ)?-(uttc_int64_t)bS:(uttc_int64_t)bS)<(uttc_signbit(ee)?-(uttc_int64_t)dP:(uttc_int64_t)dP);}
#endif
#if TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE bool uttc_islessf(uttc_ieee754_float bZ,uttc_ieee754_float ee){return bZ<ee;}
#else
UTTC_LINKAGE bool uttc_islessf(uttc_ieee754_float bZ,uttc_ieee754_float ee){uttc_uint32_t bS=uttc_torawf(uttc_fabsf(bZ)),dP=uttc_torawf(uttc_fabsf(ee));return!uttc_isnanf(bZ)&&!uttc_isnanf(ee)&&(uttc_signbitf(bZ)?-(uttc_int32_t)bS:(uttc_int32_t)bS)<(uttc_signbitf(ee)?-(uttc_int32_t)dP:(uttc_int32_t)dP);}
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE bool uttc_islessh(uttc_ieee754_half bZ,uttc_ieee754_half ee){return bZ<ee;}
#else
UTTC_LINKAGE bool uttc_islessh(uttc_ieee754_half bZ,uttc_ieee754_half ee){uttc_uint16_t bS=uttc_torawh(uttc_fabsh(bZ)),dP=uttc_torawh(uttc_fabsh(ee));return!uttc_isnanh(bZ)&&!uttc_isnanh(ee)&&(uttc_signbith(bZ)?-(uttc_int16_t)bS:(uttc_int16_t)bS)<(uttc_signbith(ee)?-(uttc_int16_t)dP:(uttc_int16_t)dP);}
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754_ARM
UTTC_LINKAGE bool uttc_islessh2(uttc_ieee754_half_alt bZ,uttc_ieee754_half_alt ee){return bZ<ee;}
#else
UTTC_LINKAGE bool uttc_islessh2(uttc_ieee754_half_alt bZ,uttc_ieee754_half_alt ee){uttc_uint16_t bS=uttc_torawh(uttc_fabsh(bZ)),dP=uttc_torawh(uttc_fabsh(ee));return(uttc_signbith(bZ)?-(uttc_int16_t)bS:(uttc_int16_t)bS)<(uttc_signbith(ee)?-(uttc_int16_t)dP:(uttc_int16_t)dP);}
#endif
#if TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE bool uttc_islessequal(uttc_ieee754_double bZ,uttc_ieee754_double ee){return bZ<=ee;}
#else
UTTC_LINKAGE bool uttc_islessequal(uttc_ieee754_double bZ,uttc_ieee754_double ee){uttc_uint64_t bS=uttc_toraw(uttc_fabs(bZ)),dP=uttc_toraw(uttc_fabs(ee));return!uttc_isnan(bZ)&&!uttc_isnan(ee)&&(uttc_signbit(bZ)?-(uttc_int64_t)bS:(uttc_int64_t)bS)<=(uttc_signbit(ee)?-(uttc_int64_t)dP:(uttc_int64_t)dP);}
#endif
#if TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE bool uttc_islessequalf(uttc_ieee754_float bZ,uttc_ieee754_float ee){return bZ<=ee;}
#else
UTTC_LINKAGE bool uttc_islessequalf(uttc_ieee754_float bZ,uttc_ieee754_float ee){uttc_uint32_t bS=uttc_torawf(uttc_fabsf(bZ)),dP=uttc_torawf(uttc_fabsf(ee));return!uttc_isnanf(bZ)&&!uttc_isnanf(ee)&&(uttc_signbitf(bZ)?-(uttc_int32_t)bS:(uttc_int32_t)bS)<=(uttc_signbitf(ee)?-(uttc_int32_t)dP:(uttc_int32_t)dP);}
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE bool uttc_islessequalh(uttc_ieee754_half bZ,uttc_ieee754_half ee){return bZ<=ee;}
#else
UTTC_LINKAGE bool uttc_islessequalh(uttc_ieee754_half bZ,uttc_ieee754_half ee){uttc_uint16_t bS=uttc_torawh(uttc_fabsh(bZ)),dP=uttc_torawh(uttc_fabsh(ee));return!uttc_isnanh(bZ)&&!uttc_isnanh(ee)&&(uttc_signbith(bZ)?-(uttc_int16_t)bS:(uttc_int16_t)bS)<=(uttc_signbith(ee)?-(uttc_int16_t)dP:(uttc_int16_t)dP);}
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754_ARM
UTTC_LINKAGE bool uttc_islessequalh2(uttc_ieee754_half_alt bZ,uttc_ieee754_half_alt ee){return bZ<=ee;}
#else
UTTC_LINKAGE bool uttc_islessequalh2(uttc_ieee754_half_alt bZ,uttc_ieee754_half_alt ee){uttc_uint16_t bS=uttc_torawh(uttc_fabsh(bZ)),dP=uttc_torawh(uttc_fabsh(ee));return(uttc_signbith(bZ)?-(uttc_int16_t)bS:(uttc_int16_t)bS)<=(uttc_signbith(ee)?-(uttc_int16_t)dP:(uttc_int16_t)dP);}
#endif
#if TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE uttc_ieee754_double uttc_addsub(uttc_ieee754_double bZ,uttc_ieee754_double ee,bool eF){return eF?bZ-ee:bZ+ee;}UTTC_LINKAGE uttc_ieee754_double uttc_multiply(uttc_ieee754_double bZ,uttc_ieee754_double ee){return bZ*ee;}UTTC_LINKAGE uttc_ieee754_double uttc_divide(uttc_ieee754_double aI,uttc_ieee754_double aK){return aI/aK;}
#else
UTTC_LINKAGE uttc_ieee754_double uttc_addsub(uttc_ieee754_double bZ,uttc_ieee754_double ee,bool eF){const int dE=5;if(uttc_isnan(bZ))return bZ;if(uttc_isnan(ee))return ee;if(eF)ee^=UTTC_SIGNBIT_MASK;if(uttc_isinf(bZ)){if(uttc_isinf(ee)){if(uttc_signbit(bZ)!=uttc_signbit(ee))return UTTC_NAN;else return bZ;}}else if(uttc_isinf(ee))return ee;int aW=uttc_exponent(bZ)-uttc_exponent(ee);int aE=aW;if(!aE)aE=uttc_mantissa(bZ)-uttc_mantissa(ee);if(aE<0){uttc_ieee754_double eS=bZ;bZ=ee;ee=eS;aW=-aW;}bool cL=uttc_signbit(bZ);uttc_uint16_t cK=uttc_exponent(bZ);if(cL==uttc_signbit(ee)){uttc_uint64_t N=uttc_mantissa(bZ);if(cK)N+=UTTC_MANTISSA_MASK+1;N<<=dE;uttc_uint64_t ct=uttc_mantissa(ee);if(uttc_exponent(ee))ct+=UTTC_MANTISSA_MASK+1;N+=(ct<<dE)>>aW;int ef=dE;const int cQ=((UTTC_MANTISSA_MASK+1)<<(dE+1))-1;if(N>cQ){ef+=1;cK+=1;}N=(N+(1<<(ef-1)))>>ef;}else{uttc_uint64_t N=uttc_mantissa(bZ);if(cK)N+=UTTC_MANTISSA_MASK+1;N<<=dE;uttc_uint64_t ct=uttc_mantissa(ee);if(uttc_exponent(ee))ct+=UTTC_MANTISSA_MASK+1;N-=(ct<<dE)>>aW;const int cQ=((UTTC_MANTISSA_MASK+1)<<dE)-1;while(N<=cQ&&cK>0){N<<=1;cK-=1;}N=((N+(1<<(dE-1)))>>dE);if(!N)cL=false;}return uttc_from_raw(((uttc_uint64_t)cL<<UTTC_SIGNBIT_SHIFT)|((uttc_uint64_t)cK<<UTTC_EXPONENT_SHIFT)|(N&UTTC_MANTISSA_MASK));}UTTC_LINKAGE uttc_ieee754_double uttc_multiply(uttc_ieee754_double bZ,uttc_ieee754_double ee){uttc_assert_ex(false,"Non-native multiplication of IEEE 754 double not implemented.");return bZ;}UTTC_LINKAGE uttc_ieee754_double uttc_divide(uttc_ieee754_double aI,uttc_ieee754_double aK){uttc_assert_ex(false,"Non-native division of IEEE 754 double not implemented.");return aI;}
#endif
#if TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE uttc_ieee754_float uttc_addsubf(uttc_ieee754_float bZ,uttc_ieee754_float ee,bool eF){return eF?bZ-ee:bZ+ee;}
#else
UTTC_LINKAGE uttc_ieee754_float uttc_addsubf(uttc_ieee754_float bZ,uttc_ieee754_float ee,bool eF){uttc_assert_ex(false,"Non-native addition/subtraction of IEEE 754 float not implemented.");return bZ;}UTTC_LINKAGE uttc_ieee754_float uttc_multiplyf(uttc_ieee754_float bZ,uttc_ieee754_float ee){uttc_assert_ex(false,"Non-native multiplication of IEEE 754 float not implemented.");return bZ;}UTTC_LINKAGE uttc_ieee754_float uttc_dividef(uttc_ieee754_float aI,uttc_ieee754_float aK){uttc_assert_ex(false,"Non-native division of IEEE 754 float not implemented.");return aI;}
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754
UTTC_LINKAGE uttc_ieee754_half uttc_addsubh(uttc_ieee754_half bZ,uttc_ieee754_half ee,bool eF){return eF?bZ-ee:bZ+ee;}
#else
UTTC_LINKAGE uttc_ieee754_half uttc_addsubh(uttc_ieee754_half bZ,uttc_ieee754_half ee,bool eF){uttc_assert_ex(false,"Non-native addition/subtraction of IEEE 754 half not implemented.");return bZ;}UTTC_LINKAGE uttc_ieee754_half uttc_multiplyh(uttc_ieee754_half bZ,uttc_ieee754_half ee){uttc_assert_ex(false,"Non-native multiplication of IEEE 754 half not implemented.");return bZ;}UTTC_LINKAGE uttc_ieee754_half uttc_divideh(uttc_ieee754_half aI,uttc_ieee754_half aK){uttc_assert_ex(false,"Non-native division of IEEE 754 half not implemented.");return aI;}
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754_ARM
UTTC_LINKAGE uttc_ieee754_half_alt uttc_addsubh2(uttc_ieee754_half_alt bZ,uttc_ieee754_half_alt ee,bool eF){return eF?bZ-ee:bZ+ee;}
#else
UTTC_LINKAGE uttc_ieee754_half_alt uttc_addsubh2(uttc_ieee754_half_alt bZ,uttc_ieee754_half_alt ee,bool eF){uttc_assert_ex(false,"Non-native addition/subtraction of IEEE 754 (alternative) half not implemented.");return bZ;}UTTC_LINKAGE uttc_ieee754_half_alt uttc_multiplyh2(uttc_ieee754_half_alt bZ,uttc_ieee754_half_alt ee){uttc_assert_ex(false,"Non-native multiplication of IEEE 754 (alternative) half not implemented.");return bZ;}UTTC_LINKAGE uttc_ieee754_half_alt uttc_divideh2(uttc_ieee754_half_alt aI,uttc_ieee754_half_alt aK){uttc_assert_ex(false,"Non-native division of IEEE 754 (alternative) half not implemented.");return aI;}
#endif
#if ( TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754 ) && ( TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754 )
UTTC_LINKAGE uttc_ieee754_float uttc_half2float(uttc_ieee754_half fm){return(uttc_ieee754_float)fm;}UTTC_LINKAGE uttc_ieee754_half uttc_float2half_ex(uttc_ieee754_float fm,uttc_round_t eh){(void)eh;return(uttc_ieee754_half)fm;}
#else
UTTC_LINKAGE uttc_ieee754_float uttc_half2float(uttc_ieee754_half fm){switch(uttc_fp_classifyh(fm)){case UTTC_FP_INFINITE:return uttc_signbith(fm)?UTTC_NINFINITYF:UTTC_INFINITYF;case UTTC_FP_NAN:return uttc_from_rawf(((uttc_uint32_t)uttc_signbith(fm)<<UTTC_SIGNBIT_SHIFTF)|UTTC_EXPONENT_MASKF|uttc_mantissah(fm));case UTTC_FP_ZERO:return uttc_from_rawf((uttc_uint32_t)uttc_signbith(fm)<<UTTC_SIGNBIT_SHIFTF);case UTTC_FP_NORMAL:return uttc_from_rawf(((uttc_uint32_t)uttc_signbith(fm)<<UTTC_SIGNBIT_SHIFTF)|(uttc_uint32_t)((int)uttc_exponenth(fm)-UTTC_EXPONENT_BIASH+UTTC_EXPONENT_BIASF)<<UTTC_EXPONENT_SHIFTF|((uttc_uint32_t)uttc_mantissah(fm)<<(UTTC_EXPONENT_SHIFTF-UTTC_EXPONENT_SHIFTH)));case UTTC_FP_SUBNORMAL:{uttc_uint16_t cY=uttc_log2_16(uttc_mantissah(fm));return uttc_from_rawf(((uttc_uint32_t)uttc_signbith(fm)<<UTTC_SIGNBIT_SHIFTF)|((uttc_uint32_t)(UTTC_EXPONENT_BIASF-(UTTC_EXPONENT_BIASH-1)-(UTTC_EXPONENT_SHIFTH-cY))<<UTTC_EXPONENT_SHIFTF)|(((uttc_uint32_t)uttc_mantissah(fm)<<(UTTC_EXPONENT_SHIFTF-cY))&UTTC_MANTISSA_MASKF));}}}UTTC_LINKAGE uttc_ieee754_half uttc_float2half_ex(uttc_ieee754_float fm,uttc_round_t eh){switch(uttc_fp_classifyf(fm)){case UTTC_FP_INFINITE:return uttc_signbitf(fm)?UTTC_NINFINITYH:UTTC_INFINITYH;case UTTC_FP_NAN:return UTTC_NANH;case UTTC_FP_ZERO:return uttc_from_rawh((uttc_uint16_t)uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFTH);case UTTC_FP_SUBNORMAL:switch(eh){case UTTC_ROUND_AWAY_FROM_ZERO:return uttc_from_rawh(UTTC_SIGNBIT_MASKH*uttc_signbitf(fm)|1);case UTTC_ROUND_TOWARD_POS_INFINITY:return uttc_from_rawh((uttc_uint16_t)1u<<(UTTC_SIGNBIT_SHIFTH*uttc_signbitf(fm)));case UTTC_ROUND_TOWARD_NEG_INFINITY:return uttc_from_rawh((UTTC_SIGNBIT_MASKH|1)*uttc_signbitf(fm));default:return uttc_from_rawh((uttc_uint16_t)uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFTH);}break;case UTTC_FP_NORMAL:break;}int aV=uttc_exponentf(fm)-UTTC_EXPONENT_BIASF+UTTC_EXPONENT_BIASH;bool ar;bool as;uttc_uint16_t dY;if(aV>0){dY=(uttc_mantissaf(fm)>>(UTTC_EXPONENT_SHIFTF-UTTC_EXPONENT_SHIFTH));ar=uttc_mantissaf(fm)&(1<<(UTTC_EXPONENT_SHIFTF-UTTC_EXPONENT_SHIFTH-1));as=uttc_mantissaf(fm)&((1<<(UTTC_EXPONENT_SHIFTF-UTTC_EXPONENT_SHIFTH-1))-1);}else if(aV>=-UTTC_EXPONENT_SHIFTH){int ep=UTTC_EXPONENT_SHIFTF-UTTC_EXPONENT_SHIFTH+1-aV;dY=(uttc_mantissaf(fm)+UTTC_MANTISSA_MASKF+1)>>ep;ar=1L<<(ep-1);as=uttc_mantissaf(fm)&(ar-1);ar=uttc_mantissaf(fm)&ar;aV=0;}else{dY=0;aV=0;ar=false;as=true;}switch(eh){case UTTC_ROUND_TOWARD_POS_INFINITY:dY+=ar||as?1-uttc_signbitf(fm):0;break;case UTTC_ROUND_TOWARD_NEG_INFINITY:dY+=ar||as?uttc_signbitf(fm):0;break;case UTTC_ROUND_AWAY_FROM_ZERO:dY+=ar||as?1:0;break;case UTTC_ROUND_TO_NEAREST:dY+=ar?1:0;break;case UTTC_ROUND_TO_NEAREST_TIES_ZERO:dY+=ar&&as?1:0;break;case UTTC_ROUND_TO_NEAREST_TIES_EVEN:case UTTC_ROUND_TO_NEAREST_IEEE753:if(as)dY+=ar?1:0;else if(ar)dY+=dY%2u;break;default:break;}if(dY>UTTC_MANTISSA_MASKH&&aV!=0){dY>>=1;++aV;}if(aV>(int)(UTTC_EXPONENT_MASKH>>UTTC_EXPONENT_SHIFTH))switch(eh){case UTTC_ROUND_TOWARD_POS_INFINITY:return uttc_signbitf(fm)?UTTC_SIGNBIT_MASKH|(UTTC_EXPONENT_MASKH-1):UTTC_EXPONENT_MASKH;case UTTC_ROUND_TOWARD_NEG_INFINITY:return uttc_signbitf(fm)?UTTC_SIGNBIT_MASKH|UTTC_EXPONENT_MASKH:UTTC_EXPONENT_MASKH-1;case UTTC_ROUND_TO_NEAREST_IEEE753:case UTTC_ROUND_AWAY_FROM_ZERO:return(uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFTH)|UTTC_EXPONENT_MASKH;default:return(uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFTH)|(UTTC_EXPONENT_MASKH-1);}return uttc_from_rawh(dY|((uttc_uint8_t)aV<<UTTC_EXPONENT_SHIFTH)|(uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFTH));}
#endif
UTTC_LINKAGE uttc_ieee754_half uttc_float2half(uttc_ieee754_float fm){return uttc_float2half_ex(fm,UTTC_ROUND_TO_NEAREST_IEEE753);}
#if ( TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754 ) && ( TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754 )
UTTC_LINKAGE uttc_ieee754_float uttc_half_alt2float(uttc_ieee754_half_alt fm){return(uttc_ieee754_float)fm;}UTTC_LINKAGE uttc_ieee754_half_alt uttc_float2half_alt_ex(uttc_ieee754_float fm,uttc_round_t eh){(void)eh;return(uttc_ieee754_half_alt)fm;}
#else
UTTC_LINKAGE uttc_ieee754_float uttc_half_alt2float(uttc_ieee754_half_alt fm){switch(uttc_fp_classifyh2(fm)){case UTTC_FP_ZERO:return uttc_from_rawf((uttc_uint32_t)uttc_signbith2(fm)<<UTTC_SIGNBIT_SHIFTF);case UTTC_FP_NORMAL:return uttc_from_rawf(((uttc_uint32_t)uttc_signbith2(fm)<<UTTC_SIGNBIT_SHIFTF)|(uttc_uint32_t)((int)uttc_exponenth2(fm)-UTTC_EXPONENT_BIASH+UTTC_EXPONENT_BIASF)<<UTTC_EXPONENT_SHIFTF|((uttc_uint32_t)uttc_mantissah2(fm)<<(UTTC_EXPONENT_SHIFTF-UTTC_EXPONENT_SHIFTH)));case UTTC_FP_SUBNORMAL:default:{uttc_uint16_t cY=uttc_log2_16(uttc_mantissah2(fm));return uttc_from_rawf(((uttc_uint32_t)uttc_signbith2(fm)<<UTTC_SIGNBIT_SHIFTF)|((uttc_uint32_t)(UTTC_EXPONENT_BIASF-(UTTC_EXPONENT_BIASH-1)-(UTTC_EXPONENT_SHIFTH-cY))<<UTTC_EXPONENT_SHIFTF)|(((uttc_uint32_t)uttc_mantissah2(fm)<<(UTTC_EXPONENT_SHIFTF-cY))&UTTC_MANTISSA_MASKF));}}}UTTC_LINKAGE uttc_ieee754_half_alt uttc_float2half_alt_ex(uttc_ieee754_float fm,uttc_round_t eh){uttc_assert_ex(false,"Non-native conversion between IEEE 754 float and half not implemented.");uttc_ieee754_half_alt eS=0;return eS;}
#endif
UTTC_LINKAGE uttc_ieee754_half_alt uttc_float2half_alt(uttc_ieee754_float fm){return uttc_float2half_alt_ex(fm,UTTC_ROUND_TO_NEAREST_IEEE753);}
#if ( TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754 ) && ( TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754 )
UTTC_LINKAGE uttc_ieee754_double uttc_float2double(uttc_ieee754_float fm){return(uttc_ieee754_double)fm;}UTTC_LINKAGE uttc_ieee754_float uttc_double2float_ex(uttc_ieee754_double fm,uttc_round_t eh){(void)eh;return(uttc_ieee754_float)fm;}
#else
UTTC_LINKAGE uttc_ieee754_double uttc_float2double(uttc_ieee754_float fm){switch(uttc_fp_classifyf(fm)){case UTTC_FP_INFINITE:return uttc_signbitf(fm)?UTTC_NINFINITY:UTTC_INFINITY;case UTTC_FP_NAN:return uttc_from_raw(((uttc_uint64_t)uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFT)|UTTC_EXPONENT_MASK|uttc_mantissaf(fm));case UTTC_FP_ZERO:return uttc_from_raw((uttc_uint64_t)uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFT);case UTTC_FP_NORMAL:return uttc_from_raw(((uttc_uint64_t)uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFT)|(uttc_uint64_t)((int)uttc_exponentf(fm)-UTTC_EXPONENT_BIASF+UTTC_EXPONENT_BIAS)<<UTTC_EXPONENT_SHIFT|((uttc_uint64_t)uttc_mantissaf(fm)<<(UTTC_EXPONENT_SHIFT-UTTC_EXPONENT_SHIFTF)));case UTTC_FP_SUBNORMAL:{uttc_uint16_t cY=uttc_log2_32(uttc_mantissaf(fm));return uttc_from_raw(((uttc_uint64_t)uttc_signbitf(fm)<<UTTC_SIGNBIT_SHIFT)|((uttc_uint64_t)(UTTC_EXPONENT_BIAS-(UTTC_EXPONENT_BIASF-1)-(UTTC_EXPONENT_SHIFTF-cY))<<UTTC_EXPONENT_SHIFT)|(((uttc_uint64_t)uttc_mantissaf(fm)<<(UTTC_EXPONENT_SHIFT-cY+1))&UTTC_MANTISSA_MASK));}}}UTTC_LINKAGE uttc_ieee754_float uttc_double2float_ex(uttc_ieee754_double fm){uttc_assert_ex(false,"Non-native conversion between IEEE 754 float and double not implemented.");uttc_ieee754_double eS=0;return eS;}
#endif
UTTC_LINKAGE uttc_ieee754_float uttc_double2float(uttc_ieee754_double fm){return uttc_double2float_ex(fm,UTTC_ROUND_TO_NEAREST_IEEE753);}UTTC_LINKAGE uttc_ieee754_double uttc_half2double(uttc_ieee754_half fm){return uttc_float2double(uttc_half2float(fm));}UTTC_LINKAGE uttc_ieee754_half uttc_double2half_ex(uttc_ieee754_double fm,uttc_round_t eh){return uttc_float2half_ex(uttc_double2float_ex(fm,eh),eh);}UTTC_LINKAGE uttc_ieee754_half uttc_double2half(uttc_ieee754_double fm){return uttc_double2half_ex(fm,UTTC_ROUND_TO_NEAREST_IEEE753);}UTTC_LINKAGE uttc_ieee754_double uttc_half_alt2double(uttc_ieee754_half_alt fm){return uttc_float2double(uttc_half_alt2float(fm));}UTTC_LINKAGE uttc_ieee754_half_alt uttc_double2half_alt_ex(uttc_ieee754_double fm,uttc_round_t eh){return uttc_float2half_alt_ex(uttc_double2float_ex(fm,eh),eh);}UTTC_LINKAGE uttc_ieee754_half_alt uttc_double2half_alt(uttc_ieee754_double fm){return uttc_double2half_alt_ex(fm,UTTC_ROUND_TO_NEAREST_IEEE753);}
#define UTTC_DEFINE_NEXTAFTER( D , eG , A , o , m ) UTTC_LINKAGE uttc_ieee754_##D uttc_nextafter##eG( uttc_ieee754_##D bt , uttc_ieee754_##D to ) { uttc_uint##o##_t bs = uttc_toraw##eG(bt), eZ = uttc_toraw##eG(to); uttc_uint##o##_t be = uttc_toraw##eG( uttc_fabs##eG(bt) ), eK = uttc_toraw##eG( uttc_fabs(to) ); if( m ){ if( uttc_isnan##eG(bt) ) return bt; if( uttc_isnan##eG(to) || bs == eZ || !( be | eK ) ) return to; } if( !be ) return uttc_from_raw( ( eZ & UTTC_SIGNBIT_MASK##A ) + 1 ); char bd = ( be == bs ? (uttc_int##o##_t)be : -(uttc_int##o##_t)be ) < ( eK == eZ ? (uttc_int##o##_t)eK : -(uttc_int##o##_t)eK ) ? 1 : 0 ; return uttc_from_raw##eG( bs + ( ( ( bs >> (o-1) ) ^ bd ) << 1 ) - 1 ); }
UTTC_DEFINE_NEXTAFTER(double,,,64,true)UTTC_DEFINE_NEXTAFTER(float,f,F,32,true)UTTC_DEFINE_NEXTAFTER(half,h,H,16,true)UTTC_DEFINE_NEXTAFTER(half_alt,h2,H2,16,false)
#define UTTC_DEFINE_CEIL( D , eG , A , o , m ) UTTC_LINKAGE uttc_ieee754_##D uttc_ceil##eG( uttc_ieee754_##D fn ) { int aY = uttc_exponent##eG( fn ); if( m && aY == (UTTC_EXPONENT_MASK >> UTTC_EXPONENT_SHIFT) ) return fn; uttc_uint##o##_t dY = uttc_toraw##eG(fn); if( !dY ) return dY; if( aY < UTTC_EXPONENT_BIAS##A ) return uttc_from_raw##eG( dY >= UTTC_SIGNBIT_MASK##A ? UTTC_SIGNBIT_MASK##A : ( (uttc_uint##o##_t)UTTC_EXPONENT_BIAS##A << UTTC_EXPONENT_SHIFT##A ) ); int br = UTTC_EXPONENT_SHIFT##A + UTTC_EXPONENT_BIAS##A - aY; if( br <= 0 ) return fn; dY &= ( (uttc_uint##o##_t)(UTTC_SIGNBIT_MASK##A | UTTC_EXPONENT_MASK##A | UTTC_MANTISSA_MASK##A) << br ); if( dY != uttc_toraw##eG(fn) && !uttc_signbit##eG( fn ) ) dY += 1uLL << br; return uttc_from_raw##eG( dY ); }
UTTC_DEFINE_CEIL(double,,,64,true)UTTC_DEFINE_CEIL(float,f,F,32,true)UTTC_DEFINE_CEIL(half,h,H,16,true)UTTC_DEFINE_CEIL(half_alt,h2,H2,16,false)
#if TARGET_NATIVE_DOUBLE == FLOAT_BEHAVIOR_IEEE_754
#define uttc_fromhex( fm ) fm
#else
#define uttc_fromhex( fm ) uttc_fromhex_implementation( #fm )
#endif
#if TARGET_NATIVE_FLOAT == FLOAT_BEHAVIOR_IEEE_754
#define uttc_fromhexf( fm ) fm ## f
#else
#define uttc_fromhexf( fm ) uttc_fromhexf_implementation( #fm )
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754
#define uttc_fromhexh( fm ) ((TARGET_IEEE_754_HALF_TYPE)fm)
#else
#define uttc_fromhexh( fm ) uttc_fromhexh_implementation( #fm )
#endif
#if TARGET_NATIVE_HALF == FLOAT_BEHAVIOR_IEEE_754_ARM
#define uttc_fromhexh2( fm ) ((TARGET_IEEE_754_HALF_ALT_TYPE)fm)
#else
#define uttc_fromhexh2( fm ) uttc_fromhexh_implementation( #fm )
#endif
#define getxalpha( c ) ((unsigned char)(c & 0xDF) - 'A')
#define getxnum( c ) ((unsigned char)(c - '0'))
#define UTTC_DEFINE_FROMHEX_IMPLEMENTATION( D , eG , A , o ) UTTC_LINKAGE uttc_ieee754_##D uttc_fromhex##eG##_implementation( const char* eD ) { unsigned char fm; bool et = false; if( eD[0] == '-' ){ eD++; et = true; } if( eD[0] == '0' && (eD[1] & 0xDF) == 'X' ) eD += 2; if( (fm = getxnum(*eD)) < 10u ) {} else if( (fm = getxalpha( *eD )) < 6u ) fm += 10; else goto ec; if( fm == 0 ) return et ? uttc_from_raw##eG( UTTC_SIGNBIT_MASK##A ) : uttc_from_raw##eG( 0 ); ++eD; if( eD[0] == '.' ) ++eD; unsigned char V = uttc_log2_8( fm ); signed char ep = UTTC_EXPONENT_SHIFT##A - V; uttc_uint##o##_t dY = 0u; while( ep > -4 ){ dY |= ( ep >= 0 ? fm << ep : fm >> -ep ); if( (fm = getxnum(eD[0])) < 10u ) {} else if( (fm = getxalpha(eD[0])) < 6u ) fm += 10; else break; ++eD; ep -= 4; } while( getxnum(*eD) < 10u || getxalpha(*eD) < 6u ) ++eD; if( eD[0] != 'p' ) goto ec; ++eD; bool aX = false; uttc_int##o##_t aY = 0; if( *eD == '-' ){ aX = true; eD++; } else if( *eD == '+' ) eD++; while( *eD >= '0' && *eD <= '9' ){ aY = aY * 10 + ( *eD - '0' ); eD++; } aY = ( aX ? -aY : aY ) + UTTC_EXPONENT_BIAS##A + V; uttc_assert( aY <= (uttc_int##o##_t)( UTTC_EXPONENT_MASK##A >> UTTC_EXPONENT_SHIFT##A ) ); if( aY <= 0 ){ dY >>= 1; dY >>= -aY; aY = 0; } else dY &= UTTC_MANTISSA_MASK##A; return uttc_from_raw##eG( dY | ( (uttc_uint##o##_t)aY << UTTC_EXPONENT_SHIFT##A ) | ( et ? UTTC_SIGNBIT_MASK##A : 0u ) ); ec: return UTTC_NAN##A; }
UTTC_DEFINE_FROMHEX_IMPLEMENTATION(double,,,64)UTTC_DEFINE_FROMHEX_IMPLEMENTATION(float,f,F,32)UTTC_DEFINE_FROMHEX_IMPLEMENTATION(half,h,H,16)UTTC_DEFINE_FROMHEX_IMPLEMENTATION(half_alt,h2,H2,16)
#undef getxalpha
#undef getxnum
#define BIG_INT_MAX_BLOCKS 38
typedef struct uttc_big_integer{uttc_uint32_t cf;uttc_uint32_t Y[BIG_INT_MAX_BLOCKS];}uttc_big_integer;UTTC_LINKAGE void uttc_bi_set_32(uttc_big_integer*cS,uttc_uint64_t fm){cS->Y[0]=fm;cS->cf=fm?1:0;}UTTC_LINKAGE void uttc_bi_set_64(uttc_big_integer*cS,uttc_uint64_t fm){if(fm>0xFFFFFFFFuL){cS->Y[0]=fm&0xFFFFFFFFuL;cS->Y[1]=(fm>>32)&0xFFFFFFFFuL;cS->cf=2;}else{cS->Y[0]=(uttc_uint32_t)fm;cS->cf=(fm!=0);}}UTTC_LINKAGE void uttc_bi_set_imax(uttc_big_integer*cS,uttc_uintmax_t fm){cS->cf=0;while(fm){cS->Y[cS->cf++]=fm&0xFFFFFFFFuL;fm>>=32;}}UTTC_LINKAGE uttc_uint64_t uttc_bi_get_64(uttc_big_integer*cS){switch(cS->cf){case 1:return cS->Y[0];case 2:return(uttc_uint64_t)cS->Y[0]|((uttc_uint64_t)cS->Y[1]<<32);default:uttc_assert(cS->cf<=2);}return 0;}UTTC_LINKAGE uttc_uint32_t uttc_bi_get_32(uttc_big_integer*cS){switch(cS->cf){case 1:return cS->Y[0];default:uttc_assert(cS->cf<=1);}return 0;}UTTC_LINKAGE uttc_uint16_t uttc_bi_get_16(uttc_big_integer*cS){return uttc_bi_get_32(cS);}UTTC_LINKAGE void uttc_bi_set_zero(uttc_big_integer*cS){cS->cf=0;}UTTC_LINKAGE void uttc_bi_set_nonempty_zero(uttc_big_integer*cS){cS->cf=1;cS->Y[0]=0;}UTTC_LINKAGE bool uttc_bi_is_zero(const uttc_big_integer*cS){return!cS->cf;}UTTC_LINKAGE void uttc_bi_assign(uttc_big_integer*aC,const uttc_big_integer*eA){uttc_int32_t i;for(i=0;i<eA->cf;i++)aC->Y[i]=eA->Y[i];aC->cf=eA->cf;}UTTC_LINKAGE uttc_int32_t uttc_bi_compare(const uttc_big_integer*ci,const uttc_big_integer*ed){uttc_int32_t cg=ci->cf-ed->cf;if(cg!=0)return cg;uttc_int32_t i;for(i=ci->cf-1;i>=0;--i)if(ci->Y[i]==ed->Y[i])continue;else if(ci->Y[i]>ed->Y[i])return 1;else return-1;return 0;}UTTC_LINKAGE void uttc_bi_sum(uttc_big_integer*dY,const uttc_big_integer*ci,const uttc_big_integer*ed){const uttc_big_integer*bT;const uttc_big_integer*ev;if(ci->cf<ed->cf){ev=ci;bT=ed;}else{ev=ed;bT=ci;}const uttc_uint32_t bX=bT->cf;const uttc_uint32_t ez=ev->cf;dY->cf=bX;uttc_uint64_t ab=0;const uttc_uint32_t*bV=bT->Y;const uttc_uint32_t*bW=bV+bX;const uttc_uint32_t*ew=ev->Y;const uttc_uint32_t*ex=ew+ez;uttc_uint32_t*dZ=dY->Y;while(ew!=ex){uttc_uint64_t eH=ab+(uttc_uint64_t)*bV+(uttc_uint64_t)*ew;ab=eH>>32;*dZ=eH&0xFFFFFFFF;++bV;++ew;++dZ;}while(bV!=bW){uttc_uint64_t eH=ab+(uttc_uint64_t)*bV;ab=eH>>32;*dZ=eH&0xFFFFFFFF;++bV;++dZ;}if(ab!=0){uttc_assert(ab==1);uttc_assert((uttc_uint32_t)(dZ-dY->Y)==bX&&(bX<BIG_INT_MAX_BLOCKS));*dZ=1;dY->cf=bX+1;}else dY->cf=bX;}UTTC_LINKAGE void uttc_bi_add(uttc_big_integer*dY,const uttc_big_integer*fn){uttc_big_integer al;uttc_bi_assign(&al,dY);uttc_bi_sum(dY,&al,fn);}UTTC_LINKAGE bool uttc_bi_dif(uttc_big_integer*dY,const uttc_big_integer*ci,const uttc_big_integer*ed){bool cJ=false;int af=uttc_bi_compare(ci,ed);if(af<0)cJ=true;else if(!af){uttc_bi_set_zero(dY);return false;}const uttc_big_integer*bT=cJ?ed:ci;const uttc_big_integer*ev=cJ?ci:ed;const uttc_uint32_t bX=bT->cf;const uttc_uint32_t ez=ev->cf;if(uttc_bi_is_zero(ev))return cJ;dY->cf=bX;uttc_uint64_t ab=0;const uttc_uint32_t*bV=bT->Y;const uttc_uint32_t*bW=bV+bX;const uttc_uint32_t*ew=ev->Y;const uttc_uint32_t*ex=ew+ez;uttc_uint32_t*dZ=dY->Y;int eY=0;while(ew!=ex){uttc_int64_t eH=(uttc_int64_t)*bV-(uttc_int64_t)*ew-(uttc_int64_t)ab;ab=0;while(eH<0){eH+=1uLL<<32;++ab;}eY=(uttc_uint32_t)eH?0:eY+1;*dZ=(uttc_uint32_t)eH;++bV;++ew;++dZ;}while(bV!=bW){uttc_int64_t eH=(uttc_int64_t)*bV-ab;ab=0;while(eH<0){eH+=1uLL<<32;++ab;}eY=(uttc_uint32_t)eH?0:eY+1;*dZ=(uttc_uint32_t)eH;++bV;++dZ;if(!ab)break;}while(bV!=bW){*dZ=*bV;eY=*dZ?0:eY+1;++bV;++dZ;}dY->cf-=eY;return cJ;}UTTC_LINKAGE bool uttc_bi_sub(uttc_big_integer*dY,const uttc_big_integer*fn){uttc_big_integer al;uttc_bi_assign(&al,dY);return uttc_bi_dif(dY,&al,fn);}UTTC_LINKAGE void uttc_bi_mutliply(uttc_big_integer*dY,const uttc_big_integer*ci,const uttc_big_integer*ed){uttc_assert(dY!=ci&&dY!=ed);const uttc_big_integer*bT;const uttc_big_integer*ev;if(ci->cf<ed->cf){ev=ci;bT=ed;}else{ev=ed;bT=ci;}uttc_uint32_t cB=bT->cf+ev->cf;uttc_assert(cB<=BIG_INT_MAX_BLOCKS);uttc_uint32_t*am=dY->Y;uttc_uint32_t*aN=aN=am+cB;while(am!=aN)*am++=0;const uttc_uint32_t*bU=bT->Y;const uttc_uint32_t*bW=bU+bT->cf;uttc_uint32_t*eb=dY->Y;const uttc_uint32_t*ew=ev->Y;const uttc_uint32_t*ex=ew+ev->cf;for(;ew!=ex;++ew,++eb){const uttc_uint32_t cF=*ew;if(cF!=0){const uttc_uint32_t*bV=bU;uttc_uint32_t*dZ=eb;uttc_uint64_t ab=0;do{uttc_uint64_t dL=*dZ+(*bV)*(uttc_uint64_t)cF+ab;ab=dL>>32;*dZ=dL&0xFFFFFFFF;++bV;++dZ;}while(bV!=bW);uttc_assert(dZ<dY->Y+cB);*dZ=(uttc_uint32_t)(ab&0xFFFFFFFF);}}if(cB>0&&dY->Y[cB-1]==0)dY->cf=cB-1;else dY->cf=cB;}UTTC_LINKAGE void uttc_bi_cmultiply(uttc_big_integer*dY,const uttc_big_integer*ci,uttc_uint32_t ed){uttc_uint32_t ab=0;uttc_uint32_t*dZ=dY->Y;const uttc_uint32_t*dt=ci->Y;const uttc_uint32_t*du=ci->Y+ci->cf;for(;dt!=du;++dt,++dZ){uttc_uint64_t dL=(uttc_uint64_t)*dt*ed+ab;*dZ=(uttc_uint32_t)(dL&0xFFFFFFFF);ab=dL>>32;}if(ab!=0){uttc_assert(ci->cf+1<=BIG_INT_MAX_BLOCKS);*dZ=(uttc_uint32_t)ab;dY->cf=ci->cf+1;}else dY->cf=ci->cf;}UTTC_LINKAGE void uttc_bi_times(uttc_big_integer*dY,uttc_uint32_t fn){uttc_big_integer al;uttc_bi_assign(&al,dY);uttc_bi_cmultiply(dY,&al,fn);}UTTC_LINKAGE void uttc_bi_times2(uttc_big_integer*dY,const uttc_big_integer*in){uttc_uint32_t ab=0;uttc_uint32_t*dZ=dY->Y;const uttc_uint32_t*dt=in->Y;const uttc_uint32_t*du=in->Y+in->cf;for(;dt!=du;++dt,++dZ){uttc_uint32_t am=*dt;*dZ=(am<<1)|ab;ab=am>>31;}if(ab!=0){uttc_assert(in->cf+1<=BIG_INT_MAX_BLOCKS);*dZ=ab;dY->cf=in->cf+1;}else dY->cf=in->cf;}UTTC_LINKAGE void uttc_bi_double(uttc_big_integer*dY){uttc_uint32_t ab=0;uttc_uint32_t*am=dY->Y;uttc_uint32_t*aN=dY->Y+dY->cf;for(;am!=aN;++am){uttc_uint32_t eS=*am;*am=(eS<<1)|ab;ab=eS>>31;}if(ab!=0){uttc_assert(dY->cf+1<=BIG_INT_MAX_BLOCKS);*am=ab;++dY->cf;}}UTTC_LINKAGE void uttc_bi_times10(uttc_big_integer*dY){uttc_uint64_t ab=0;uttc_uint32_t*am=dY->Y;uttc_uint32_t*aN=dY->Y+dY->cf;for(;am!=aN;++am){uttc_uint64_t dL=(uttc_uint64_t)(*am)*10ull+ab;*am=(uttc_uint32_t)(dL&0xFFFFFFFF);ab=dL>>32;}if(ab!=0){uttc_assert(dY->cf+1<=BIG_INT_MAX_BLOCKS);*am=(uttc_uint32_t)ab;++dY->cf;}}UTTC_LINKAGE uttc_uint32_t uttc_bi_power10_32(int bK){uttc_uint32_t cs[]={1,10,100,1000,10000,100000,1000000,10000000,};return cs[bK];}UTTC_LINKAGE uttc_big_integer*uttc_bi_power10_big(int bK){static uttc_big_integer cs[]={{1,{100000000}},{2,{0x6fc10000,0x002386f2}},{4,{0x00000000,0x85acef81,0x2d6d415b,0x000004ee,}},{7,{0x00000000,0x00000000,0xbf6a1f01,0x6e38ed64,0xdaa797ed,0xe93ff9f4,0x00184f03,}},{14,{0x00000000,0x00000000,0x00000000,0x00000000,0x2e953e01,0x03df9909,0x0f1538fd,0x2374e42f,0xd3cff5ec,0xc404dc08,0xbccdb0da,0xa6337f19,0xe91f2603,0x0000024e,}},{27,{0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x982e7c01,0xbed3875b,0xd8d99f72,0x12152f87,0x6bde50c6,0xcf4a6e70,0xd595d80f,0x26b2716e,0xadc666b0,0x1d153624,0x3c42d35a,0x63ff540e,0xcc5573c0,0x65f9ef17,0x55bc28f2,0x80dcc7f7,0xf46eeddc,0x5fdcefce,0x000553f7,}}};return&cs[bK];}UTTC_LINKAGE void uttc_bi_pow10(uttc_big_integer*dY,uttc_uint32_t aY){uttc_assert(aY<512);uttc_big_integer eM;uttc_big_integer eN;uttc_big_integer*ap=&eM;uttc_big_integer*cO=&eN;uttc_uint32_t ey=aY&0x7;uttc_bi_set_32(ap,uttc_bi_power10_32(ey));aY>>=3;uttc_uint32_t eJ=0;while(aY!=0){if(aY&1){uttc_bi_mutliply(cO,ap,uttc_bi_power10_big(eJ));uttc_big_integer*eI=ap;ap=cO;cO=eI;}++eJ;aY>>=1;}uttc_bi_assign(dY,ap);}UTTC_LINKAGE void uttc_bi_mutliply_pow10(uttc_big_integer*dY,const uttc_big_integer*in,uttc_uint32_t aY){uttc_assert(aY<512);uttc_big_integer eM;uttc_big_integer eN;uttc_big_integer*ap=&eM;uttc_big_integer*cO=&eN;uttc_uint32_t ey=aY&0x7;if(ey!=0)uttc_bi_cmultiply(ap,in,uttc_bi_power10_32(ey));else uttc_bi_assign(ap,in);aY>>=3;uttc_uint32_t eJ=0;while(aY!=0){if(aY&1){uttc_bi_mutliply(cO,ap,uttc_bi_power10_big(eJ));uttc_big_integer*eI=ap;ap=cO;cO=eI;}++eJ;aY>>=1;}uttc_bi_assign(dY,ap);}UTTC_LINKAGE void uttc_bi_pow2(uttc_big_integer*dY,uttc_uint32_t aY){uttc_uint32_t X=aY/32;uttc_assert(X<BIG_INT_MAX_BLOCKS);uttc_uint32_t i=0;while(i<=X)dY->Y[i++]=0;dY->cf=X+1;uttc_uint32_t U=aY%32;dY->Y[X]|=1<<U;}UTTC_LINKAGE uttc_uint32_t uttc_bi_divmod_maxquot9(uttc_big_integer*aI,const uttc_big_integer*aK){uttc_assert(!uttc_bi_is_zero(aK)&&aK->Y[aK->cf-1]>=8&&aK->Y[aK->cf-1]<0xFFFFFFFF&&aI->cf<=aK->cf);uttc_uint32_t cf=aK->cf;if(aI->cf<aK->cf)return 0;const uttc_uint32_t*bl=aK->Y+cf-1;uttc_uint32_t*bk=aI->Y+cf-1;uttc_uint32_t dO=*bk/(*bl+1);uttc_assert(dO<=9);if(dO!=0){const uttc_uint32_t*aL=aK->Y;uttc_uint32_t*aJ=aI->Y;uttc_uint64_t Z=0;uttc_uint64_t ab=0;do{uttc_uint64_t dL=(uttc_uint64_t)*aL*(uttc_uint64_t)dO+ab;ab=dL>>32;uttc_uint64_t aE=(uttc_uint64_t)*aJ-(dL&0xFFFFFFFF)-Z;Z=(aE>>32)&1;*aJ=aE&0xFFFFFFFF;++aL;++aJ;}while(aL<=bl);while(cf>0&&aI->Y[cf-1]==0)--cf;aI->cf=cf;}if(uttc_bi_compare(aI,aK)>=0){++dO;const uttc_uint32_t*aL=aK->Y;uttc_uint32_t*aJ=aI->Y;uttc_uint64_t Z=0;do{uttc_uint64_t aE=(uttc_uint64_t)*aJ-(uttc_uint64_t)*aL-Z;Z=(aE>>32)&1;*aJ=aE&0xFFFFFFFF;++aL;++aJ;}while(aL<=bl);while(cf>0&&aI->Y[cf-1]==0)--cf;aI->cf=cf;}return dO;}UTTC_LINKAGE void uttc_bi_shl(uttc_big_integer*dY,uttc_uint32_t ep){if(!dY->cf||!ep)return;uttc_uint32_t er=ep/32;uttc_uint32_t eq=ep%32;const uttc_uint32_t*bH=dY->Y;uttc_int32_t bJ=dY->cf;uttc_assert(bJ+er<BIG_INT_MAX_BLOCKS);if(eq==0){uttc_uint32_t*bI=dY->Y+bJ;uttc_uint32_t*dp=bI+er;while(bI>=bH)*dp--=*bI--;uttc_uint32_t i=0;while(i<er)dY->Y[i++]=0;dY->cf+=er;}else{uttc_int32_t bG=bJ-1;uttc_uint32_t dn=bJ+er;uttc_assert(dn<BIG_INT_MAX_BLOCKS);dY->cf=dn+1;const uttc_uint32_t cr=32-eq;uttc_uint32_t bA=0;uttc_uint32_t W=dY->Y[bG];uttc_uint32_t cq=W>>cr;while(bG>0){dY->Y[dn]=bA|cq;bA=W<<eq;--bG;--dn;W=dY->Y[bG];cq=W>>cr;}uttc_assert(dn==er+1);dY->Y[dn]=bA|cq;dY->Y[dn-1]=W<<eq;uttc_uint32_t i=0;while(i<er)dY->Y[i++]=0;if(dY->Y[dY->cf-1]==0)--dY->cf;}}UTTC_LINKAGE bool uttc_bi_shr(uttc_big_integer*dY,uttc_uint32_t ep){if(!dY->cf||!ep)return false;uttc_uint32_t er=ep/32;uttc_uint32_t eq=ep%32;uttc_uint32_t*bH=dY->Y;uttc_int32_t bJ=dY->cf;if(er>=bJ){if(uttc_bi_is_zero(dY))return false;uttc_bi_set_zero(dY);return true;}bool dk=false;uttc_uint32_t*bI=bH;uttc_uint32_t*aN=bH+er;while(bI<aN)if(*bI++){dk=true;break;}if(eq==0){uttc_uint32_t*dp=bH;uttc_uint32_t*bI=dp+er;uttc_uint32_t*aN=bH+bJ;while(bI<aN)*dp++=*bI++;dY->cf-=er;}else{const uttc_uint32_t bB=32-eq;if(!dk)dk=dY->Y[er]&(0xFFFFFFFF>>bB)?true:false;uttc_int32_t bG=er;uttc_uint32_t dn=0;dY->cf-=er;uttc_uint32_t bA=bG+1<bJ?dY->Y[bG+1]<<bB:0;uttc_uint32_t cq=dY->Y[bG]>>eq;while(bG<bJ){dY->Y[dn]=bA|cq;++bG;++dn;cq=dY->Y[bG]>>eq;bA=bG+1<bJ?dY->Y[bG+1]<<bB:0;}if(dY->Y[dY->cf-1]==0)--dY->cf;}return dk;}UTTC_LINKAGE uttc_uint32_t uttc_bi_log2(const uttc_big_integer*fn){uttc_uint32_t bE=fn->cf;while(bE>0&&fn->Y[bE-1]==0)--bE;if(!bE)return 0;bE-=1;return(uttc_uint32_t)uttc_log2_32(fn->Y[bE])+bE*32;}typedef enum{UTTC_CUTOFFMODE_UNIQUE,UTTC_CUTOFFMODE_TOTALLENGTH,UTTC_CUTOFFMODE_FRACTIONLENGTH}uttc_cuttoffmode_t;UTTC_LINKAGE uttc_uint32_t uttc_al_float_dragon4(const uttc_big_integer*cu,const uttc_int32_t aY,const uttc_uint32_t cv,const bool bx,const uttc_cuttoffmode_t au,uttc_uint32_t av,char*aC,uttc_uint32_t aD,uttc_int32_t*dq){char*an=aC;uttc_assert(aD>0);if(uttc_bi_is_zero(cu)){*an='0';*dq=0;return 1;}uttc_big_integer ej;uttc_big_integer em;uttc_big_integer el;uttc_big_integer*ek;uttc_big_integer dm;if(bx){if(aY>0){uttc_bi_assign(&em,cu);uttc_bi_shl(&em,aY+2);uttc_bi_set_32(&ej,4);uttc_bi_pow2(&el,aY);uttc_bi_pow2(&dm,aY+1);}else{uttc_bi_assign(&em,cu);uttc_bi_shl(&em,2);uttc_bi_pow2(&ej,-aY+2);uttc_bi_set_32(&el,1);uttc_bi_set_32(&dm,2);}ek=&dm;}else{if(aY>0){uttc_bi_assign(&em,cu);uttc_bi_shl(&em,aY+1);uttc_bi_set_32(&ej,2);uttc_bi_pow2(&el,aY);}else{uttc_bi_assign(&em,cu);uttc_bi_shl(&em,1);uttc_bi_pow2(&ej,-aY+1);uttc_bi_set_32(&el,1);}ek=&el;}const uttc_ieee754_double cl=0.30102999566398119521373889472449;uttc_int32_t aG=(uttc_int32_t)uttc_ceil((uttc_ieee754_double)((uttc_int32_t)cv+aY)*cl-0.69);if(au==UTTC_CUTOFFMODE_FRACTIONLENGTH&&aG<=-(uttc_int32_t)av)aG=-(uttc_int32_t)av+1;if(aG>0){uttc_big_integer eL;uttc_bi_mutliply_pow10(&eL,&ej,aG);ej=eL;}else if(aG<0){uttc_big_integer dA;uttc_bi_pow10(&dA,-aG);uttc_big_integer eL;uttc_bi_mutliply(&eL,&em,&dA);em=eL;uttc_bi_mutliply(&eL,&el,&dA);el=eL;if(ek!=&el)uttc_bi_times2(ek,&el);}if(uttc_bi_compare(&em,&ej)>=0){aG=aG+1;}else{uttc_bi_times10(&em);uttc_bi_times10(&el);if(ek!=&el)uttc_bi_times2(ek,&el);}uttc_int32_t at=aG-aD;switch(au){case UTTC_CUTOFFMODE_UNIQUE:break;case UTTC_CUTOFFMODE_TOTALLENGTH:{uttc_int32_t aA=aG-(uttc_int32_t)av;if(aA>at)at=aA;}break;case UTTC_CUTOFFMODE_FRACTIONLENGTH:{uttc_int32_t aA=-(uttc_int32_t)av;if(aA>at)at=aA;}break;}*dq=aG-1;uttc_assert(ej.cf>0);uttc_uint32_t bC=ej.Y[ej.cf-1];if(bC<8||bC>429496729){uttc_uint32_t bD=uttc_log2_32(bC);uttc_uint32_t ep=(32+27-bD)%32;uttc_bi_shl(&ej,ep);uttc_bi_shl(&em,ep);uttc_bi_shl(&el,ep);if(ek!=&el)uttc_bi_times2(ek,&el);}bool cp;bool bz;uttc_uint32_t dr;if(au==UTTC_CUTOFFMODE_UNIQUE){while(true){aG=aG-1;dr=uttc_bi_divmod_maxquot9(&em,&ej);uttc_assert(dr<10);uttc_big_integer en;uttc_bi_sum(&en,&em,ek);cp=uttc_bi_compare(&em,&el)<0;bz=uttc_bi_compare(&en,&ej)>0;if(cp|bz|(aG==at))break;*an=(char)('0'+dr);++an;uttc_bi_times10(&em);uttc_bi_times10(&el);if(ek!=&el)uttc_bi_times2(ek,&el);}}else{cp=false;bz=false;while(true){aG=aG-1;dr=uttc_bi_divmod_maxquot9(&em,&ej);uttc_assert(dr<10);if(uttc_bi_is_zero(&em)|(aG==at))break;*an=(char)('0'+dr);++an;uttc_bi_times10(&em);}}bool eg=cp;if(cp==bz){uttc_bi_double(&em);uttc_int32_t ae=uttc_bi_compare(&em,&ej);eg=ae<0;if(ae==0)eg=(dr&1)==0;}if(eg){*an=(char)('0'+dr);++an;}else{if(dr==9){while(true){if(an==aC){*an='1';++an;*dq+=1;break;}--an;if(*an!='9'){*an+=1;++an;break;}}}else{*an=(char)('0'+dr+1);++an;}}uttc_uint32_t ds=(uttc_uint32_t)(an-aC);uttc_assert(ds<=aD);return ds;}typedef enum{UTTC_FLOAT_FMT_POSITIONAL,UTTC_FLOAT_FMT_SCIENTIFIC,UTTC_FLOAT_FMT_MIXED,}uttc_float_format;UTTC_LINKAGE uttc_uint32_t uttc_al_float_format_positional(char*aC,uttc_uint32_t aD,const uttc_big_integer*cu,uttc_int32_t aY,uttc_uint32_t cv,bool bx,uttc_int32_t dC,const char*O){uttc_assert(aD>0);uttc_int32_t dJ;uttc_uint32_t cZ;uttc_uint32_t cA=aD-1;cZ=uttc_al_float_dragon4(cu,aY,cv,bx,dC<0?UTTC_CUTOFFMODE_UNIQUE:UTTC_CUTOFFMODE_FRACTIONLENGTH,dC<0?0:dC,aC,cA,&dJ);uttc_assert(cZ>0);uttc_assert(cZ<=aD);uttc_uint32_t cV=0;if(dJ>=0){uttc_uint32_t db=dJ+1;if(cZ<db){if(db>cA)db=cA;while(cZ<db)aC[cZ++]=O[1];}else if(cZ>(uttc_uint32_t)db){cV=cZ-db;uttc_uint32_t cy=cA-db-1;if(cV>cy)cV=cy;uttc_memmove(aC+db+1,aC+db,cV);aC[db]=O[0];cZ=db+1+cV;}}else{if(cA>2){uttc_uint32_t cW=(uttc_uint32_t)-dJ-1;uttc_uint32_t cz=cA-2;if(cW>cz)cW=cz;uttc_uint32_t aH=2+cW;cV=cZ;uttc_uint32_t cy=cA-aH;if(cV>cy)cV=cy;uttc_memmove(aC+aH,aC,cV);uttc_uint32_t i=2;while(i<aH)aC[i++]=O[1];cV+=cW;cZ=cV;}if(cA>1){aC[1]=O[0];cZ+=1;}if(cA>0){aC[0]=O[1];cZ+=1;}}if(dC>(uttc_int32_t)cV&&cZ<cA){if(cV==0)aC[cZ++]=O[0];uttc_uint32_t eV=cZ+(dC-cV);if(eV>cA)eV=cA;while(cZ<eV)aC[cZ++]=O[1];}uttc_assert(cZ<=cA);aC[cZ]='\0';return cZ;}UTTC_LINKAGE uttc_uint32_t uttc_al_float_format_scientific(char*aC,uttc_uint32_t aD,const uttc_big_integer*cu,uttc_int32_t aY,uttc_uint32_t cv,bool bx,uttc_int32_t dC,const char*O){uttc_assert(aD>0);uttc_int32_t dJ;uttc_uint32_t cZ;cZ=uttc_al_float_dragon4(cu,aY,cv,bx,dC<0?UTTC_CUTOFFMODE_UNIQUE:UTTC_CUTOFFMODE_TOTALLENGTH,dC<0?0:dC+1,aC,aD,&dJ);uttc_assert(cZ>0);uttc_assert(cZ<=aD);char*ao=aC;if(aD>1){ao+=1;aD-=1;}uttc_uint32_t cV=cZ-1;if(cV>0&&aD>1){uttc_uint32_t cy=aD-2;if(cV>cy)cV=cy;uttc_memmove(ao+1,ao,cV);ao[0]=O[0];ao+=(1+cV);aD-=(1+cV);}if(dC>(uttc_int32_t)cV&&aD>1){if(cV==0){*ao=O[0];++ao;--aD;}uttc_uint32_t dc=(dC-cV);if(dc>aD-1)dc=aD-1;char*aO=ao+dc;while(ao<aO)*ao++=O[1];}if(aD>1){char ba[6];uttc_uint8_t bb=0;ba[bb++]=O[15];if(dJ>=0)ba[bb++]='+';else{ba[bb++]='-';dJ=-dJ;}uttc_assert(dJ<10000);uttc_uint32_t eQ=dJ/1000;uttc_uint32_t bF=(dJ-eQ*1000)/100;uttc_uint32_t eO=(dJ-eQ*1000-bF*100)/10;uttc_uint32_t dl=(dJ-eQ*1000-bF*100-eO*10);if(eQ)ba[bb++]=O[1+eQ];if(bF)ba[bb++]=O[1+bF];ba[bb++]=O[1+eO];ba[bb++]=O[1+dl];uttc_uint32_t cx=aD-1;uttc_uint32_t bc=(bb<cx)?bb:cx;uttc_memcpy(ao,ba,bc);ao+=bc;aD-=bc;}uttc_assert(aD>0);ao[0]='\0';return ao-aC;}static void uttc_al_float_format_hex(char*aC,const uttc_big_integer*fn,uttc_uint32_t cX,const char*O){if(uttc_bi_is_zero(fn)){*aC='0';return;}uttc_big_integer al;uttc_bi_assign(&al,fn);do{aC[--cX]=O[(al.Y[0]&0xF)+1];uttc_bi_shr(&al,4);}while(!uttc_bi_is_zero(&al));}static uttc_uint32_t uttc_al_float_format_special(char*aC,uttc_uint32_t aD,const uttc_big_integer*cu,const char*O){uttc_assert(aD>0);uttc_uint32_t cA=aD-1;if(uttc_bi_is_zero(cu)){uttc_uint32_t dK=(8<cA)?8:cA;char bL[8]={O[19],O[24],O[16],O[19],O[24],O[19],O[30],O[35]};uttc_memcpy(aC,bL,dK);aC[dK]='\0';return dK;}else{uttc_uint32_t dK=(3<cA)?3:cA;char cH[3]={O[24],O[11],O[24]};uttc_memcpy(aC,cH,dK);if(cA>3)aC[dK++]='(';if(cA>5){aC[dK++]='0';aC[dK++]=O[34];}uttc_uint32_t cX=uttc_bi_log2(cu)/4+1;if(cA>=dK+cX){uttc_al_float_format_hex(aC+dK,cu,cX,O);dK+=cX;}if(dK<cA)aC[dK++]=')';aC[dK]='\0';return dK;}}UTTC_LINKAGE uttc_uint32_t uttc_ieee754_print_double(char*aC,uttc_uint32_t aD,uttc_ieee754_double fn,uttc_float_format format,uttc_int32_t dC,const char*O){if(aD==0)return 0;if(aD==1){aC[0]='\0';return 0;}uttc_uint32_t bm=uttc_exponent(fn);uttc_uint64_t bn=uttc_mantissa(fn);uttc_uint32_t dG=0;if(uttc_signbit(fn)){aC[0]='-';++aC;--aD;++dG;uttc_assert(aD>0);}if(bm==(UTTC_EXPONENT_MASK>>UTTC_EXPONENT_SHIFT)){uttc_big_integer cu;uttc_bi_set_64(&cu,bn);return uttc_al_float_format_special(aC,aD,&cu,O)+dG;}else{uttc_big_integer cu;uttc_int32_t aY;uttc_uint32_t cv;bool bx;if(bm!=0){uttc_bi_set_64(&cu,(1ull<<52)|bn);aY=bm-1023-52;cv=52;bx=(bm!=1)&&(bn==0);}else{uttc_bi_set_64(&cu,bn);aY=1-1023-52;cv=uttc_log2_64(bn);bx=false;}if(format==UTTC_FLOAT_FMT_MIXED){uttc_int32_t dJ;uttc_int32_t dI=uttc_al_float_dragon4(&cu,aY,cv,bx,dC<0?UTTC_CUTOFFMODE_UNIQUE:UTTC_CUTOFFMODE_TOTALLENGTH,dC<0?0:1,aC,aD-1,&dJ);if(dC<0){if(dJ>0&&dJ>=dI)dI=dJ+1;if(dC<-dI)dC=dI;else dC=-dC;}if(dC>dJ&&dJ>=-4)format=UTTC_FLOAT_FMT_POSITIONAL,dC=dC-1-dJ;else format=UTTC_FLOAT_FMT_SCIENTIFIC,dC-=1;}switch(format){case UTTC_FLOAT_FMT_POSITIONAL:return uttc_al_float_format_positional(aC,aD,&cu,aY,cv,bx,dC,O)+dG;case UTTC_FLOAT_FMT_SCIENTIFIC:return uttc_al_float_format_scientific(aC,aD,&cu,aY,cv,bx,dC,O)+dG;default:aC[0]='\0';return 0;}}}UTTC_LINKAGE uttc_uint32_t uttc_ieee754_print_float(char*aC,uttc_uint32_t aD,uttc_ieee754_float fn,uttc_float_format format,uttc_int32_t dC,const char*O){if(aD==0)return 0;if(aD==1){aC[0]='\0';return 0;}uttc_uint32_t bm=uttc_exponentf(fn);uttc_uint32_t bn=uttc_mantissaf(fn);uttc_uint32_t dG=0;if(uttc_signbitf(fn)){aC[0]='-';++aC;--aD;++dG;uttc_assert(aD>0);}if(bm==(UTTC_EXPONENT_MASKF>>UTTC_EXPONENT_SHIFTF)){uttc_big_integer cu;uttc_bi_set_32(&cu,bn);return uttc_al_float_format_special(aC,aD,&cu,O)+dG;}else{uttc_big_integer cu;uttc_int32_t aY;uttc_uint32_t cv;bool bx;if(bm!=0){uttc_bi_set_32(&cu,(1UL<<23)|bn);aY=bm-127-23;cv=23;bx=(bm!=1)&&(bn==0);}else{uttc_bi_set_32(&cu,bn);aY=1-127-23;cv=uttc_log2_32(bn);bx=false;}if(format==UTTC_FLOAT_FMT_MIXED){uttc_int32_t dJ;uttc_int32_t dI=uttc_al_float_dragon4(&cu,aY,cv,bx,dC<0?UTTC_CUTOFFMODE_UNIQUE:UTTC_CUTOFFMODE_TOTALLENGTH,dC<0?0:1,aC,aD-1,&dJ);if(dC<0){if(dJ>0&&dJ>=dI)dI=dJ+1;if(dC<-dI)dC=dI;else dC=-dC;}if(dC>dJ&&dJ>=-4)format=UTTC_FLOAT_FMT_POSITIONAL,dC=dC-1-dJ;else format=UTTC_FLOAT_FMT_SCIENTIFIC,dC-=1;}switch(format){case UTTC_FLOAT_FMT_POSITIONAL:return uttc_al_float_format_positional(aC,aD,&cu,aY,cv,bx,dC,O)+dG;case UTTC_FLOAT_FMT_SCIENTIFIC:return uttc_al_float_format_scientific(aC,aD,&cu,aY,cv,bx,dC,O)+dG;default:aC[0]='\0';return 0;}}}UTTC_LINKAGE uttc_uint32_t uttc_ieee754_print_half(char*aC,uttc_uint32_t aD,uttc_ieee754_half fn,uttc_float_format format,uttc_int32_t dC,const char*O){if(aD==0)return 0;if(aD==1){aC[0]='\0';return 0;}uttc_uint16_t bm=uttc_exponenth(fn);uttc_uint16_t bn=uttc_mantissah(fn);uttc_uint32_t dG=0;if(uttc_signbith(fn)){aC[0]='-';++aC;--aD;++dG;uttc_assert(aD>0);}if(bm==(UTTC_EXPONENT_MASKH>>UTTC_EXPONENT_SHIFTH)){uttc_big_integer cu;uttc_bi_set_32(&cu,bn);return uttc_al_float_format_special(aC,aD,&cu,O)+dG;}else{uttc_big_integer cu;uttc_int32_t aY;uttc_uint32_t cv;bool bx;if(bm!=0){uttc_bi_set_32(&cu,(1UL<<10)|bn);aY=bm-15-10;cv=10;bx=(bm!=1)&&(bn==0);}else{uttc_bi_set_32(&cu,bn);aY=1-15-10;cv=uttc_log2_16(bn);bx=false;}if(format==UTTC_FLOAT_FMT_MIXED){uttc_int32_t dJ;uttc_int32_t dI=uttc_al_float_dragon4(&cu,aY,cv,bx,dC<0?UTTC_CUTOFFMODE_UNIQUE:UTTC_CUTOFFMODE_TOTALLENGTH,dC<0?0:1,aC,aD-1,&dJ);if(dC<0){if(dJ>0&&dJ>=dI)dI=dJ+1;if(dC<-dI)dC=dI;else dC=-dC;}if(dC>dJ&&dJ>=-4)format=UTTC_FLOAT_FMT_POSITIONAL,dC=dC-1-dJ;else format=UTTC_FLOAT_FMT_SCIENTIFIC,dC-=1;}switch(format){case UTTC_FLOAT_FMT_POSITIONAL:return uttc_al_float_format_positional(aC,aD,&cu,aY,cv,bx,dC,O)+dG;case UTTC_FLOAT_FMT_SCIENTIFIC:return uttc_al_float_format_scientific(aC,aD,&cu,aY,cv,bx,dC,O)+dG;default:aC[0]='\0';return 0;}}}UTTC_LINKAGE uttc_uint32_t uttc_ieee754_print_half_alt(char*aC,uttc_uint32_t aD,uttc_ieee754_half_alt fn,uttc_float_format format,uttc_int32_t dC,const char*O){if(aD==0)return 0;if(aD==1){aC[0]='\0';return 0;}uttc_uint16_t bm=uttc_exponenth2(fn);uttc_uint16_t bn=uttc_mantissah2(fn);uttc_uint32_t dG=0;if(uttc_signbith2(fn)){aC[0]='-';++aC;--aD;++dG;uttc_assert(aD>0);}uttc_big_integer cu;uttc_int32_t aY;uttc_uint32_t cv;bool bx;if(bm!=0){uttc_bi_set_32(&cu,(1UL<<10)|bn);aY=bm-15-10;cv=10;bx=(bm!=1)&&(bn==0);}else{uttc_bi_set_32(&cu,bn);aY=1-15-10;cv=uttc_log2_16(bn);bx=false;}if(format==UTTC_FLOAT_FMT_MIXED){uttc_int32_t dJ;uttc_int32_t dI=uttc_al_float_dragon4(&cu,aY,cv,bx,dC<0?UTTC_CUTOFFMODE_UNIQUE:UTTC_CUTOFFMODE_TOTALLENGTH,dC<0?0:1,aC,aD-1,&dJ);if(dC<0){if(dJ>0&&dJ>=dI)dI=dJ+1;if(dC<-dI)dC=dI;else dC=-dC;}if(dC>dJ&&dJ>=-4)format=UTTC_FLOAT_FMT_POSITIONAL,dC=dC-1-dJ;else format=UTTC_FLOAT_FMT_SCIENTIFIC,dC-=1;}switch(format){case UTTC_FLOAT_FMT_POSITIONAL:return uttc_al_float_format_positional(aC,aD,&cu,aY,cv,bx,dC,O)+dG;case UTTC_FLOAT_FMT_SCIENTIFIC:return uttc_al_float_format_scientific(aC,aD,&cu,aY,cv,bx,dC,O)+dG;default:aC[0]='\0';return 0;}}UTTC_LINKAGE void uttc_print_double(uttc_ieee754_double fm){char cC[50];uttc_ieee754_print_double(cC,50,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");uttc_print_string(cC);}UTTC_LINKAGE uttc_size_t uttc_serialize_double(uttc_ieee754_float fm,char*aa){return uttc_ieee754_print_double(aa,50,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");}UTTC_LINKAGE void uttc_print_float(uttc_ieee754_float fm){char cC[50];uttc_ieee754_print_float(cC,50,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");uttc_print_string(cC);}UTTC_LINKAGE uttc_size_t uttc_serialize_float(uttc_ieee754_float fm,char*aa){return uttc_ieee754_print_float(aa,50,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");}UTTC_LINKAGE void uttc_print_half(uttc_ieee754_half fm){char cC[50];uttc_ieee754_print_half(cC,50,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");uttc_print_string(cC);}UTTC_LINKAGE uttc_size_t uttc_serialize_half(uttc_ieee754_float fm,char*aa){return uttc_ieee754_print_half(aa,50,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");}UTTC_LINKAGE void uttc_print_half_alt(uttc_ieee754_half_alt fm){char cC[50];uttc_ieee754_print_half_alt(cC,50,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");uttc_print_string(cC);}UTTC_LINKAGE uttc_size_t uttc_serialize_half_alt(uttc_ieee754_float fm,char*aa){return uttc_ieee754_print_half_alt(aa,50,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");}
#ifndef UTTC_BF_MANTISSA_BYTES
#define UTTC_BF_MANTISSA_BYTES 16
#endif
#ifndef UTTC_BF_TOTAL_BYTES
#define UTTC_BF_TOTAL_BYTES 20
#endif
#define UTTC_BF_EXPONENT_BYTES ( UTTC_BF_TOTAL_BYTES - UTTC_BF_MANTISSA_BYTES )
#define UTTC_BF_EXPONENT_MAX ( ( 1uL << ( UTTC_BF_EXPONENT_BYTES * 8uL - 1uL ) ) - 1uL )
#define UTTC_BF_NAN "\177\377\377\377\377\177\377\377\377\377\377\377\377\377\377\377\377\377\377\377"
#define UTTC_BF_INFINITY "\177\377\377\377\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTTC_BF_NINFINITY "\377\377\377\377\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTTC_BF_0 "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTTC_BF_N0 "\200\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTTC_BF_1 "\077\377\377\377\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTTC_BF_N1 "\277\377\377\377\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
typedef struct uttc_big_float{uttc_uint8_t aw[UTTC_BF_TOTAL_BYTES];}uttc_big_float;
#define uttc_from_rawb( fn ) ((uttc_big_float){ .aw = fn })
#define UTTC_EXPONENT_BIASB ( ( 1 << ( ( UTTC_BF_TOTAL_BYTES - UTTC_BF_MANTISSA_BYTES ) * 8 - 2 ) ) - 1 )
#define UTTC_EXPONENT_SHIFTB UTTC_BF_MANTISSA_BYTES * 8
#define UTTC_INFINITYB uttc_from_rawb( UTTC_BF_INFINITY )
#define UTTC_NINFINITYB uttc_from_rawb( UTTC_BF_NINFINITY )
#define UTTC_NANB uttc_from_rawb( UTTC_BF_NAN )
UTTC_LINKAGE bool uttc_signbitb(const uttc_big_float fm){return fm.aw[0]>>7u;}UTTC_LINKAGE void uttc_set_signbitb(uttc_big_float*fm,bool et){if(et)fm->aw[0]|=0x80u;else fm->aw[0]&=0x7Fu;}UTTC_LINKAGE uttc_big_float uttc_negateb(uttc_big_float fm){fm.aw[0]^=0x80u;return fm;}UTTC_LINKAGE uttc_uint32_t uttc_exponentb(const uttc_big_float fm){uttc_uint32_t dY=fm.aw[0]&0x7F;unsigned int i=1;while(i<UTTC_BF_EXPONENT_BYTES){dY<<=8;dY|=fm.aw[i];i++;}return dY;}UTTC_LINKAGE void uttc_set_exponentb(uttc_big_float*fm,uttc_uint32_t aY){uttc_uint32_t aq=UTTC_BF_EXPONENT_BYTES-1;while(aq>0){fm->aw[aq--]=aY&0xFF;aY>>=8;}fm->aw[0]=(aY&0x7Fu)|(uttc_signbitb(*fm)<<7u);}UTTC_LINKAGE uttc_big_integer uttc_mantissab(const uttc_big_float fm){uttc_big_integer dY;unsigned int i=UTTC_BF_EXPONENT_BYTES;while(i<UTTC_BF_TOTAL_BYTES&&!fm.aw[i])i++;if(i<UTTC_BF_TOTAL_BYTES){uttc_bi_set_nonempty_zero(&dY);do{uttc_bi_shl(&dY,8);dY.Y[0]|=fm.aw[i++];}while(i<UTTC_BF_TOTAL_BYTES);}else uttc_bi_set_zero(&dY);return dY;}UTTC_LINKAGE void uttc_set_mantissab(uttc_big_float*fm,uttc_big_integer cu){unsigned int i=UTTC_BF_TOTAL_BYTES-1;while(i>=UTTC_BF_EXPONENT_BYTES){if(!cu.cf)break;fm->aw[i--]=cu.Y[0]&0xFFu;uttc_bi_shr(&cu,8);}while(i>=UTTC_BF_EXPONENT_BYTES)fm->aw[i--]=0u;}UTTC_LINKAGE uttc_ieee754_class_t uttc_fp_classifyb(const uttc_big_float fm){unsigned int i;if(uttc_exponentb(fm)==UTTC_BF_EXPONENT_MAX){for(i=UTTC_BF_EXPONENT_BYTES;i<UTTC_BF_TOTAL_BYTES;i++)if(fm.aw[i])return UTTC_FP_NAN;return UTTC_FP_INFINITE;}else if(!uttc_exponentb(fm)){for(i=UTTC_BF_EXPONENT_BYTES;i<UTTC_BF_TOTAL_BYTES;i++)if(fm.aw[i])return UTTC_FP_SUBNORMAL;return UTTC_FP_ZERO;}return UTTC_FP_NORMAL;}UTTC_LINKAGE bool uttc_isnanb(const uttc_big_float fm){return uttc_fp_classifyb(fm)==UTTC_FP_NAN;}UTTC_LINKAGE bool uttc_isinfb(const uttc_big_float fm){return uttc_fp_classifyb(fm)==UTTC_FP_INFINITE;}UTTC_LINKAGE uttc_big_float uttc_fabsb(uttc_big_float fm){fm.aw[0]&=0x7F;return fm;}
#define uttc_nanb( fm ) uttc_nanb_implementation( #fm )
UTTC_LINKAGE uttc_big_float uttc_nanb_implementation(const char*eD){
#define getxalpha( c ) ((unsigned char)(c & 0xDF) - 'A')
#define getxnum( c ) ((unsigned char)(c - '0'))
if(eD[0]=='0'&&(eD[1]&0xDF)=='X')eD+=2;uttc_big_float dY={UTTC_BF_INFINITY};uttc_big_integer dy;uttc_bi_set_nonempty_zero(&dy);unsigned char fm;while(*eD){if((fm=getxnum(*eD))<10u){}else if((fm=getxalpha(*eD))<6u)fm+=10;else break;uttc_bi_shl(&dy,4);dy.Y[0]|=fm;++eD;}if(uttc_bi_is_zero(&dy))dY=UTTC_NANB;else uttc_set_mantissab(&dY,dy);
#undef getxalpha
#undef getxnum
return dY;}
#define uttc_nanpayloadb( ... ) uttc_mantissab( __VA_ARGS__ )
UTTC_LINKAGE uttc_uint32_t uttc_bf_print(char*aC,uttc_uint32_t aD,uttc_big_float fn,uttc_float_format format,uttc_int32_t dC,const char*O){if(aD==0)return 0;if(aD==1){aC[0]='\0';return 0;}uttc_uint32_t bm=uttc_exponentb(fn);uttc_big_integer bn=uttc_mantissab(fn);uttc_uint32_t dG=0;if(uttc_signbitb(fn)){aC[0]='-';++aC;--aD;++dG;uttc_assert(aD>0);}if(bm==UTTC_BF_EXPONENT_MAX)return uttc_al_float_format_special(aC,aD,&bn,O)+dG;else{uttc_big_integer cu;uttc_int32_t aY;uttc_uint32_t cv;bool bx;if(bm!=0){uttc_big_integer bY;uttc_bi_set_32(&bY,1);uttc_bi_shl(&bY,UTTC_BF_MANTISSA_BYTES*8);uttc_bi_sum(&cu,&bn,&bY);aY=bm-UTTC_EXPONENT_BIASB-(UTTC_BF_MANTISSA_BYTES*8);cv=(UTTC_BF_MANTISSA_BYTES*8);bx=(bm!=1)&&uttc_bi_is_zero(&bn);}else{uttc_bi_assign(&cu,&bn);aY=1-UTTC_EXPONENT_BIASB-(UTTC_BF_MANTISSA_BYTES*8);cv=uttc_bi_log2(&cu);bx=false;}if(format==UTTC_FLOAT_FMT_MIXED){uttc_int32_t dJ;uttc_int32_t dI=uttc_al_float_dragon4(&cu,aY,cv,bx,dC<0?UTTC_CUTOFFMODE_UNIQUE:UTTC_CUTOFFMODE_TOTALLENGTH,dC<0?0:1,aC,aD-1,&dJ);if(dC<0){if(dJ>0&&dJ>=dI)dI=dJ+1;if(dC<-dI)dC=dI;else dC=-dC;}if(dC>dJ&&dJ>=-4)format=UTTC_FLOAT_FMT_POSITIONAL,dC=dC-1-dJ;else format=UTTC_FLOAT_FMT_SCIENTIFIC,dC-=1;}switch(format){case UTTC_FLOAT_FMT_POSITIONAL:return uttc_al_float_format_positional(aC,aD,&cu,aY,cv,bx,dC,O)+dG;case UTTC_FLOAT_FMT_SCIENTIFIC:return uttc_al_float_format_scientific(aC,aD,&cu,aY,cv,bx,dC,O)+dG;default:aC[0]='\0';return 0;}}}UTTC_LINKAGE void uttc_print_big_float(uttc_big_float fm){char cC[100];uttc_bf_print(cC,100,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");uttc_print_string(cC);}UTTC_LINKAGE uttc_size_t uttc_serialize_big_float(uttc_big_float fm,char*aC){return uttc_bf_print(aC,100,fm,UTTC_FLOAT_FMT_SCIENTIFIC,-1,".0123456789abcdefghijklmnopqrstuvwxyz");}
#ifndef TARGET_SILENT
extern
#ifdef __cplusplus
"C"
#endif
int putchar(int);UTTC_LINKAGE void uttc_print_char(char ch){putchar(ch);}
#else
UTTC_LINKAGE void uttc_print_char(char){}
#endif
UTTC_LINKAGE uttc_size_t uttc_serialize_char(char ch,char*aC){if(aC)*aC=ch;return 1;}UTTC_LINKAGE void uttc_print_string(const char*eE){if(!eE)return;while(*eE)uttc_print_char(*eE++);}UTTC_LINKAGE uttc_size_t uttc_serialize_string(const char*eE,char*aC){if(!eE)return aC[0]='\0';uttc_size_t cc=0;while(*eE)cc+=uttc_serialize_char(*eE++,aC++);*aC='\0';return cc;}UTTC_LINKAGE char*uttc_print_int_ex(int fn,unsigned int R,const char*O){static char cC[65];bool et;char*dY=&cC[sizeof(cC)];*--dY='\0';if(fn==UTTC_INT_MIN){*--dY=O[-(UTTC_INT_MIN+R)%R];fn+=R;fn/=-(int)R;++fn;et=true;}else{et=fn<0;fn=et?-fn:fn;}do{*--dY=O[fn%R];fn/=R;}while(fn);if(et)*--dY='-';return dY;}UTTC_LINKAGE void uttc_print_int(int fn){uttc_print_string(uttc_print_int_ex(fn,10,"0123456789"));}UTTC_LINKAGE uttc_size_t uttc_serialize_int(int fn,char*aC){return uttc_serialize_string(uttc_print_int_ex(fn,10,"0123456789"),aC);}UTTC_LINKAGE char*uttc_print_long_ex(long long int fn,unsigned int R,const char*O){static char cC[129];bool et;char*dY=&cC[sizeof(cC)];*--dY='\0';if(fn==UTTC_LONGLONG_MIN){*--dY=O[-(UTTC_LONGLONG_MIN+R)%R];fn+=R;fn/=-(int)R;++fn;et=true;}else{et=fn<0;fn=et?-fn:fn;}do{*--dY=O[fn%R];fn/=R;}while(fn);if(et)*--dY='-';return dY;}UTTC_LINKAGE void uttc_print_long(long long int fn){uttc_print_string(uttc_print_long_ex(fn,10,"0123456789"));}UTTC_LINKAGE uttc_size_t uttc_serialize_long(long long int fn,char*aC){return uttc_serialize_string(uttc_print_long_ex(fn,10,"0123456789"),aC);}UTTC_LINKAGE char*uttc_print_unsigned_int_ex(unsigned int fn,unsigned int R,const char*O){static char cC[65];char*dY=&cC[sizeof(cC)];*--dY='\0';do{*--dY=O[fn%R],fn/=R;}while(fn);return dY;}UTTC_LINKAGE void uttc_print_unsigned_int(unsigned int fn){uttc_print_string(uttc_print_unsigned_int_ex(fn,10,"0123456789"));}UTTC_LINKAGE uttc_size_t uttc_serialize_unsigned_int(unsigned int fn,char*aC){return uttc_serialize_string(uttc_print_unsigned_int_ex(fn,10,"0123456789"),aC);}UTTC_LINKAGE char*uttc_print_unsigned_long_ex(unsigned long long fn,unsigned int R,const char*O){static char cC[129];char*dY=&cC[sizeof(cC)];*--dY='\0';do{*--dY=O[fn%R],fn/=R;}while(fn);return dY;}UTTC_LINKAGE void uttc_print_unsigned_long(unsigned long long fn){uttc_print_string(uttc_print_unsigned_long_ex(fn,10,"0123456789"));}UTTC_LINKAGE uttc_size_t uttc_serialize_unsigned_long(unsigned long long fn,char*aC){return uttc_serialize_string(uttc_print_unsigned_long_ex(fn,10,"0123456789"),aC);}UTTC_LINKAGE void uttc_print_cesura(){uttc_print_char(6);}UTTC_LINKAGE void uttc_print_bool(bool fm){uttc_print_string(fm?"true":"false");}UTTC_LINKAGE uttc_size_t uttc_serialize_bool(bool fm,char*aC){return uttc_serialize_string(fm?"true":"false",aC);}UTTC_LINKAGE char*uttc_print_big_integer_ex(uttc_big_integer*fn,unsigned int R,const char*O){static char cC[200];uttc_uint8_t ep;if(uttc_bi_is_zero(fn))return"0";if(R==10){char*bR=&cC[0];uttc_uint32_t dB=0;uttc_big_integer aK;uttc_big_integer al;uttc_bi_assign(&al,fn);do{++dB;uttc_bi_pow10(&aK,dB);}while(uttc_bi_compare(&aK,&al)<=0);while(true){dB--;uttc_bi_pow10(&aK,dB);uttc_uint8_t aF=0;while(!uttc_bi_sub(&al,&aK))aF++;*bR++=O[aF];if(uttc_bi_is_zero(&al))break;uttc_bi_sub(&aK,&al);uttc_bi_assign(&al,&aK);}while(dB-->0)*bR++=O[0];*bR++='\0';return&cC[0];}else if(R==(1<<(ep=uttc_log2_32(R)))){char*dY=&cC[sizeof(cC)];*--dY='\0';uttc_big_integer al;uttc_bi_assign(&al,fn);do{*--dY=O[al.Y[0]&(R-1)];uttc_bi_shr(&al,ep);}while(!uttc_bi_is_zero(&al));return dY;}char*bR=&cC[0];uttc_big_integer al;uttc_bi_assign(&al,fn);uttc_uint32_t dB=0;uttc_big_integer aK;do{++dB;uttc_bi_set_32(&aK,1);uttc_uint32_t i=0;while(i++<dB)uttc_bi_times(&aK,R);}while(uttc_bi_compare(&aK,&al)<=0);while(true){dB--;uttc_bi_set_32(&aK,1);uttc_uint32_t i=0;while(i++<dB)uttc_bi_times(&aK,R);uttc_uint8_t aF=0;while(!uttc_bi_sub(&al,&aK))aF++;*bR++=O[aF];if(uttc_bi_is_zero(&al))break;uttc_bi_sub(&aK,&al);uttc_bi_assign(&al,&aK);}while(dB-->0)*bR++=O[0];*bR++='\0';return&cC[0];}UTTC_LINKAGE void uttc_print_big_integer(uttc_big_integer*fn){uttc_print_string(uttc_print_big_integer_ex(fn,10,"0123456789"));}UTTC_LINKAGE uttc_size_t uttc_serialize_big_integer(uttc_big_integer*fn,char*aC){return uttc_serialize_string(uttc_print_big_integer_ex(fn,10,"0123456789"),aC);}
#define PRINT_BUFFER_SIZE 256
UTTC_LINKAGE void uttc_vprint(const char*bq,uttc_va_list Q){if(!bq)return;bool bM=false;unsigned int fb=2;bool ca=false;unsigned char bp=0;bool P=false;bool dv=false;int bh=0;int dC=UTTC_INT_MIN;bool dF=false;bool bi=false;bool dD=false;static char aa[PRINT_BUFFER_SIZE];unsigned int ax=0;while(*bq){if(*bq=='%'){if(bM){uttc_print_char('%');bM=false;}else bM=true;}else if(bM){if(*bq=='-'&&!bi&&!dD)ca=true;if(*bq=='-'&&dD&&!dF&&dC==UTTC_INT_MIN)dF=true;else if(*bq=='+'&&!bi&&!dD)bp=true;else if(*bq=='0'&&!bi&&!dD)dv=true;else if(*bq=='#'&&!bi&&!dD&&!P)P=true;else if(*bq>='0'&&*bq<='9'){int fn=*bq-'0';if(dD){if(dC<0&&!dF)dC=0;dC=dC*10+(dF?-fn:fn);}else{bh=bh*10+(*bq-'0');bi=true;}}else if(*bq=='.'&&!dD)dD=true;else if(*bq=='*'&&!bi&&!dD){bh=uttc_va_arg(Q,int);bi=true;if(bh<0){bh=-bh;ca=true;}}else if(*bq=='*'&&dD){dC=uttc_va_arg(Q,int);dD=true;}else if(*bq=='d'||*bq=='i'){
#define DO_PRINT( y , C ) ax = y( uttc_va_arg( Q , C ) , aa )
switch(fb){case 0:DO_PRINT(uttc_serialize_int,int);break;case 1:DO_PRINT(uttc_serialize_int,int);break;case 2:DO_PRINT(uttc_serialize_int,int);break;case 3:DO_PRINT(uttc_serialize_long,long);break;case 4:DO_PRINT(uttc_serialize_long,long long);break;case 5:DO_PRINT(uttc_serialize_long,uttc_intmax_t);break;case 6:DO_PRINT(uttc_serialize_long,uttc_intptr_t);break;case 7:DO_PRINT(uttc_serialize_long,uttc_ptrdiff_t);break;case 8:DO_PRINT(uttc_serialize_big_integer,uttc_big_integer*);break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
if(dC==0&&aa[0]=='0'&&ax==1){aa[0]='\0';ax=0;}goto ac;}else if(*bq=='o'){
#define DO_PRINT( y , C ) ax = uttc_serialize_string( y( uttc_va_arg( Q , C ) , 8 , "01234567" ) , aa )
switch(fb){case 0:DO_PRINT(uttc_print_unsigned_int_ex,int);break;case 1:DO_PRINT(uttc_print_unsigned_int_ex,int);break;case 2:DO_PRINT(uttc_print_unsigned_int_ex,int);break;case 3:DO_PRINT(uttc_print_unsigned_long_ex,long);break;case 4:DO_PRINT(uttc_print_unsigned_long_ex,long long);break;case 5:DO_PRINT(uttc_print_unsigned_long_ex,uttc_intmax_t);break;case 6:DO_PRINT(uttc_print_unsigned_long_ex,uttc_intptr_t);break;case 7:DO_PRINT(uttc_print_unsigned_long_ex,uttc_ptrdiff_t);break;case 8:DO_PRINT(uttc_print_big_integer_ex,uttc_big_integer*);break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
if(dC==0&&aa[0]=='0'&&ax==1){aa[0]='\0';ax=0;}goto ac;}else if(*bq=='x'){
#define DO_PRINT( y , C ) ax = uttc_serialize_string( y( uttc_va_arg( Q , C ) , 16 , "0123456789abcdef" ) , aa )
switch(fb){case 0:DO_PRINT(uttc_print_unsigned_int_ex,int);break;case 1:DO_PRINT(uttc_print_unsigned_int_ex,int);break;case 2:DO_PRINT(uttc_print_unsigned_int_ex,int);break;case 3:DO_PRINT(uttc_print_unsigned_long_ex,long);break;case 4:DO_PRINT(uttc_print_unsigned_long_ex,long long);break;case 5:DO_PRINT(uttc_print_unsigned_long_ex,uttc_intmax_t);break;case 6:DO_PRINT(uttc_print_unsigned_long_ex,uttc_intptr_t);break;case 7:DO_PRINT(uttc_print_unsigned_long_ex,uttc_ptrdiff_t);break;case 8:DO_PRINT(uttc_print_big_integer_ex,uttc_big_integer*);break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
if(dC==0&&aa[0]=='0'&&ax==1){aa[0]='\0';ax=0;}goto ac;}else if(*bq=='X'){
#define DO_PRINT( y , C ) ax = uttc_serialize_string( y( uttc_va_arg( Q , C ) , 16 , "0123456789ABCDEF" ) , aa )
switch(fb){case 0:DO_PRINT(uttc_print_unsigned_int_ex,unsigned int);break;case 1:DO_PRINT(uttc_print_unsigned_int_ex,unsigned int);break;case 2:DO_PRINT(uttc_print_unsigned_int_ex,unsigned int);break;case 3:DO_PRINT(uttc_print_unsigned_long_ex,unsigned long);break;case 4:DO_PRINT(uttc_print_unsigned_long_ex,unsigned long long);break;case 5:DO_PRINT(uttc_print_unsigned_long_ex,uttc_uintmax_t);break;case 6:DO_PRINT(uttc_print_unsigned_long_ex,uttc_size_t);break;case 7:DO_PRINT(uttc_print_unsigned_long_ex,uttc_uintptr_t);break;case 8:DO_PRINT(uttc_print_big_integer_ex,uttc_big_integer*);break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
if(dC==0&&aa[0]=='0'&&ax==1){aa[0]='\0';ax=0;}goto ac;}else if(*bq=='u'){
#define DO_PRINT( y , C ) ax = uttc_serialize_string( y( uttc_va_arg( Q , C ) , 10 , "0123456789" ) , aa )
switch(fb){case 0:DO_PRINT(uttc_print_unsigned_int_ex,unsigned int);break;case 1:DO_PRINT(uttc_print_unsigned_int_ex,unsigned int);break;case 2:DO_PRINT(uttc_print_unsigned_int_ex,unsigned int);break;case 3:DO_PRINT(uttc_print_unsigned_long_ex,unsigned long);break;case 4:DO_PRINT(uttc_print_unsigned_long_ex,unsigned long long);break;case 5:DO_PRINT(uttc_print_unsigned_long_ex,uttc_uintmax_t);break;case 6:DO_PRINT(uttc_print_unsigned_long_ex,uttc_size_t);break;case 7:DO_PRINT(uttc_print_unsigned_long_ex,uttc_uintptr_t);break;case 8:DO_PRINT(uttc_print_big_integer_ex,uttc_big_integer*);break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
if(dC==0&&aa[0]=='0'&&ax==1){aa[0]='\0';ax=0;}goto ac;}else if(*bq=='f'){if(!dD)dC=6;
#define DO_PRINT( y , a , u ) ax = y( aa , PRINT_BUFFER_SIZE , (a)( uttc_va_arg( Q , u ) ) , UTTC_FLOAT_FMT_POSITIONAL , dC , ".0123456789abcdefghijklmnopqrstuvwxyz" )
switch(fb){case 0:if(P){DO_PRINT(uttc_ieee754_print_half_alt,TARGET_IEEE_754_HALF_ALT_PRM_CAST,TARGET_IEEE_754_HALF_ALT_PRM_TYPE);break;}else{DO_PRINT(uttc_ieee754_print_half,TARGET_IEEE_754_HALF_PRM_CAST,TARGET_IEEE_754_HALF_PRM_TYPE);break;}case 1:DO_PRINT(uttc_ieee754_print_float,TARGET_IEEE_754_FLOAT_PRM_CAST,TARGET_IEEE_754_FLOAT_PRM_TYPE);break;case 2:DO_PRINT(uttc_ieee754_print_double,TARGET_IEEE_754_DOUBLE_PRM_CAST,TARGET_IEEE_754_DOUBLE_PRM_TYPE);break;case 8:ax=uttc_bf_print(aa,PRINT_BUFFER_SIZE,uttc_va_arg(Q,uttc_big_float),UTTC_FLOAT_FMT_POSITIONAL,dC,".0123456789abcdefghijklmnopqrstuvwxyz");break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
goto ac;}else if(*bq=='F'){if(!dD)dC=6;
#define DO_PRINT( y , a , u ) ax = y( aa , PRINT_BUFFER_SIZE , (a)( uttc_va_arg( Q , u ) ) , UTTC_FLOAT_FMT_POSITIONAL , dC , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
switch(fb){case 0:if(P){DO_PRINT(uttc_ieee754_print_half_alt,TARGET_IEEE_754_HALF_ALT_PRM_CAST,TARGET_IEEE_754_HALF_ALT_PRM_TYPE);break;}else{DO_PRINT(uttc_ieee754_print_half,TARGET_IEEE_754_HALF_PRM_CAST,TARGET_IEEE_754_HALF_PRM_TYPE);break;}case 1:DO_PRINT(uttc_ieee754_print_float,TARGET_IEEE_754_FLOAT_PRM_CAST,TARGET_IEEE_754_FLOAT_PRM_TYPE);break;case 2:DO_PRINT(uttc_ieee754_print_double,TARGET_IEEE_754_DOUBLE_PRM_CAST,TARGET_IEEE_754_DOUBLE_PRM_TYPE);break;case 8:ax=uttc_bf_print(aa,PRINT_BUFFER_SIZE,uttc_va_arg(Q,uttc_big_float),UTTC_FLOAT_FMT_POSITIONAL,dC,".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ");break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
goto ac;}else if(*bq=='e'){if(!dD)dC=6;
#define DO_PRINT( y , a , u ) ax = y( aa , PRINT_BUFFER_SIZE , (a)( uttc_va_arg( Q , u ) ) , UTTC_FLOAT_FMT_SCIENTIFIC , dC , ".0123456789abcdefghijklmnopqrstuvwxyz" )
switch(fb){case 0:if(P){DO_PRINT(uttc_ieee754_print_half_alt,TARGET_IEEE_754_HALF_ALT_PRM_CAST,TARGET_IEEE_754_HALF_ALT_PRM_TYPE);break;}else{DO_PRINT(uttc_ieee754_print_half,TARGET_IEEE_754_HALF_PRM_CAST,TARGET_IEEE_754_HALF_PRM_TYPE);break;}case 1:DO_PRINT(uttc_ieee754_print_float,TARGET_IEEE_754_FLOAT_PRM_CAST,TARGET_IEEE_754_FLOAT_PRM_TYPE);break;case 2:DO_PRINT(uttc_ieee754_print_double,TARGET_IEEE_754_DOUBLE_PRM_CAST,TARGET_IEEE_754_DOUBLE_PRM_TYPE);break;case 8:ax=uttc_bf_print(aa,PRINT_BUFFER_SIZE,uttc_va_arg(Q,uttc_big_float),UTTC_FLOAT_FMT_SCIENTIFIC,dC,".0123456789abcdefghijklmnopqrstuvwxyz");break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
goto ac;}else if(*bq=='E'){if(!dD)dC=6;
#define DO_PRINT( y , a , u ) ax = y( aa , PRINT_BUFFER_SIZE , (a)( uttc_va_arg( Q , u ) ) , UTTC_FLOAT_FMT_SCIENTIFIC , dC , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
switch(fb){case 0:if(P){DO_PRINT(uttc_ieee754_print_half_alt,TARGET_IEEE_754_HALF_ALT_PRM_CAST,TARGET_IEEE_754_HALF_ALT_PRM_TYPE);break;}else{DO_PRINT(uttc_ieee754_print_half,TARGET_IEEE_754_HALF_PRM_CAST,TARGET_IEEE_754_HALF_PRM_TYPE);break;}case 1:DO_PRINT(uttc_ieee754_print_float,TARGET_IEEE_754_FLOAT_PRM_CAST,TARGET_IEEE_754_FLOAT_PRM_TYPE);break;case 2:DO_PRINT(uttc_ieee754_print_double,TARGET_IEEE_754_DOUBLE_PRM_CAST,TARGET_IEEE_754_DOUBLE_PRM_TYPE);break;case 8:ax=uttc_bf_print(aa,PRINT_BUFFER_SIZE,uttc_va_arg(Q,uttc_big_float),UTTC_FLOAT_FMT_SCIENTIFIC,dC,".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ");break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
goto ac;}else if(*bq=='g'){if(!dD)dC=6;
#define DO_PRINT( y , a , u ) ax = y( aa , PRINT_BUFFER_SIZE , (a)( uttc_va_arg( Q , u ) ) , UTTC_FLOAT_FMT_MIXED , dC , ".0123456789abcdefghijklmnopqrstuvwxyz" )
switch(fb){case 0:if(P){DO_PRINT(uttc_ieee754_print_half_alt,TARGET_IEEE_754_HALF_ALT_PRM_CAST,TARGET_IEEE_754_HALF_ALT_PRM_TYPE);break;}else{DO_PRINT(uttc_ieee754_print_half,TARGET_IEEE_754_HALF_PRM_CAST,TARGET_IEEE_754_HALF_PRM_TYPE);break;}case 1:DO_PRINT(uttc_ieee754_print_float,TARGET_IEEE_754_FLOAT_PRM_CAST,TARGET_IEEE_754_FLOAT_PRM_TYPE);break;case 2:DO_PRINT(uttc_ieee754_print_double,TARGET_IEEE_754_DOUBLE_PRM_CAST,TARGET_IEEE_754_DOUBLE_PRM_TYPE);break;case 8:ax=uttc_bf_print(aa,PRINT_BUFFER_SIZE,uttc_va_arg(Q,uttc_big_float),UTTC_FLOAT_FMT_MIXED,dC,".0123456789abcdefghijklmnopqrstuvwxyz");break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
goto ac;}else if(*bq=='G'){if(!dD)dC=6;
#define DO_PRINT( y , a , u ) ax = y( aa , PRINT_BUFFER_SIZE , (a)( uttc_va_arg( Q , u ) ) , UTTC_FLOAT_FMT_MIXED , dC , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
switch(fb){case 0:if(P){DO_PRINT(uttc_ieee754_print_half_alt,TARGET_IEEE_754_HALF_ALT_PRM_CAST,TARGET_IEEE_754_HALF_ALT_PRM_TYPE);break;}else{DO_PRINT(uttc_ieee754_print_half,TARGET_IEEE_754_HALF_PRM_CAST,TARGET_IEEE_754_HALF_PRM_TYPE);break;}case 1:DO_PRINT(uttc_ieee754_print_float,TARGET_IEEE_754_FLOAT_PRM_CAST,TARGET_IEEE_754_FLOAT_PRM_TYPE);break;case 2:DO_PRINT(uttc_ieee754_print_double,TARGET_IEEE_754_DOUBLE_PRM_CAST,TARGET_IEEE_754_DOUBLE_PRM_TYPE);break;case 8:ax=uttc_bf_print(aa,PRINT_BUFFER_SIZE,uttc_va_arg(Q,uttc_big_float),UTTC_FLOAT_FMT_MIXED,dC,".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ");break;default:uttc_print_string("<unsupported type spec>");break;}
#undef DO_PRINT
goto ac;}else if(*bq=='c'){switch(fb){case 2:{unsigned char fn=uttc_va_arg(Q,int);if(P&&!uttc_isprint(fn)){switch(fn){case'\a':ax=uttc_serialize_string("\\a",aa);break;case'\b':ax=uttc_serialize_string("\\b",aa);break;case'\f':ax=uttc_serialize_string("\\f",aa);break;case'\n':ax=uttc_serialize_string("\\n",aa);break;case'\r':ax=uttc_serialize_string("\\r",aa);break;case'\t':ax=uttc_serialize_string("\\t",aa);break;case'\v':ax=uttc_serialize_string("\\v",aa);break;case'\\':ax=uttc_serialize_string("\\\\",aa);break;case'\'':ax=uttc_serialize_string("\\'",aa);break;case'\"':ax=uttc_serialize_string("\\\"",aa);break;default:{char*aC=aa;*aC++='\\';if(fn<64){if(fn<8)*aC++='0';*aC++='0';}ax=uttc_serialize_string(uttc_print_unsigned_int_ex(fn,8,"01234567"),aC)+(aC-aa);aa[ax]='\0';break;}}}else{ax=uttc_serialize_char(fn,aa);aa[ax]='\0';}break;}default:uttc_print_string("<unsupported type spec>");break;}goto ac;}else if(*bq=='s'){switch(fb){case 2:if(dD){const char*fn=uttc_va_arg(Q,const char*);if(fn)while(*fn){if(P&&(int)ax>=dC-3&&fn[1]&&fn[2]&&fn[3]){aa[ax++]='.';aa[ax++]='.';aa[ax++]='.';break;}else if((int)ax>=dC)break;aa[ax++]=*fn++;}aa[ax]='\0';}else ax=uttc_serialize_string(uttc_va_arg(Q,const char*),aa);break;default:uttc_print_string("<unsupported type spec>");break;}goto ac;}else if(*bq=='p'){switch(fb){case 2:aa[ax++]='0';aa[ax++]='x';ax+=uttc_serialize_string(uttc_print_unsigned_long_ex((uttc_intptr_t)uttc_va_arg(Q,const void*),16,"0123456789abcdef"),aa+ax);break;default:uttc_print_string("<unsupported type spec>");break;}goto ac;}else if(*bq=='b'){switch(fb){case 2:ax=uttc_serialize_string(uttc_va_arg(Q,int)?"true":"false",aa);break;default:uttc_print_string("<unsupported type spec>");break;}goto ac;}else if(*bq=='h')switch(fb){case 1:fb=0;break;case 2:fb=1;break;default:uttc_print_string("<unsupported type spec>");break;}else if(*bq=='l')switch(fb){case 2:fb=3;break;case 3:fb=4;break;default:uttc_print_string("<unsupported type spec>");break;}else if(*bq=='j'&&bM&&fb==2)fb=5;else if(*bq=='z'&&bM&&fb==2)fb=6;else if(*bq=='t'&&bM&&fb==2)fb=7;else if(*bq=='L'&&bM&&fb==2)fb=8;else{uttc_print_string("<unsupported syntax at '");uttc_print_char(*bq);uttc_print_string("'>");goto ac;}}else uttc_print_char(*bq);++bq;continue;ac:if(ax>0){if(bp&&aa[0]!='-'){uttc_print_char(bp==1?'+':' ');ax++;}if(!ca){int am=bh-ax;for(;am>0;--am)uttc_print_char(dv?'0':' ');}uttc_print_string(aa);if(ca){int am=bh-ax;for(;am>0;--am)uttc_print_char(dv?'0':' ');}aa[0]='\0';ax=0;}bq++;bM=false;dD=false;dF=false;bi=false;dv=false;ca=false;P=false;bp=0;fb=2;dC=UTTC_INT_MIN;bh=0;}(void)P;}UTTC_LINKAGE UTTC_ATTRIBUTE_FORMAT(printf,1,2)void uttc_print(const char*bq,...){uttc_va_list Q;uttc_va_start(Q,bq);uttc_vprint(bq,Q);uttc_va_end(Q);} UTTC_LINKAGE int uttc_abs(int fm){return fm<0&&fm!=UTTC_INT_MIN?-fm:fm;}UTTC_LINKAGE long int uttc_labs(long int fm){return fm<0L&&fm!=UTTC_LONG_MIN?-fm:fm;}UTTC_LINKAGE long long int uttc_llabs(long long int fm){return fm<0LL&&fm!=UTTC_LONGLONG_MIN?-fm:fm;}UTTC_LINKAGE uttc_intmax_t uttc_imaxabs(uttc_intmax_t fm){return fm<0LL&&fm!=UTTC_INTMAX_MIN?-fm:fm;} typedef uttc_ieee754_float uttc_duration_t;
#ifndef TARGET_NO_TIME
#include <time.h>
typedef clock_t uttc_timepoint_t;UTTC_LINKAGE uttc_timepoint_t uttc_get_time(){return clock();}UTTC_LINKAGE uttc_duration_t uttc_get_duration(uttc_timepoint_t eB,uttc_timepoint_t aN){return((uttc_duration_t)(aN-eB))/CLOCKS_PER_SEC;}
#else
typedef uttc_uint32_t uttc_timepoint_t;UTTC_LINKAGE uttc_timepoint_t uttc_get_time(){static uttc_timepoint_t am=0;return am++;}UTTC_LINKAGE uttc_duration_t uttc_get_duration(uttc_timepoint_t eB,uttc_timepoint_t aN){return eB-aN;}
#endif
#define TARGET_MAX_TRACE_LEN 1024
#define TARGET_MAX_NUM_PROPERTY_VALUES 32
typedef enum uttc_pr_value_t{UTTC_PR_VALUE_REGULAR=-9876543,UTTC_PR_VALUE_THROWING=UTTC_PR_VALUE_REGULAR+1}uttc_pr_value_t;typedef struct uttc_property_t{const char*cG;uttc_pr_value_t fp[TARGET_MAX_NUM_PROPERTY_VALUES];uttc_size_t ay;uttc_size_t da;}uttc_property_t;typedef struct uttc_pr_trace_entry_t{uttc_property_t*dN;uttc_size_t fn;}uttc_pr_trace_entry_t;typedef enum uttc_pr_trace_cmd_t{UTTC_PR_TRACE_CMD_RESET,UTTC_PR_TRACE_CMD_ACCESS,UTTC_PR_TRACE_CMD_COMPARE,UTTC_PR_TRACE_CMD_FINISH,UTTC_PR_TRACE_CMD_IS_COVERED,UTTC_PR_TRACE_CMD_SET,UTTC_PR_TRACE_CMD_START,UTTC_PR_TRACE_CMD_GET}uttc_pr_trace_cmd_t;typedef union uttc_pr_trace_cmd_data_t{struct{uttc_property_t*dN;}M;struct{uttc_pr_trace_entry_t*eW;uttc_size_t eX;}ae;struct{uttc_pr_trace_entry_t*eW;uttc_size_t eX;}eo;struct{uttc_pr_trace_entry_t**eW;uttc_size_t*eX;}bw;}uttc_pr_trace_cmd_data_t;
#define UTTC_PR_EMPTY_TRACE_CMD_DATA ((uttc_pr_trace_cmd_data_t){0})
UTTC_LINKAGE uttc_size_t uttc_pr_trace(uttc_pr_trace_cmd_t ad,uttc_pr_trace_cmd_data_t aw){static uttc_pr_trace_entry_t eW[TARGET_MAX_TRACE_LEN];static uttc_size_t eX=0;static uttc_size_t cT=0;static bool aT=false;switch(ad){case UTTC_PR_TRACE_CMD_RESET:uttc_memset(eW,0,sizeof(eW));eX=0;cT=0;break;case UTTC_PR_TRACE_CMD_SET:uttc_memcpy(eW,aw.eo.eW,sizeof(uttc_pr_trace_entry_t)*aw.eo.eX);aw.eo.eX=0;break;case UTTC_PR_TRACE_CMD_GET:*aw.bw.eW=eW;*aw.bw.eX=eX;break;case UTTC_PR_TRACE_CMD_COMPARE:{if(aw.ae.eX!=eX)return false;uttc_size_t i=0;for(;i<eX;i++){if(aw.ae.eW[i].dN!=eW[i].dN)return false;else if(aw.ae.eW[i].fn!=eW[i].fn)return false;}return true;}case UTTC_PR_TRACE_CMD_ACCESS:if(!aT)return aw.M.dN->ay;if(cT<eX){uttc_pr_trace_entry_t cR=eW[cT++];if(cR.dN!=aw.M.dN)uttc_print("uttc_pr_trace: Non-Reproducible series of calls to properties! Expected call to '%s', got '%s'.",cR.dN->cG,aw.M.dN->cG);return cR.fn;}if(!aw.M.dN->da){uttc_print("uttc_pr_trace: Property '%s' cannot have a value.",aw.M.dN->cG);return aw.M.dN->ay;}uttc_assert(eX<TARGET_MAX_TRACE_LEN);eW[eX++]=(uttc_pr_trace_entry_t){aw.M.dN,0};cT++;return 0;case UTTC_PR_TRACE_CMD_START:if(aT)return 0;aT=true;cT=0;int bO=eX;while(bO--){if(++eW[bO].fn>=eW[bO].dN->da)continue;if(eW[bO].fn<eW[bO].dN->da)break;}eX=bO+1;break;case UTTC_PR_TRACE_CMD_FINISH:if(!aT)return 0;aT=false;break;case UTTC_PR_TRACE_CMD_IS_COVERED:{int bO=eX;while(bO--){if(eW[bO].fn+1>=eW[bO].dN->da)continue;if(eW[bO].fn+1<eW[bO].dN->da)return 0;}return 1;}default:return 0;}return 1;}UTTC_LINKAGE uttc_pr_value_t uttc_pr_access(uttc_property_t*dM){uttc_size_t fo=uttc_pr_trace(UTTC_PR_TRACE_CMD_ACCESS,(uttc_pr_trace_cmd_data_t){.M={dM}});
#ifdef TARGET_VERBOSE
uttc_print("Property Access: \"%s\" = %d\n",dM->cG,fo);
#endif
return dM->fp[fo];}UTTC_LINKAGE void uttc_pr_reset(){uttc_pr_trace(UTTC_PR_TRACE_CMD_RESET,UTTC_PR_EMPTY_TRACE_CMD_DATA);}UTTC_LINKAGE void uttc_pr_enter(){uttc_pr_trace(UTTC_PR_TRACE_CMD_START,UTTC_PR_EMPTY_TRACE_CMD_DATA);}UTTC_LINKAGE void uttc_pr_leave(){uttc_pr_trace(UTTC_PR_TRACE_CMD_FINISH,UTTC_PR_EMPTY_TRACE_CMD_DATA);}UTTC_LINKAGE bool uttc_pr_is_covered(){return uttc_pr_trace(UTTC_PR_TRACE_CMD_IS_COVERED,UTTC_PR_EMPTY_TRACE_CMD_DATA);}UTTC_LINKAGE bool uttc_pr_compare_trace(uttc_pr_trace_entry_t*eW,uttc_size_t eX){return uttc_pr_trace(UTTC_PR_TRACE_CMD_COMPARE,(uttc_pr_trace_cmd_data_t){.ae={eW,eX}});}UTTC_LINKAGE bool uttc_pr_assert_trace(uttc_pr_trace_entry_t*eW,uttc_size_t eX){if(!uttc_pr_compare_trace(eW,eX)){uttc_print_string("Unexpected call to properties, test cases can not cover the dynamic executing of the SUT!\n");return false;}return true;}UTTC_LINKAGE void uttc_pr_get_trace(uttc_pr_trace_entry_t**eW,uttc_size_t*eX){uttc_pr_trace(UTTC_PR_TRACE_CMD_GET,(uttc_pr_trace_cmd_data_t){.bw={eW,eX}});}UTTC_LINKAGE void uttc_puttrace(){uttc_pr_trace_entry_t*eW;uttc_size_t eX;uttc_pr_get_trace(&eW,&eX);uttc_print_unsigned_int(eX);uttc_print_cesura();uttc_size_t i=0;for(;i<eX;i++){uttc_print_string(eW[i].dN->cG);uttc_print_cesura();uttc_print_unsigned_int(eW[i].fn);}} UTTC_LINKAGE bool uttc_islessb(const uttc_big_float bZ,const uttc_big_float ee){if(uttc_isnanb(bZ)||uttc_isnanb(ee))return false;bool ei=uttc_signbitb(bZ);if(ei!=uttc_signbitb(ee))return ei;int aE=(bZ.aw[0]&0x7F)-(ee.aw[0]&0x7F);if(!aE)aE=uttc_memcmp((char*)&bZ+1,(char*)&ee+1,sizeof(uttc_big_float)-1);return ei?aE>0:aE<0;}UTTC_LINKAGE bool uttc_islessequalb(const uttc_big_float bZ,const uttc_big_float ee){if(uttc_isnanb(bZ)||uttc_isnanb(ee))return false;bool cb=uttc_signbitb(bZ);if(cb!=uttc_signbitb(ee))return cb;int aE=(bZ.aw[0]&0x7F)-(ee.aw[0]&0x7F);if(!aE)aE=uttc_memcmp((char*)&bZ+1,(char*)&ee+1,sizeof(uttc_big_float)-1);return cb?aE>=0:aE<=0;}
#define UTTC_BF_DEFINE_TO_BIGFLOAT( D , eG , A , o ) UTTC_LINKAGE uttc_big_float uttc_##D##2big_float( uttc_ieee754_##D fm ){ uttc_big_float dY = uttc_from_rawb(UTTC_BF_0); switch( uttc_fp_classify##eG(fm) ){ case UTTC_FP_INFINITE: if( uttc_signbit##eG(fm) ) dY = UTTC_NINFINITYB; else dY = UTTC_INFINITYB; break; case UTTC_FP_NAN: dY = UTTC_NANB; break; case UTTC_FP_ZERO: if( uttc_signbit##eG(fm) ) dY = uttc_from_rawb(UTTC_BF_N0); break; case UTTC_FP_NORMAL:{ uttc_size_t ea; uttc_uint##o##_t cu = uttc_mantissa##eG(fm) << (o - UTTC_EXPONENT_SHIFT##A); uttc_uint32_t cK = (uttc_int32_t)uttc_exponent##eG(fm) - UTTC_EXPONENT_BIAS##A + UTTC_EXPONENT_BIASB; uttc_set_exponentb( &dY , cK ); uttc_set_signbitb( &dY , uttc_signbit##eG(fm) ); for( ea = 0 ; ea < UTTC_BF_MANTISSA_BYTES && ea < o/8 ; ea++ ) dY.aw[ea + UTTC_BF_EXPONENT_BYTES] = ( cu >> ( o - 8 - ea * 8 ) ) & 0xFFu; break; } case UTTC_FP_SUBNORMAL:{ uttc_size_t ea; uttc_uint8_t cY = uttc_log2_##o( uttc_mantissa##eG(fm) ); uttc_uint##o##_t cu = uttc_mantissa##eG(fm) << (o - cY); uttc_uint32_t cK = UTTC_EXPONENT_BIASB - ( UTTC_EXPONENT_BIAS##A - 1 ) - ( UTTC_EXPONENT_SHIFT##A - cY ); uttc_set_exponentb( &dY , cK ); uttc_set_signbitb( &dY , uttc_signbit##eG(fm) ); for( ea = 0 ; ea < UTTC_BF_MANTISSA_BYTES && ea < o/8 ; ea++ ) dY.aw[ea + UTTC_BF_EXPONENT_BYTES] = ( cu >> ( o - 8 - ea * 8 ) ) & 0xFFu; break; } default: break; } return dY; }
UTTC_BF_DEFINE_TO_BIGFLOAT(double,,,64)UTTC_BF_DEFINE_TO_BIGFLOAT(float,f,F,32)UTTC_BF_DEFINE_TO_BIGFLOAT(half,h,H,16)UTTC_BF_DEFINE_TO_BIGFLOAT(half_alt,h2,H2,16)
#define UTTC_BF_DEFINE_FROM_BIGFLOAT( D , eG , A , o ) UTTC_LINKAGE uttc_ieee754_##D uttc_big_float2##D##_ex( uttc_big_float fm , uttc_round_t eh ) { switch( uttc_fp_classifyb( fm ) ) { case UTTC_FP_INFINITE: return uttc_signbitb(fm) ? UTTC_NINFINITY##A : UTTC_INFINITY##A; case UTTC_FP_NAN: return UTTC_NAN##A; case UTTC_FP_ZERO: return uttc_from_raw##eG( (uttc_uint##o##_t)uttc_signbitb(fm) << UTTC_SIGNBIT_SHIFT##A ); case UTTC_FP_SUBNORMAL: switch( eh ){ case UTTC_ROUND_AWAY_FROM_ZERO: return uttc_from_raw##eG( UTTC_SIGNBIT_MASK##A * uttc_signbitb(fm) | 1u ); case UTTC_ROUND_TOWARD_POS_INFINITY: return uttc_from_raw##eG( (uttc_uint##o##_t)1u << ( UTTC_SIGNBIT_SHIFT##A * uttc_signbitb(fm) ) ); case UTTC_ROUND_TOWARD_NEG_INFINITY: return uttc_from_raw##eG( ( UTTC_SIGNBIT_MASK##A | 1 ) * uttc_signbitb(fm) ); default: return uttc_from_raw##eG( (uttc_uint##o##_t)uttc_signbitb(fm) << UTTC_SIGNBIT_SHIFT##A ); } break; case UTTC_FP_NORMAL: break; } int aV = uttc_exponentb( fm ) - UTTC_EXPONENT_BIASB + UTTC_EXPONENT_BIAS##A; bool ar; bool as; uttc_uint##o##_t dY; if( aV > 0 ){ uttc_big_integer cu = uttc_mantissab( fm ); as = uttc_bi_shr( &cu , UTTC_EXPONENT_SHIFTB - UTTC_EXPONENT_SHIFT##A - 1 ); ar = uttc_bi_shr( &cu , 1 ); dY = uttc_bi_get_##o( &cu ); } else if( aV >= -UTTC_EXPONENT_SHIFT##A ){ uttc_big_integer cu = uttc_mantissab( fm ); as = uttc_bi_shr( &cu , UTTC_EXPONENT_SHIFTB - UTTC_EXPONENT_SHIFT##A - aV ); ar = uttc_bi_shr( &cu , 1 ); dY = uttc_bi_get_##o( &cu ) | ( (uttc_uint##o##_t)1u << (UTTC_EXPONENT_SHIFT##A + aV - 1) ); aV = 0; } else{ dY = 0; ar = false; as = true; } switch( eh ){ case UTTC_ROUND_TOWARD_POS_INFINITY: dY += ar || as ? 1 - uttc_signbitb( fm ) : 0; break; case UTTC_ROUND_TOWARD_NEG_INFINITY: dY += ar || as ? uttc_signbitb( fm ) : 0; break; case UTTC_ROUND_AWAY_FROM_ZERO: dY += ar || as ? 1 : 0; break; case UTTC_ROUND_TO_NEAREST: dY += ar ? 1 : 0; break; case UTTC_ROUND_TO_NEAREST_TIES_ZERO: dY += ar && as ? 1 : 0; break; case UTTC_ROUND_TO_NEAREST_TIES_EVEN: case UTTC_ROUND_TO_NEAREST_IEEE753: if( as ) dY += ar ? 1 : 0; else if( ar ) dY += dY % 2u; break; default: break; } if( dY > UTTC_MANTISSA_MASK##A && aV != 0 ){ dY >>= 1; ++aV; } if( aV > (int)(UTTC_EXPONENT_MASK##A >> UTTC_EXPONENT_SHIFT##A) ) switch( eh ){ case UTTC_ROUND_TOWARD_POS_INFINITY: return uttc_from_raw##eG( uttc_signbitb( fm ) ? UTTC_SIGNBIT_MASK##A | ( UTTC_EXPONENT_MASK##A - 1 ) : UTTC_EXPONENT_MASK##A ); case UTTC_ROUND_TOWARD_NEG_INFINITY: return uttc_from_raw##eG( uttc_signbitb( fm ) ? UTTC_SIGNBIT_MASK##A | UTTC_EXPONENT_MASK##A : UTTC_EXPONENT_MASK##A - 1 ); case UTTC_ROUND_TO_NEAREST_IEEE753: case UTTC_ROUND_AWAY_FROM_ZERO: return uttc_from_raw##eG( ( (uttc_uint##o##_t)uttc_signbitb( fm ) << UTTC_SIGNBIT_SHIFT##A ) | UTTC_EXPONENT_MASK##A ); default: return uttc_from_raw##eG( ( (uttc_uint##o##_t)uttc_signbitb( fm ) << UTTC_SIGNBIT_SHIFT##A ) | ( UTTC_EXPONENT_MASK##A - 1 ) ); } return uttc_from_raw##eG( dY | ( (uttc_uint##o##_t)aV << UTTC_EXPONENT_SHIFT##A ) | ( (uttc_uint##o##_t)uttc_signbitb( fm ) << UTTC_SIGNBIT_SHIFT##A ) ); } UTTC_LINKAGE uttc_ieee754_##D uttc_big_float2##D( uttc_big_float fn ){ return uttc_big_float2##D##_ex( fn , UTTC_ROUND_TO_NEAREST_IEEE753 ); }
UTTC_BF_DEFINE_FROM_BIGFLOAT(double,,,64)UTTC_BF_DEFINE_FROM_BIGFLOAT(float,f,F,32)UTTC_BF_DEFINE_FROM_BIGFLOAT(half,h,H,16)UTTC_BF_DEFINE_FROM_BIGFLOAT(half_alt,h2,H2,16) UTTC_LINKAGE uttc_big_float uttc_addsubb(uttc_big_float bZ,uttc_big_float ee,bool eF){const int dE=5;if(uttc_isnanb(bZ))return bZ;if(uttc_isnanb(ee))return ee;if(eF)ee=uttc_negateb(ee);if(uttc_isinfb(bZ)){if(uttc_isinfb(ee)){if(uttc_signbitb(bZ)!=uttc_signbitb(ee))return UTTC_NANB;else return bZ;}}else if(uttc_isinfb(ee))return ee;uttc_int64_t aW=(uttc_int64_t)uttc_exponentb(bZ)-(uttc_int64_t)uttc_exponentb(ee);uttc_int64_t aE=aW;if(!aE){uttc_uint32_t aq=UTTC_BF_EXPONENT_BYTES;while(aq<UTTC_BF_TOTAL_BYTES){if((aE=(bZ.aw[aq]-ee.aw[aq])))break;aq++;}}if(aE<0){uttc_big_float eS=bZ;bZ=ee;ee=eS;aW=-aW;}bool cL=uttc_signbitb(bZ);uttc_uint32_t cK=uttc_exponentb(bZ);uttc_big_integer bY;uttc_bi_set_32(&bY,1);uttc_bi_shl(&bY,UTTC_BF_MANTISSA_BYTES*8);uttc_big_integer N=uttc_mantissab(bZ);if(cK)uttc_bi_add(&N,&bY);uttc_bi_shl(&N,dE);uttc_big_integer ct=uttc_mantissab(ee);if(uttc_exponentb(ee))uttc_bi_add(&ct,&bY);if(dE>aW)uttc_bi_shl(&ct,dE-aW);else uttc_bi_shr(&ct,aW-dE);int cI=dE;if(cL==uttc_signbitb(ee)){uttc_bi_add(&N,&ct);if(uttc_bi_log2(&N)>(UTTC_BF_MANTISSA_BYTES*8+dE)){cI+=1;cK+=1;}}else{uttc_bi_sub(&N,&ct);if(uttc_bi_is_zero(&N)){cL=false;cK=0;}else{uttc_int32_t cP=(uttc_int32_t)(UTTC_BF_MANTISSA_BYTES*8+dE)-uttc_bi_log2(&N);if(cK>0&&cP>0){if((uttc_uint32_t)cP>cK)cP=cK;uttc_bi_shl(&N,cP);cK-=cP;}}}uttc_big_integer eR;uttc_bi_set_32(&eR,1u<<(cI-1));uttc_bi_add(&N,&eR);uttc_bi_shr(&N,cI);uttc_big_float dY;uttc_set_mantissab(&dY,N);uttc_set_exponentb(&dY,cK);uttc_set_signbitb(&dY,cL);return dY;}UTTC_LINKAGE uttc_big_float uttc_multiplyb(uttc_big_float bZ,uttc_big_float ee){uttc_assert_ex(false,"Multiplication of big floats not implemented.");return bZ;}UTTC_LINKAGE uttc_big_float uttc_divideb(uttc_big_float aI,uttc_big_float aK){uttc_assert_ex(false,"Division of big floats not implemented.");return aI;}
#define uttc_fromhexb( fm ) uttc_fromhexm_implementation( #fm )
UTTC_LINKAGE uttc_big_float uttc_fromhexm_implementation(const char*eD){
#define getxalpha( c ) ((unsigned char)(c & 0xDF) - 'A')
#define getxnum( c ) ((unsigned char)(c - '0'))
uttc_big_float dY={UTTC_BF_0};uttc_int32_t aZ=0;unsigned char fm;unsigned int ea;bool et=false;if(eD[0]=='-'){eD++;et=true;}if(eD[0]=='0'&&(eD[1]&0xDF)=='X')eD+=2;if((fm=getxnum(*eD))<10u){}else if((fm=getxalpha(*eD))<6u)fm+=10;else return UTTC_NANB;++eD;if(eD[0]=='.')++eD;switch(fm){case 0:return et?(uttc_big_float){UTTC_BF_N0}:(uttc_big_float){UTTC_BF_0};case 1:for(ea=UTTC_BF_EXPONENT_BYTES;ea<UTTC_BF_TOTAL_BYTES;ea++,eD+=2){if((fm=getxnum(eD[0]))<10u)dY.aw[ea]|=fm<<4;else if((fm=getxalpha(eD[0]))<6u)dY.aw[ea]|=(fm+=10)<<4;else break;if((fm=getxnum(eD[1]))<10u)dY.aw[ea]|=fm;else if((fm=getxalpha(eD[1]))<6u)dY.aw[ea]|=(fm+=10);else break;}break;case 2:case 3:aZ-=1;for(ea=UTTC_BF_EXPONENT_BYTES;ea<UTTC_BF_TOTAL_BYTES;ea++,eD+=2){dY.aw[ea]|=fm<<7;if((fm=getxnum(eD[0]))<10u)dY.aw[ea]|=fm<<3;else if((fm=getxalpha(eD[0]))<6u)dY.aw[ea]|=(fm+=10)<<3;else break;if((fm=getxnum(eD[1]))<10u)dY.aw[ea]|=fm>>1;else if((fm=getxalpha(eD[1]))<6u)dY.aw[ea]|=(fm+=10)>>1;else break;}break;case 4:case 5:case 6:case 7:aZ-=2;for(ea=UTTC_BF_EXPONENT_BYTES;ea<UTTC_BF_TOTAL_BYTES;ea++,eD+=2){dY.aw[ea]|=fm<<6;if((fm=getxnum(eD[0]))<10u)dY.aw[ea]|=fm<<2;else if((fm=getxalpha(eD[0]))<6u)dY.aw[ea]|=(fm+=10)<<2;else break;if((fm=getxnum(eD[1]))<10u)dY.aw[ea]|=fm>>2;else if((fm=getxalpha(eD[1]))<6u)dY.aw[ea]|=(fm+=10)>>2;else break;}break;case 8:case 9:case 10:case 11:case 12:case 13:case 14:case 15:aZ-=3;for(ea=UTTC_BF_EXPONENT_BYTES;ea<UTTC_BF_TOTAL_BYTES;ea++,eD+=2){dY.aw[ea]|=fm<<5;if((fm=getxnum(eD[0]))<10u)dY.aw[ea]|=fm<<1;else if((fm=getxalpha(eD[0]))<6u)dY.aw[ea]|=(fm+=10)<<1;else break;if((fm=getxnum(eD[1]))<10u)dY.aw[ea]|=fm>>3;else if((fm=getxalpha(eD[1]))<6u)dY.aw[ea]|=(fm+=10)>>3;else break;}break;}while(getxnum(*eD)<10u||getxalpha(*eD)<6u)eD++;if(eD[0]!='p')return UTTC_NANB;eD++;bool aX=false;uttc_int32_t aY=0;if(*eD=='-'){aX=true;eD++;}else if(*eD=='+')eD++;while(*eD>='0'&&*eD<='9'){aY=aY*10+(*eD-'0');++eD;}aY=(aX?-aY:aY)+UTTC_EXPONENT_BIASB-aZ;uttc_assert(aY>0&&aY<(uttc_int32_t)UTTC_BF_EXPONENT_MAX);uttc_uint32_t aV=aY;for(ea=UTTC_BF_EXPONENT_BYTES-1;ea>0;ea--){dY.aw[ea]=aV&0xFF;aV>>=8;}dY.aw[0]=(aV&0x7Fu)|(et<<7u);
#undef getxalpha
#undef getxnum
return dY;}UTTC_LINKAGE uttc_big_float uttc_nextafterb(const uttc_big_float bt,const uttc_big_float to){unsigned int i=0;uttc_uint32_t bg=uttc_exponentb(bt);if(bg==UTTC_BF_EXPONENT_MAX)for(i=UTTC_BF_EXPONENT_BYTES;i<UTTC_BF_TOTAL_BYTES;i++)if(bt.aw[i])return bt;uttc_uint32_t eP=uttc_exponentb(to);if(eP==UTTC_BF_EXPONENT_MAX)for(i=UTTC_BF_EXPONENT_BYTES;i<UTTC_BF_TOTAL_BYTES;i++)if(to.aw[i])return to;int aE=(bt.aw[0]&0x7F)-(to.aw[0]&0x7F);if(!aE){aE=uttc_memcmp((char*)&bt+1,(char*)&to+1,sizeof(uttc_big_float)-1);if(!aE)return to;}bool eT=!eP;if(!bg){bool bu=true;for(i=UTTC_BF_EXPONENT_BYTES;i<UTTC_BF_TOTAL_BYTES;i++){bu|=bt.aw[i];eT|=to.aw[i];}if(bu){if(eT)return to;else if(uttc_signbitb(to))return uttc_from_rawb(UTTC_BF_N1);else return uttc_from_rawb(UTTC_BF_1);}}uttc_big_float dY=bt;if(aE<0){for(i=UTTC_BF_TOTAL_BYTES-1;i>=UTTC_BF_EXPONENT_BYTES;i--)if(dY.aw[i]<255u){dY.aw[i]++;break;}while(++i<UTTC_BF_TOTAL_BYTES)dY.aw[i]=0;}else{for(i=UTTC_BF_TOTAL_BYTES-1;i>=UTTC_BF_EXPONENT_BYTES;i--)if(dY.aw[i]>0u){dY.aw[i]--;break;}while(++i<UTTC_BF_TOTAL_BYTES)dY.aw[i]=255u;}return dY;}
#ifndef TARGET_ULP_TOO_LARGE
#define TARGET_ULP_TOO_LARGE 999
#endif
typedef struct uttc_error_t{uttc_big_float L;uttc_ieee754_double fc;}uttc_error_t;typedef enum{UTTC_DM_ABSOLUTE=0,UTTC_DM_UNITS_IN_LAST_PLACE=1}uttc_deviation_metric;typedef struct uttc_deviation_threshold{uttc_big_float fn;uttc_deviation_metric cD;bool J:1;bool S:1;bool K:1;bool T:1;}uttc_deviation_threshold;
#define uttc_threshold( ... ) ((uttc_deviation_threshold){ __VA_ARGS__ })
UTTC_LINKAGE bool uttc_check_classifyable_implementation(uttc_ieee754_class_t dd,uttc_ieee754_class_t dR,bool de,bool dS,uttc_error_t*aC){if(dd==UTTC_FP_INFINITE){if(de!=dS||dR!=UTTC_FP_INFINITE)aC->L=UTTC_INFINITYB,aC->fc=UTTC_INFINITY;return false;}else if(dR==UTTC_FP_INFINITE){aC->L=UTTC_INFINITYB,aC->fc=UTTC_INFINITY;return false;}if(dd==UTTC_FP_NAN&&dR==UTTC_FP_NAN)return false;else if(dd==UTTC_FP_NAN||dR==UTTC_FP_NAN){aC->L=UTTC_INFINITYB,aC->fc=UTTC_INFINITY;return false;}if(dd==UTTC_FP_ZERO&&dR==UTTC_FP_ZERO){aC->L=uttc_from_rawb(UTTC_BF_0),aC->fc=de==dS?0.:1.;return false;}return true;}
#define UTTC_DEFINE_CHECK_CLASSIFYABLE( D , A ) UTTC_LINKAGE bool uttc_check_classifyable##A( uttc_ieee754_##D df , uttc_big_float dT , uttc_error_t* aC ){ return uttc_check_classifyable_implementation( uttc_fp_classify##A(df) , uttc_fp_classifyb(dT) , uttc_signbit##A(df) , uttc_signbitb(dT) , aC ); }
UTTC_DEFINE_CHECK_CLASSIFYABLE(double,)UTTC_DEFINE_CHECK_CLASSIFYABLE(float,f)UTTC_DEFINE_CHECK_CLASSIFYABLE(half,h)UTTC_DEFINE_CHECK_CLASSIFYABLE(half_alt,h2)
#define UTTC_DEFINE_DETERMINE_ERROR( D , eG , A , l ) UTTC_LINKAGE uttc_error_t uttc_determine_error##eG( uttc_ieee754_##D df , uttc_big_float dT ) { uttc_error_t dY = { 0 , 0 }; if( !uttc_check_classifyable##eG( df , dT , &dY ) ) return dY; uttc_big_float di = uttc_##D##2big_float( df ); uttc_ieee754_##D cM = uttc_nextafter##eG( df , uttc_islessb( di , dT ) ? UTTC_INFINITY##A : UTTC_NINFINITY##A ); uttc_big_float cN = uttc_##D##2big_float( cM ); dY.L = uttc_addsubb( di , dT , true ); uttc_ieee754_double fd = uttc_divide( uttc_big_float2double( uttc_addsubb( dT , di , true ) ) , uttc_big_float2double( uttc_addsubb( cN , di , true ) ) ); if( uttc_fabs(fd) > TARGET_ULP_TOO_LARGE ){ dY.fc = fd; return dY; } if( uttc_islessb( di , dT ) ) while( true ) { if( uttc_islessb( dT , cN ) ){ uttc_ieee754_double aR = uttc_divide( uttc_big_float2double( uttc_addsubb( dT , di , true ) ) , uttc_big_float2double( uttc_addsubb( cN , di , true ) ) ); dY.fc -= aR; break; } if( !l && df == cM ){ dY.fc = -TARGET_ULP_TOO_LARGE; break; } dY.fc -= 1.; df = cM; di = cN; cM = uttc_nextafter##eG( df , UTTC_INFINITY##A ); cN = uttc_##D##2big_float( cM ); } else if( uttc_islessb( dT , di ) ) while( true ) { if( uttc_islessb( cN , dT ) ){ uttc_ieee754_double aR = uttc_big_float2double( uttc_addsubb( di , dT , true ) ) / uttc_big_float2double( uttc_addsubb( di , cN , true ) ); dY.fc += aR; break; } if( !l && df == cM ){ dY.fc = TARGET_ULP_TOO_LARGE; break; } dY.fc += 1.; df = cM; di = cN; cM = uttc_nextafter##eG( df , UTTC_NINFINITY##A ); cN = uttc_##D##2big_float( cM ); } return dY; }
UTTC_DEFINE_DETERMINE_ERROR(double,,,true)UTTC_DEFINE_DETERMINE_ERROR(float,f,F,true)UTTC_DEFINE_DETERMINE_ERROR(half,h,H,true)UTTC_DEFINE_DETERMINE_ERROR(half_alt,h2,H2,false)UTTC_LINKAGE bool uttc_check_threshold(uttc_error_t aQ,uttc_deviation_threshold eR){bool dY=false;bool bN=false;uttc_big_float aP;bool aS;const char*fe;if(eR.cD==UTTC_DM_ABSOLUTE){aP=aQ.L;fe="ABS";}else if(eR.cD==UTTC_DM_UNITS_IN_LAST_PLACE){aP=uttc_double2big_float(aQ.fc);bN=aQ.fc>TARGET_ULP_TOO_LARGE||aQ.fc<-TARGET_ULP_TOO_LARGE;fe="ULP";}else{uttc_print("Invalid threshold deviation metric set!");return false;}bool S=uttc_signbitb(aP);if(S){aP=uttc_negateb(aP);aS=eR.T;if(!eR.S)eR.fn=uttc_from_rawb(UTTC_BF_0);}else{aS=eR.K;if(!eR.J)eR.fn=uttc_from_rawb(UTTC_BF_0);}dY=aS?uttc_islessb(aP,eR.fn):uttc_islessequalb(aP,eR.fn);
#ifndef TARGET_VERBOSE
if(!dY)
#endif
{if(bN&&S)uttc_print("ca.(-)%11.-5Lg",aP);else if(bN)uttc_print("ca. %13.-7Lg",aP);else if(S)uttc_print("(-)%14.-8Lg",aP);else uttc_print("%17.-11Lg",aP);uttc_print(" = [%s DEV] = %s THRESHOLD OF %.Lg %s\n",fe,dY?"MEETS":"VIOLATES",eR.fn,aS?"(inclusive)":"(exclusive)");}return dY;}
#ifndef TARGET_VERBOSE
#define DEFINE_IS_APPROXIMATELY_FUNCTION_IMPLEMENTATION( A , e , t ) UTTC_LINKAGE bool uttc_is_approximately##A##_implementation( e df , uttc_big_float dT , uttc_deviation_threshold eR , const char* dj , const char* dX , int dW ){ bool dY = uttc_check_threshold( uttc_determine_error##A( df , dT ) , eR ); if( !dY ){ uttc_print( "%#21.21s = [OBS] = " t "\n" , dj , df ); uttc_print( "%#21.21s = [REF] = %.*Lg\n" , dX , dW , dT ); } return dY; }
#else
#define DEFINE_IS_APPROXIMATELY_FUNCTION_IMPLEMENTATION( A , e , t ) UTTC_LINKAGE bool uttc_is_approximately##A##_implementation( e df , uttc_big_float dT , uttc_deviation_threshold eR , const char* dj , const char* dX , int dW ){ bool dY = uttc_check_threshold( uttc_determine_error##A( df , dT ) , eR ); uttc_print( "%#21.21s = [OBS] = " t "\n" , dj , df ); uttc_print( "%#21.21s = [REF] = %.*Lg\n" , dX , dW , dT ); return dY; }
#endif
DEFINE_IS_APPROXIMATELY_FUNCTION_IMPLEMENTATION(,uttc_ieee754_double,"%.-17g")DEFINE_IS_APPROXIMATELY_FUNCTION_IMPLEMENTATION(f,uttc_ieee754_float,"%.-8hg")DEFINE_IS_APPROXIMATELY_FUNCTION_IMPLEMENTATION(h,uttc_ieee754_half,"%.-5hhg")DEFINE_IS_APPROXIMATELY_FUNCTION_IMPLEMENTATION(h2,uttc_ieee754_half_alt,"%#.-5hhg")
#define uttc_is_approximatelyb( s , v , B ) uttc_is_approximately_implementation( s , v , B , #s , #v , UTTC_INT_MIN )
#define uttc_is_approximatelyfb( s , v , B ) uttc_is_approximatelyf_implementation( s , v , B , #s , #v , UTTC_INT_MIN )
#define uttc_is_approximatelyhb( s , v , B ) uttc_is_approximatelyh_implementation( s , v , B , #s , #v , UTTC_INT_MIN )
#define uttc_is_approximatelyh2b( s , v , B ) uttc_is_approximatelyh2_implementation( s , v , B , #s , #v , UTTC_INT_MIN )
#define uttc_is_approximately( s , v , B ) uttc_is_approximately_implementation( s , uttc_double2big_float(v) , B , #s , #v , -17 )
#define uttc_is_approximatelyf( s , v , B ) uttc_is_approximatelyf_implementation( s , uttc_double2big_float(v) , B , #s , #v , -17 )
#define uttc_is_approximatelyh( s , v , B ) uttc_is_approximatelyh_implementation( s , uttc_double2big_float(v) , B , #s , #v , -17 )
#define uttc_is_approximatelyh2( s , v , B ) uttc_is_approximatelyh2_implementation( s , uttc_double2big_float(v) , B , #s , #v , -17 )
#define uttc_is_approximatelyff( s , v , B ) uttc_is_approximatelyf_implementation( s , uttc_float2big_float(v) , B , #s , #v , -8 )
#define uttc_is_approximatelyhf( s , v , B ) uttc_is_approximatelyh_implementation( s , uttc_float2big_float(v) , B , #s , #v , -8 )
#define uttc_is_approximatelyh2f( s , v , B ) uttc_is_approximatelyh2_implementation( s , uttc_float2big_float(v) , B , #s , #v , -8 )
#define uttc_is_approximatelyhh( s , v , B ) uttc_is_approximatelyh_implementation( s , uttc_half2big_float(v) , B , #s , #v , -5 )
#define uttc_is_approximatelyh2h2( s , v , B ) uttc_is_approximatelyh2_implementation( s , uttc_half_alt2big_float(v) , B , #s , #v , -5 )
#ifndef TARGET_VERBOSE
#define DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION( r , e , t , w , I , d ) UTTC_LINKAGE bool r##_implementation( e df , e dT , const char* dj , const char* dX ){ bool dY = d; if( !dY ){ uttc_print( "%#21.21s = [OBS] = " t "\n" , dj , df ); uttc_print( "%#21.21s = [" w "] = " t "\n" , dX , dT ); uttc_print( "                        [CMP] = %s\n" , dY ? I : "NOT " I ); } return dY; }
#else
#define DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION( r , e , t , w , I , d ) UTTC_LINKAGE bool r##_implementation( e df , e dT , const char* dj , const char* dX ){ bool dY = d; uttc_print( "%#21.21s = [OBS] = " t "\n" , dj , df ); uttc_print( "%#21.21s = [" w "] = " t "\n" , dX , dT ); uttc_print( "                        [CMP] = %s\n" , dY ? I : "NOT " I ); return dY; }
#endif
DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_exactly,uttc_intmax_t,"%jd","REF","EQUAL",(df==dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_exactlyu,uttc_uintmax_t,"%ju","REF","EQUAL",(df==dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_exactlyb,bool,"%b","REF","EQUAL",df?(dT?true:false):(dT?false:true))DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_exactlyp,const void*,"0x%p","REF","EQUAL",(df==dT)?true:false)
#define uttc_is_exactly( s , v ) uttc_is_exactly_implementation( s , v , #s , #v )
#define uttc_is_exactlyu( s , v ) uttc_is_exactlyu_implementation( s , v , #s , #v )
#define uttc_is_exactlyp( s , v ) uttc_is_exactlyp_implementation( s , v , #s , #v )
#define uttc_is_exactlyb( s , v ) uttc_is_exactlyb_implementation( s , v , #s , #v )
DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_greater,uttc_intmax_t,"%jd","MIN","GREATER",(df>dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_greateru,uttc_uintmax_t,"%ju","MIN","GREATER",(df>dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_greaterp,const void*,"0x%p","MIN","GREATER",(df>dT)?true:false)
#define uttc_is_greater( s , v ) uttc_is_greater_implementation( s , v , #s , #v )
#define uttc_is_greateru( s , v ) uttc_is_greateru_implementation( s , v , #s , #v )
#define uttc_is_greaterp( s , v ) uttc_is_greaterp_implementation( s , v , #s , #v )
DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_greater_equal,uttc_intmax_t,"%jd","MIN","GREATER-EQUAL",(df>=dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_greater_equalu,uttc_uintmax_t,"%ju","MIN","GREATER-EQUAL",(df>=dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_greater_equalp,const void*,"0x%p","MIN","GREATER-EQUAL",(df>=dT)?true:false)
#define uttc_is_greater_equal( s , v ) uttc_is_greater_equal_implementation( s , v , #s , #v )
#define uttc_is_greater_equalu( s , v ) uttc_is_greater_equalu_implementation( s , v , #s , #v )
#define uttc_is_greater_equalp( s , v ) uttc_is_greater_equalp_implementation( s , v , #s , #v )
DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_lower,uttc_intmax_t,"%jd","MAX","LOWER",(df<dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_loweru,uttc_uintmax_t,"%ju","MAX","LOWER",(df<dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_lowerp,const void*,"0x%p","MAX","LOWER",(df<dT)?true:false)
#define uttc_is_lower( s , v ) uttc_is_lower_implementation( s , v , #s , #v )
#define uttc_is_loweru( s , v ) uttc_is_loweru_implementation( s , v , #s , #v )
#define uttc_is_lowerp( s , v ) uttc_is_lowerp_implementation( s , v , #s , #v )
DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_lower_equal,uttc_intmax_t,"%jd","MAX","LOWER-EQUAL",(df<=dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_lower_equalu,uttc_uintmax_t,"%ju","MAX","LOWER-EQUAL",(df<=dT)?true:false)DEFINE_COMPARISON_FUNCTION_IMPLEMENTATION(uttc_is_lower_equalp,const void*,"0x%p","MAX","LOWER-EQUAL",(df<=dT)?true:false)
#define uttc_is_lower_equal( s , v ) uttc_is_lower_equal_implementation( s , v , #s , #v )
#define uttc_is_lower_equalu( s , v ) uttc_is_lower_equalu_implementation( s , v , #s , #v )
#define uttc_is_lower_equalp( s , v ) uttc_is_lower_equalp_implementation( s , v , #s , #v )
UTTC_LINKAGE bool uttc_is_congruent_implementation(const void*df,const void*dT,uttc_size_t cU,const char*dj,const char*dX){const unsigned char*dh=(const unsigned char*)df;const unsigned char*dV=(const unsigned char*)dT;if(df==dT){
#ifdef TARGET_VERBOSE
uttc_print("%#21.21s = [&OBS] = 0x%p\n",dj,df);uttc_print("%#21.21s = [&REF] = 0x%p\n",dX,dT);if(df){uttc_print("                [REF] = [OBS] = \"");uttc_size_t i=0;for(;i<cU;++i)if(i>16){uttc_print("...");break;}else uttc_print("%#c",dh[i]);}uttc_print("\"\n                        [CMP] = SAME ADDRESS -> CONGRUENT\n");
#endif
return true;}else if(!df||!dT){uttc_print("%#21.21s = [&OBS] = 0x%p\n",dj,df);uttc_print("%#21.21s = [&REF] = 0x%p\n",dX,dT);uttc_print("\"\n                        [CMP] = NULL POINTER -> NOT CONGRUENT\n");}uttc_size_t bO=0;for(;bO!=cU;bO++){unsigned char dg=dh[bO];unsigned char dU=dV[bO];if(dg!=dU){uttc_print("%#21.21s = [&OBS] = 0x%p\n",dj,df);uttc_print("%#21.21s = [&REF] = 0x%p\n",dX,dT);uttc_print("                        [OBS] = \"");uttc_size_t eB=bO>10?bO-10:0;if(eB>0)uttc_print("...");uttc_size_t i;for(i=eB;i<cU;++i)if(i-eB>16){uttc_print("...");break;}else uttc_print("%#c",dh[i]);uttc_print("\"\n                        [REF] = \"");if(eB>0)uttc_print("...");for(i=eB;i<cU;++i)if(i-eB>16){uttc_print("...");break;}else uttc_print("%#c",dV[i]);uttc_print("\"\n                        [OBS:%zu] = 0x%x ('%#c')\n",bO+1,dg,dg);uttc_print("                        [REF:%zu] = 0x%x ('%#c')\n",bO+1,dU,dU);uttc_print("                        [CMP] = NOT BYTE-WISE CONGRUENT\n");return false;}}
#ifdef TARGET_VERBOSE
uttc_print("%#21.21s = [&OBS] = 0x%p\n",dj,df);uttc_print("%#21.21s = [&REF] = 0x%p\n",dX,dT);uttc_print("                        [OBS] = \"");uttc_size_t i;for(i=0;i<cU;++i)if(i>16){uttc_print("...");break;}else uttc_print("%#c",dh[i]);uttc_print("\"\n                        [REF] = \"");for(i=0;i<cU;++i)if(i>16){uttc_print("...");break;}else uttc_print("%#c",dV[i]);uttc_print("\"\n                        [CMP] = BYTE-WISE CONGRUENT (%zu BYTES)\n");
#endif
return true;}
#define uttc_is_congruent( s , v , b ) uttc_is_congruent_implementation( s , v , b , #s , #v )
typedef uttc_uint32_t uttc_hash_t;UTTC_LINKAGE uttc_hash_t uttc_hash_word(uttc_uint32_t x){x++;x^=x>>17;x*=0xed5ad4bb;x^=x>>11;x*=0xac4c1b51;x^=x>>15;x*=0x31848bab;x^=x>>14;return x;}UTTC_LINKAGE uttc_hash_t uttc_hash_combine_asymmetric(uttc_hash_t ci,uttc_hash_t ed){return ci^(ed+0x9e3779b9+(ci<<6)+(ci>>2));}UTTC_LINKAGE uttc_hash_t uttc_hash_combine_symmetric(uttc_hash_t ci,uttc_hash_t ed){return ci^ed;}UTTC_LINKAGE uttc_hash_t uttc_hash_buffer(const void*aa,uttc_size_t eu){uttc_hash_t dY=0;if(eu){const uttc_uint8_t*bO=(const uttc_uint8_t*)aa;uttc_size_t ce=(eu+3)/4;uttc_uint32_t fm=0;switch(eu%4){case 0:do{fm=(fm<<8)+*bO++;case 3:fm=(fm<<8)+*bO++;case 2:fm=(fm<<8)+*bO++;case 1:fm=(fm<<8)+*bO++;dY=uttc_hash_combine_asymmetric(dY,uttc_hash_word(fm));fm=0;}while(--ce);}}return dY;}
#define uttc_hash( fm ) uttc_hash_buffer( &(fm) , sizeof(fm) )
#define DEFINE_VERBOSE_FUNCTION( C , E , j ) UTTC_LINKAGE C uttc_verbose ## E( C fn ){ uttc_print( j "\n" , fn ); return fn; } UTTC_LINKAGE C* uttc_verbose ## E ## _ptr ( C* fn ){ uttc_print( "0x%p\n" , (void*)fn ); return fn; }
UTTC_LINKAGE void*uttc_verbose_void_ptr(void*fn){uttc_print("0x%p\n",fn);return fn;}DEFINE_VERBOSE_FUNCTION(short,short,"%hd")DEFINE_VERBOSE_FUNCTION(short int,es,"%hd")DEFINE_VERBOSE_FUNCTION(int,int,"%d")DEFINE_VERBOSE_FUNCTION(long,long,"%ld")DEFINE_VERBOSE_FUNCTION(long int,cm,"%ld")DEFINE_VERBOSE_FUNCTION(long long,cn,"%lld")DEFINE_VERBOSE_FUNCTION(long long int,co,"%lld")DEFINE_VERBOSE_FUNCTION(unsigned short,fk,"%hu")DEFINE_VERBOSE_FUNCTION(unsigned short int,fl,"%hu")DEFINE_VERBOSE_FUNCTION(unsigned int,ff,"%u")DEFINE_VERBOSE_FUNCTION(unsigned long,fg,"%lu")DEFINE_VERBOSE_FUNCTION(unsigned long int,fh,"%lu")DEFINE_VERBOSE_FUNCTION(unsigned long long,fi,"%llu")DEFINE_VERBOSE_FUNCTION(unsigned long long int,fj,"%llu")DEFINE_VERBOSE_FUNCTION(uttc_intmax_t,uttc_intmax_t,"%jd")DEFINE_VERBOSE_FUNCTION(uttc_uintmax_t,uttc_uintmax_t,"%ju")DEFINE_VERBOSE_FUNCTION(uttc_size_t,uttc_size_t,"%zu")DEFINE_VERBOSE_FUNCTION(uttc_ptrdiff_t,uttc_ptrdiff_t,"%td")DEFINE_VERBOSE_FUNCTION(uttc_ieee754_double_t,uttc_ieee754_double_t,"%.g")DEFINE_VERBOSE_FUNCTION(uttc_ieee754_float_t,uttc_ieee754_float_t,"%.hg")DEFINE_VERBOSE_FUNCTION(uttc_ieee754_half_t,uttc_ieee754_half_t,"%.hhg")DEFINE_VERBOSE_FUNCTION(uttc_ieee754_half_alt_t,uttc_ieee754_half_alt_t,"%#.hhg")DEFINE_VERBOSE_FUNCTION(uttc_big_float,uttc_big_float,"%.Lg") typedef enum{UTTC_VD_PASSING=0,UTTC_VD_VERDICTLESS=-1,UTTC_VD_FAILING=-2,UTTC_VD_ERROR=-3}uttc_verdict;typedef enum{UTTC_ST_UNDEFINED=0,UTTC_ST_VERDICT=1,UTTC_ST_TEST_CASE=2,UTTC_ST_CHECK=3,UTTC_ST_REQUIREMENT_GROUP=4,UTTC_ST_MIXED=5,UTTC_ST_DYNAMIC=UTTC_ST_MIXED}uttc_statistic_type;typedef struct{uttc_size_t eU;uttc_size_t fr;uttc_size_t aP;uttc_size_t bf;uttc_size_t dx;uttc_size_t aj;uttc_size_t ag;uttc_size_t ah;uttc_ieee754_float ak;uttc_ieee754_float aM;uttc_statistic_type fa;}uttc_statistic;
#define UTTC_DYNAMIC_STATISTIC ((uttc_statistic){ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1.f , 0.f , UTTC_ST_DYNAMIC })
#define UTTC_VERDICT_STATISTIC ((uttc_statistic){ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1.f , 0.f , UTTC_ST_VERDICT })
#define UTTC_CHECK_STATISTIC ((uttc_statistic){ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1.f , 0.f , UTTC_ST_CHECK })
#define UTTC_TEST_CASE_STATISTIC ((uttc_statistic){ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1.f , 0.f , UTTC_ST_TEST_CASE })
#define UTTC_REQUIREMENT_GROUP_STATISTIC ((uttc_statistic){ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1.f , 0.f , UTTC_ST_REQUIREMENT_GROUP })
void uttc_st_verdict_push(uttc_statistic*eC,uttc_verdict fq,bool cw,uttc_statistic_type fa){if(eC->eU++){if(eC->fa!=fa&&eC->fa!=UTTC_ST_VERDICT)eC->fa=UTTC_ST_MIXED;}else if(eC->fa==UTTC_ST_DYNAMIC)eC->fa=fa;switch(fq){case UTTC_VD_VERDICTLESS:eC->fr+=1;eC->aj+=cw?1:0;break;case UTTC_VD_ERROR:eC->aP+=1;eC->ag+=cw?1:0;break;case UTTC_VD_FAILING:eC->bf+=1;eC->ah+=cw?1:0;break;case UTTC_VD_PASSING:eC->dx+=1;break;default:break;}}void uttc_st_verdict(uttc_statistic*eC,uttc_size_t az,uttc_verdict fq,bool cw,const char*by,uttc_statistic_type fa){if(eC[0].fa==UTTC_ST_VERDICT||eC[0].fa==fa)uttc_st_verdict_push(&eC[0],fq,cw,fa);uttc_st_verdict_push(&eC[az],fq,cw,fa);
#ifndef TARGET_VERBOSE
if(fq!=UTTC_VD_PASSING&&!cw)
#endif
{switch(fa){case UTTC_ST_VERDICT:uttc_print_string("Verdict");break;case UTTC_ST_TEST_CASE:uttc_print_string("Test case");break;case UTTC_ST_CHECK:uttc_print_string("Check");break;case UTTC_ST_REQUIREMENT_GROUP:uttc_print_string("Requirement group");break;default:break;}if(by)uttc_print(" (%s)",by);switch(fq){case UTTC_VD_VERDICTLESS:uttc_print_string(" is VERDICTLESS");break;case UTTC_VD_PASSING:uttc_print_string(" is PASSING");break;case UTTC_VD_FAILING:uttc_print_string(" is FAILING");break;case UTTC_VD_ERROR:uttc_print_string(" is ERROR");break;default:break;}if(cw&&fq!=UTTC_VD_PASSING)uttc_print_string(" but COMPLIANT");uttc_print_string(".\n");}}void uttc_st_accumulate_inside(uttc_statistic*eC,uttc_size_t az,const char*by,uttc_statistic_type fa){uttc_verdict fq;bool cw=false;if(!eC[az+1].eU)fq=UTTC_VD_VERDICTLESS;else if(eC[az+1].aP!=eC[az+1].ag)fq=UTTC_VD_ERROR;else if(eC[az+1].bf!=eC[az+1].ah)fq=UTTC_VD_FAILING;else if(eC[az+1].aj!=eC[az+1].aj)fq=UTTC_VD_VERDICTLESS;else if(eC[az+1].aP)fq=UTTC_VD_ERROR,cw=true;else if(eC[az+1].bf)fq=UTTC_VD_FAILING,cw=true;else if(eC[az+1].fr)fq=UTTC_VD_VERDICTLESS,cw=true;else fq=UTTC_VD_PASSING;uttc_st_verdict(eC,az,fq,cw,by,fa);eC[az+1]=UTTC_DYNAMIC_STATISTIC;}void uttc_st_accumulate_global(uttc_statistic*eC){if(eC[0].fa==UTTC_ST_DYNAMIC)eC[0]=eC[1];else{}eC[1]=UTTC_DYNAMIC_STATISTIC;}
#define uttc_st_check( z , g , G , p , k ) uttc_st_verdict( z , g , G , p , k , UTTC_ST_CHECK )
#define uttc_st_test_case( z , g , k ) uttc_st_accumulate_inside( z , g , k , UTTC_ST_TEST_CASE )
#define uttc_st_meet( z , g , k ) uttc_st_accumulate_inside( z , g , k , UTTC_ST_REQUIREMENT_GROUP )
void uttc_st_print_verdict(uttc_statistic eC){uttc_size_t ai=eC.ag+eC.ah+eC.aj;if(!eC.eU)uttc_print_string("VERDICTLESS");else if(eC.aP!=eC.ag)uttc_print_string("ERROR");else if(eC.bf!=eC.ah)uttc_print_string("FAILING");else if(eC.fr!=eC.aj)uttc_print_string("VERDICTLESS");else if(ai)uttc_print_string("COMPLIANT");else if(eC.dx)uttc_print_string("PASSING");else uttc_print_string("!!ACCUMULATION ERROR!!");}void uttc_st_print(uttc_statistic eC,bool bv){uttc_size_t ai=eC.ag+eC.ah+eC.aj;uttc_print_unsigned_long(eC.eU);if(bv||eC.fr){uttc_print_string(" / ");uttc_print_unsigned_long(eC.fr);}if(bv||eC.aP){uttc_print_string(" / ");uttc_print_unsigned_long(eC.aP);}if(bv||eC.bf){uttc_print_string(" / ");uttc_print_unsigned_long(eC.bf);}if(bv||eC.dx||ai){if(bv||eC.dx){if(bv||ai)uttc_print_string(" / ");else uttc_print_string(" // ");uttc_print_unsigned_long(eC.dx);}if(bv||ai){uttc_print_string(" // ");uttc_print_unsigned_long(eC.dx+ai);}}uttc_print_string(" [total");if(bv||eC.fr)uttc_print_string("/verdictless");if(bv||eC.aP)uttc_print_string("/error");if(bv||eC.bf)uttc_print_string("/failing");if(bv||eC.dx||ai){if(bv||eC.dx){if(bv||ai)uttc_print_string("/passing");else uttc_print_string("//passing");}if(bv||ai)uttc_print_string("//compliant");}switch(eC.fa){case UTTC_ST_VERDICT:uttc_print_string(" verdicts]");break;case UTTC_ST_TEST_CASE:uttc_print_string(" test cases]");break;case UTTC_ST_CHECK:uttc_print_string(" checks]");break;case UTTC_ST_REQUIREMENT_GROUP:uttc_print_string(" requirement groups]");break;default:uttc_print_char(']');break;}}bool uttc_st_is_compliant(uttc_statistic eC){if(!eC.eU)return false;if(eC.aP!=eC.ag)return false;if(eC.bf!=eC.ah)return false;if(eC.fr!=eC.aj)return false;return eC.dx>0?true:false;}
