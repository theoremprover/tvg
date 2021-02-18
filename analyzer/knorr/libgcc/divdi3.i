





typedef long int _Int32t;
typedef unsigned long int _Uint32t;







typedef long int _Ptrdifft;






typedef long unsigned int _Sizet;











  typedef __builtin_va_list va_list;

typedef long long _Longlong;
typedef unsigned long long _ULonglong;
typedef int _Wchart;
typedef unsigned int _Wintt;
typedef va_list _Va_list;





void _Atexit(void (*)(void));

typedef char _Sysch_t;


typedef signed char int8_t;
typedef short int int16_t;
typedef long int int32_t;

typedef long long unsigned int uint64_t;

typedef long long int int_least64_t;
typedef long long unsigned int uint_least64_t;
typedef unsigned char uint8_t;
typedef short unsigned int uint16_t;
typedef long unsigned int uint32_t;

typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef long int int_least32_t;
typedef unsigned char uint_least8_t;
typedef short unsigned int uint_least16_t;
typedef long unsigned int uint_least32_t;

typedef int int_fast8_t;
typedef int int_fast16_t;
typedef int int_fast32_t;

typedef unsigned int uint_fast8_t;
typedef unsigned int uint_fast16_t;
typedef unsigned int uint_fast32_t;

typedef long long int int_fast64_t;
typedef long long unsigned int uint_fast64_t;
typedef long long int intmax_t;
typedef long long unsigned int uintmax_t;
typedef long long int int64_t;
typedef long int intptr_t;
typedef long unsigned int uintptr_t;


extern uint32_t __udiv6432 (uint64_t, uint32_t);

extern int64_t __divdi3 (int64_t, int64_t);
extern int64_t __moddi3 (int64_t, int64_t);
extern int64_t __divmoddi4 (int64_t, int64_t, int64_t*);

extern uint64_t __udivdi3 (uint64_t, uint64_t);
extern uint64_t __umoddi3 (uint64_t, uint64_t);
extern uint64_t __udivmoddi4 (uint64_t, uint64_t, uint64_t*);





int64_t
__divdi3 (int64_t n, int64_t d)
{
  uint64_t n_abs = n >= 0 ? (uint64_t) n : -(uint64_t) n;
  uint64_t d_abs = d >= 0 ? (uint64_t) d : -(uint64_t) d;
  uint64_t q_abs;

  if (d == 0)
    return (n >= 0) ? 9223372036854775807LL : (-9223372036854775807LL - 1);

  if (d == -1 && n == (-9223372036854775807LL - 1))
    return 9223372036854775807LL;

  q_abs = __udivdi3 (n_abs, d_abs);

  return (n < 0) == (d < 0) ? (int64_t) q_abs : -(int64_t) q_abs;
}
