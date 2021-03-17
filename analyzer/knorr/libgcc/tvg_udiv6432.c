





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

uint32_t __udiv6432 (uint64_t n, uint32_t d)
{
  const uint32_t b = (1<<16);


  uint32_t nn1, nn0;


  uint32_t dn1, dn0;


  uint32_t q1, q0;


  uint32_t nn32, nn21, nn10;


  uint32_t rhat;


  int s;


  uint32_t n1, n0;

  n1 = n >> 32;
  n0 = n;


  if (n1 >= d)
    return 4294967295UL;


  s = __builtin_clz(d);


  d = d << s;


  dn1 = d >> 16;
  dn0 = d & 0xFFFF;

  nn32 = s
    ? (n1 << s) | (n0 >> (32 - s))
    : n1;


  nn10 = n0 << s;


  nn1 = nn10 >> 16;
  nn0 = nn10 & 0xFFFF;


  q1 = nn32 / dn1;
  rhat = nn32 - q1*dn1;

again1:
  if (q1 >= b || q1*dn0 > b*rhat + nn1)
    {
      q1 = q1 - 1;
      rhat = rhat + dn1;
      if (solver_pragma(1,1,2) && rhat < b) goto again1;
    }

  nn21 = nn32*b + nn1 - q1*d;


  q0 = nn21/dn1;
  rhat = nn21 - q0*dn1;

//    return rhat;

again2:
  if (q0 >= b || q0*dn0 > b*rhat + nn0)
    {
      q0 = q0 - 1;
      rhat = rhat + dn1;
     if (solver_pragma(1,1,2) && rhat < b) goto again2;
    }

    return q1*b + q0;
}
