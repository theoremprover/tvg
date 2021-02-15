






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









typedef int sig_atomic_t;
typedef void _Sigfun(int);


_Sigfun * signal(int, _Sigfun *);



int raise(int);




static _Sigfun *sigtable[44] = {0};
_Sigfun *(signal)(int sig, _Sigfun *fun)
 {
 _Sigfun *s;

 if (sig <= 0 || 44 <= sig || fun == (( _Sigfun *)-1))
  return ((( _Sigfun *)-1));

 (void)0;
 s = sigtable[sig], sigtable[sig] = fun;
 (void)0;
 return (s);
 }


