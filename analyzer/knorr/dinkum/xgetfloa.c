






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





typedef const short *_Ctype_t;

_Ctype_t _Getpctype(void);
_Ctype_t _Getptolower(void);
_Ctype_t _Getptoupper(void);


extern _Ctype_t __attribute__((fardata)) _Ctype;
extern _Ctype_t __attribute__((fardata)) _Tolotab;
extern _Ctype_t __attribute__((fardata)) _Touptab;


int isalnum(int);
int isalpha(int);
int iscntrl(int);
int isdigit(int);
int isgraph(int);
int islower(int);
int isprint(int);
int ispunct(int);
int isspace(int);
int isupper(int);
int isxdigit(int);
int tolower(int);
int toupper(int);


int isblank(int);












struct lconv
 {

 char *currency_symbol;
 char *int_curr_symbol;
 char *mon_decimal_point;
 char *mon_grouping;
 char *mon_thousands_sep;
 char *negative_sign;
 char *positive_sign;

 char frac_digits;
 char n_cs_precedes;
 char n_sep_by_space;
 char n_sign_posn;
 char p_cs_precedes;
 char p_sep_by_space;
 char p_sign_posn;

 char int_frac_digits;

 char int_n_cs_precedes;
 char int_n_sep_by_space;
 char int_n_sign_posn;
 char int_p_cs_precedes;
 char int_p_sep_by_space;
 char int_p_sign_posn;



 char *decimal_point;
 char *grouping;
 char *thousands_sep;
 char *_Frac_grouping;
 char *_Frac_sep;
 char *_False;
 char *_True;


 char *_No;
 char *_Yes;
 };

struct _Linfo;



struct lconv *localeconv(void);
char *setlocale(int, const char *);
extern struct lconv _Locale;










typedef _Sizet size_t;





typedef _Wchart wchar_t;


typedef struct
 {
 int quot;
 int rem;
 } div_t;

typedef struct
 {
 long quot;
 long rem;
 } ldiv_t;


typedef struct
 {
 _Longlong quot;
 _Longlong rem;
 } _Lldiv_t;



typedef _Lldiv_t lldiv_t;




void exit(int) __attribute__((__noreturn__));

void _Exit(int) __attribute__((__noreturn__));
char *getenv(const char *);
int system(const char *);



int abs(int);
long labs(long);

void abort(void) __attribute__((__noreturn__));
void *calloc(size_t, size_t);
div_t div(int, int);
void free(void *);
ldiv_t ldiv(long, long);
void *malloc(size_t);
int mblen(const char *, size_t);
size_t mbstowcs(wchar_t *,
 const char *, size_t);
int mbtowc(wchar_t *, const char *, size_t);
int rand(void);
void srand(unsigned int);
void *realloc(void *, size_t);
long strtol(const char *, char **, int);
size_t wcstombs(char *,
 const wchar_t *, size_t);
int wctomb(char *, wchar_t);


void lcong48(unsigned short [7]);
unsigned short *seed48(unsigned short [3]);
void srand48(long);

double drand48(void);
long lrand48(void);
long mrand48(void);

double erand48(unsigned short [3]);
long nrand48(unsigned short [3]);
long jrand48(unsigned short [3]);

int getopt(int, char *const *, const char *);
void *memalign(size_t, size_t);
char *mktemp(char *);
int putenv(char *);
int rand_r(unsigned *);
char *tempnam(const char *, const char *);

extern char *optarg;
extern int optind, opterr, optopt;


unsigned long _Stoul(const char *, char **, int);
float _Stof(const char *, char **, long);
double _Stod(const char *, char **, long);
long double _Stold(const char *, char **, long);
_Longlong _Stoll(const char *, char **, int);
_ULonglong _Stoull(const char *, char **, int);

float _Stofx(const char *, char **, long, int *);
double _Stodx(const char *, char **, long, int *);
long double _Stoldx(const char *, char **, long, int *);
long _Stolx(const char *, char **, int, int *);
unsigned long _Stoulx(const char *, char **, int, int *);
_Longlong _Stollx(const char *, char **, int, int *);
_ULonglong _Stoullx(const char *, char **, int, int *);

size_t _Getmbcurmax(void);


_Longlong llabs(_Longlong);
lldiv_t lldiv(_Longlong, _Longlong);





typedef int _Cmpfun(const void *, const void *);

      int atexit(void (*)(void));
void *bsearch(const void *, const void *,
 size_t, size_t, _Cmpfun *);
void qsort(void *, size_t, size_t, _Cmpfun *);

double atof(const char *);
int atoi(const char *);
long atol(const char *);
double strtod(const char *, char **);
unsigned long strtoul(const char *,
 char **, int);
_Longlong atoll(const char *);
float strtof(const char *,
 char **);
long double strtold(const char *,
 char **);
_Longlong strtoll(const char *,
 char **, int);
_ULonglong strtoull(const char *,
 char **, int);











int (_Fail_s)(const char *, size_t);



typedef int errno_t;




typedef size_t rsize_t;


typedef void (*constraint_handler_t)(const char *,
 void *, errno_t);

constraint_handler_t set_constraint_handler_s(
 constraint_handler_t);
void abort_handler_s(const char *,
 void *, errno_t);
void ignore_handler_s(const char *,
 void *, errno_t);

errno_t getenv_s(size_t *, char *,
 rsize_t, const char *);


typedef int _Cmpfun_s(const void *, const void *, void *);

void *bsearch_s(const void *, const void *,
 rsize_t, rsize_t, _Cmpfun_s *, void *);
errno_t qsort_s(void *,
 rsize_t, rsize_t, _Cmpfun_s *, void *);




errno_t wctomb_s(int *,
 char *, rsize_t,
 wchar_t);
errno_t mbstowcs_s(size_t *,
 wchar_t *, rsize_t,
 const char *, rsize_t);
errno_t wcstombs_s(size_t *,
 char *, rsize_t,
 const wchar_t *, rsize_t);









int memcmp(const void *, const void *, size_t);
void *memcpy(void *, const void *, size_t);
void *memset(void *, int, size_t);
char *strcat(char *, const char *);
int strcmp(const char *, const char *);
char *strcpy(char *, const char *);
size_t strlen(const char *);

void *memmove(void *, const void *, size_t);
int strcoll(const char *, const char *);
size_t strcspn(const char *, const char *);
char *strerror(int);
char *strncat(char *, const char *, size_t);
int strncmp(const char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);
size_t strspn(const char *, const char *);
char *strtok(char *, const char *);
size_t strxfrm(char *, const char *, size_t);


char *strdup(const char *);
int strcasecmp(const char *, const char *);
int strncasecmp(const char *, const char *, size_t);
char *strtok_r(char *, const char *, char **);






char *strchr(const char *, int);
char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
char *strstr(const char *, const char *);


void *memchr(const void *, int, size_t);







errno_t memcpy_s(void *, rsize_t,
 const void *, rsize_t);
errno_t memmove_s(void *, rsize_t,
 const void *, rsize_t);

errno_t strcpy_s(char *, rsize_t,
 const char *);
errno_t strncpy_s(char *, rsize_t,
 const char *, rsize_t);
errno_t strcat_s(char *, rsize_t,
 const char *);
errno_t strncat_s(char *, rsize_t,
 const char *, rsize_t);
char *strtok_s(char *, rsize_t *,
 const char *, char **);

errno_t strerror_s(char *, rsize_t, errno_t);
size_t strerrorlen_s(errno_t);

size_t strnlen_s(const char *, size_t);











typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } __attribute__ ((__may_alias__)) _Dconst;


void _Feraise(int);


double _Cosh(double, double);



short _Dtest(double *);

short _Exp(double *, double, long);
double _Log(double, int);
double _Sin(double, unsigned int);
double _Sinh(double, double);
extern _Dconst _Denorm, _Hugeval, _Inf,
 _Nan, _Snan;


float _FCosh(float, float);
short _FDtest(float *);
short _FExp(float *, float, long);
float _FLog(float, int);
float _FSin(float, unsigned int);
float _FSinh(float, float);
extern _Dconst _FDenorm, _FInf, _FNan, _FSnan;


long double _LCosh(long double, long double);
short _LDtest(long double *);
short _LExp(long double *, long double, long);
long double _LLog(long double, int);
long double _LSin(long double, unsigned int);
long double _LSinh(long double, long double);
extern _Dconst _LDenorm, _LInf, _LNan, _LSnan;













extern int __attribute__((fardata)) _Errno;





















typedef float float_t;
typedef double double_t;

int _FFpcomp(float, float);
int _Fpcomp(double, double);
int _LFpcomp(long double, long double);

int _FDclass(float);
int _Dclass(double);
int _LDclass(long double);

int _FDsign(float);
int _Dsign(double);
int _LDsign(long double);



double acos(double);
double asin(double);
double atan(double);
double atan2(double, double);
double ceil(double);
double exp(double);
double fabs(double);
double floor(double);
double fmod(double, double);
double frexp(double, int *);
double ldexp(double, int);
double modf(double, double *);
double pow(double, double);
double sqrt(double);
double tan(double);
double tanh(double);


double acosh(double);
double asinh(double);
double atanh(double);
double cbrt(double);
double copysign(double, double);
double erf(double);
double erfc(double);
double exp2(double);
double expm1(double);
double fdim(double, double);
double fma(double, double, double);
double fmax(double, double);
double fmin(double, double);
double hypot(double, double);
int ilogb(double);
double lgamma(double);
_Longlong llrint(double);
_Longlong llround(double);
double log1p(double);
double logb(double);
long lrint(double);
long lround(double);
double nan(const char *);
double nearbyint(double);
double nextafter(double, double);
double nexttoward(double, long double);
double remainder(double, double);
double remquo(double, double, int *);
double rint(double);
double round(double);
double scalbn(double, int);
double scalbln(double, long);
double tgamma(double);
double trunc(double);



float acosf(float);
float asinf(float);
float atanf(float);
float atan2f(float, float);
float ceilf(float);
float expf(float);
float fabsf(float);
float floorf(float);
float fmodf(float, float);
float frexpf(float, int *);
float ldexpf(float, int);
float modff(float, float *);
float powf(float, float);
float sqrtf(float);
float tanf(float);
float tanhf(float);


float acoshf(float);
float asinhf(float);
float atanhf(float);
float cbrtf(float);
float copysignf(float, float);
float erff(float);
float erfcf(float);
float expm1f(float);
float exp2f(float);
float fdimf(float, float);
float fmaf(float, float, float);
float fmaxf(float, float);
float fminf(float, float);
float hypotf(float, float);
int ilogbf(float);
float lgammaf(float);
_Longlong llrintf(float);
_Longlong llroundf(float);
float log1pf(float);
float logbf(float);
long lrintf(float);
long lroundf(float);
float nanf(const char *);
float nearbyintf(float);
float nextafterf(float, float);
float nexttowardf(float, long double);
float remainderf(float, float);
float remquof(float, float, int *);
float rintf(float);
float roundf(float);
float scalbnf(float, int);
float scalblnf(float, long);
float tgammaf(float);
float truncf(float);



long double acosl(long double);
long double asinl(long double);
long double atanl(long double);
long double atan2l(long double, long double);
long double ceill(long double);
long double expl(long double);
long double fabsl(long double);
long double floorl(long double);
long double fmodl(long double, long double);
long double frexpl(long double, int *);
long double ldexpl(long double, int);
long double modfl(long double, long double *);
long double powl(long double, long double);
long double sqrtl(long double);
long double tanl(long double);
long double tanhl(long double);


long double acoshl(long double);
long double asinhl(long double);
long double atanhl(long double);
long double cbrtl(long double);
long double copysignl(long double, long double);
long double erfl(long double);
long double erfcl(long double);
long double exp2l(long double);
long double expm1l(long double);
long double fdiml(long double, long double);
long double fmal(long double, long double, long double);
long double fmaxl(long double, long double);
long double fminl(long double, long double);
long double hypotl(long double, long double);
int ilogbl(long double);
long double lgammal(long double);
_Longlong llrintl(long double);
_Longlong llroundl(long double);
long double log1pl(long double);
long double logbl(long double);
long lrintl(long double);
long lroundl(long double);
long double nanl(const char *);
long double nearbyintl(long double);
long double nextafterl(long double, long double);
long double nexttowardl(long double, long double);
long double remainderl(long double, long double);
long double remquol(long double, long double, int *);
long double rintl(long double);
long double roundl(long double);
long double scalbnl(long double, int);
long double scalblnl(long double, long);
long double tgammal(long double);
long double truncl(long double);




double cos(double);
double cosh(double);
double log(double);
double log10(double);
double sin(double);
double sinh(double);


void sincos (double, double *, double *);
double log2(double);







float cosf(float);
float coshf(float);
float logf(float);
float log10f(float);
float sinf(float);
float sinhf(float);


void sincosf (float, float *, float *);
float log2f(float);







long double cosl(long double);
long double coshl(long double);
long double logl(long double);
long double log10l(long double);
long double sinl(long double);
long double sinhl(long double);


void sincosl(long double, long double *, long double *);
long double log2l(long double);











typedef _Ptrdifft ptrdiff_t;






int _Stopfx(const char **, char **);
int _Stoflt(const char *, const char *, char **,
 long[], int);
int _Stoxflt(const char *, const char *, char **,
 long[], int);
int _WStopfx(const wchar_t **, wchar_t **);
int _WStoflt(const wchar_t *, const wchar_t *, wchar_t **,
 long[], int);
int _WStoxflt(const wchar_t *, const wchar_t *, wchar_t **,
 long[], int);
typedef union
 {
 unsigned short _Sh[sizeof(double)/sizeof(short)];
 double _Val;
 } __attribute__ ((__may_alias__)) _Dval;


double _Atan(double, int);






short _Dint(double *, short);
short _Dnorm(_Dval *);
short _Dscale(double *, long);
short _Dunscale(short *, double *);

double _Hypot(double, double, int *);
double _Poly(double, const double *, int);
extern _Dconst _Eps, _Rteps;
extern double _Xbig, _Zero;

double _Xp_getw(double *, int);
double *_Xp_setn(double *, int, long);
double *_Xp_setw(double *, int, double);
double *_Xp_addh(double *, int, double);
double *_Xp_mulh(double *, int, double);
double *_Xp_movx(double *, int, double *);
double *_Xp_addx(double *, int, double *, int);
double *_Xp_subx(double *, int, double *, int);
double *_Xp_ldexpx(double *, int, int);
double *_Xp_mulx(double *, int, double *, int, double *);
double *_Xp_invx(double *, int, double *);
double *_Xp_sqrtx(double *, int, double *);


typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;

float _FAtan(float, int);
short _FDint(float *, short);
short _FDnorm(_Fval *);
short _FDscale(float *, long);
short _FDunscale(short *, float *);
float _FHypot(float, float, int *);
float _FPoly(float, const float *, int);
extern _Dconst _FEps, _FRteps;
extern float _FXbig, _FZero;

float _FXp_getw(float *, int);
float *_FXp_setn(float *, int, long);
float *_FXp_setw(float *, int, float);
float *_FXp_addh(float *, int, float);
float *_FXp_mulh(float *, int, float);
float *_FXp_movx(float *, int, float *);
float *_FXp_addx(float *, int, float *, int);
float *_FXp_subx(float *, int, float *, int);
float *_FXp_ldexpx(float *, int, int);
float *_FXp_mulx(float *, int, float *, int, float *);
float *_FXp_invx(float *, int, float *);
float *_FXp_sqrtx(float *, int, float *);


typedef union
 {
 unsigned short _Sh[sizeof(long double)/sizeof(short)];
 long double _Val;
 } __attribute__ ((__may_alias__)) _Lval;

long double _LAtan(long double, int);
short _LDint(long double *, short);
short _LDnorm(_Lval *);
short _LDscale(long double *, long);
short _LDunscale(short *, long double *);
long double _LHypot(long double, long double, int *);
long double _LPoly(long double, const long double *, int);
extern _Dconst _LEps, _LRteps;
extern long double _LXbig, _LZero;

long double _LXp_getw(long double *, int);
long double *_LXp_setn(long double *, int, long);
long double *_LXp_setw(long double *, int, long double);
long double *_LXp_addh(long double *, int, long double);
long double *_LXp_mulh(long double *, int, long double);
long double *_LXp_movx(long double *, int, long double *);
long double *_LXp_addx(long double *, int,
 long double *, int);
long double *_LXp_subx(long double *, int,
 long double *, int);
long double *_LXp_ldexpx(long double *, int, int);
long double *_LXp_mulx(long double *, int, long double *,
 int, long double *);
long double *_LXp_invx(long double *, int, long double *);
long double *_LXp_sqrtx(long double *, int, long double *);
















typedef struct _Mbstatet
 {
 unsigned long _Wchar;
 unsigned short _Byte, _State;
 } _Mbstatet;
typedef struct fpos_t
 {
 _Longlong _Off;
 _Mbstatet _Wstate;
 } fpos_t;



struct _Dnk_filet
 {
 unsigned short _Mode;
 unsigned char _Idx;
 signed long _Handle;

 unsigned char *_Buf, *_Bend, *_Next;
 unsigned char *_Rend, *_Wend, *_Rback;

 _Wchart *_WRback, _WBack[2];
 unsigned char *_Rsave, *_WRend, *_WWend;

 _Mbstatet _Wstate;
 char *_Tmpnam;
 unsigned char _Back[8], _Cbuf;
 };



typedef struct _Dnk_filet _Filet;


typedef _Filet FILE;



extern FILE __attribute__((fardata)) _Stdin;
extern FILE __attribute__((fardata)) _Stdout;
extern FILE __attribute__((fardata)) _Stderr;

void clearerr(FILE *);
int fclose(FILE *);
int feof(FILE *);
int ferror(FILE *);
int fflush(FILE *);
int fgetc(FILE *);
int fgetpos(FILE *, fpos_t *);
char *fgets(char *, int, FILE *);
FILE *fopen(const char *, const char *);





int fprintf(FILE *, const char *, ...);
int fputc(int, FILE *);
int fputs(const char *, FILE *);
size_t fread(void *, size_t, size_t, FILE *);
FILE *freopen(const char *, const char *,
 FILE *);





int fscanf(FILE * , const char *, ...);
int fseek(FILE *, long, int);
int fsetpos(FILE *, const fpos_t *);
long ftell(FILE *);
size_t fwrite(const void *, size_t, size_t,
 FILE *);
char *gets(char *);
void perror(const char *);





int printf(const char *, ...);
int puts(const char *);
int remove(const char *);
int rename(const char *, const char *);
void rewind(FILE *);





int scanf(const char *, ...);
void setbuf(FILE * , char *);
int setvbuf(FILE * , char *, int, size_t);





int sprintf(char *, const char *, ...);





int sscanf(const char *, const char *, ...);
FILE *tmpfile(void);
char *tmpnam(char *);
int ungetc(int, FILE *);
int vfprintf(FILE *, const char *, _Va_list);
int vprintf(const char *, _Va_list);
int vsprintf(char *, const char *, _Va_list);


FILE *fdopen(signed long, const char *);
int fileno(FILE *);
int getw(FILE *);
int putw(int, FILE *);


long _Fgpos(FILE *, fpos_t *);
int _Flocale(FILE *, const char *, int);
void _Fsetlocale(FILE *, int);
int _Fspos(FILE *, const fpos_t *, long, int);






extern FILE __attribute__((fardata)) *_Files[20];







int snprintf(char *, size_t,
 const char *, ...);
int vsnprintf(char *, size_t,
 const char *, _Va_list);
int vfscanf(FILE *,
 const char *, _Va_list);
int vscanf(const char *, _Va_list);
int vsscanf(const char *,
 const char *, _Va_list);


int getc(FILE *);
int getchar(void);
int putc(int, FILE *);
int putchar(int);


errno_t tmpfile_s(FILE * *);
errno_t tmpnam_s(char *, rsize_t);

errno_t fopen_s(FILE * *,
 const char *, const char *);
errno_t freopen_s(FILE * *,
 const char *, const char *, FILE *);

int fprintf_s(FILE *, const char *, ...);
int fscanf_s(FILE *, const char *, ...);
int printf_s(const char *, ...);
int scanf_s(const char *, ...);
int snprintf_s(char *, rsize_t,
 const char *, ...);
int sscanf_s(const char *,
 const char *, ...);
int sprintf_s(char *, rsize_t,
 const char *, ...);
int vfprintf_s(FILE *,
 const char *,
 _Va_list);
int vfscanf_s(FILE *,
 const char *,
 _Va_list);
int vprintf_s(const char *,
 _Va_list);
int vscanf_s(const char *,
 _Va_list);
int vsnprintf_s(char *, rsize_t,
 const char *,
 _Va_list);
int vsprintf_s(char *, rsize_t,
 const char *,
 _Va_list);
int vsscanf_s(const char *,
 const char *,
 _Va_list);

char *gets_s(char *, rsize_t);






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








typedef struct
 {
 union
  {
  _Longlong li;
  _ULonglong uli;
  long double ld;
  } v;
 void *(*pfn)(void *, const char *, size_t);
 void *arg;
 char *s;
 int n0, nz0, n1, nz1, n2, nz2;
 int argno, prec, nchar, width;
 unsigned short flags;
 char qual;
 char secure;
 char sep;
 } _Pft;

typedef struct
 {
 int (*pfn)(void *, int, int);
 void *arg;
 va_list ap;
 const char *s;
 int nchar, nget, width;
 size_t prec;
 char noconv, qual, stored;
 char secure;
 char sep;
 } _Sft;

void _Closreg(void);
FILE *_Fofind(void);
void _Fofree(FILE *);
FILE *_Foprep(const _Sysch_t *, const _Sysch_t *,
 FILE *, signed long, int);
signed long _Fopen(const _Sysch_t *, unsigned int, int);
int _Frprep(FILE *);
int _Ftmpnam(char *, int);
int _Fwprep(FILE *);
void _Genld(_Pft *, char, char *, short, short);
int _Getfld(_Sft *);
int _Getfloat(_Sft *, void *);
int _Getint(_Sft *, void *);
int _Getstr(_Sft *, int);
void _Ldtob(_Pft *, char);
void _Litob(_Pft *, char);
int _Printf(void *(*)(void *, const char *, size_t),
 void *, const char *, va_list, int);
int _Putfld(_Pft *, va_list *, char, char *);
int _Putstr(_Pft *, const wchar_t *);
int _Puttxt(_Pft *, const char *);
int _Scanf(int (*)(void *, int, int),
 void *, const char *, va_list, int);
void _Vacopy(va_list *, va_list);



int _Getfloat(_Sft *px, void *pans)
 {
 char ac[8 + 48 + 16], *p, seen;
 int ch, nsig;
 int dlen, pten;
 static const char digits[] = "0123456789abcdefABCDEF";

 px->nget = 0 < px->width ? px->width : 0x7fffffff;
 p = ac, ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
 pten = 0;
 if (ch == '+' || ch == '-')
  *p++ = ch, ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
 dlen = 10;
 seen = 0;
 if (ch == '0')
  {
  ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
  if (ch != 'x' && ch != 'X')
   seen = 1;
  else
   {
   *p++ = '0';
   *p++ = 'x';
   ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
   dlen = 16 + 6;
   seen = 0;
   }
  }
 else if (ch == 'n' || ch == 'N')
  {
  dlen = 0;
  *p++ = 'n', ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
  if (ch != 'a' && ch != 'A')
   do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
  else
   {
   *p++ = 'a', ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
   if (ch != 'n' && ch != 'N')
    do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
   else if ((ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)))) != '(')
    {
    do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
    *p++ = 'n';
    seen = 1;
    }
   else
    {
    for (; (_Ctype[(int)(ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1))))] & (0x20|0x10|0x02|0x200)) || ch == '_'; )
     ;
    if (ch != ')')
     do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
    else
     {
     *p++ = 'n';
     seen = 1;
     }
    }
   }
  }
 else if (ch == 'i' || ch == 'I')
  {
  dlen = 0;
  *p++ = 'i', ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
  if (ch != 'n' && ch != 'N')
   do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
  else
   {
   *p++ = 'n', ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
   if (ch != 'f' && ch != 'F')
    do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
   else if ((ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)))) != 'i' && ch != 'I')
    {
    do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
    *p++ = 'f';
    seen = 1;
    }
   else
    {
    if ((ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)))) != 'n' && ch != 'N'
     || (ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)))) != 'i' && ch != 'I'
     || (ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)))) != 't' && ch != 'T'
     || (ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)))) != 'y' && ch != 'Y')
     do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
    else
     {
     *p++ = 'f';
     seen = 1;
     }
    }
   }
  }
 if (0 < dlen)
  {
  for (; ch == '0'; seen = 1)
   ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
  if (seen)
   *p++ = '0';
  for (nsig = 0; ch != (-1) && memchr(&digits[0], ch, dlen);
   ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1))), seen = 1)
   if (nsig < 48)
    *p++ = ch, ++nsig;
   else
    ++pten;
  if (ch == localeconv()->decimal_point[0])
   *p++ = ch, ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
  if (nsig == 0)
   {
   for (; ch == '0'; ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1))), seen = 1)
    --pten;
   if (pten < 0)
    *p++ = '0', ++pten;
   }
  for (; ch != (-1) && memchr(&digits[0], ch, dlen);
   ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1))), seen = 1)
   if (nsig < 48)
    *p++ = ch, ++nsig;
  if (seen && (dlen == 10 && (ch == 'e' || ch == 'E')
   || dlen != 10 && (ch == 'p' || ch == 'P')))
   {
   *p++ = ch, ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
   if (ch == '+' || ch == '-')
    *p++ = ch, ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)));
   for (seen = 0; ch == '0'; ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1))), seen = 1)
    ;
   if (seen)
    *p++ = '0';
   for (nsig = 0; (_Ctype[(int)(ch)] & 0x20); ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1))), seen = 1)
    if (nsig < 8)
     *p++ = ch, ++nsig;
   }
  do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
  }
 if (!seen)
  return (p == ac && ch == (-1) ? (-1) : 0);
 *p = '\0';
 if (!px->noconv)
  {
  long double ldval;

  if (dlen <= 10)
   ldval = _Stold(ac, 0, pten);
  else
   {
   ldval = _Stold(ac, 0, 0);
   _LDscale(&ldval, pten * 4);
   }

  px->stored = 1;
  if (px->qual == 'l')
   if (pans != 0) *(double *)pans = (double)(ldval); else {double *p = __builtin_va_arg(px->ap,double *); if (p == 0) return ((-1) - _Fail_s("scanf_s: bad floating-point argument", sizeof ("scanf_s: bad floating-point argument"))); *p = (double)(ldval); }
  else if (px->qual != 'L')
   if (pans != 0) *(float *)pans = (float)(ldval); else {float *p = __builtin_va_arg(px->ap,float *); if (p == 0) return ((-1) - _Fail_s("scanf_s: bad floating-point argument", sizeof ("scanf_s: bad floating-point argument"))); *p = (float)(ldval); }
  else
   if (pans != 0) *(long double *)pans = (long double)(ldval); else {long double *p = __builtin_va_arg(px->ap,long double *); if (p == 0) return ((-1) - _Fail_s("scanf_s: bad floating-point argument", sizeof ("scanf_s: bad floating-point argument"))); *p = (long double)(ldval); }
  }
 return (1);
 }

