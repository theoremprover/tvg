






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












typedef struct
 {
 const unsigned short *_Tab[16];
 } _Statab;












typedef long clock_t;
typedef long time_t;




struct tm
 {
 int tm_sec;
 int tm_min;
 int tm_hour;
 int tm_mday;
 int tm_mon;
 int tm_year;
 int tm_wday;
 int tm_yday;
 int tm_isdst;
 };

time_t time(time_t *);



char *asctime(const struct tm *);
clock_t clock(void);
char *ctime(const time_t *);
double difftime(time_t, time_t);
struct tm *gmtime(const time_t *);
struct tm *localtime(const time_t *);
time_t mktime(struct tm *);
size_t strftime(char *, size_t,
 const char *, const struct tm *);




errno_t asctime_s(char *, rsize_t, const struct tm *);
errno_t ctime_s(char *, rsize_t, const time_t *);
struct tm *gmtime_s(const time_t *,
 struct tm *);
struct tm *localtime_s(const time_t *,
 struct tm *);









typedef struct
 {
 const char *_Am_pm;
 const char *_Days;
  const char *_Abday;
  const char *_Day;
 const char *_Months;
  const char *_Abmon;
  const char *_Mon;
 const char *_Formats;
  const char *_D_t_fmt;
  const char *_D_fmt;
  const char *_T_fmt;
  const char *_T_fmt_ampm;
 const char *_Era_Formats;
  const char *_Era_D_t_fmt;
  const char *_Era_D_fmt;
  const char *_Era_T_fmt;
  const char *_Era_T_fmt_ampm;
 const char *_Era;
 const char *_Alt_digits;
 const char *_Isdst;
 const char *_Tzone;
 } _Tinfo;



size_t _CStrftime(char *, size_t, const char *,
 const struct tm *, const _Tinfo *);
_Tinfo *_Getptimes(void);















typedef void *_Rmtx;
void _Mtxinit(_Rmtx *);
void _Mtxdst(_Rmtx *);
void _Mtxlock(_Rmtx *);
void _Mtxunlock(_Rmtx *);
typedef char _Once_t;



typedef void (*_Tlsdtor_t)(void *);
int _Atthreadexit(void (*)(void));
void _Destroytls(void);












typedef _Ptrdifft ptrdiff_t;













typedef _Sizet wctrans_t;


typedef _Sizet wctype_t;
typedef _Wintt wint_t;








int _Iswctype(wint_t, wctype_t);
wint_t _Towctrans(wint_t, wctrans_t);



int iswalnum(wint_t);
int iswalpha(wint_t);
int iswcntrl(wint_t);
int iswctype(wint_t, wctype_t);
int iswdigit(wint_t);
int iswgraph(wint_t);
int iswlower(wint_t);
int iswprint(wint_t);
int iswpunct(wint_t);
int iswspace(wint_t);
int iswupper(wint_t);
int iswxdigit(wint_t);

wint_t towlower(wint_t);
wint_t towupper(wint_t);


int iswblank(wint_t);





wctrans_t wctrans(const char *);
wctype_t wctype(const char *);


wint_t (towctrans)(wint_t, wctrans_t);





typedef struct
 {
 const char *_Name;
 size_t _Off;
 } _Wctab;
typedef const _Wctab *_PWctab;



const _Wctab *_Getpwctrtab(void);
const _Wctab *_Getpwctytab(void);







extern int (*_Tls_setup__Costate)(void); extern _Statab _Costate;
extern int (*_Tls_setup__WCostate)(void); extern _Statab _WCostate;
extern int (*_Tls_setup__Mbstate)(void); extern _Statab _Mbstate;
extern int (*_Tls_setup__Wcstate)(void); extern _Statab _Wcstate;
extern int (*_Tls_setup__Ctype)(void); extern _Ctype_t _Ctype;
extern int (*_Tls_setup__Wctrans)(void); extern _PWctab _Wctrans;
extern int (*_Tls_setup__Wctype)(void); extern _PWctab _Wctype;
extern int (*_Tls_setup__Tolotab)(void); extern _Ctype_t _Tolotab;
extern int (*_Tls_setup__Touptab)(void); extern _Ctype_t _Touptab;
extern int (*_Tls_setup__Mbcurmax)(void); extern char _Mbcurmax;
extern int (*_Tls_setup__Locale)(void); extern struct lconv _Locale;
extern int (*_Tls_setup__Times)(void); extern _Tinfo _Times;











typedef struct
 {
 unsigned char wday, hour, day, mon, year;
 } Dstrule;



int _Daysto(int, int);
const char * _Gentime(const struct tm *, const _Tinfo *,
 char, char, int *, char *);
Dstrule * _Getdst(const char *);
const char * _Gettime(const char *, int, int *);
int _Isdst(const struct tm *);
const char * _Getzone(void);
struct tm * _Ttotm(struct tm *, time_t, int);
time_t _Tzoff(void);





typedef const char *old_t;

static old_t oldzone = 0; static int (*_Tls_setup_oldzone)(void) = 0;
static long tzoff = 0; static int (*_Tls_setup_tzoff)(void) = 0;

time_t _Tzoff(void)
 {
 old_t *poldzone = ((_Tls_setup_oldzone && _Tls_setup_oldzone()), (&(oldzone)));
 _Tinfo *ptimes = ((_Tls_setup__Times && _Tls_setup__Times()), (&(_Times)));
 long *ptzoff = ((_Tls_setup_tzoff && _Tls_setup_tzoff()), (&(tzoff)));

 if (*poldzone != ptimes->_Tzone)
  {
  const char *p, *pe;
  int n;

  if (ptimes->_Tzone[0] == '\0')
   ptimes->_Tzone = _Getzone();
  p = _Gettime(ptimes->_Tzone, 2, &n);
  *ptzoff = strtol(p, (char **)&pe, 10);
  if (pe - p != n
   || *ptzoff <= -(100 * 13) || (100 * 13) <= *ptzoff)
   *ptzoff = 0;
  *ptzoff -= (*ptzoff / 100) * 40;
  *ptzoff = -*ptzoff;
  *poldzone = ptimes->_Tzone;
  }
 return (-*ptzoff * 60);
 }

struct tm *(localtime)(const time_t *tod)
 {
 return (_Ttotm(0, *tod + _Tzoff(), -1));
 }

