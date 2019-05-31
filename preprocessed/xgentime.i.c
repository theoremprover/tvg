# 1 "/arm-libs/library-src/dinkum/source/./xgentime.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./xgentime.c" 2

# 1 "/toolchain/arm/include/string.h" 1 3





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
# 7 "/toolchain/arm/include/string.h" 2 3
# 24 "/toolchain/arm/include/string.h" 3
typedef _Sizet size_t;




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
# 94 "/toolchain/arm/include/string.h" 3
char *strchr(const char *, int);
char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
char *strstr(const char *, const char *);
# 118 "/toolchain/arm/include/string.h" 3
void *memchr(const void *, int, size_t);
# 129 "/toolchain/arm/include/string.h" 3
typedef int errno_t;




typedef size_t rsize_t;


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
# 3 "/arm-libs/library-src/dinkum/source/./xgentime.c" 2
# 1 "/toolchain/arm/include/stdlib.h" 1 3
# 46 "/toolchain/arm/include/stdlib.h" 3
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




extern float _Stofx(const char *, char **,
 long, int *);
extern double _Stodx(const char *, char **,
 long, int *);
extern long double _Stoldx(const char *, char **,
 long, int *);
extern long _Stolx(const char *, char **,
 int, int *);
extern unsigned long _Stoulx(const char *, char **,
 int, int *);
extern long long _Stollx(const char *, char **,
 int, int *);
extern unsigned long long _Stoullx(const char *, char **,
 int, int *);



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
# 311 "/toolchain/arm/include/stdlib.h" 3
typedef int _Cmpfun(const void *, const void *);

      int atexit(void (*)(void)) ;
void *bsearch(const void *, const void *,
 size_t, size_t, _Cmpfun *);
void qsort(void *, size_t, size_t, _Cmpfun *);

double atof(const char *);
int atoi(const char *);
long atol(const char *);
double strtod(const char *, char **);
unsigned long strtoul(const char *,
 char **, int);
# 332 "/toolchain/arm/include/stdlib.h" 3
_Longlong atoll(const char *);
float strtof(const char *,
 char **);
long double strtold(const char *,
 char **);
_Longlong strtoll(const char *,
 char **, int);
_ULonglong strtoull(const char *,
 char **, int);
# 360 "/toolchain/arm/include/stdlib.h" 3
int (_Fail_s)(const char *, size_t);
# 372 "/toolchain/arm/include/stdlib.h" 3
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
# 417 "/toolchain/arm/include/stdlib.h" 3
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
# 4 "/arm-libs/library-src/dinkum/source/./xgentime.c" 2
# 1 "/toolchain/arm/include/xtime.h" 1 3



# 1 "/toolchain/arm/include/xtinfo.h" 1 3



# 1 "/toolchain/arm/include/time.h" 1 3
# 34 "/toolchain/arm/include/time.h" 3
typedef long clock_t;
# 45 "/toolchain/arm/include/time.h" 3
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
# 96 "/toolchain/arm/include/time.h" 3
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
# 124 "/toolchain/arm/include/time.h" 3
errno_t asctime_s(char *, rsize_t, const struct tm *);
errno_t ctime_s(char *, rsize_t, const time_t *);
struct tm *gmtime_s(const time_t *,
 struct tm *);
struct tm *localtime_s(const time_t *,
 struct tm *);
# 5 "/toolchain/arm/include/xtinfo.h" 2 3






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
# 5 "/toolchain/arm/include/xtime.h" 2 3





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
# 5 "/arm-libs/library-src/dinkum/source/./xgentime.c" 2
# 14 "/arm-libs/library-src/dinkum/source/./xgentime.c"
static const char *getval(char *s, int val, int n,
 int *pn, char qual, const _Tinfo *tin)
 {
 if (val < 0)
  val = 0;
 if (qual == 'O')
  {
  const char *p = _Gettime(tin->_Alt_digits, val, pn);

  if (0 < *pn)
   return (p);
  }
 *pn = n;
 for (s += n, *s = '\0'; 0 <= --n; val /= 10)
  *--s = (char)(val % 10 + '0');
 return (s);
 }

static int isleapyr(int year)
 {
 return (year % 4 == 0 && (year % 100 != 0
  || (0 < year && year / 400 == 100)
  || (year < 0 && -year / 400 == 300)));
 }

static int wkyr(int wstart, int wday, int yday)
 {
 wday = (wday + 7 - wstart) % 7;
 return ((yday + 7 - wday) / 7);
 }

static int ISOwkyr(int year, int wday, int yday)
 {
 int wkno = wkyr(1, wday, yday);
 int isleap = isleapyr(year);
 int yunleap = yday - isleap;
 int jan1 = (371 - yday + wday) % 7;
 int dec32 = (jan1 + isleap + 365) % 7;

 if ((364 <= yunleap && dec32 == 2)
  || (363 <= yunleap && dec32 == 3)
  || (362 <= yunleap && dec32 == 4))
  wkno = -1;
 else if (jan1 == 2 || jan1 == 3 || jan1 == 4)
  ++wkno;
 return (wkno);
 }

static int ISOweek(int year, int wday, int yday)
 {
 int wkno = ISOwkyr(year, wday, yday);

 if (wkno == 0)
  return (ISOwkyr(year - 1, wday + 7 - yday,
   isleapyr(year - 1) ? 366 : 365));
 else if (0 < wkno)
  return (wkno);
 else
  return (1);
 }

static int ISOyear(int year, int wday, int yday)
 {
 int wkno = ISOwkyr(year, wday, yday);

 if (wkno == 0)
  return (year - 1);
 else if (0 < wkno)
  return (year);
 else
  return (year + 1);
 }

static int cmp_era_date(const struct tm *t, const char *s)
 {
 char *eptr;
 long val;

 val = strtol(s, &eptr, 10);
 if (s == eptr || *eptr != '/')
  return (2);
 else if (val != t->tm_year + 1900)
  return (t->tm_year + 1900 < val ? -1 : +1);

 val = strtol(s = eptr + 1, &eptr, 10);
 if (s == eptr || *eptr != '/')
  return (2);
 else if (val != t->tm_mon + 1)
  return (t->tm_mon + 1 < val ? -1 : +1);

 val = strtol(eptr + 1, &eptr, 10);
 if (s == eptr)
  return (2);
 else if (val != t->tm_mday)
  return (t->tm_mday < val ? -1 : +1);
 else
  return (0);
 }

static const char *getera(const struct tm *t, const _Tinfo *tin)
 {
 const char *s;
 int i, len;

 for (i = 0; *(s = _Gettime(tin->_Era, i, &len)) != '\0'; ++i)
  {
  const char *s1 = _Gettime(s + 1, 1, &len);
  int ans = cmp_era_date(t, s1);

  if ((*s == '-' && (ans == -1 || ans == 0))
   || (*s == '+' && (ans == 0 || ans == 1)))
   {
   s1 = _Gettime(s + 1, 2, &len);
   if (s1[0] == *s && s1[1] == '*')
    return (s);
   ans = cmp_era_date(t, s1);
   if ((*s == '-' && (ans == 0 || ans == 1))
    || (*s == '+' && (ans == -1 || ans == 0)))
    return (s);
   }
  }
 return ("+:");
 }

const char *_Gentime(const struct tm *t, const _Tinfo *tin,
 char qual, char code, int *pn, char *ac)
 {
 const char *p;

 switch (code)
  {
 case 'a':
  p = _Gettime(tin->_Abday, t->tm_wday << 1, pn);
  break;

 case 'A':
  p = _Gettime(tin->_Day, (t->tm_wday << 1) + 1, pn);
  break;

 case 'b':
  p = _Gettime(tin->_Abmon, t->tm_mon << 1, pn);
  break;

 case 'B':
  p = _Gettime(tin->_Mon, (t->tm_mon << 1) + 1, pn);
  break;

 case 'c':
  p = _Gettime(qual == 'E' ? tin->_Era_D_t_fmt : tin->_D_t_fmt, 0, pn);
  *pn = -*pn;
  break;

 case 'C':
  if (qual != 'E' || *(p = _Gettime(getera(t, tin) + 1, 3, pn)) == '\0')
   p = getval(ac, 19 + t->tm_year / 100, 2, pn, qual, tin);
  break;

 case 'd':
  p = getval(ac, t->tm_mday, 2, pn, qual, tin);
  break;

 case 'D':
  p = "%m/%d/%y";
  *pn = -8;
  break;

 case 'e':
  p = getval(ac, t->tm_mday, 2, pn, qual, tin);
  if (ac[0] == '0')
   ac[0] = ' ';
  break;

 case 'F':
  p = "%Y-%m-%d";
  *pn = -8;
  break;

 case 'g':
   {
  int year = ISOyear(t->tm_year, t->tm_wday, t->tm_yday) % 100;

  if (year < 0)
   year += 100;
  p = getval(ac, year, 2, pn, qual, tin);
   }
  break;

 case 'G':
  p = getval(ac, ISOyear(t->tm_year, t->tm_wday, t->tm_yday) + 1900, 4,
   pn, qual, tin);
  break;

 case 'h':
  p = _Gettime(tin->_Abmon, t->tm_mon << 1, pn);
  break;

 case 'H':
  p = getval(ac, t->tm_hour, 2, pn, qual, tin);
  break;

 case 'I':
  p = getval(ac, (t->tm_hour + 11) % 12 + 1, 2, pn, qual, tin);
  break;

 case 'j':
  p = getval(ac, t->tm_yday + 1, 3, pn, qual, tin);
  break;

 case 'm':
  p = getval(ac, t->tm_mon + 1, 2, pn, qual, tin);
  break;

 case 'M':
  p = getval(ac, t->tm_min, 2, pn, qual, tin);
  break;

 case 'n':
  p = "\n";
  *pn = 1;
  break;

 case 'p':
  p = _Gettime(tin->_Am_pm, 12 <= t->tm_hour, pn);
  break;

 case 'r':
  p = _Gettime(qual == 'E' ? tin->_Era_T_fmt_ampm : tin->_T_fmt_ampm,
   3, pn);
  *pn = -*pn;
  break;

 case 'R':
  p = "%H:%M";
  *pn = -5;
  break;

 case 'S':
  p = getval(ac, t->tm_sec, 2, pn, qual, tin);
  break;

 case 't':
  p = "\t";
  *pn = 1;
  break;

 case 'T':
  p = "%H:%M:%S";
  *pn = -8;
  break;

 case 'u':
  p = getval(ac, t->tm_wday == 0 ? 7 : t->tm_wday, 1, pn, qual, tin);
  break;

 case 'U':
  p = getval(ac, wkyr(0, t->tm_wday, t->tm_yday), 2,
   pn, qual, tin);
  break;

 case 'V':
  p = getval(ac, ISOweek(t->tm_year, t->tm_wday, t->tm_yday), 2,
   pn, qual, tin);
  break;

 case 'w':
  p = getval(ac, t->tm_wday, 1, pn, qual, tin);
  break;

 case 'W':
  p = getval(ac, wkyr(1, t->tm_wday, t->tm_yday), 2,
   pn, qual, tin);
  break;

 case 'x':
  p = _Gettime(qual == 'E' ? tin->_Era_D_fmt : tin->_D_fmt, 1, pn);
  *pn = -*pn;
  break;

 case 'X':
  p = _Gettime(qual == 'E' ? tin->_Era_T_fmt : tin->_T_fmt, 2, pn);
  *pn = -*pn;
  break;

 case 'y':
   {
  int year = t->tm_year % 100;
  int digits = 2;

  if (year < 0)
   year += 100;
  if (qual == 'E' && (p = getera(t, tin))[2] != '\0')
   {
   char *eptr;
   long val = 1900 + t->tm_year
    - strtol(_Gettime(p + 1, 1, pn), 0, 10);

   if (p[0] == '-')
    val = -val;
   val += strtol(p + 2, &eptr, 10);
   if (p + 2 != eptr && eptr[0] == p[1])
    {
    year = (int)val;
    for (digits = 1; 0 < (val /= 10); )
     ++digits;
    }
   }
  p = getval(ac, year, digits, pn, qual, tin);
   }
  break;

 case 'Y':
  if (qual != 'E'
   || (*(p = _Gettime(getera(t, tin) + 1, 4, pn)) == ';'
    || *pn == 0))
   p = getval(ac, t->tm_year + 1900, 4, pn, qual, tin);
  else
   {
   const char *p1 = strchr(p, ';');

   if (p1 != 0)
    *pn = (int)(p1 - p);
   *pn = -*pn;
   }
  break;

 case 'z':
  p = _Gettime(tin->_Tzone[0] == '\0' ? _Getzone() : tin->_Tzone,
   2, pn);
  if (0 < *pn && 0 < t->tm_isdst)
   {
   int val = strtol(p, 0, 10) + 100;

   if (0 <= val)
    ac[0] = '+';
   else
    ac[0] = '-', val = -val;
   p = getval(ac + 1, val, 4, pn, qual, tin) - 1;
   ++*pn;
   }
  break;

 case 'Z':
  p = _Gettime(tin->_Tzone[0] == '\0' ? _Getzone() : tin->_Tzone,
   0 < t->tm_isdst, pn);
  break;

 case '%':
  p = "%";
  *pn = 1;
  break;

 default:
  ac[0] = '%';
  ac[1] = code;
  ac[2] = '\0';
  p = ac;
  *pn = 2;
  }
 return (p);
 }
