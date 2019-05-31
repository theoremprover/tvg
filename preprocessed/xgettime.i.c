# 1 "/arm-libs/library-src/dinkum/source/./xgettime.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./xgettime.c" 2

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
# 3 "/arm-libs/library-src/dinkum/source/./xgettime.c" 2
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
# 4 "/arm-libs/library-src/dinkum/source/./xgettime.c" 2


const char *_Gettime(const char *s, int n, int *len)
 {
 const char delim = (char)(*s ? *s++ : '\0');
 const char *s1;

 for (; ; --n, s = s1 + 1)
  {
  if ((s1 = strchr(s, delim)) == 0)
   s1 = s + strlen(s);
  if (n <= 0)
   {
   *len = (int)(s1 - s);
   return (s);
   }
  else if (*s1 == '\0')
   {
   *len = 0;
   return (s1);
   }
  }
 }
