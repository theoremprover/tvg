






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





static int strspn_s(const char *s1, const char *s2, size_t *psize)
 {
 const char *sc1, *sc2;
 size_t n = *psize;

 for (sc1 = s1; 0 < n && *sc1 != '\0'; --n, ++sc1)
  for (sc2 = s2; ; ++sc2)
   if (*sc2 == '\0')
    {
    *psize = sc1 - s1;
    return (0);
    }
   else if (*sc1 == *sc2)
    break;

 if (n == 0)
  return (1);
 else
  {
  *psize = sc1 - s1;
  return (0);
  }
 }

static size_t strcspn_s(const char *s1, const char *s2, size_t *psize)
 {
 const char *sc1, *sc2;
 size_t n = *psize;

 for (sc1 = s1; 0 < n && *sc1 != '\0'; --n, ++sc1)
  for (sc2 = s2; *sc2 != '\0'; ++sc2)
   if (*sc1 == *sc2)
    {
    *psize = sc1 - s1;
    return (0);
    }

 if (n == 0)
  return (1);
 else
  {
  *psize = sc1 - s1;
  return (0);
  }
 }


char *(strtok_s)(char * s1, rsize_t *psize1,
 const char * s2, char ** pbuf1)
 {
 char *sbegin, *send;
 size_t n = *psize1;

 if (psize1 == 0 || ((_Sizet)(-1) >> 1) < *psize1 || s2 == 0 || pbuf1 == 0)
  return (_Fail_s("strtok_s: bad argument", sizeof ("strtok_s: bad argument")), (char *)0);

 sbegin = s1 != 0 ? s1 : *pbuf1;
 if (strspn_s(sbegin, s2, &n))
  return (_Fail_s("strtok_s: missing token", sizeof ("strtok_s: missing token")), (char *)0);
 sbegin += n;
 *psize1 -= n;
 if (*sbegin == '\0')
  return ((char *)0);

 n = *psize1;
 if (strcspn_s(sbegin, s2, &n))
  return (_Fail_s("strtok_s: missing delimiter", sizeof ("strtok_s: missing delimiter")), (char *)0);
 send = sbegin + n;
 *psize1 -= n;
 if (*send != '\0')
  {
  *send++ = '\0';
  if (0 < *psize1)
   --*psize1;
  }
 *pbuf1 = send;
 return (sbegin);
 }

