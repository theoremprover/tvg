






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




typedef _Ptrdifft ptrdiff_t;
typedef _Sizet size_t;





typedef _Wchart wchar_t;
typedef size_t rsize_t;












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








static void swap(char *qb, char *qe, size_t size)
 {
 if (size < 64)
  {
  unsigned int i;

  for (i = 0; i < size; ++i)
   {
   char tmp = qb[i];

   qb[i] = qe[i];
   qe[i] = tmp;
   }
  }
 else
  {
  char buf[256];
  size_t ms;

  for (ms = size; 0 < ms; )
   {
   size_t m = ms < sizeof (buf) ? ms : sizeof (buf);
   memcpy(buf, qb, m);
   memcpy(qb, qe, m);
   memcpy(qe, buf, m);
   ms -= m, qb += m, qe += m;
   }
  }
 }

static void rot3(char *qf, char *qm, char *ql, size_t size)
 {
 if (size < 64)
  {
  unsigned int i;

  for (i = 0; i < size; ++i)
   {
   char tmp = qf[i];

   qf[i] = ql[i];
   ql[i] = qm[i];
   qm[i] = tmp;
   }
  }
 else
  {
  char buf[256];
  size_t ms;

  for (ms = size; 0 < ms; )
   {
   size_t m = ms < sizeof (buf) ? ms : sizeof (buf);

   memcpy(buf, qf, m);
   memcpy(qf, ql, m);
   memcpy(ql, qm, m);
   memcpy(qm, buf, m);
   ms -= m, qf += m, qm += m, ql += m;
   }
  }
 }

static void rotate(char *qb, char *qe, size_t size)
 {
 char buf[256];
 size_t ms, mtot = qe - qb + size;

 for (ms = size; 0 < ms; )
  {
  size_t m = ms < sizeof (buf) ? ms : sizeof (buf);

  memcpy(buf, qe + (size - m), m);
  memmove(qb + m, qb, mtot - m);
  memcpy(qb, buf, m);
  ms -= m;
  }
 }

static void med3(char *qf, char *qm, char *ql, size_t size, _Cmpfun *cmp)
 {
 if ((*cmp)(qm, qf) < 0)
  swap(qf, qm, size);
 if ((*cmp)(ql, qm) < 0)
  swap(qm, ql, size);
 if ((*cmp)(qm, qf) < 0)
  swap(qf, qm, size);
 }

static void median(char *qf, char *qm, char *ql, size_t size, _Cmpfun *cmp)
 {
 size_t nelem = (ql - qf) / size;

 if (nelem <= 40)
  med3(qf, qm, ql, size, cmp);
 else
  {
  ptrdiff_t step = (ptrdiff_t)((nelem / 8 + 1) * size);

  med3(qf, qf + step, qf + 2 * step, size, cmp);
  med3(qm - step, qm, qm + step, size, cmp);
  med3(ql - 2 * step, ql - step, ql, size, cmp);
  med3(qf + step, qm, ql - step, size, cmp);
  }
 }

static void adjust_heap(char *qb, size_t h, size_t n,
 size_t size, _Cmpfun *cmp)
 {
 size_t h0 = h;
 size_t k = 2 * h + 2;
 char *qh = qb + h * size;
 char *qk = qb + k * size;

 for (; k <= n; k = 2 * k + 2, qk = qb + k * size)
  {
  if (k == n || (*cmp)(qk, qk - size) < 0)
   --k, qk -= size;
  swap(qh, qk, size);
  h = k, qh = qk;
  }

 for (; h0 < h; )
  {
  size_t i = (h - 1) / 2;
  char *qi = qb + i * size;
  if ((*cmp)(qh, qi) <= 0)
   break;
  swap(qi, qh, size);
  h = i, qh = qi;
  }
 }

static void intro_sort(char *qb, size_t n, size_t ideal,
 size_t size, _Cmpfun *cmp)
 {
 for (; 0 < ideal && 32 < n; )
  {
  size_t m = n / 2;
  char *qm = qb + m * size;
  char *qf = qb, *ql = qb + n * size;
  char *pf = qm, *pl = qm + size;
  char *gf, *gl;

  median(qf, qm, ql - size, size, cmp);
  while (qf < pf && (cmp)(pf - size, pf) == 0)
   pf -= size;
  while (pl < ql && (cmp)(pl, pf) == 0)
   pl += size;
  gf = pl;
  gl = pf;

  for (;;)
   {
   int c;

   for (; gf < ql; gf += size)
    if ((c = (cmp)(pf, gf)) < 0)
     ;
    else if (0 < c)
     break;
    else
     swap(pl, gf, size), pl += size;
   for (; qf < gl; gl -= size)
    if ((c = (cmp)(gl - size, pf)) < 0)
     ;
    else if (0 < c)
     break;
    else
     swap(pf -= size, gl - size, size);
   if (gl == qf && gf == ql)
    break;

   if (gl == qf)
    {
    if (pl == gf)
     swap(gf, pf, size);
    else
     rot3(gf, pf, pl, size);
    gf += size, pf += size, pl += size;
    }
   else if (gf == ql)
    {
    gl -= size, pl -= size, pf -= size;
    if (gl == pf)
     swap(pf, pl, size);
    else
     rot3(gl, pl, pf, size);
    }
   else
    swap(gf, gl -= size, size), gf += size;
   }

  ideal /= 2;
  ideal += ideal / 2;
  m = (pf - qb) / size;
  n = (ql - pl) / size;
  if (m <= n)
   intro_sort(qb, m, ideal, size, cmp), qb = pl;
  else
   intro_sort(pl, n, ideal, size, cmp), n = m;
  }

 if (32 < n)
  {
  size_t h;
  char *qe;

  for (h = n / 2; 0 < h; )
   adjust_heap(qb, --h, n, size, cmp);
  for (qe = qb + n * size; 1 < n; )
   {
   swap(qb, qe -= size, size);
   adjust_heap(qb, 0, --n, size, cmp);
   }
  }
 else if (1 < n)
  {
  char *qm;

  for (qm = qb; 0 < --n; )
   {
   qm += size;
   if ((*cmp)(qm, qb) < 0)
    rotate(qb, qm, size);
   else
    {
    char *qx = qm, *qx0 = qm;
    for (; (*cmp)(qm, qx0 -= size) < 0; qx = qx0)
     ;
    if (qx != qm)
     rotate(qx, qm, size);
    }
   }
  }
 }

void (qsort)(void *base, size_t n, size_t size, _Cmpfun *cmp)
 {
 if (base != 0 && cmp != 0)
  intro_sort((char *)base, n, n, size, cmp);
 }

