







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














typedef _Ptrdifft ptrdiff_t;
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



















typedef struct _Cell
 {
 size_t _Size;
 struct _Cell *_Next;
 } _Cell;

typedef struct
 {
 _Cell **_Plast;
 _Cell *_Head;
 } _Altab;



void * _Getmem(size_t);
extern _Altab _Aldata;



static void *(myrealloc)(void *ptr, size_t size_arg,
 _Cell *q, size_t size)
 {
 if (_Aldata._Head != 0)
  {
  _Cell **qb = &_Aldata._Head;
  char *const qn = (char *)q + q->_Size;

  while ((*qb)->_Next != 0
   && ((*qb)->_Next) < (q))
   qb = &(*qb)->_Next;
  if ((qn) == ((char *)*qb))
   {
   _Aldata._Plast = 0;
   (void)0

                           ;
   q->_Size += (*qb)->_Size;
   *qb = (*qb)->_Next;
   if (size <= q->_Size)
    return (realloc(ptr, size_arg));
   }
  else if ((qn)
   == ((char *)(*qb)->_Next))
   {
   _Aldata._Plast = 0;
   (void)0

                                  ;
   q->_Size += (*qb)->_Next->_Size;
   (*qb)->_Next = (*qb)->_Next->_Next;
   if (size <= q->_Size)
    return (realloc(ptr, size_arg));
   }
  if (*qb != 0 && ((char *)*qb + (*qb)->_Size)
   == ((char *)q)
   && size <= (*qb)->_Size + q->_Size)
   {
   _Cell *const next_q = (*qb)->_Next;
    _Aldata._Plast = 0;
   (void)0
                       ;
   (*qb)->_Size += q->_Size;
   ptr = memmove((char *)*qb + (__builtin_offsetof(_Cell,_Next) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)), ptr,
    q->_Size - (__builtin_offsetof(_Cell,_Next) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)));
   *qb = next_q;
   return (realloc(ptr, size_arg));
   }
  }
 return (0);
 }

void *(realloc)(void *ptr, size_t size_arg)
 {
 _Cell *q;
 size_t size;

 (void)0;
 if (ptr == 0)
  return (malloc(size_arg));
 if (size_arg == 0)
  {
  free(ptr);
  return (0);
  }
 size = (size_arg + ((__builtin_offsetof(_Cell,_Next) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)) + ((1 << 3U) - 1))) & ~((1 << 3U) - 1);
 if (size <= size_arg)
  return (0);
 if (size < (sizeof (_Cell) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)))
  size = (sizeof (_Cell) + ((1 << 3U) - 1) & ~((1 << 3U) - 1));
 q = (_Cell *)((char *)ptr - (__builtin_offsetof(_Cell,_Next) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)));
 if (q->_Size < (sizeof (_Cell) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)) || (q->_Size & ((1 << 3U) - 1)) != 0)
  return (0);
 if (size <= q->_Size - (sizeof (_Cell) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)))
  {
  _Cell *const new_q = (_Cell *)((char *)q + size);

  new_q->_Size = q->_Size - size;
  q->_Size = size;
  free((char *)new_q + (__builtin_offsetof(_Cell,_Next) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)));
  return (ptr);
  }
 else if (size <= q->_Size)
  return (ptr);
 else
  {
  void *new_p;

  (void)0;
  new_p = myrealloc(ptr, size_arg, q, size);
  (void)0;
  if (new_p != 0)
   return (new_p);
  else if ((new_p = malloc(size_arg)) == 0)
   return (0);
  else
   {
   memcpy(new_p, ptr, q->_Size - (__builtin_offsetof(_Cell,_Next) + ((1 << 3U) - 1) & ~((1 << 3U) - 1)));
   free(ptr);
   return (new_p);
   }
  }
 }

