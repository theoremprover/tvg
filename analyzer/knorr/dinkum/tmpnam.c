






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














typedef _Ptrdifft ptrdiff_t;









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



char *(tmpnam)(char *s)
 {
 char *p;
 int i;
 unsigned short t;
 static char buf[16] = {0};
 static unsigned short seed = 0;
 (void)0;
 if (s == 0)
  s = buf;
 ++seed;
 strcpy(s, "ctm");
 i = 5;
 p = s + strlen(s) + i;
 strcpy(p, ".tmp");


 for (t = seed; 0 <= --i; t >>= 3)
  *--p = (char)('0' + (t & 07));
 (void)0;
 remove(s);
 return (s);
 }

