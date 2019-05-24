# 1 "/arm-libs/library-src/dinkum/source/./xgetstr.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./xgetstr.c" 2

# 1 "/toolchain/arm/include/ctype.h" 1 3




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
# 6 "/toolchain/arm/include/ctype.h" 2 3
# 25 "/toolchain/arm/include/ctype.h" 3
typedef const short *_Ctype_t;

_Ctype_t _Getpctype(void);
_Ctype_t _Getptolower(void);
_Ctype_t _Getptoupper(void);


extern _Ctype_t _Ctype;
extern _Ctype_t _Tolotab;
extern _Ctype_t _Touptab;
# 130 "/toolchain/arm/include/ctype.h" 3
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
# 3 "/arm-libs/library-src/dinkum/source/./xgetstr.c" 2
# 1 "/toolchain/arm/include/stdlib.h" 1 3
# 40 "/toolchain/arm/include/stdlib.h" 3
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
# 4 "/arm-libs/library-src/dinkum/source/./xgetstr.c" 2
# 1 "/toolchain/arm/include/string.h" 1 3
# 29 "/toolchain/arm/include/string.h" 3
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
# 137 "/toolchain/arm/include/string.h" 3
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
# 5 "/arm-libs/library-src/dinkum/source/./xgetstr.c" 2
# 1 "/toolchain/arm/include/xwchar.h" 1 3



# 1 "/toolchain/arm/include/wchar.h" 1 3
# 32 "/toolchain/arm/include/wchar.h" 3
typedef struct _Mbstatet
 {
 unsigned long _Wchar;
 unsigned short _Byte, _State;
# 63 "/toolchain/arm/include/wchar.h" 3
 } _Mbstatet;


typedef _Mbstatet mbstate_t;
# 78 "/toolchain/arm/include/wchar.h" 3
struct tm;
struct _Dnk_filet;



typedef struct _Dnk_filet _Filet;
# 110 "/toolchain/arm/include/wchar.h" 3
typedef _Wintt wint_t;






wint_t fgetwc(_Filet *);
wchar_t *fgetws(wchar_t *, int,
 _Filet *);
wint_t fputwc(wchar_t, _Filet *);
int fputws(const wchar_t *,
 _Filet *);
int fwide(_Filet *, int);
int fwprintf(_Filet *,
 const wchar_t *, ...);
int fwscanf(_Filet *,
 const wchar_t *, ...);
wint_t getwc(_Filet *);
wint_t getwchar(void);
wint_t putwc(wchar_t, _Filet *);
wint_t putwchar(wchar_t);
int swprintf(wchar_t *, size_t,
 const wchar_t *, ...);
int swscanf(const wchar_t *,
 const wchar_t *, ...);
wint_t ungetwc(wint_t, _Filet *);
int vfwprintf(_Filet *,
 const wchar_t *, _Va_list);
int vswprintf(wchar_t *, size_t,
 const wchar_t *, _Va_list);
int vwprintf(const wchar_t *, _Va_list);
int wprintf(const wchar_t *, ...);
int wscanf(const wchar_t *, ...);


int vfwscanf(_Filet *,
 const wchar_t *, _Va_list);
int vswscanf(const wchar_t *,
 const wchar_t *, _Va_list);
int vwscanf(const wchar_t *, _Va_list);



size_t mbrlen(const char *,
 size_t, mbstate_t *);
size_t mbrtowc(wchar_t *, const char *,
 size_t, mbstate_t *);
size_t mbsrtowcs(wchar_t *,
 const char **, size_t, mbstate_t *);
int mbsinit(const mbstate_t *);
size_t wcrtomb(char *,
 wchar_t, mbstate_t *);
size_t wcsrtombs(char *,
 const wchar_t **, size_t, mbstate_t *);
long wcstol(const wchar_t *,
 wchar_t **, int);


_Longlong wcstoll(const wchar_t *,
 wchar_t **, int);
_ULonglong wcstoull(const wchar_t *,
 wchar_t **, int);



wchar_t *wcscat(wchar_t *, const wchar_t *);
int wcscmp(const wchar_t *, const wchar_t *);
wchar_t *wcscpy(wchar_t *, const wchar_t *);
size_t wcslen(const wchar_t *);
int wcsncmp(const wchar_t *, const wchar_t *, size_t);
wchar_t *wcsncpy(wchar_t *,
 const wchar_t *, size_t);

int wcscoll(const wchar_t *, const wchar_t *);
size_t wcscspn(const wchar_t *, const wchar_t *);
wchar_t *wcsncat(wchar_t *,
 const wchar_t *, size_t);
size_t wcsspn(const wchar_t *, const wchar_t *);
wchar_t *wcstok(wchar_t *, const wchar_t *,
 wchar_t **);
size_t wcsxfrm(wchar_t *,
 const wchar_t *, size_t);
int wmemcmp(const wchar_t *, const wchar_t *, size_t);
wchar_t *wmemcpy(wchar_t *,
 const wchar_t *, size_t);
wchar_t *wmemmove(wchar_t *, const wchar_t *, size_t);
wchar_t *wmemset(wchar_t *, wchar_t, size_t);


size_t wcsftime(wchar_t *, size_t,
 const wchar_t *, const struct tm *);

wint_t _Btowc(int);
int _Wctob(wint_t);
double _WStod(const wchar_t *, wchar_t **, long);
float _WStof(const wchar_t *, wchar_t **, long);
long double _WStold(const wchar_t *, wchar_t **, long);
unsigned long _WStoul(const wchar_t *, wchar_t **, int);
# 230 "/toolchain/arm/include/wchar.h" 3
wchar_t *wmemchr(const wchar_t *, wchar_t, size_t);




# 1 "/toolchain/arm/include/xwcstod.h" 1 3
# 22 "/toolchain/arm/include/xwcstod.h" 3
double wcstod(const wchar_t *, wchar_t **);
unsigned long wcstoul(const wchar_t *, wchar_t **, int);
# 235 "/toolchain/arm/include/wchar.h" 2 3
# 1 "/toolchain/arm/include/xwstr.h" 1 3
# 63 "/toolchain/arm/include/xwstr.h" 3
wchar_t *wcschr(const wchar_t *, wchar_t);
wchar_t *wcspbrk(const wchar_t *, const wchar_t *);
wchar_t *wcsrchr(const wchar_t *, wchar_t);
wchar_t *wcsstr(const wchar_t *, const wchar_t *);
wint_t btowc(int);
int wctob(wint_t);


float wcstof(const wchar_t *,
 wchar_t **);
long double wcstold(const wchar_t *,
 wchar_t **);
# 236 "/toolchain/arm/include/wchar.h" 2 3
# 250 "/toolchain/arm/include/wchar.h" 3
int fwprintf_s(_Filet *,
 const wchar_t *, ...);
int fwscanf_s(_Filet *,
 const wchar_t *, ...);
int snwprintf_s(wchar_t *, rsize_t,
 const wchar_t *, ...);
int swprintf_s(wchar_t *, rsize_t,
 const wchar_t *, ...);
int swscanf_s(const wchar_t *,
 const wchar_t *, ...);
int vfwprintf_s(_Filet *,
 const wchar_t *,
 _Va_list);
int vfwscanf_s(_Filet *,
 const wchar_t *,
 _Va_list);
int vsnwprintf_s(wchar_t *, rsize_t,
 const wchar_t *,
 _Va_list);
int vswprintf_s(wchar_t *, rsize_t,
 const wchar_t *,
 _Va_list);
int vswscanf_s(const wchar_t *,
 const wchar_t *,
 _Va_list);
int vwprintf_s(const wchar_t *,
 _Va_list);
int vwscanf_s(const wchar_t *,
 _Va_list);
int wprintf_s(const wchar_t *, ...);
int wscanf_s(const wchar_t *, ...);

errno_t wcscpy_s(wchar_t *, rsize_t,
 const wchar_t *);
errno_t wcsncpy_s(wchar_t *, rsize_t,
 const wchar_t *, rsize_t);
errno_t wmemcpy_s(wchar_t *, rsize_t,
 const wchar_t *, rsize_t);
errno_t wmemmove_s(wchar_t *, rsize_t,
 const wchar_t *, rsize_t);
errno_t wcscat_s(wchar_t *, rsize_t,
 const wchar_t *);
errno_t wcsncat_s(wchar_t *, rsize_t,
 const wchar_t *, rsize_t);
wchar_t *wcstok_s(wchar_t *, rsize_t *,
 const wchar_t *, wchar_t **);

size_t wcsnlen_s(const wchar_t *, size_t);

errno_t wcrtomb_s(size_t *,
 char *, rsize_t,
 wchar_t,
 mbstate_t *);
errno_t mbsrtowcs_s(size_t *,
 wchar_t *, rsize_t,
 const char **, rsize_t,
 mbstate_t *);
errno_t wcsrtombs_s(size_t *,
 char *, rsize_t,
 const wchar_t **, rsize_t,
 mbstate_t *);


size_t wcsnrtombs(char * restrict dst, const wchar_t ** restrict src,
                  size_t nwc, size_t len, mbstate_t * restrict ps);

size_t mbsnrtowcs(wchar_t * restrict dst, const char ** restrict src,
                  size_t nms, size_t len, mbstate_t * restrict ps);
# 5 "/toolchain/arm/include/xwchar.h" 2 3
# 1 "/toolchain/arm/include/wctype.h" 1 3
# 35 "/toolchain/arm/include/wctype.h" 3
typedef _Sizet wctrans_t;


typedef _Sizet wctype_t;
# 53 "/toolchain/arm/include/wctype.h" 3
# 1 "/toolchain/arm/include/xwcc.h" 1 3





int _Iswctype(wint_t, wctype_t);
wint_t _Towctrans(wint_t, wctrans_t);
# 96 "/toolchain/arm/include/xwcc.h" 3
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
# 54 "/toolchain/arm/include/wctype.h" 2 3


wctrans_t wctrans(const char *);
wctype_t wctype(const char *);
# 72 "/toolchain/arm/include/wctype.h" 3
wint_t (towctrans)(wint_t, wctrans_t);
# 6 "/toolchain/arm/include/xwchar.h" 2 3
# 1 "/toolchain/arm/include/xstate.h" 1 3
# 20 "/toolchain/arm/include/xstate.h" 3
typedef struct
 {
 const unsigned short *_Tab[16];
 } _Statab;
# 7 "/toolchain/arm/include/xwchar.h" 2 3




int _Mbtowc(wchar_t *, const char *, size_t, mbstate_t *);
size_t _Wcsftime(wchar_t *, size_t, const char *, size_t,
 const struct tm *);
int _Wctomb(char *, wchar_t, mbstate_t *);
long double _WStold(const wchar_t *, wchar_t **, long);
_Longlong _WStoll(const wchar_t *, wchar_t **, int);
unsigned long _WStoul(const wchar_t *, wchar_t **, int);
_ULonglong _WStoull(const wchar_t *, wchar_t **, int);

int _Mbtowcx(wchar_t *, const char *, size_t, mbstate_t *,
 _Statab *);
int _Wctombx(char *, wchar_t, mbstate_t *,
 _Statab *, _Statab *);

_Statab *_Getpmbstate(void);
_Statab *_Getpwcstate(void);
_Statab *_Getpcostate(void);
_Statab *_Getpwcostate(void);
# 6 "/arm-libs/library-src/dinkum/source/./xgetstr.c" 2
# 1 "/toolchain/arm/include/xstdio.h" 1 3



# 1 "/toolchain/arm/include/limits.h" 1 3
# 5 "/toolchain/arm/include/xstdio.h" 2 3
# 1 "/toolchain/arm/include/stdarg.h" 1 3
# 6 "/toolchain/arm/include/xstdio.h" 2 3
# 1 "/toolchain/arm/include/stddef.h" 1 3
# 41 "/toolchain/arm/include/stddef.h" 3
typedef _Ptrdifft ptrdiff_t;
# 7 "/toolchain/arm/include/xstdio.h" 2 3
# 1 "/toolchain/arm/include/stdio.h" 1 3
# 98 "/toolchain/arm/include/stdio.h" 3
typedef struct fpos_t
 {
 _Longlong _Off;
 _Mbstatet _Wstate;
 } fpos_t;



struct _Dnk_filet
 {
 unsigned short _Mode;
 unsigned char _Idx;
 signed char _Handle;

 unsigned char *_Buf, *_Bend, *_Next;
 unsigned char *_Rend, *_Wend, *_Rback;

 _Wchart *_WRback, _WBack[2];
 unsigned char *_Rsave, *_WRend, *_WWend;

 _Mbstatet _Wstate;
 char *_Tmpnam;
 unsigned char _Back[8], _Cbuf;
 };






typedef _Filet FILE;



extern FILE _Stdin, _Stdout, _Stderr;

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


FILE *fdopen(signed char, const char *);
int fileno(FILE *);
int getw(FILE *);
int putw(int, FILE *);


long _Fgpos(FILE *, fpos_t *);
int _Flocale(FILE *, const char *, int);
void _Fsetlocale(FILE *, int);
int _Fspos(FILE *, const fpos_t *, long, int);






extern FILE *_Files[20];







int snprintf(char *, size_t,
 const char *, ...);
int vsnprintf(char *, size_t,
 const char *, _Va_list);
int vfscanf(FILE *,
 const char *, _Va_list);
int vscanf(const char *, _Va_list);
int vsscanf(const char *,
 const char *, _Va_list);
# 305 "/toolchain/arm/include/stdio.h" 3
int getc(FILE *);
int getchar(void);
int putc(int, FILE *);
int putchar(int);
# 346 "/toolchain/arm/include/stdio.h" 3
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

int vasprintf(char **ret, const char *format, va_list ap);
# 8 "/toolchain/arm/include/xstdio.h" 2 3


# 1 "/toolchain/arm/include/stdint.h" 1 3
# 25 "/toolchain/arm/include/stdint.h" 3
typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int int64_t;

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef long long unsigned int uint64_t;

typedef signed char int_least8_t;
typedef short int_least16_t;
typedef int int_least32_t;
typedef long long int int_least64_t;

typedef unsigned char uint_least8_t;
typedef unsigned short uint_least16_t;
typedef unsigned int uint_least32_t;
typedef long long unsigned int uint_least64_t;

typedef signed char int_fast8_t;
typedef short int_fast16_t;
typedef int int_fast32_t;
typedef long long int int_fast64_t;

typedef unsigned char uint_fast8_t;
typedef unsigned short uint_fast16_t;
typedef unsigned int uint_fast32_t;
typedef long long unsigned int uint_fast64_t;

typedef int intptr_t;
typedef unsigned int uintptr_t;

typedef long long int intmax_t;
typedef long long unsigned int uintmax_t;
# 11 "/toolchain/arm/include/xstdio.h" 2 3
# 68 "/toolchain/arm/include/xstdio.h" 3
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
# 114 "/toolchain/arm/include/xstdio.h" 3
void _Closreg(void);
FILE *_Fofind(void);
void _Fofree(FILE *);
FILE *_Foprep(const _Sysch_t *, const _Sysch_t *,
 FILE *, signed char, int);
signed char _Fopen(const _Sysch_t *, unsigned int, int);
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
# 7 "/arm-libs/library-src/dinkum/source/./xgetstr.c" 2


static int ranmatch(const char *s, int c, size_t n)
 {
 const unsigned char uc = (unsigned char)c;
 const unsigned char *su = (const unsigned char *)s;

 while (3 <= n)
  if (su[1] == '-')
   if (su[0] <= uc && uc <= su[2])
    return (1);
   else
    su += 3, n -= 3;
  else if (su[0] == uc)
   return (1);
  else
   ++su, --n;
 for (; 0 < n; ++su, --n)
  if (su[0] == uc)
   return (1);
 return (0);
 }

int _Getstr(_Sft *px, int sfl)
 {
 char comp;
 char *s = 0;
 char range = 0;
 char seen = 0;
 const char *pset = 0;
 const char *t;
 int ch, nset;
 int wfl = px->qual == 'l';
 wchar_t *p = 0;
 _Mbstatet mbst = {0};

 if (sfl < 0)
  {
  comp = (char)(*++px->s == '^' ? *px->s++ : '\0');
  t = strchr(*px->s == ']' ? px->s + 1 : px->s, ']');
  if (t == 0)
   return (0);
  nset = (int)(t - px->s);
  pset = px->s;
  px->s = t;
  if (3 <= nset && memchr(pset + 1, '-', nset - 2))
   range = 1;
  }
 px->nget = 0 < px->width ? px->width :
  sfl != 0 ? 0x7fffffff : 1;

 if (px->noconv)
  ;
 else if (wfl)
  {
  p = __builtin_va_arg(px->ap, wchar_t *);
  if (p == 0)
   return ((-1)
    - _Fail_s("scanf_s: bad %lc, %ls, or %l[ argument", sizeof ("scanf_s: bad %lc, %ls, or %l[ argument")));
  }
 else
  {
  s = __builtin_va_arg(px->ap, char *);
  if (s == 0)
   return ((-1)
    - _Fail_s("scanf_s: bad %c, %s, or %[ argument", sizeof ("scanf_s: bad %c, %s, or %[ argument")));
  }

 if (!px->noconv && px->secure)
  {
  size_t nsize = __builtin_va_arg(px->ap, size_t);

  if (((_Sizet)(-1) >> 1) < nsize)
   return ((-1) - _Fail_s("scanf_s: bad %c, %s, or %[ size", sizeof ("scanf_s: bad %c, %s, or %[ size")));

  if (nsize < px->prec)
   px->prec = nsize;
  }

  {
 int nc = 0;

 while ((ch = (0 <= --(px)->nget ? (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1)) : (++(px)->nchar, (-1)))) != (-1))
  if ((0 < sfl && (_Ctype[(int)(ch)] & (0x40|0x04|0x100))) || (sfl < 0
   && ((!range && ((!comp && !memchr(pset, ch, nset))
     || (comp && memchr(pset, ch, nset))))
    || (range && ((!comp && !ranmatch(pset, ch, nset))
     || (comp && ranmatch(pset, ch, nset)))))))
   break;
  else if (!wfl)
   {
   if (px->noconv)
    ;
   else if (px->prec == 0)
    {
    seen = 1;
    break;
    }
   else
    {
    *s++ = (char)ch;
    px->stored = 1;
    --px->prec;
    }
   seen = 2;
   }
  else
   {
   char buf[8];
   int n;
   wchar_t wc;

   buf[nc++] = (char)ch;
   switch (n = _Mbtowc(&wc, buf, nc, &mbst))
    {
   case -2:
    seen |= 1;
    if (nc < sizeof (buf))
     break;

   case -1:
    for (; 0 < nc; )
     {
     int ch = buf[--nc];
     do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
     }
    return (seen ? 0 : (-1));

   case 0:
    if (wc == L'\0')
     n = (int)(strlen(buf) + 1);

   default:
    if (n == -3)
     n = 0;
    for (; n < nc; )
     {
     int ch = buf[--nc];
     do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
     }

    if (px->noconv)
     ;
    else if (px->prec == 0)
     {
     seen = 1;
     break;
     }
    else
     {
     *p++ = wc;
     px->stored = 1;
     --px->prec;
     }
    --px->width;
    seen = 2;
    nc = 0;
    }
   }
  }
 do if ((int)(ch) != (-1)) (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0)); else --(px)->nchar; while (0);
 if (sfl != 0 && seen == 2)
  ;
 else if (seen == 2)
  return (1);
 else if ((seen & 1) != 0 || ch == (-1))
  return ((-1));
 else
  return (0);

 if (px->noconv)
  ;
 else if (px->prec == 0)
  return (0);
 else if (wfl)
  *p = L'\0';
 else
  *s = '\0';
 return (1);
 }
