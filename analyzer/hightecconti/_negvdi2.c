











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






int _Fltrounds(void);
















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





typedef int errno_t;







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








typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;

typedef unsigned short cnt_t;

typedef long off_t;
typedef long paddr_t;
typedef long key_t;

typedef char * caddr_t;

typedef unsigned short ino_t;
typedef int mode_t;
typedef short pid_t;
typedef short dev_t;
typedef unsigned short uid_t;
typedef unsigned short gid_t;
typedef short nlink_t;
typedef unsigned long daddr_t;




typedef int _ssize_t;














extern int __attribute__((fardata)) _Errno;





















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










typedef long off_t;


void _exit (int __status ) __attribute__ ((noreturn));
int access(const char *__path, int __amode );
unsigned alarm (unsigned __secs );
int chdir (const char *__path );
int chmod (const char *__path, mode_t __mode );
int close (int __fildes );
int dup (int __fildes );
int dup2 (int __fildes, int __fildes2 );
int isatty (int __fildes );
int link (const char *__path1, const char *__path2 );
off_t lseek (int __fildes, off_t __offset, int __whence );
int rmdir (const char *__path );
void * sbrk (ptrdiff_t __incr);
int unlink (const char *__path );
size_t write (int __fd, const void *__buf, size_t __nbyte );
size_t read (int __fd, void *__buf, size_t __nbyte );
int truncate (const char *, off_t __length);












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









typedef unsigned int hashval_t;




typedef hashval_t (*htab_hash) (const void *);






typedef int (*htab_eq) (const void *, const void *);



typedef void (*htab_del) (void *);





typedef int (*htab_trav) (void **, void *);





typedef void *(*htab_alloc) (size_t, size_t);


typedef void (*htab_free) (void *);



typedef void *(*htab_alloc_with_arg) (void *, size_t, size_t);
typedef void (*htab_free_with_arg) (void *, void *);
struct htab {

  htab_hash hash_f;


  htab_eq eq_f;


  htab_del del_f;


  void ** entries;


  size_t size;


  size_t n_elements;


  size_t n_deleted;



  unsigned int searches;



  unsigned int collisions;


  htab_alloc alloc_f;
  htab_free free_f;


  void * alloc_arg;
  htab_alloc_with_arg alloc_with_arg_f;
  htab_free_with_arg free_with_arg_f;



  unsigned int size_prime_index;
};

typedef struct htab *htab_t;


enum insert_option {NO_INSERT, INSERT};



extern htab_t htab_create_alloc (size_t, htab_hash,
                                    htab_eq, htab_del,
                                    htab_alloc, htab_free);

extern htab_t htab_create_alloc_ex (size_t, htab_hash,
                                      htab_eq, htab_del,
                                      void *, htab_alloc_with_arg,
                                      htab_free_with_arg);

extern htab_t htab_create_typed_alloc (size_t, htab_hash, htab_eq, htab_del,
     htab_alloc, htab_alloc, htab_free);


extern htab_t htab_create (size_t, htab_hash, htab_eq, htab_del);
extern htab_t htab_try_create (size_t, htab_hash, htab_eq, htab_del);

extern void htab_set_functions_ex (htab_t, htab_hash,
                                       htab_eq, htab_del,
                                       void *, htab_alloc_with_arg,
                                       htab_free_with_arg);

extern void htab_delete (htab_t);
extern void htab_empty (htab_t);

extern void * htab_find (htab_t, const void *);
extern void ** htab_find_slot (htab_t, const void *, enum insert_option);
extern void * htab_find_with_hash (htab_t, const void *, hashval_t);
extern void ** htab_find_slot_with_hash (htab_t, const void *,
       hashval_t, enum insert_option);
extern void htab_clear_slot (htab_t, void **);
extern void htab_remove_elt (htab_t, void *);
extern void htab_remove_elt_with_hash (htab_t, void *, hashval_t);

extern void htab_traverse (htab_t, htab_trav, void *);
extern void htab_traverse_noresize (htab_t, htab_trav, void *);

extern size_t htab_size (htab_t);
extern size_t htab_elements (htab_t);
extern double htab_collisions (htab_t);


extern htab_hash htab_hash_pointer;


extern htab_eq htab_eq_pointer;


extern hashval_t htab_hash_string (const void *);


extern hashval_t iterative_hash (const void *, size_t, hashval_t);
extern int filename_cmp (const char *s1, const char *s2);


extern int filename_ncmp (const char *s1, const char *s2,
     size_t n);

extern hashval_t filename_hash (const void *s);

extern int filename_eq (const void *s1, const void *s2);
struct _dont_use_rtx_here_;
struct _dont_use_rtvec_here_;
union _dont_use_tree_here_;
enum function_class {
  function_c94,
  function_c99_misc,
  function_c99_math_complex,
  function_sincos
};



enum memmodel
{
  MEMMODEL_RELAXED = 0,
  MEMMODEL_CONSUME = 1,
  MEMMODEL_ACQUIRE = 2,
  MEMMODEL_RELEASE = 3,
  MEMMODEL_ACQ_REL = 4,
  MEMMODEL_SEQ_CST = 5,
  MEMMODEL_LAST = 6
};






typedef void (*gt_pointer_operator) (void *, void *);


typedef unsigned char uchar;
typedef struct
{

  const char *option;




  int by_compiler;



  int by_assembler;



  const char *macro;



  int fixit;


  const char *ifx_name;
} tric_erratum_t;

enum
  {


tric_errata_cpu048,



tric_errata_cpu060,


tric_errata_cpu069,


tric_errata_cpu070,


tric_errata_cpu072,


tric_errata_cpu076,



tric_errata_cpu081,



tric_errata_cpu082,


tric_errata_cpu083,



tric_errata_cpu094,



tric_errata_cpu095,

tric_errata_cpu096,


tric_errata_cpu101,

tric_errata_cpu116,






tric_errata_cpu114,

    tric_errata_max
  };



typedef struct
{

  const char *name;


  int id;


  const char *libname;
} tric_core_t;



typedef struct
{

  const char *name;


  const char *target_name;


  const char *core_mtc;


  const char *ld_mcpu;
} tric_device_t;

extern tric_erratum_t tric_errata[];
extern const tric_core_t tric_cores[];
extern const tric_core_t *tric_core;
extern const tric_device_t tric_devices[];
extern const tric_device_t *tric_device;

extern void tric_set_core (const char*);
extern void tric_set_device (const char*);
extern const char *tric_device_to_startfile (int argc, const char **argv);
extern const char *tric_device_to_ld (int argc, const char **argv);
extern const char *tric_device_to_as (int argc, const char **argv);
extern const char *tric_self_specs (int argc, const char **argv);
extern const char *insert_tooldir_spec_function (int argc, const char **argv);
typedef struct
{
  const char * name;
  const char * args;
  char ret;
  int fast;
} tric_libfunc_info_t;
typedef struct
{
  int call_cookie;
  int argno;
  union _dont_use_tree_here_ * fntype;
  int libfunc_p;
  int outgoing;
  int update;
  int this_argno;
  unsigned int args_mask;
  tric_libfunc_info_t libfunc;
} CUMULATIVE_ARGS;
enum reg_class
{
    NO_REGS,

    REGCLASS_D15,
    REGCLASS_A10,
    REGCLASS_A15,

    REGCLASS_ND15,
    REGCLASS_D,
    REGCLASS_NA10,
    REGCLASS_NA15,
    REGCLASS_A,
    REGCLASS_R,

    ALL_REGS,

    LIM_REG_CLASSES
};
extern int tric_move_ratio (int);
extern int tric_clear_ratio (int);
extern int tric_set_ratio (int);
struct machine_function
{

  int is_leaf;



  int is_interrupt;






  int sibcall_fails;




  int bogus_pxhndcall;

  int anchor_completed;




  struct
  {
    struct _dont_use_rtx_here_ * reg;



    struct _dont_use_rtx_here_ * symbol;
  } pic_offset;
};

struct tric_segment_trap
{

  int have_pre_inc;


  int have_pre_dec;


  int do_ivopts;


  int do_ivopts_base_costs;


  int do_ivopts_use_address;



  int do_sched_change_address;
};











enum machine_mode
{
  VOIDmode,
  BLKmode,
  CCmode,
  BImode,
  QImode,
  HImode,
  SImode,
  DImode,
  TImode,
  PDImode,
  QQmode,
  HQmode,
  SQmode,
  DQmode,
  TQmode,
  UQQmode,
  UHQmode,
  USQmode,
  UDQmode,
  UTQmode,
  HAmode,
  SAmode,
  DAmode,
  TAmode,
  UHAmode,
  USAmode,
  UDAmode,
  UTAmode,
  HFmode,
  SFmode,
  DFmode,
  SDmode,
  DDmode,
  TDmode,
  CQImode,
  CHImode,
  CSImode,
  CDImode,
  CTImode,
  HCmode,
  SCmode,
  DCmode,
  MAX_MACHINE_MODE,

  MIN_MODE_RANDOM = VOIDmode,
  MAX_MODE_RANDOM = BLKmode,

  MIN_MODE_CC = CCmode,
  MAX_MODE_CC = CCmode,

  MIN_MODE_INT = QImode,
  MAX_MODE_INT = TImode,

  MIN_MODE_PARTIAL_INT = PDImode,
  MAX_MODE_PARTIAL_INT = PDImode,

  MIN_MODE_FRACT = QQmode,
  MAX_MODE_FRACT = TQmode,

  MIN_MODE_UFRACT = UQQmode,
  MAX_MODE_UFRACT = UTQmode,

  MIN_MODE_ACCUM = HAmode,
  MAX_MODE_ACCUM = TAmode,

  MIN_MODE_UACCUM = UHAmode,
  MAX_MODE_UACCUM = UTAmode,

  MIN_MODE_FLOAT = HFmode,
  MAX_MODE_FLOAT = DFmode,

  MIN_MODE_DECIMAL_FLOAT = SDmode,
  MAX_MODE_DECIMAL_FLOAT = TDmode,

  MIN_MODE_COMPLEX_INT = CQImode,
  MAX_MODE_COMPLEX_INT = CTImode,

  MIN_MODE_COMPLEX_FLOAT = HCmode,
  MAX_MODE_COMPLEX_FLOAT = DCmode,

  MIN_MODE_VECTOR_INT = VOIDmode,
  MAX_MODE_VECTOR_INT = VOIDmode,

  MIN_MODE_VECTOR_FRACT = VOIDmode,
  MAX_MODE_VECTOR_FRACT = VOIDmode,

  MIN_MODE_VECTOR_UFRACT = VOIDmode,
  MAX_MODE_VECTOR_UFRACT = VOIDmode,

  MIN_MODE_VECTOR_ACCUM = VOIDmode,
  MAX_MODE_VECTOR_ACCUM = VOIDmode,

  MIN_MODE_VECTOR_UACCUM = VOIDmode,
  MAX_MODE_VECTOR_UACCUM = VOIDmode,

  MIN_MODE_VECTOR_FLOAT = VOIDmode,
  MAX_MODE_VECTOR_FLOAT = VOIDmode,

  NUM_MACHINE_MODES = MAX_MACHINE_MODE
};

       
extern int __gcc_bcmp (const unsigned char *, const unsigned char *, size_t);
extern void __clear_cache (char *, char *);
extern void __eprintf (const char *, const char *, unsigned int, const char *)
  __attribute__ ((__noreturn__));
typedef int QItype __attribute__ ((mode (QI)));
typedef unsigned int UQItype __attribute__ ((mode (QI)));
typedef int HItype __attribute__ ((mode (HI)));
typedef unsigned int UHItype __attribute__ ((mode (HI)));


typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));


typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
typedef float SFtype __attribute__ ((mode (SF)));
typedef _Complex float SCtype __attribute__ ((mode (SC)));


typedef float DFtype __attribute__ ((mode (DF)));
typedef _Complex float DCtype __attribute__ ((mode (DC)));
typedef int cmp_return_type __attribute__((mode (__libgcc_cmp_return__)));
typedef int shift_count_type __attribute__((mode (__libgcc_shift_count__)));
extern DItype __muldi3 (DItype, DItype);
extern DItype __divdi3 (DItype, DItype);
extern UDItype __udivdi3 (UDItype, UDItype);
extern UDItype __umoddi3 (UDItype, UDItype);
extern DItype __moddi3 (DItype, DItype);




extern UDItype __udivmoddi4 (UDItype, UDItype, UDItype *);




extern DItype __negdi2 (DItype);


extern DItype __lshrdi3 (DItype, shift_count_type);
extern DItype __ashldi3 (DItype, shift_count_type);
extern DItype __ashrdi3 (DItype, shift_count_type);




extern USItype __udiv_w_sdiv (USItype *, USItype, USItype, USItype);


extern cmp_return_type __cmpdi2 (DItype, DItype);
extern cmp_return_type __ucmpdi2 (DItype, DItype);


extern SItype __bswapsi2 (SItype);


extern DItype __bswapdi2 (DItype);


extern SItype __absvsi2 (SItype);
extern SItype __addvsi3 (SItype, SItype);
extern SItype __subvsi3 (SItype, SItype);
extern SItype __mulvsi3 (SItype, SItype);
extern SItype __negvsi2 (SItype);
extern DItype __absvdi2 (DItype);
extern DItype __addvdi3 (DItype, DItype);
extern DItype __subvdi3 (DItype, DItype);
extern DItype __mulvdi3 (DItype, DItype);
extern DItype __negvdi2 (DItype);
extern DItype __fixsfdi (SFtype);
extern SFtype __floatdisf (DItype);
extern SFtype __floatundisf (UDItype);
extern USItype __fixunssfsi (SFtype);
extern UDItype __fixunssfdi (SFtype);
extern SFtype __powisf2 (SFtype, int);
extern SCtype __divsc3 (SFtype, SFtype, SFtype, SFtype);
extern SCtype __mulsc3 (SFtype, SFtype, SFtype, SFtype);


extern DItype __fixdfdi (DFtype);
extern DFtype __floatdidf (DItype);
extern DFtype __floatundidf (UDItype);
extern USItype __fixunsdfsi (DFtype);
extern UDItype __fixunsdfdi (DFtype);
extern DFtype __powidf2 (DFtype, int);
extern DCtype __divdc3 (DFtype, DFtype, DFtype, DFtype);
extern DCtype __muldc3 (DFtype, DFtype, DFtype, DFtype);
  struct DWstruct {SItype low, high;};






typedef union
{
  struct DWstruct s;
  DItype ll;
} DWunion;



extern const UQItype __popcount_tab[256];





extern const UQItype __clz_tab[256];







extern int __clzdi2 (UDItype);
extern int __clzsi2 (USItype);
extern int __ctzsi2 (USItype);
extern int __ctzdi2 (UDItype);
extern int __clrsbsi2 (SItype);
extern int __clrsbdi2 (DItype);
extern int __ffssi2 (USItype);
extern int __ffsdi2 (DItype);
extern int __popcountsi2 (USItype);
extern int __popcountdi2 (UDItype);
extern int __paritysi2 (USItype);
extern int __paritydi2 (UDItype);


extern void __enable_execute_stack (void *);
DItype
__negvdi2 (DItype a)
{
  const DItype w = -(UDItype) a;

  if (a >= 0 ? w > 0 : w < 0)
    abort ();

  return w;
}
