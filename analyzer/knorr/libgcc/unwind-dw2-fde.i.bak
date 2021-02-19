











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

       
enum dwarf_tag { DW_TAG_padding = 0x00
, DW_TAG_array_type = 0x01
, DW_TAG_class_type = 0x02
, DW_TAG_entry_point = 0x03
, DW_TAG_enumeration_type = 0x04
, DW_TAG_formal_parameter = 0x05
, DW_TAG_imported_declaration = 0x08
, DW_TAG_label = 0x0a
, DW_TAG_lexical_block = 0x0b
, DW_TAG_member = 0x0d
, DW_TAG_pointer_type = 0x0f
, DW_TAG_reference_type = 0x10
, DW_TAG_compile_unit = 0x11
, DW_TAG_string_type = 0x12
, DW_TAG_structure_type = 0x13
, DW_TAG_subroutine_type = 0x15
, DW_TAG_typedef = 0x16
, DW_TAG_union_type = 0x17
, DW_TAG_unspecified_parameters = 0x18
, DW_TAG_variant = 0x19
, DW_TAG_common_block = 0x1a
, DW_TAG_common_inclusion = 0x1b
, DW_TAG_inheritance = 0x1c
, DW_TAG_inlined_subroutine = 0x1d
, DW_TAG_module = 0x1e
, DW_TAG_ptr_to_member_type = 0x1f
, DW_TAG_set_type = 0x20
, DW_TAG_subrange_type = 0x21
, DW_TAG_with_stmt = 0x22
, DW_TAG_access_declaration = 0x23
, DW_TAG_base_type = 0x24
, DW_TAG_catch_block = 0x25
, DW_TAG_const_type = 0x26
, DW_TAG_constant = 0x27
, DW_TAG_enumerator = 0x28
, DW_TAG_file_type = 0x29
, DW_TAG_friend = 0x2a
, DW_TAG_namelist = 0x2b
, DW_TAG_namelist_item = 0x2c
, DW_TAG_packed_type = 0x2d
, DW_TAG_subprogram = 0x2e
, DW_TAG_template_type_param = 0x2f
, DW_TAG_template_value_param = 0x30
, DW_TAG_thrown_type = 0x31
, DW_TAG_try_block = 0x32
, DW_TAG_variant_part = 0x33
, DW_TAG_variable = 0x34
, DW_TAG_volatile_type = 0x35

, DW_TAG_dwarf_procedure = 0x36
, DW_TAG_restrict_type = 0x37
, DW_TAG_interface_type = 0x38
, DW_TAG_namespace = 0x39
, DW_TAG_imported_module = 0x3a
, DW_TAG_unspecified_type = 0x3b
, DW_TAG_partial_unit = 0x3c
, DW_TAG_imported_unit = 0x3d
, DW_TAG_condition = 0x3f
, DW_TAG_shared_type = 0x40

, DW_TAG_type_unit = 0x41
, DW_TAG_rvalue_reference_type = 0x42
, DW_TAG_template_alias = 0x43

, DW_TAG_lo_user = 0x4080
, DW_TAG_hi_user = 0xffff


, DW_TAG_MIPS_loop = 0x4081


, DW_TAG_HP_array_descriptor = 0x4090
, DW_TAG_HP_Bliss_field = 0x4091
, DW_TAG_HP_Bliss_field_set = 0x4092


, DW_TAG_format_label = 0x4101
, DW_TAG_function_template = 0x4102
, DW_TAG_class_template = 0x4103
, DW_TAG_GNU_BINCL = 0x4104
, DW_TAG_GNU_EINCL = 0x4105


, DW_TAG_GNU_template_template_param = 0x4106





, DW_TAG_GNU_template_parameter_pack = 0x4107
, DW_TAG_GNU_formal_parameter_pack = 0x4108




, DW_TAG_GNU_call_site = 0x4109
, DW_TAG_GNU_call_site_parameter = 0x410a

, DW_TAG_upc_shared_type = 0x8765
, DW_TAG_upc_strict_type = 0x8766
, DW_TAG_upc_relaxed_type = 0x8767

, DW_TAG_PGI_kanji_type = 0xA000
, DW_TAG_PGI_interface_block = 0xA020
};

enum dwarf_form { DW_FORM_addr = 0x01
, DW_FORM_block2 = 0x03
, DW_FORM_block4 = 0x04
, DW_FORM_data2 = 0x05
, DW_FORM_data4 = 0x06
, DW_FORM_data8 = 0x07
, DW_FORM_string = 0x08
, DW_FORM_block = 0x09
, DW_FORM_block1 = 0x0a
, DW_FORM_data1 = 0x0b
, DW_FORM_flag = 0x0c
, DW_FORM_sdata = 0x0d
, DW_FORM_strp = 0x0e
, DW_FORM_udata = 0x0f
, DW_FORM_ref_addr = 0x10
, DW_FORM_ref1 = 0x11
, DW_FORM_ref2 = 0x12
, DW_FORM_ref4 = 0x13
, DW_FORM_ref8 = 0x14
, DW_FORM_ref_udata = 0x15
, DW_FORM_indirect = 0x16

, DW_FORM_sec_offset = 0x17
, DW_FORM_exprloc = 0x18
, DW_FORM_flag_present = 0x19
, DW_FORM_ref_sig8 = 0x20

, DW_FORM_GNU_addr_index = 0x1f01
, DW_FORM_GNU_str_index = 0x1f02


, DW_FORM_GNU_ref_alt = 0x1f20
, DW_FORM_GNU_strp_alt = 0x1f21
};

enum dwarf_attribute { DW_AT_sibling = 0x01
, DW_AT_location = 0x02
, DW_AT_name = 0x03
, DW_AT_ordering = 0x09
, DW_AT_subscr_data = 0x0a
, DW_AT_byte_size = 0x0b
, DW_AT_bit_offset = 0x0c
, DW_AT_bit_size = 0x0d
, DW_AT_element_list = 0x0f
, DW_AT_stmt_list = 0x10
, DW_AT_low_pc = 0x11
, DW_AT_high_pc = 0x12
, DW_AT_language = 0x13
, DW_AT_member = 0x14
, DW_AT_discr = 0x15
, DW_AT_discr_value = 0x16
, DW_AT_visibility = 0x17
, DW_AT_import = 0x18
, DW_AT_string_length = 0x19
, DW_AT_common_reference = 0x1a
, DW_AT_comp_dir = 0x1b
, DW_AT_const_value = 0x1c
, DW_AT_containing_type = 0x1d
, DW_AT_default_value = 0x1e
, DW_AT_inline = 0x20
, DW_AT_is_optional = 0x21
, DW_AT_lower_bound = 0x22
, DW_AT_producer = 0x25
, DW_AT_prototyped = 0x27
, DW_AT_return_addr = 0x2a
, DW_AT_start_scope = 0x2c
, DW_AT_bit_stride = 0x2e
, DW_AT_upper_bound = 0x2f
, DW_AT_abstract_origin = 0x31
, DW_AT_accessibility = 0x32
, DW_AT_address_class = 0x33
, DW_AT_artificial = 0x34
, DW_AT_base_types = 0x35
, DW_AT_calling_convention = 0x36
, DW_AT_count = 0x37
, DW_AT_data_member_location = 0x38
, DW_AT_decl_column = 0x39
, DW_AT_decl_file = 0x3a
, DW_AT_decl_line = 0x3b
, DW_AT_declaration = 0x3c
, DW_AT_discr_list = 0x3d
, DW_AT_encoding = 0x3e
, DW_AT_external = 0x3f
, DW_AT_frame_base = 0x40
, DW_AT_friend = 0x41
, DW_AT_identifier_case = 0x42
, DW_AT_macro_info = 0x43
, DW_AT_namelist_items = 0x44
, DW_AT_priority = 0x45
, DW_AT_segment = 0x46
, DW_AT_specification = 0x47
, DW_AT_static_link = 0x48
, DW_AT_type = 0x49
, DW_AT_use_location = 0x4a
, DW_AT_variable_parameter = 0x4b
, DW_AT_virtuality = 0x4c
, DW_AT_vtable_elem_location = 0x4d

, DW_AT_allocated = 0x4e
, DW_AT_associated = 0x4f
, DW_AT_data_location = 0x50
, DW_AT_byte_stride = 0x51
, DW_AT_entry_pc = 0x52
, DW_AT_use_UTF8 = 0x53
, DW_AT_extension = 0x54
, DW_AT_ranges = 0x55
, DW_AT_trampoline = 0x56
, DW_AT_call_column = 0x57
, DW_AT_call_file = 0x58
, DW_AT_call_line = 0x59
, DW_AT_description = 0x5a
, DW_AT_binary_scale = 0x5b
, DW_AT_decimal_scale = 0x5c
, DW_AT_small = 0x5d
, DW_AT_decimal_sign = 0x5e
, DW_AT_digit_count = 0x5f
, DW_AT_picture_string = 0x60
, DW_AT_mutable = 0x61
, DW_AT_threads_scaled = 0x62
, DW_AT_explicit = 0x63
, DW_AT_object_pointer = 0x64
, DW_AT_endianity = 0x65
, DW_AT_elemental = 0x66
, DW_AT_pure = 0x67
, DW_AT_recursive = 0x68

, DW_AT_signature = 0x69
, DW_AT_main_subprogram = 0x6a
, DW_AT_data_bit_offset = 0x6b
, DW_AT_const_expr = 0x6c
, DW_AT_enum_class = 0x6d
, DW_AT_linkage_name = 0x6e

, DW_AT_lo_user = 0x2000
, DW_AT_hi_user = 0x3fff


, DW_AT_MIPS_fde = 0x2001
, DW_AT_MIPS_loop_begin = 0x2002
, DW_AT_MIPS_tail_loop_begin = 0x2003
, DW_AT_MIPS_epilog_begin = 0x2004
, DW_AT_MIPS_loop_unroll_factor = 0x2005
, DW_AT_MIPS_software_pipeline_depth = 0x2006
, DW_AT_MIPS_linkage_name = 0x2007
, DW_AT_MIPS_stride = 0x2008
, DW_AT_MIPS_abstract_name = 0x2009
, DW_AT_MIPS_clone_origin = 0x200a
, DW_AT_MIPS_has_inlines = 0x200b

, DW_AT_HP_block_index = 0x2000
, DW_AT_HP_unmodifiable = 0x2001
, DW_AT_HP_prologue = 0x2005
, DW_AT_HP_epilogue = 0x2008
, DW_AT_HP_actuals_stmt_list = 0x2010
, DW_AT_HP_proc_per_section = 0x2011
, DW_AT_HP_raw_data_ptr = 0x2012
, DW_AT_HP_pass_by_reference = 0x2013
, DW_AT_HP_opt_level = 0x2014
, DW_AT_HP_prof_version_id = 0x2015
, DW_AT_HP_opt_flags = 0x2016
, DW_AT_HP_cold_region_low_pc = 0x2017
, DW_AT_HP_cold_region_high_pc = 0x2018
, DW_AT_HP_all_variables_modifiable = 0x2019
, DW_AT_HP_linkage_name = 0x201a
, DW_AT_HP_prof_flags = 0x201b
, DW_AT_HP_unit_name = 0x201f
, DW_AT_HP_unit_size = 0x2020
, DW_AT_HP_widened_byte_size = 0x2021
, DW_AT_HP_definition_points = 0x2022
, DW_AT_HP_default_location = 0x2023
, DW_AT_HP_is_result_param = 0x2029


, DW_AT_sf_names = 0x2101
, DW_AT_src_info = 0x2102
, DW_AT_mac_info = 0x2103
, DW_AT_src_coords = 0x2104
, DW_AT_body_begin = 0x2105
, DW_AT_body_end = 0x2106
, DW_AT_GNU_vector = 0x2107


, DW_AT_GNU_guarded_by = 0x2108
, DW_AT_GNU_pt_guarded_by = 0x2109
, DW_AT_GNU_guarded = 0x210a
, DW_AT_GNU_pt_guarded = 0x210b
, DW_AT_GNU_locks_excluded = 0x210c
, DW_AT_GNU_exclusive_locks_required = 0x210d
, DW_AT_GNU_shared_locks_required = 0x210e


, DW_AT_GNU_odr_signature = 0x210f


, DW_AT_GNU_template_name = 0x2110


, DW_AT_GNU_call_site_value = 0x2111
, DW_AT_GNU_call_site_data_value = 0x2112
, DW_AT_GNU_call_site_target = 0x2113
, DW_AT_GNU_call_site_target_clobbered = 0x2114
, DW_AT_GNU_tail_call = 0x2115
, DW_AT_GNU_all_tail_call_sites = 0x2116
, DW_AT_GNU_all_call_sites = 0x2117
, DW_AT_GNU_all_source_call_sites = 0x2118

, DW_AT_GNU_macros = 0x2119

, DW_AT_GNU_dwo_name = 0x2130
, DW_AT_GNU_dwo_id = 0x2131
, DW_AT_GNU_ranges_base = 0x2132
, DW_AT_GNU_addr_base = 0x2133
, DW_AT_GNU_pubnames = 0x2134
, DW_AT_GNU_pubtypes = 0x2135


, DW_AT_GNU_discriminator = 0x2136

, DW_AT_VMS_rtnbeg_pd_address = 0x2201



, DW_AT_use_GNAT_descriptive_type = 0x2301
, DW_AT_GNAT_descriptive_type = 0x2302

, DW_AT_control_flow = 0x2FC3

, DW_AT_upc_threads_scaled = 0x3210

, DW_AT_PGI_lbase = 0x3a00
, DW_AT_PGI_soffset = 0x3a01
, DW_AT_PGI_lstride = 0x3a02
};

enum dwarf_location_atom { DW_OP_addr = 0x03
, DW_OP_deref = 0x06
, DW_OP_const1u = 0x08
, DW_OP_const1s = 0x09
, DW_OP_const2u = 0x0a
, DW_OP_const2s = 0x0b
, DW_OP_const4u = 0x0c
, DW_OP_const4s = 0x0d
, DW_OP_const8u = 0x0e
, DW_OP_const8s = 0x0f
, DW_OP_constu = 0x10
, DW_OP_consts = 0x11
, DW_OP_dup = 0x12
, DW_OP_drop = 0x13
, DW_OP_over = 0x14
, DW_OP_pick = 0x15
, DW_OP_swap = 0x16
, DW_OP_rot = 0x17
, DW_OP_xderef = 0x18
, DW_OP_abs = 0x19
, DW_OP_and = 0x1a
, DW_OP_div = 0x1b
, DW_OP_minus = 0x1c
, DW_OP_mod = 0x1d
, DW_OP_mul = 0x1e
, DW_OP_neg = 0x1f
, DW_OP_not = 0x20
, DW_OP_or = 0x21
, DW_OP_plus = 0x22
, DW_OP_plus_uconst = 0x23
, DW_OP_shl = 0x24
, DW_OP_shr = 0x25
, DW_OP_shra = 0x26
, DW_OP_xor = 0x27
, DW_OP_bra = 0x28
, DW_OP_eq = 0x29
, DW_OP_ge = 0x2a
, DW_OP_gt = 0x2b
, DW_OP_le = 0x2c
, DW_OP_lt = 0x2d
, DW_OP_ne = 0x2e
, DW_OP_skip = 0x2f
, DW_OP_lit0 = 0x30
, DW_OP_lit1 = 0x31
, DW_OP_lit2 = 0x32
, DW_OP_lit3 = 0x33
, DW_OP_lit4 = 0x34
, DW_OP_lit5 = 0x35
, DW_OP_lit6 = 0x36
, DW_OP_lit7 = 0x37
, DW_OP_lit8 = 0x38
, DW_OP_lit9 = 0x39
, DW_OP_lit10 = 0x3a
, DW_OP_lit11 = 0x3b
, DW_OP_lit12 = 0x3c
, DW_OP_lit13 = 0x3d
, DW_OP_lit14 = 0x3e
, DW_OP_lit15 = 0x3f
, DW_OP_lit16 = 0x40
, DW_OP_lit17 = 0x41
, DW_OP_lit18 = 0x42
, DW_OP_lit19 = 0x43
, DW_OP_lit20 = 0x44
, DW_OP_lit21 = 0x45
, DW_OP_lit22 = 0x46
, DW_OP_lit23 = 0x47
, DW_OP_lit24 = 0x48
, DW_OP_lit25 = 0x49
, DW_OP_lit26 = 0x4a
, DW_OP_lit27 = 0x4b
, DW_OP_lit28 = 0x4c
, DW_OP_lit29 = 0x4d
, DW_OP_lit30 = 0x4e
, DW_OP_lit31 = 0x4f
, DW_OP_reg0 = 0x50
, DW_OP_reg1 = 0x51
, DW_OP_reg2 = 0x52
, DW_OP_reg3 = 0x53
, DW_OP_reg4 = 0x54
, DW_OP_reg5 = 0x55
, DW_OP_reg6 = 0x56
, DW_OP_reg7 = 0x57
, DW_OP_reg8 = 0x58
, DW_OP_reg9 = 0x59
, DW_OP_reg10 = 0x5a
, DW_OP_reg11 = 0x5b
, DW_OP_reg12 = 0x5c
, DW_OP_reg13 = 0x5d
, DW_OP_reg14 = 0x5e
, DW_OP_reg15 = 0x5f
, DW_OP_reg16 = 0x60
, DW_OP_reg17 = 0x61
, DW_OP_reg18 = 0x62
, DW_OP_reg19 = 0x63
, DW_OP_reg20 = 0x64
, DW_OP_reg21 = 0x65
, DW_OP_reg22 = 0x66
, DW_OP_reg23 = 0x67
, DW_OP_reg24 = 0x68
, DW_OP_reg25 = 0x69
, DW_OP_reg26 = 0x6a
, DW_OP_reg27 = 0x6b
, DW_OP_reg28 = 0x6c
, DW_OP_reg29 = 0x6d
, DW_OP_reg30 = 0x6e
, DW_OP_reg31 = 0x6f
, DW_OP_breg0 = 0x70
, DW_OP_breg1 = 0x71
, DW_OP_breg2 = 0x72
, DW_OP_breg3 = 0x73
, DW_OP_breg4 = 0x74
, DW_OP_breg5 = 0x75
, DW_OP_breg6 = 0x76
, DW_OP_breg7 = 0x77
, DW_OP_breg8 = 0x78
, DW_OP_breg9 = 0x79
, DW_OP_breg10 = 0x7a
, DW_OP_breg11 = 0x7b
, DW_OP_breg12 = 0x7c
, DW_OP_breg13 = 0x7d
, DW_OP_breg14 = 0x7e
, DW_OP_breg15 = 0x7f
, DW_OP_breg16 = 0x80
, DW_OP_breg17 = 0x81
, DW_OP_breg18 = 0x82
, DW_OP_breg19 = 0x83
, DW_OP_breg20 = 0x84
, DW_OP_breg21 = 0x85
, DW_OP_breg22 = 0x86
, DW_OP_breg23 = 0x87
, DW_OP_breg24 = 0x88
, DW_OP_breg25 = 0x89
, DW_OP_breg26 = 0x8a
, DW_OP_breg27 = 0x8b
, DW_OP_breg28 = 0x8c
, DW_OP_breg29 = 0x8d
, DW_OP_breg30 = 0x8e
, DW_OP_breg31 = 0x8f
, DW_OP_regx = 0x90
, DW_OP_fbreg = 0x91
, DW_OP_bregx = 0x92
, DW_OP_piece = 0x93
, DW_OP_deref_size = 0x94
, DW_OP_xderef_size = 0x95
, DW_OP_nop = 0x96

, DW_OP_push_object_address = 0x97
, DW_OP_call2 = 0x98
, DW_OP_call4 = 0x99
, DW_OP_call_ref = 0x9a
, DW_OP_form_tls_address = 0x9b
, DW_OP_call_frame_cfa = 0x9c
, DW_OP_bit_piece = 0x9d


, DW_OP_implicit_value = 0x9e
, DW_OP_stack_value = 0x9f

, DW_OP_lo_user = 0xe0
, DW_OP_hi_user = 0xff


, DW_OP_GNU_push_tls_address = 0xe0

, DW_OP_GNU_uninit = 0xf0
, DW_OP_GNU_encoded_addr = 0xf1


, DW_OP_GNU_implicit_pointer = 0xf2


, DW_OP_GNU_entry_value = 0xf3


, DW_OP_GNU_const_type = 0xf4
, DW_OP_GNU_regval_type = 0xf5
, DW_OP_GNU_deref_type = 0xf6
, DW_OP_GNU_convert = 0xf7
, DW_OP_GNU_reinterpret = 0xf9

, DW_OP_GNU_parameter_ref = 0xfa

, DW_OP_GNU_addr_index = 0xfb
, DW_OP_GNU_const_index = 0xfc

, DW_OP_HP_unknown = 0xe0
, DW_OP_HP_is_value = 0xe1
, DW_OP_HP_fltconst4 = 0xe2
, DW_OP_HP_fltconst8 = 0xe3
, DW_OP_HP_mod_range = 0xe4
, DW_OP_HP_unmod_range = 0xe5
, DW_OP_HP_tls = 0xe6

, DW_OP_PGI_omp_thread_num = 0xf8
};

enum dwarf_type { DW_ATE_void = 0x0
, DW_ATE_address = 0x1
, DW_ATE_boolean = 0x2
, DW_ATE_complex_float = 0x3
, DW_ATE_float = 0x4
, DW_ATE_signed = 0x5
, DW_ATE_signed_char = 0x6
, DW_ATE_unsigned = 0x7
, DW_ATE_unsigned_char = 0x8

, DW_ATE_imaginary_float = 0x9
, DW_ATE_packed_decimal = 0xa
, DW_ATE_numeric_string = 0xb
, DW_ATE_edited = 0xc
, DW_ATE_signed_fixed = 0xd
, DW_ATE_unsigned_fixed = 0xe
, DW_ATE_decimal_float = 0xf

, DW_ATE_UTF = 0x10

, DW_ATE_lo_user = 0x80
, DW_ATE_hi_user = 0xff


, DW_ATE_HP_float80 = 0x80
, DW_ATE_HP_complex_float80 = 0x81
, DW_ATE_HP_float128 = 0x82
, DW_ATE_HP_complex_float128 = 0x83
, DW_ATE_HP_floathpintel = 0x84
, DW_ATE_HP_imaginary_float80 = 0x85
, DW_ATE_HP_imaginary_float128 = 0x86
, DW_ATE_HP_VAX_float = 0x88
, DW_ATE_HP_VAX_float_d = 0x89
, DW_ATE_HP_packed_decimal = 0x8a
, DW_ATE_HP_zoned_decimal = 0x8b
, DW_ATE_HP_edited = 0x8c
, DW_ATE_HP_signed_fixed = 0x8d
, DW_ATE_HP_unsigned_fixed = 0x8e
, DW_ATE_HP_VAX_complex_float = 0x8f
, DW_ATE_HP_VAX_complex_float_d = 0x90

};

enum dwarf_call_frame_info { DW_CFA_advance_loc = 0x40
, DW_CFA_offset = 0x80
, DW_CFA_restore = 0xc0
, DW_CFA_nop = 0x00
, DW_CFA_set_loc = 0x01
, DW_CFA_advance_loc1 = 0x02
, DW_CFA_advance_loc2 = 0x03
, DW_CFA_advance_loc4 = 0x04
, DW_CFA_offset_extended = 0x05
, DW_CFA_restore_extended = 0x06
, DW_CFA_undefined = 0x07
, DW_CFA_same_value = 0x08
, DW_CFA_register = 0x09
, DW_CFA_remember_state = 0x0a
, DW_CFA_restore_state = 0x0b
, DW_CFA_def_cfa = 0x0c
, DW_CFA_def_cfa_register = 0x0d
, DW_CFA_def_cfa_offset = 0x0e

, DW_CFA_def_cfa_expression = 0x0f
, DW_CFA_expression = 0x10
, DW_CFA_offset_extended_sf = 0x11
, DW_CFA_def_cfa_sf = 0x12
, DW_CFA_def_cfa_offset_sf = 0x13
, DW_CFA_val_offset = 0x14
, DW_CFA_val_offset_sf = 0x15
, DW_CFA_val_expression = 0x16

, DW_CFA_lo_user = 0x1c
, DW_CFA_hi_user = 0x3f


, DW_CFA_MIPS_advance_loc8 = 0x1d

, DW_CFA_GNU_window_save = 0x2d
, DW_CFA_GNU_args_size = 0x2e
, DW_CFA_GNU_negative_offset_extended = 0x2f

};
enum dwarf_decimal_sign_encoding
  {

    DW_DS_unsigned = 0x01,
    DW_DS_leading_overpunch = 0x02,
    DW_DS_trailing_overpunch = 0x03,
    DW_DS_leading_separate = 0x04,
    DW_DS_trailing_separate = 0x05
  };


enum dwarf_endianity_encoding
  {

    DW_END_default = 0x00,
    DW_END_big = 0x01,
    DW_END_little = 0x02,

    DW_END_lo_user = 0x40,
    DW_END_hi_user = 0xff
  };


enum dwarf_array_dim_ordering
  {
    DW_ORD_row_major = 0,
    DW_ORD_col_major = 1
  };


enum dwarf_access_attribute
  {
    DW_ACCESS_public = 1,
    DW_ACCESS_protected = 2,
    DW_ACCESS_private = 3
  };


enum dwarf_visibility_attribute
  {
    DW_VIS_local = 1,
    DW_VIS_exported = 2,
    DW_VIS_qualified = 3
  };


enum dwarf_virtuality_attribute
  {
    DW_VIRTUALITY_none = 0,
    DW_VIRTUALITY_virtual = 1,
    DW_VIRTUALITY_pure_virtual = 2
  };


enum dwarf_id_case
  {
    DW_ID_case_sensitive = 0,
    DW_ID_up_case = 1,
    DW_ID_down_case = 2,
    DW_ID_case_insensitive = 3
  };


enum dwarf_calling_convention
  {
    DW_CC_normal = 0x1,
    DW_CC_program = 0x2,
    DW_CC_nocall = 0x3,

    DW_CC_lo_user = 0x40,
    DW_CC_hi_user = 0xff,

    DW_CC_GNU_renesas_sh = 0x40,
    DW_CC_GNU_borland_fastcall_i386 = 0x41,







    DW_CC_GDB_IBM_OpenCL = 0xff
  };


enum dwarf_inline_attribute
  {
    DW_INL_not_inlined = 0,
    DW_INL_inlined = 1,
    DW_INL_declared_not_inlined = 2,
    DW_INL_declared_inlined = 3
  };


enum dwarf_discrim_list
  {
    DW_DSC_label = 0,
    DW_DSC_range = 1
  };


enum dwarf_line_number_ops
  {
    DW_LNS_extended_op = 0,
    DW_LNS_copy = 1,
    DW_LNS_advance_pc = 2,
    DW_LNS_advance_line = 3,
    DW_LNS_set_file = 4,
    DW_LNS_set_column = 5,
    DW_LNS_negate_stmt = 6,
    DW_LNS_set_basic_block = 7,
    DW_LNS_const_add_pc = 8,
    DW_LNS_fixed_advance_pc = 9,

    DW_LNS_set_prologue_end = 10,
    DW_LNS_set_epilogue_begin = 11,
    DW_LNS_set_isa = 12
  };


enum dwarf_line_number_x_ops
  {
    DW_LNE_end_sequence = 1,
    DW_LNE_set_address = 2,
    DW_LNE_define_file = 3,
    DW_LNE_set_discriminator = 4,

    DW_LNE_HP_negate_is_UV_update = 0x11,
    DW_LNE_HP_push_context = 0x12,
    DW_LNE_HP_pop_context = 0x13,
    DW_LNE_HP_set_file_line_column = 0x14,
    DW_LNE_HP_set_routine_name = 0x15,
    DW_LNE_HP_set_sequence = 0x16,
    DW_LNE_HP_negate_post_semantics = 0x17,
    DW_LNE_HP_negate_function_exit = 0x18,
    DW_LNE_HP_negate_front_end_logical = 0x19,
    DW_LNE_HP_define_proc = 0x20,
    DW_LNE_HP_source_file_correlation = 0x80,

    DW_LNE_lo_user = 0x80,
    DW_LNE_hi_user = 0xff
  };


enum dwarf_line_number_hp_sfc_ops
  {
    DW_LNE_HP_SFC_formfeed = 1,
    DW_LNE_HP_SFC_set_listing_line = 2,
    DW_LNE_HP_SFC_associate = 3
  };




enum dwarf_location_list_entry_type
  {
    DW_LLE_GNU_end_of_list_entry = 0,
    DW_LLE_GNU_base_address_selection_entry = 1,
    DW_LLE_GNU_start_end_entry = 2,
    DW_LLE_GNU_start_length_entry = 3
  };
enum dwarf_source_language
  {
    DW_LANG_C89 = 0x0001,
    DW_LANG_C = 0x0002,
    DW_LANG_Ada83 = 0x0003,
    DW_LANG_C_plus_plus = 0x0004,
    DW_LANG_Cobol74 = 0x0005,
    DW_LANG_Cobol85 = 0x0006,
    DW_LANG_Fortran77 = 0x0007,
    DW_LANG_Fortran90 = 0x0008,
    DW_LANG_Pascal83 = 0x0009,
    DW_LANG_Modula2 = 0x000a,

    DW_LANG_Java = 0x000b,
    DW_LANG_C99 = 0x000c,
    DW_LANG_Ada95 = 0x000d,
    DW_LANG_Fortran95 = 0x000e,
    DW_LANG_PLI = 0x000f,
    DW_LANG_ObjC = 0x0010,
    DW_LANG_ObjC_plus_plus = 0x0011,
    DW_LANG_UPC = 0x0012,
    DW_LANG_D = 0x0013,

    DW_LANG_Python = 0x0014,

    DW_LANG_Go = 0x0016,

    DW_LANG_lo_user = 0x8000,
    DW_LANG_hi_user = 0xffff,


    DW_LANG_Mips_Assembler = 0x8001,

    DW_LANG_Upc = 0x8765,

    DW_LANG_HP_Bliss = 0x8003,
    DW_LANG_HP_Basic91 = 0x8004,
    DW_LANG_HP_Pascal91 = 0x8005,
    DW_LANG_HP_IMacro = 0x8006,
    DW_LANG_HP_Assembler = 0x8007
  };


enum dwarf_macinfo_record_type
  {
    DW_MACINFO_define = 1,
    DW_MACINFO_undef = 2,
    DW_MACINFO_start_file = 3,
    DW_MACINFO_end_file = 4,
    DW_MACINFO_vendor_ext = 255
  };


enum dwarf_macro_record_type
  {
    DW_MACRO_GNU_define = 1,
    DW_MACRO_GNU_undef = 2,
    DW_MACRO_GNU_start_file = 3,
    DW_MACRO_GNU_end_file = 4,
    DW_MACRO_GNU_define_indirect = 5,
    DW_MACRO_GNU_undef_indirect = 6,
    DW_MACRO_GNU_transparent_include = 7,


    DW_MACRO_GNU_define_indirect_alt = 8,
    DW_MACRO_GNU_undef_indirect_alt = 9,
    DW_MACRO_GNU_transparent_include_alt = 10,
    DW_MACRO_GNU_lo_user = 0xe0,
    DW_MACRO_GNU_hi_user = 0xff
  };
enum dwarf_sect
  {
    DW_SECT_INFO = 1,
    DW_SECT_TYPES = 2,
    DW_SECT_ABBREV = 3,
    DW_SECT_LINE = 4,
    DW_SECT_LOC = 5,
    DW_SECT_STR_OFFSETS = 6,
    DW_SECT_MACINFO = 7,
    DW_SECT_MACRO = 8,
    DW_SECT_MAX = 8
  };







extern const char *get_DW_TAG_name (unsigned int tag);



extern const char *get_DW_AT_name (unsigned int attr);



extern const char *get_DW_FORM_name (unsigned int form);



extern const char *get_DW_OP_name (unsigned int op);



extern const char *get_DW_ATE_name (unsigned int enc);



extern const char *get_DW_CFA_name (unsigned int opc);
typedef unsigned _Unwind_Word __attribute__((__mode__(__unwind_word__)));
typedef signed _Unwind_Sword __attribute__((__mode__(__unwind_word__)));



typedef unsigned _Unwind_Ptr __attribute__((__mode__(__pointer__)));

typedef unsigned _Unwind_Internal_Ptr __attribute__((__mode__(__pointer__)));





typedef unsigned _Unwind_Exception_Class __attribute__((__mode__(__DI__)));



typedef enum
{
  _URC_NO_REASON = 0,
  _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
  _URC_FATAL_PHASE2_ERROR = 2,
  _URC_FATAL_PHASE1_ERROR = 3,
  _URC_NORMAL_STOP = 4,
  _URC_END_OF_STACK = 5,
  _URC_HANDLER_FOUND = 6,
  _URC_INSTALL_CONTEXT = 7,
  _URC_CONTINUE_UNWIND = 8
} _Unwind_Reason_Code;
struct _Unwind_Exception;

typedef void (*_Unwind_Exception_Cleanup_Fn) (_Unwind_Reason_Code,
           struct _Unwind_Exception *);

struct _Unwind_Exception
{
  _Unwind_Exception_Class exception_class;
  _Unwind_Exception_Cleanup_Fn exception_cleanup;




  _Unwind_Word private_1;
  _Unwind_Word private_2;





} __attribute__((__aligned__));




typedef int _Unwind_Action;
struct _Unwind_Context;


extern _Unwind_Reason_Code
_Unwind_RaiseException (struct _Unwind_Exception *);



typedef _Unwind_Reason_Code (*_Unwind_Stop_Fn)
     (int, _Unwind_Action, _Unwind_Exception_Class,
      struct _Unwind_Exception *, struct _Unwind_Context *, void *);

extern _Unwind_Reason_Code
_Unwind_ForcedUnwind (struct _Unwind_Exception *, _Unwind_Stop_Fn, void *);


extern void _Unwind_DeleteException (struct _Unwind_Exception *);



extern void
_Unwind_Resume (struct _Unwind_Exception *);



extern _Unwind_Reason_Code
_Unwind_Resume_or_Rethrow (struct _Unwind_Exception *);




typedef _Unwind_Reason_Code (*_Unwind_Trace_Fn)
     (struct _Unwind_Context *, void *);

extern _Unwind_Reason_Code
_Unwind_Backtrace (_Unwind_Trace_Fn, void *);






extern _Unwind_Word _Unwind_GetGR (struct _Unwind_Context *, int);
extern void _Unwind_SetGR (struct _Unwind_Context *, int, _Unwind_Word);

extern _Unwind_Ptr _Unwind_GetIP (struct _Unwind_Context *);
extern _Unwind_Ptr _Unwind_GetIPInfo (struct _Unwind_Context *, int *);
extern void _Unwind_SetIP (struct _Unwind_Context *, _Unwind_Ptr);


extern _Unwind_Word _Unwind_GetCFA (struct _Unwind_Context *);

extern void *_Unwind_GetLanguageSpecificData (struct _Unwind_Context *);

extern _Unwind_Ptr _Unwind_GetRegionStart (struct _Unwind_Context *);
typedef _Unwind_Reason_Code (*_Unwind_Personality_Fn)
     (int, _Unwind_Action, _Unwind_Exception_Class,
      struct _Unwind_Exception *, struct _Unwind_Context *);




struct SjLj_Function_Context;
extern void _Unwind_SjLj_Register (struct SjLj_Function_Context *);
extern void _Unwind_SjLj_Unregister (struct SjLj_Function_Context *);

extern _Unwind_Reason_Code
_Unwind_SjLj_RaiseException (struct _Unwind_Exception *);
extern _Unwind_Reason_Code
_Unwind_SjLj_ForcedUnwind (struct _Unwind_Exception *, _Unwind_Stop_Fn, void *);
extern void
_Unwind_SjLj_Resume (struct _Unwind_Exception *);
extern _Unwind_Reason_Code
_Unwind_SjLj_Resume_or_Rethrow (struct _Unwind_Exception *);
extern _Unwind_Ptr _Unwind_GetDataRelBase (struct _Unwind_Context *);
extern _Unwind_Ptr _Unwind_GetTextRelBase (struct _Unwind_Context *);




extern void * _Unwind_FindEnclosingFunction (void *pc);
  typedef long _sleb128_t;
  typedef unsigned long _uleb128_t;

static unsigned int
size_of_encoded_value (unsigned char encoding) __attribute__ ((unused));

static unsigned int
size_of_encoded_value (unsigned char encoding)
{
  if (encoding == 0xff)
    return 0;

  switch (encoding & 0x07)
    {
    case 0x00:
      return sizeof (void *);
    case 0x02:
      return 2;
    case 0x03:
      return 4;
    case 0x04:
      return 8;
    }
  abort ();
}
static const unsigned char *
read_uleb128 (const unsigned char *p, _uleb128_t *val)
{
  unsigned int shift = 0;
  unsigned char byte;
  _uleb128_t result;

  result = 0;
  do
    {
      byte = *p++;
      result |= ((_uleb128_t)byte & 0x7f) << shift;
      shift += 7;
    }
  while (byte & 0x80);

  *val = result;
  return p;
}



static const unsigned char *
read_sleb128 (const unsigned char *p, _sleb128_t *val)
{
  unsigned int shift = 0;
  unsigned char byte;
  _uleb128_t result;

  result = 0;
  do
    {
      byte = *p++;
      result |= ((_uleb128_t)byte & 0x7f) << shift;
      shift += 7;
    }
  while (byte & 0x80);


  if (shift < 8 * sizeof(result) && (byte & 0x40) != 0)
    result |= -(((_uleb128_t)1L) << shift);

  *val = (_sleb128_t) result;
  return p;
}





static const unsigned char *
read_encoded_value_with_base (unsigned char encoding, _Unwind_Ptr base,
         const unsigned char *p, _Unwind_Ptr *val)
{
  union unaligned
    {
      void *ptr;
      unsigned u2 __attribute__ ((mode (HI)));
      unsigned u4 __attribute__ ((mode (SI)));
      unsigned u8 __attribute__ ((mode (DI)));
      signed s2 __attribute__ ((mode (HI)));
      signed s4 __attribute__ ((mode (SI)));
      signed s8 __attribute__ ((mode (DI)));
    } __attribute__((__packed__));

  const union unaligned *u = (const union unaligned *) p;
  _Unwind_Internal_Ptr result;

  if (encoding == 0x50)
    {
      _Unwind_Internal_Ptr a = (_Unwind_Internal_Ptr) p;
      a = (a + sizeof (void *) - 1) & - sizeof(void *);
      result = *(_Unwind_Internal_Ptr *) a;
      p = (const unsigned char *) (_Unwind_Internal_Ptr) (a + sizeof (void *));
    }
  else
    {
      switch (encoding & 0x0f)
 {
 case 0x00:
   result = (_Unwind_Internal_Ptr) u->ptr;
   p += sizeof (void *);
   break;

 case 0x01:
   {
     _uleb128_t tmp;
     p = read_uleb128 (p, &tmp);
     result = (_Unwind_Internal_Ptr) tmp;
   }
   break;

 case 0x09:
   {
     _sleb128_t tmp;
     p = read_sleb128 (p, &tmp);
     result = (_Unwind_Internal_Ptr) tmp;
   }
   break;

 case 0x02:
   result = u->u2;
   p += 2;
   break;
 case 0x03:
   result = u->u4;
   p += 4;
   break;
 case 0x04:
   result = u->u8;
   p += 8;
   break;

 case 0x0A:
   result = u->s2;
   p += 2;
   break;
 case 0x0B:
   result = u->s4;
   p += 4;
   break;
 case 0x0C:
   result = u->s8;
   p += 8;
   break;

 default:
   abort ();
 }

      if (result != 0)
 {
   result += ((encoding & 0x70) == 0x10
       ? (_Unwind_Internal_Ptr) u : base);
   if (encoding & 0x80)
     result = *(_Unwind_Internal_Ptr *) result;
 }
    }

  *val = result;
  return p;
}
struct fde_vector
{
  const void *orig_data;
  size_t count;
  const struct dwarf_fde *array[];
};

struct object
{
  void *pc_begin;
  void *tbase;
  void *dbase;
  union {
    const struct dwarf_fde *single;
    struct dwarf_fde **array;
    struct fde_vector *sort;
  } u;

  union {
    struct {
      unsigned long sorted : 1;
      unsigned long from_array : 1;
      unsigned long mixed_encoding : 1;
      unsigned long encoding : 8;


      unsigned long count : 21;
    } b;
    size_t i;
  } s;





  struct object *next;
};





struct old_object
{
  void *pc_begin;
  void *pc_end;
  struct dwarf_fde *fde_begin;
  struct dwarf_fde **fde_array;
  size_t count;
  struct old_object *next;
};

struct dwarf_eh_bases
{
  void *tbase;
  void *dbase;
  void *func;
};


extern void __register_frame_info_bases (const void *, struct object *,
      void *, void *);
extern void __register_frame_info (const void *, struct object *);
extern void __register_frame (void *);
extern void __register_frame_info_table_bases (void *, struct object *,
            void *, void *);
extern void __register_frame_info_table (void *, struct object *);
extern void __register_frame_table (void *);
extern void *__deregister_frame_info (const void *);
extern void *__deregister_frame_info_bases (const void *);
extern void __deregister_frame (void *);


typedef int sword __attribute__ ((mode (SI)));
typedef unsigned int uword __attribute__ ((mode (SI)));
typedef unsigned int uaddr __attribute__ ((mode (pointer)));
typedef int saddr __attribute__ ((mode (pointer)));
typedef unsigned char ubyte;
struct dwarf_cie
{
  uword length;
  sword CIE_id;
  ubyte version;
  unsigned char augmentation[];
} __attribute__ ((packed, aligned (__alignof__ (void *))));


struct dwarf_fde
{
  uword length;
  sword CIE_delta;
  unsigned char pc_begin[];
} __attribute__ ((packed, aligned (__alignof__ (void *))));

typedef struct dwarf_fde fde;



static __inline__ const struct dwarf_cie *
get_cie (const struct dwarf_fde *f)
{
  return (const void *)&f->CIE_delta - f->CIE_delta;
}

static __inline__ const fde *
next_fde (const fde *f)
{
  return (const fde *) ((const char *) f + f->length + sizeof (f->length));
}

extern const fde * _Unwind_Find_FDE (void *, struct dwarf_eh_bases *);

static __inline__ int
last_fde (struct object *obj __attribute__ ((__unused__)), const fde *f)
{



  return f->length == 0;

}
typedef int __gthread_key_t;
typedef int __gthread_once_t;
typedef int __gthread_mutex_t;
typedef int __gthread_recursive_mutex_t;
static __inline__ int
__gthread_active_p (void)
{
  return 0;
}

static __inline__ int
__gthread_once (__gthread_once_t *__once __attribute__((unused)), void (*__func) (void) __attribute__((unused)))
{
  return 0;
}

static __inline__ int __attribute__((unused))
__gthread_key_create (__gthread_key_t *__key __attribute__((unused)), void (*__func) (void *) __attribute__((unused)))
{
  return 0;
}

static int __attribute__((unused))
__gthread_key_delete (__gthread_key_t __key __attribute__((unused)))
{
  return 0;
}

static __inline__ void *
__gthread_getspecific (__gthread_key_t __key __attribute__((unused)))
{
  return 0;
}

static __inline__ int
__gthread_setspecific (__gthread_key_t __key __attribute__((unused)), const void *__v __attribute__((unused)))
{
  return 0;
}

static __inline__ int
__gthread_mutex_destroy (__gthread_mutex_t *__mutex __attribute__((unused)))
{
  return 0;
}

static __inline__ int
__gthread_mutex_lock (__gthread_mutex_t *__mutex __attribute__((unused)))
{
  return 0;
}

static __inline__ int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex __attribute__((unused)))
{
  return 0;
}

static __inline__ int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex __attribute__((unused)))
{
  return 0;
}

static __inline__ int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_lock (__mutex);
}

static __inline__ int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_trylock (__mutex);
}

static __inline__ int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_unlock (__mutex);
}

static __inline__ int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *__mutex)
{
  return __gthread_mutex_destroy (__mutex);
}






static struct object *unseen_objects;
static struct object *seen_objects;


static __gthread_mutex_t object_mutex = 0;
void
__register_frame_info_bases (const void *begin, struct object *ob,
        void *tbase, void *dbase)
{

  if ((const uword *) begin == 0 || *(const uword *) begin == 0)
    return;

  ob->pc_begin = (void *)-1;
  ob->tbase = tbase;
  ob->dbase = dbase;
  ob->u.single = begin;
  ob->s.i = 0;
  ob->s.b.encoding = 0xff;




  ;
  __gthread_mutex_lock (&object_mutex);

  ob->next = unseen_objects;
  unseen_objects = ob;

  __gthread_mutex_unlock (&object_mutex);
}

void
__register_frame_info (const void *begin, struct object *ob)
{
  __register_frame_info_bases (begin, ob, 0, 0);
}

void
__register_frame (void *begin)
{
  struct object *ob;


  if (*(uword *) begin == 0)
    return;

  ob = malloc (sizeof (struct object));
  __register_frame_info (begin, ob);
}





void
__register_frame_info_table_bases (void *begin, struct object *ob,
       void *tbase, void *dbase)
{
  ob->pc_begin = (void *)-1;
  ob->tbase = tbase;
  ob->dbase = dbase;
  ob->u.array = begin;
  ob->s.i = 0;
  ob->s.b.from_array = 1;
  ob->s.b.encoding = 0xff;

  ;
  __gthread_mutex_lock (&object_mutex);

  ob->next = unseen_objects;
  unseen_objects = ob;

  __gthread_mutex_unlock (&object_mutex);
}

void
__register_frame_info_table (void *begin, struct object *ob)
{
  __register_frame_info_table_bases (begin, ob, 0, 0);
}

void
__register_frame_table (void *begin)
{
  struct object *ob = malloc (sizeof (struct object));
  __register_frame_info_table (begin, ob);
}
void *
__deregister_frame_info_bases (const void *begin)
{
  struct object **p;
  struct object *ob = 0;


  if ((const uword *) begin == 0 || *(const uword *) begin == 0)
    return ob;

  ;
  __gthread_mutex_lock (&object_mutex);

  for (p = &unseen_objects; *p ; p = &(*p)->next)
    if ((*p)->u.single == begin)
      {
 ob = *p;
 *p = ob->next;
 goto out;
      }

  for (p = &seen_objects; *p ; p = &(*p)->next)
    if ((*p)->s.b.sorted)
      {
 if ((*p)->u.sort->orig_data == begin)
   {
     ob = *p;
     *p = ob->next;
     free (ob->u.sort);
     goto out;
   }
      }
    else
      {
 if ((*p)->u.single == begin)
   {
     ob = *p;
     *p = ob->next;
     goto out;
   }
      }

 out:
  __gthread_mutex_unlock (&object_mutex);
  ((void)(!(ob) ? abort (), 0 : 0));
  return (void *) ob;
}

void *
__deregister_frame_info (const void *begin)
{
  return __deregister_frame_info_bases (begin);
}

void
__deregister_frame (void *begin)
{

  if (*(uword *) begin != 0)
    free (__deregister_frame_info (begin));
}





static _Unwind_Ptr
base_from_object (unsigned char encoding, struct object *ob)
{
  if (encoding == 0xff)
    return 0;

  switch (encoding & 0x70)
    {
    case 0x00:
    case 0x10:
    case 0x50:
      return 0;

    case 0x20:
      return (_Unwind_Ptr) ob->tbase;
    case 0x30:
      return (_Unwind_Ptr) ob->dbase;
    default:
      (abort ());
    }
}




static int
get_cie_encoding (const struct dwarf_cie *cie)
{
  const unsigned char *aug, *p;
  _Unwind_Ptr dummy;
  _uleb128_t utmp;
  _sleb128_t stmp;

  aug = cie->augmentation;
  p = aug + strlen ((const char *)aug) + 1;
  if (__builtin_expect (cie->version >= 4, 0))
    {
      if (p[0] != sizeof (void *) || p[1] != 0)
 return 0xff;

      p += 2;
    }

  if (aug[0] != 'z')
    return 0x00;

  p = read_uleb128 (p, &utmp);
  p = read_sleb128 (p, &stmp);
  if (cie->version == 1)
    p++;
  else
    p = read_uleb128 (p, &utmp);

  aug++;
  p = read_uleb128 (p, &utmp);
  while (1)
    {

      if (*aug == 'R')
 return *p;

      else if (*aug == 'P')
 {



   p = read_encoded_value_with_base (*p & 0x7F, 0, p + 1, &dummy);
 }

      else if (*aug == 'L')
 p++;

      else
 return 0x00;
      aug++;
    }
}

static __inline__ int
get_fde_encoding (const struct dwarf_fde *f)
{
  return get_cie_encoding (get_cie (f));
}
static int
fde_unencoded_compare (struct object *ob __attribute__((unused)),
         const fde *x, const fde *y)
{
  _Unwind_Ptr x_ptr, y_ptr;
  memcpy (&x_ptr, x->pc_begin, sizeof (_Unwind_Ptr));
  memcpy (&y_ptr, y->pc_begin, sizeof (_Unwind_Ptr));

  if (x_ptr > y_ptr)
    return 1;
  if (x_ptr < y_ptr)
    return -1;
  return 0;
}

static int
fde_single_encoding_compare (struct object *ob, const fde *x, const fde *y)
{
  _Unwind_Ptr base, x_ptr, y_ptr;

  base = base_from_object (ob->s.b.encoding, ob);
  read_encoded_value_with_base (ob->s.b.encoding, base, x->pc_begin, &x_ptr);
  read_encoded_value_with_base (ob->s.b.encoding, base, y->pc_begin, &y_ptr);

  if (x_ptr > y_ptr)
    return 1;
  if (x_ptr < y_ptr)
    return -1;
  return 0;
}

static int
fde_mixed_encoding_compare (struct object *ob, const fde *x, const fde *y)
{
  int x_encoding, y_encoding;
  _Unwind_Ptr x_ptr, y_ptr;

  x_encoding = get_fde_encoding (x);
  read_encoded_value_with_base (x_encoding, base_from_object (x_encoding, ob),
    x->pc_begin, &x_ptr);

  y_encoding = get_fde_encoding (y);
  read_encoded_value_with_base (y_encoding, base_from_object (y_encoding, ob),
    y->pc_begin, &y_ptr);

  if (x_ptr > y_ptr)
    return 1;
  if (x_ptr < y_ptr)
    return -1;
  return 0;
}

typedef int (*fde_compare_t) (struct object *, const fde *, const fde *);
struct fde_accumulator
{
  struct fde_vector *linear;
  struct fde_vector *erratic;
};

static __inline__ int
start_fde_sort (struct fde_accumulator *accu, size_t count)
{
  size_t size;
  if (! count)
    return 0;

  size = sizeof (struct fde_vector) + sizeof (const fde *) * count;
  if ((accu->linear = malloc (size)))
    {
      accu->linear->count = 0;
      if ((accu->erratic = malloc (size)))
 accu->erratic->count = 0;
      return 1;
    }
  else
    return 0;
}

static __inline__ void
fde_insert (struct fde_accumulator *accu, const fde *this_fde)
{
  if (accu->linear)
    accu->linear->array[accu->linear->count++] = this_fde;
}
static __inline__ void
fde_split (struct object *ob, fde_compare_t fde_compare,
    struct fde_vector *linear, struct fde_vector *erratic)
{
  static const fde *marker;
  size_t count = linear->count;
  const fde *const *chain_end = &marker;
  size_t i, j, k;




  ((void)(!(sizeof (const fde *) == sizeof (const fde **)) ? abort (), 0 : 0));

  for (i = 0; i < count; i++)
    {
      const fde *const *probe;

      for (probe = chain_end;
    probe != &marker && fde_compare (ob, linear->array[i], *probe) < 0;
    probe = chain_end)
 {
   chain_end = (const fde *const*) erratic->array[probe - linear->array];
   erratic->array[probe - linear->array] = ((void *)0);
 }
      erratic->array[i] = (const fde *) chain_end;
      chain_end = &linear->array[i];
    }




  for (i = j = k = 0; i < count; i++)
    if (erratic->array[i])
      linear->array[j++] = linear->array[i];
    else
      erratic->array[k++] = linear->array[i];
  linear->count = j;
  erratic->count = k;
}






static void
frame_downheap (struct object *ob, fde_compare_t fde_compare, const fde **a,
  int lo, int hi)
{
  int i, j;

  for (i = lo, j = 2*i+1;
       j < hi;
       j = 2*i+1)
    {
      if (j+1 < hi && fde_compare (ob, a[j], a[j+1]) < 0)
 ++j;

      if (fde_compare (ob, a[i], a[j]) < 0)
 {
   do { const fde * tmp = a[i]; a[i] = a[j]; a[j] = tmp; } while (0);
   i = j;
 }
      else
 break;
    }
}




static void
frame_heapsort (struct object *ob, fde_compare_t fde_compare,
  struct fde_vector *erratic)
{



  const fde ** a = erratic->array;



  size_t n = erratic->count;
  int m;




  for (m = n/2-1; m >= 0; --m)
    frame_downheap (ob, fde_compare, a, m, n);




  for (m = n-1; m >= 1; --m)
    {
      do { const fde * tmp = a[0]; a[0] = a[m]; a[m] = tmp; } while (0);
      frame_downheap (ob, fde_compare, a, 0, m);
    }

}


static __inline__ void
fde_merge (struct object *ob, fde_compare_t fde_compare,
    struct fde_vector *v1, struct fde_vector *v2)
{
  size_t i1, i2;
  const fde * fde2;

  i2 = v2->count;
  if (i2 > 0)
    {
      i1 = v1->count;
      do
 {
   i2--;
   fde2 = v2->array[i2];
   while (i1 > 0 && fde_compare (ob, v1->array[i1-1], fde2) > 0)
     {
       v1->array[i1+i2] = v1->array[i1-1];
       i1--;
     }
   v1->array[i1+i2] = fde2;
 }
      while (i2 > 0);
      v1->count += v2->count;
    }
}

static __inline__ void
end_fde_sort (struct object *ob, struct fde_accumulator *accu, size_t count)
{
  fde_compare_t fde_compare;

  ((void)(!(!accu->linear || accu->linear->count == count) ? abort (), 0 : 0));

  if (ob->s.b.mixed_encoding)
    fde_compare = fde_mixed_encoding_compare;
  else if (ob->s.b.encoding == 0x00)
    fde_compare = fde_unencoded_compare;
  else
    fde_compare = fde_single_encoding_compare;

  if (accu->erratic)
    {
      fde_split (ob, fde_compare, accu->linear, accu->erratic);
      ((void)(!(accu->linear->count + accu->erratic->count == count) ? abort (), 0 : 0));
      frame_heapsort (ob, fde_compare, accu->erratic);
      fde_merge (ob, fde_compare, accu->linear, accu->erratic);
      free (accu->erratic);
    }
  else
    {


      frame_heapsort (ob, fde_compare, accu->linear);
    }
}






static size_t
classify_object_over_fdes (struct object *ob, const fde *this_fde)
{
  const struct dwarf_cie *last_cie = 0;
  size_t count = 0;
  int encoding = 0x00;
  _Unwind_Ptr base = 0;

  for (; ! last_fde (ob, this_fde); this_fde = next_fde (this_fde))
    {
      const struct dwarf_cie *this_cie;
      _Unwind_Ptr mask, pc_begin;


      if (this_fde->CIE_delta == 0)
 continue;



      this_cie = get_cie (this_fde);
      if (this_cie != last_cie)
 {
   last_cie = this_cie;
   encoding = get_cie_encoding (this_cie);
   if (encoding == 0xff)
     return -1;
   base = base_from_object (encoding, ob);
   if (ob->s.b.encoding == 0xff)
     ob->s.b.encoding = encoding;
   else if (ob->s.b.encoding != encoding)
     ob->s.b.mixed_encoding = 1;
 }

      read_encoded_value_with_base (encoding, base, this_fde->pc_begin,
        &pc_begin);





      mask = size_of_encoded_value (encoding);
      if (mask < sizeof (void *))
 mask = (((_Unwind_Ptr) 1) << (mask << 3)) - 1;
      else
 mask = -1;

      if ((pc_begin & mask) == 0)
 continue;

      count += 1;
      if ((void *) pc_begin < ob->pc_begin)
 ob->pc_begin = (void *) pc_begin;
    }

  return count;
}

static void
add_fdes (struct object *ob, struct fde_accumulator *accu, const fde *this_fde)
{
  const struct dwarf_cie *last_cie = 0;
  int encoding = ob->s.b.encoding;
  _Unwind_Ptr base = base_from_object (ob->s.b.encoding, ob);

  for (; ! last_fde (ob, this_fde); this_fde = next_fde (this_fde))
    {
      const struct dwarf_cie *this_cie;


      if (this_fde->CIE_delta == 0)
 continue;

      if (ob->s.b.mixed_encoding)
 {


   this_cie = get_cie (this_fde);
   if (this_cie != last_cie)
     {
       last_cie = this_cie;
       encoding = get_cie_encoding (this_cie);
       base = base_from_object (encoding, ob);
     }
 }

      if (encoding == 0x00)
 {
   _Unwind_Ptr ptr;
   memcpy (&ptr, this_fde->pc_begin, sizeof (_Unwind_Ptr));
   if (ptr == 0)
     continue;
 }
      else
 {
   _Unwind_Ptr pc_begin, mask;

   read_encoded_value_with_base (encoding, base, this_fde->pc_begin,
     &pc_begin);





   mask = size_of_encoded_value (encoding);
   if (mask < sizeof (void *))
     mask = (((_Unwind_Ptr) 1) << (mask << 3)) - 1;
   else
     mask = -1;

   if ((pc_begin & mask) == 0)
     continue;
 }

      fde_insert (accu, this_fde);
    }
}






static __inline__ void
init_object (struct object* ob)
{
  struct fde_accumulator accu;
  size_t count;

  count = ob->s.b.count;
  if (count == 0)
    {
      if (ob->s.b.from_array)
 {
   fde **p = ob->u.array;
   for (count = 0; *p; ++p)
     {
       size_t cur_count = classify_object_over_fdes (ob, *p);
       if (cur_count == (size_t) -1)
  goto unhandled_fdes;
       count += cur_count;
     }
 }
      else
 {
   count = classify_object_over_fdes (ob, ob->u.single);
   if (count == (size_t) -1)
     {
       static const fde terminator;
     unhandled_fdes:
       ob->s.i = 0;
       ob->s.b.encoding = 0xff;
       ob->u.single = &terminator;
       return;
     }
 }






      ob->s.b.count = count;
      if (ob->s.b.count != count)
 ob->s.b.count = 0;
    }

  if (!start_fde_sort (&accu, count))
    return;

  if (ob->s.b.from_array)
    {
      fde **p;
      for (p = ob->u.array; *p; ++p)
 add_fdes (ob, &accu, *p);
    }
  else
    add_fdes (ob, &accu, ob->u.single);

  end_fde_sort (ob, &accu, count);



  accu.linear->orig_data = ob->u.single;
  ob->u.sort = accu.linear;

  ob->s.b.sorted = 1;
}





static const fde *
linear_search_fdes (struct object *ob, const fde *this_fde, void *pc)
{
  const struct dwarf_cie *last_cie = 0;
  int encoding = ob->s.b.encoding;
  _Unwind_Ptr base = base_from_object (ob->s.b.encoding, ob);

  for (; ! last_fde (ob, this_fde); this_fde = next_fde (this_fde))
    {
      const struct dwarf_cie *this_cie;
      _Unwind_Ptr pc_begin, pc_range;


      if (this_fde->CIE_delta == 0)
 continue;

      if (ob->s.b.mixed_encoding)
 {


   this_cie = get_cie (this_fde);
   if (this_cie != last_cie)
     {
       last_cie = this_cie;
       encoding = get_cie_encoding (this_cie);
       base = base_from_object (encoding, ob);
     }
 }

      if (encoding == 0x00)
 {
   const _Unwind_Ptr *pc_array = (const _Unwind_Ptr *) this_fde->pc_begin;
   pc_begin = pc_array[0];
   pc_range = pc_array[1];
   if (pc_begin == 0)
     continue;
 }
      else
 {
   _Unwind_Ptr mask;
   const unsigned char *p;

   p = read_encoded_value_with_base (encoding, base,
         this_fde->pc_begin, &pc_begin);
   read_encoded_value_with_base (encoding & 0x0F, 0, p, &pc_range);





   mask = size_of_encoded_value (encoding);
   if (mask < sizeof (void *))
     mask = (((_Unwind_Ptr) 1) << (mask << 3)) - 1;
   else
     mask = -1;

   if ((pc_begin & mask) == 0)
     continue;
 }

      if ((_Unwind_Ptr) pc - pc_begin < pc_range)
 return this_fde;
    }

  return ((void *)0);
}




static __inline__ const fde *
binary_search_unencoded_fdes (struct object *ob, void *pc)
{
  struct fde_vector *vec = ob->u.sort;
  size_t lo, hi;

  for (lo = 0, hi = vec->count; lo < hi; )
    {
      size_t i = (lo + hi) / 2;
      const fde *const f = vec->array[i];
      void *pc_begin;
      uaddr pc_range;
      memcpy (&pc_begin, (const void * const *) f->pc_begin, sizeof (void *));
      memcpy (&pc_range, (const uaddr *) f->pc_begin + 1, sizeof (uaddr));

      if (pc < pc_begin)
 hi = i;
      else if (pc >= pc_begin + pc_range)
 lo = i + 1;
      else
 return f;
    }

  return ((void *)0);
}

static __inline__ const fde *
binary_search_single_encoding_fdes (struct object *ob, void *pc)
{
  struct fde_vector *vec = ob->u.sort;
  int encoding = ob->s.b.encoding;
  _Unwind_Ptr base = base_from_object (encoding, ob);
  size_t lo, hi;

  for (lo = 0, hi = vec->count; lo < hi; )
    {
      size_t i = (lo + hi) / 2;
      const fde *f = vec->array[i];
      _Unwind_Ptr pc_begin, pc_range;
      const unsigned char *p;

      p = read_encoded_value_with_base (encoding, base, f->pc_begin,
     &pc_begin);
      read_encoded_value_with_base (encoding & 0x0F, 0, p, &pc_range);

      if ((_Unwind_Ptr) pc < pc_begin)
 hi = i;
      else if ((_Unwind_Ptr) pc >= pc_begin + pc_range)
 lo = i + 1;
      else
 return f;
    }

  return ((void *)0);
}

static __inline__ const fde *
binary_search_mixed_encoding_fdes (struct object *ob, void *pc)
{
  struct fde_vector *vec = ob->u.sort;
  size_t lo, hi;

  for (lo = 0, hi = vec->count; lo < hi; )
    {
      size_t i = (lo + hi) / 2;
      const fde *f = vec->array[i];
      _Unwind_Ptr pc_begin, pc_range;
      const unsigned char *p;
      int encoding;

      encoding = get_fde_encoding (f);
      p = read_encoded_value_with_base (encoding,
     base_from_object (encoding, ob),
     f->pc_begin, &pc_begin);
      read_encoded_value_with_base (encoding & 0x0F, 0, p, &pc_range);

      if ((_Unwind_Ptr) pc < pc_begin)
 hi = i;
      else if ((_Unwind_Ptr) pc >= pc_begin + pc_range)
 lo = i + 1;
      else
 return f;
    }

  return ((void *)0);
}

static const fde *
search_object (struct object* ob, void *pc)
{


  if (! ob->s.b.sorted)
    {
      init_object (ob);




      if (pc < ob->pc_begin)
 return ((void *)0);
    }

  if (ob->s.b.sorted)
    {
      if (ob->s.b.mixed_encoding)
 return binary_search_mixed_encoding_fdes (ob, pc);
      else if (ob->s.b.encoding == 0x00)
 return binary_search_unencoded_fdes (ob, pc);
      else
 return binary_search_single_encoding_fdes (ob, pc);
    }
  else
    {

      if (ob->s.b.from_array)
 {
   fde **p;
   for (p = ob->u.array; *p ; p++)
     {
       const fde *f = linear_search_fdes (ob, *p, pc);
       if (f)
  return f;
     }
   return ((void *)0);
 }
      else
 return linear_search_fdes (ob, ob->u.single, pc);
    }
}

const fde *
_Unwind_Find_FDE (void *pc, struct dwarf_eh_bases *bases)
{
  struct object *ob;
  const fde *f = ((void *)0);

  ;
  __gthread_mutex_lock (&object_mutex);




  for (ob = seen_objects; ob; ob = ob->next)
    if (pc >= ob->pc_begin)
      {
 f = search_object (ob, pc);
 if (f)
   goto fini;
 break;
      }


  while ((ob = unseen_objects))
    {
      struct object **p;

      unseen_objects = ob->next;
      f = search_object (ob, pc);


      for (p = &seen_objects; *p ; p = &(*p)->next)
 if ((*p)->pc_begin < ob->pc_begin)
   break;
      ob->next = *p;
      *p = ob;

      if (f)
 goto fini;
    }

 fini:
  __gthread_mutex_unlock (&object_mutex);

  if (f)
    {
      int encoding;
      _Unwind_Ptr func;

      bases->tbase = ob->tbase;
      bases->dbase = ob->dbase;

      encoding = ob->s.b.encoding;
      if (ob->s.b.mixed_encoding)
 encoding = get_fde_encoding (f);
      read_encoded_value_with_base (encoding, base_from_object (encoding, ob),
        f->pc_begin, &func);
      bases->func = (void *) func;
    }

  return f;
}
