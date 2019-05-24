# 1 "/arm-libs/library-src/dinkum/source/./xstrerro.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./xstrerro.c" 2

# 1 "/toolchain/arm/include/errno.h" 1 3




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
# 6 "/toolchain/arm/include/errno.h" 2 3
# 490 "/toolchain/arm/include/errno.h" 3
extern int _Errno;
# 507 "/toolchain/arm/include/errno.h" 3
typedef int errno_t;
# 3 "/arm-libs/library-src/dinkum/source/./xstrerro.c" 2
# 1 "/toolchain/arm/include/string.h" 1 3
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
# 134 "/toolchain/arm/include/string.h" 3
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
# 4 "/arm-libs/library-src/dinkum/source/./xstrerro.c" 2
# 1 "/toolchain/arm/include/xtls.h" 1 3



# 1 "/toolchain/arm/include/xmtx.h" 1 3



# 1 "/toolchain/arm/include/yvals.h" 1 3
# 5 "/toolchain/arm/include/xmtx.h" 2 3
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
# 6 "/toolchain/arm/include/xmtx.h" 2 3



typedef void *_Rmtx;
# 35 "/toolchain/arm/include/xmtx.h" 3
void _Mtxinit(_Rmtx *);
void _Mtxdst(_Rmtx *);
void _Mtxlock(_Rmtx *);
void _Mtxunlock(_Rmtx *);
# 47 "/toolchain/arm/include/xmtx.h" 3
typedef char _Once_t;
# 5 "/toolchain/arm/include/xtls.h" 2 3


typedef void (*_Tlsdtor_t)(void *);
int _Atthreadexit(void (*)(void));
void _Destroytls(void);
# 5 "/arm-libs/library-src/dinkum/source/./xstrerro.c" 2





static char sbuf[sizeof ("error xxxx")]; static int (*_Tls_setup_sbuf)(void) = 0;




static char *posix_name[] = {
 "argument_list_too_long",
 "permission_denied",
 "address_in_use",
 "address_not_available",
 "address_family_not_supported",
 "resource_unavailable_try_again",
 "connection_already_in_progress",
 "bad_file_descriptor",
 "bad_message",
 "device_or_resource_busy",
 "operation_canceled",
 "no_child_process",
 "connection_aborted",
 "connection_refused",
 "connection_reset",
 "resource_deadlock_would_occur",
 "destination_address_required",
 "argument_out_of_domain",
 "file_exists",
 "bad_address",
 "file_too_large",
 "host_unreachable",
 "identifier_removed",
 "illegal_byte_sequence",
 "operation_in_progress",
 "interrupted",
 "invalid_argument",
 "io_error",
 "already_connected",
 "is_a_directory",
 "too_many_synbolic_link_levels",
 "too_many_files_open",
 "too_many_links",
 "message_size",
 "filename_too_long",
 "network_down",
 "network_reset",
 "network_unreachable",
 "too_many_files_open_in_system",
 "no_buffer_space",
 "no_message_available",
 "no_such_device",
 "no_such_file_or_directory",
 "executable_format_error",
 "no_lock_available",
 "no_link",
 "not_enough_memory",
 "no_message",
 "no_protocol_option",
 "no_space_on_device",
 "no_stream_resources",
 "not_a_stream",
 "function_not_supported",
 "not_connected",
 "not_a_directory",
 "directory_not_empty",
 "state_not_recoverable",
 "not_a_socket",
 "not_supported",
 "inappropriate_io_control_operation",
 "no_such_device_or_address",
 "operation_not_supported",
 "other",
 "value_too_large",
 "owner_dead",
 "operation_not_permitted",
 "broken_pipe",
 "protocol_error",
 "protocol_not_supported",
 "wrong_protocol_type",
 "result_out_of_range",
 "read_only_file_system",
 "invalid_seek",
 "no_such_process",
 "stream_timeout",
 "timed_out",
 "text_file_busy",
 "operation_would_block",
 "cross_device_link",
 };

static char hex(int code)
 {
 code &= 0xf;
 return ((char)(code < 10 ? code + '0' : code - 10 + 'a'));
 }

char *_Strerror(int errcode, char *buf)
 {
 switch (errcode)
  {

 case 0x0007:
  return ("argument list too long");



 case 0x000D:
  return ("permission denied");



 case 0x000B:
  return ("resource temporarily unavailable");



 case 0x0009:
  return ("bad file descriptor");



 case 0x004D:
  return ("bad message");



 case 0x0010:
  return ("resource busy");



 case 0x002F:
  return ("operation canceled");



 case 0x000A:
  return ("no child processes");



 case 0x002D:
  return ("resource deadlock avoided");



 case 0x0021:
  return ("domain error");



 case 0x0011:
  return ("file exists");



 case 0x000E:
  return ("bad address");



 case 0x001B:
  return ("file too large");



 case 0x0098:
  return ("file positioning error");



 case 0x0058:
  return ("multibyte encoding error");



 case 0x0096:
  return ("operation in progress");



 case 0x0004:
  return ("interrupted function call");



 case 0x0016:
  return ("invalid argument");



 case 0x0005:
  return ("input/output error");



 case 0x0015:
  return ("is a directory");



 case 0x0018:
  return ("too many open files");



 case 0x001F:
  return ("too many links");



 case 0x0061:
  return ("bad message buffer length");



 case 0x004E:
  return ("filename too long");



 case 0x0017:
  return ("too many files open in system");



 case 0x0013:
  return ("no such device");



 case 0x0002:
  return ("no such file or directory");



 case 0x0008:
  return ("exec format error");



 case 0x002E:
  return ("no locks available");



 case 0x000C:
  return ("not enough space");



 case 0x001C:
  return ("no space left on device");



 case 0x0059:
  return ("function not implemented");



 case 0x0014:
  return ("not a directory");



 case 0x005D:
  return ("directory not empty");



 case 0x0030:
  return ("not supported");



 case 0x0019:
  return ("bad I/O control operation");



 case 0x0006:
  return ("no such device or address");



 case 0x0001:
  return ("operation not permitted");



 case 0x0020:
  return ("broken pipe");



 case 0x0022:
  return ("range error");



 case 0x001E:
  return ("read-only file system");



 case 0x001D:
  return ("invalid seek");



 case 0x0003:
  return ("no such process");



 case 0x0091:
  return ("operation timed out");



 case 0x0012:
  return ("improper link");


 case 0:
  return ("no error");

 default:
  if (1000 <= errcode && errcode < (1000 + sizeof (posix_name) / sizeof (posix_name[0])))
   return (posix_name[errcode - 1000]);

  if (buf == 0)
   buf = ((_Tls_setup_sbuf && _Tls_setup_sbuf()), (&(sbuf[0])));
  strcpy(buf, "error xxxx");
  buf[9] = hex(errcode);
  buf[8] = hex(errcode >>= 4);
  buf[7] = hex(errcode >>= 4);
  buf[6] = hex(errcode >>= 4);
  return (&buf[0]);
  }
 }
