












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







typedef int errno_t;




typedef size_t rsize_t;


errno_t asctime_s(char *, rsize_t, const struct tm *);
errno_t ctime_s(char *, rsize_t, const time_t *);
struct tm *gmtime_s(const time_t *,
 struct tm *);
struct tm *localtime_s(const time_t *,
 struct tm *);









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













typedef struct _Mbstatet
 {
 unsigned long _Wchar;
 unsigned short _Byte, _State;
 } _Mbstatet;


typedef _Mbstatet mbstate_t;
struct tm;
struct _Dnk_filet;



typedef struct _Dnk_filet _Filet;





typedef _Wchart wchar_t;




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


wchar_t *wmemchr(const wchar_t *, wchar_t, size_t);




double wcstod(const wchar_t *, wchar_t **);
unsigned long wcstoul(const wchar_t *, wchar_t **, int);


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















typedef _Sizet wctrans_t;


typedef _Sizet wctype_t;





int _Iswctype(wint_t, wctype_t);
wint_t _Towctrans(wint_t, wctrans_t);



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





wctrans_t wctrans(const char *);
wctype_t wctype(const char *);


wint_t (towctrans)(wint_t, wctrans_t);











typedef struct
 {
 const unsigned short *_Tab[16];
 } _Statab;





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



size_t (wcsftime)(wchar_t * buf, size_t bufsize,
 const wchar_t * fmt, const struct tm * t)
 {
 int ch;
 const wchar_t *ibuf = buf;

 while (0 < bufsize && fmt[0] != L'\0')
  if (fmt[0] != L'%' || (ch = _Wctob(fmt[1])) <= 0)
   *buf++ = *fmt++, --bufsize;
  else
   {
   char nfmt[3];
   int n = 2;

   nfmt[0] = '%', nfmt[1] = (char)ch, fmt += 2;
   if ((ch == 'E' || ch == 'O') && fmt[0] != L'\0')
    nfmt[n++] = (char)_Wctob(*fmt++);
   if ((n = (int)_Wcsftime(buf, bufsize, nfmt, n, t)) < 0)
    return (0);
   buf += n, bufsize -= n;
   }
 if (bufsize == 0)
  return (0);
 *buf = L'\0';
 return (buf - (wchar_t *)ibuf);
 }

