






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











typedef struct
 {
 const unsigned short *_Tab[16];
 } _Statab;



typedef struct Xfrm {
 const unsigned char *sbegin;
 const unsigned char *sin;
 const unsigned char *send;
 long weight;
 unsigned short phase, state, wc;
 } Xfrm;



int _Strcollx(const char *, const char *, _Statab *);
size_t _Strxfrmx(char *, const char *, size_t, _Statab *);

size_t _CStrxfrm(char *, size_t, Xfrm *, _Statab *);



size_t _CStrxfrm(char *sout, size_t size, Xfrm *ps,
 _Statab *pcostate)
 {
 size_t nout = 0;

 if (pcostate->_Tab[0] == 0)
  {
  for (ps->sin = ps->sbegin; nout < size; ++ps->sin, ++sout)
   {
   ++nout;
   if ((*sout = *ps->sin) == '\0')
    break;
   }
  ps->sbegin = ps->sin;
  return (nout);
  }
 else if ((pcostate->_Tab[0][0] & 0xfff) != 0)
  {
  unsigned short ignores = 0;

  if (ps->state == 0)
   {
   ps->state = pcostate->_Tab[0][0] & ~0xfff;
   if ((ps->state & 0x4000) == 0)
    ps->sin = ps->sbegin;
   else
    {
    ps->sin += strlen((const char *)ps->sin);
    ps->send = ps->sin;
    }
   }
  for (; nout < size; )
   {
   unsigned short code;

   if (ps->weight != 0)
    if (ignores != 0)
     {
     sout[nout++] = (char)ignores;
     ignores = 0;
     }
    else if (0x80 <= ps->weight)
     {

     sout[nout++] = (char)(0x80 | (ps->weight >> 6));
     ps->weight = 0x40 | (ps->weight & 0x3f);
     }
    else
     {
     sout[nout++] = (char)ps->weight;
     ps->weight = 0;
     }
   else if (ps->wc == 0)
    {
    if ((ps->state & 0x4000) == 0)
     ps->wc = *ps->sin == '\0' ? '\0' : *ps->sin++;
    else
     ps->wc = ps->sin <= ps->sbegin ? '\0' : *--ps->sin;
    if (ps->wc != '\0')
     ps->wc |= ps->phase << 8;
    else if ((pcostate->_Tab[ps->phase][0] & 0x1000) == 0)
     {
     sout[nout++] = '\0';
     break;
     }
    else
     {
     ignores = 0;
     sout[nout++] = '\1';
     ps->state = pcostate->_Tab[++ps->phase][0] & ~0xfff;
     if ((ps->state & 0x4000) == 0)
      ps->sin = ps->sbegin;
     else if (ps->send != 0)
      ps->sin = ps->send;
     else
      {
      ps->sin += strlen((const char *)ps->sin);
      ps->send = ps->sin;
      }
     }
    }
   else if ((code = pcostate->_Tab[ps->wc >> 8][ps->wc & 0xff])
    & 0x8000)
    ps->wc = code & 0xfff;
   else if (code & 0x2000 || code == 0)
    {
    ps->weight = code & 0xfff;

    if (code & 0x1000)
     ++ps->wc;
    else
     ps->wc = 0;
    }
   else if (code & 0x4000)
    {
    int match;

    if ((ps->state & 0x4000) == 0)
     for (match = 1; code & 0x4000; ++ps->wc,
      code = pcostate->_Tab[ps->wc >> 8][ps->wc & 0xff])
      {
      if (match == 0)
       ;
      else if (ps->sin[match - 1] == '\0')
       match = 0;
      else if (ps->sin[match - 1] == (code & 0xff))
       ++match;
      else
       match = 0;
      }
    else
     for (match = -1; code & 0x4000; ++ps->wc,
      code = pcostate->_Tab[ps->wc >> 8][ps->wc & 0xff])
      {
      if (match == 0)
       ;
      else if (&ps->sin[match + 1] <= ps->sbegin)
       match = 0;
      else if (ps->sin[match + 1] == (code & 0xff))
       --match;
      else
       match = 0;

      }
    if (match == 0)
     ++ps->wc;
    else if ((ps->state & 0x4000) == 0)
     ps->sin += match - 1;
    else
     ps->sin += match + 1;
    }
   else if ((ps->state & 0x2000) && ++ignores == 0xff)
    {
    sout[nout++] = (char)ignores;
    ignores = 0;
    }
   }
  return (nout);
  }
 else
  {
  int limit = 0;

  for (; nout < size; )
   {
   unsigned short code;
   const unsigned short *stab;

   if (16 <= ps->state
    || (stab = pcostate->_Tab[ps->state]) == 0
    || (16*0xff) <= ++limit
    || (code = stab[*ps->sin]) == 0)
    {
    sout[nout++] = '\0';
    ps->state = 16;
    return (nout);
    }
   ps->state = (code & 0x0f00) >> 8;
   if (code & 0x8000)
    ps->wc = ps->wc & ~0xff | code & 0x00ff;
   if (code & 0x1000)
    ps->wc = ps->wc >> 8 & 0xff | ps->wc << 8;
   if (code & 0x4000)
    {
    limit = 0;
    if (*ps->sin != '\0')
     ++ps->sin;
    else
     ps->sin = ps->sbegin;
    }
   if (code & 0x2000
    && (sout[nout++] = (code & 0x00ff) ? code : ps->wc) == '\0')
    return (nout);
   }
  return (nout);
  }
 }

