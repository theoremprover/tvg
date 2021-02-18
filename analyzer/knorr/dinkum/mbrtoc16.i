






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







extern int __attribute__((fardata)) _Errno;







typedef int errno_t;





typedef unsigned short char16_t;
typedef unsigned int char32_t;









typedef struct _Mbstatet
 {
 unsigned long _Wchar;
 unsigned short _Byte, _State;
 } _Mbstatet;


typedef _Mbstatet mbstate_t;
typedef _Sizet size_t;



size_t mbrtoc16(char16_t *, const char *,
 size_t, mbstate_t *);
size_t c16rtomb(char *, char16_t,
 mbstate_t *);

size_t mbrtoc32(char32_t *, const char *,
 size_t, mbstate_t *);
size_t c32rtomb(char *, char32_t,
 mbstate_t *);





size_t (mbrtoc16)(char16_t * pwc, const char * s,
 size_t nin, mbstate_t * pst)
 {
 unsigned char *su;
 char state = (char)pst->_State;
 unsigned long wc = pst->_Wchar;

 if (s == 0)
  {
  pwc = 0;
  s = "";
  nin = 1;
  }

 for (su = (unsigned char *)s; ; ++su, --nin)
  {
  if (state == 16)
   {
   if (pwc != 0)
    *pwc = (char16_t)(0xdc00 | wc & 0x03ff);
   pst->_State = 0;
   return ((size_t)-3);
   }
  else if (nin == 0)
   {
   pst->_Wchar = wc;
   pst->_State = state;
   return ((size_t)-2);
   }
  else if (0 < state)
   {
   if ((*su & 0xc0) != 0x80)
    {
    ( _Errno) = 0x0058;
    return ((size_t)-1);
    }
   wc = (unsigned long)((wc << 6) | (*su & 0x3f));
   --state;
   }
  else if ((*su & 0x80) == 0)
   wc = *su;
  else if ((*su & 0xe0) == 0xc0)
   {
   wc = (unsigned long)(*su & 0x1f);
   state = 1;
   }
  else if ((*su & 0xf0) == 0xe0)
   {
   wc = (unsigned long)(*su & 0x0f);
   state = 2;
   }
  else if ((*su & 0xf8) == 0xf0)
   {
   wc = (unsigned long)(*su & 0x07);
   state = 3;
   }
  else if ((*su & 0xfc) == 0xf8)
   {
   wc = (unsigned long)(*su & 0x03);
   state = 4;
   }
  else if ((*su & 0xfc) == 0xfc)
   {
   wc = (unsigned long)(*su & 0x03);
   state = 5;
   }
  else
   {
   ( _Errno) = 0x0058;
   return ((size_t)-1);
   }
  if (state == 0)
   {
   if (0x10ffff < wc)
    {
    ( _Errno) = 0x0058;
    return ((size_t)-1);
    }
   else if (0xffff < wc)
    {
    if (pwc != 0)
     *pwc = (char16_t)(0xd800 | (wc >> 10) - 0x0040);
    pst->_State = 16;
    return ((const char *)++su - s);
    }
   else
    {
    if (pwc != 0)
     *pwc = (char16_t)wc;
    pst->_State = 0;
    return (wc == 0 ? 0 : (const char *)++su - s);
    }
   }
  }
 }

