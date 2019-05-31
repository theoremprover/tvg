# 1 "/arm-libs/library-src/dinkum/source/./mbrtoc16.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./mbrtoc16.c" 2

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
# 3 "/arm-libs/library-src/dinkum/source/./mbrtoc16.c" 2
# 1 "/toolchain/arm/include/uchar.h" 1 3
# 29 "/toolchain/arm/include/uchar.h" 3
typedef unsigned short char16_t;
typedef unsigned int char32_t;
# 40 "/toolchain/arm/include/uchar.h" 3
typedef struct _Mbstatet
 {
 unsigned long _Wchar;
 unsigned short _Byte, _State;
# 71 "/toolchain/arm/include/uchar.h" 3
 } _Mbstatet;


typedef _Mbstatet mbstate_t;
# 83 "/toolchain/arm/include/uchar.h" 3
typedef _Sizet size_t;



size_t mbrtoc16(char16_t *, const char *,
 size_t, mbstate_t *);
size_t c16rtomb(char *, char16_t,
 mbstate_t *);

size_t mbrtoc32(char32_t *, const char *,
 size_t, mbstate_t *);
size_t c32rtomb(char *, char32_t,
 mbstate_t *);
# 4 "/arm-libs/library-src/dinkum/source/./mbrtoc16.c" 2




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
    *pwc = (char16_t)(0xdc00 | (wc & 0x03ff));
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
