# 1 "/arm-libs/library-src/dinkum/source/./c16rtomb.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./c16rtomb.c" 2

# 1 "/toolchain/arm/include/uchar.h" 1 3




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
# 6 "/toolchain/arm/include/uchar.h" 2 3
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
# 3 "/arm-libs/library-src/dinkum/source/./c16rtomb.c" 2


size_t (c16rtomb)(char * s, char16_t c16,
 mbstate_t * pst)
 {
 int nextra;
 char buf[6];
 unsigned char *su;
 char state = (char)pst->_State;
 unsigned long wc = pst->_Wchar;

 if (s == 0)
  {
  s = &buf[0];
  c16 = 0;
  }
 su = (unsigned char *)s;

 if (state != 0)
  {
  if (c16 < 0xdc00 || 0xe000 <= c16)
   return ((size_t)-1);
  pst->_State = 0;
  wc |= (unsigned long)(c16 - 0xdc00);
  }
 else if (c16 < 0xd800 || 0xdc00 <= c16)
  wc = (unsigned long)c16;
 else
  {
  pst->_State = 1;
  pst->_Wchar = (unsigned long)((c16 - 0xd800 + 0x0040) << 10);
  return (0);
  }

 if ((wc & ~0x7fUL) == 0)
  {
  *su++ = (unsigned char)wc;
  nextra = 0;
  }
 else if ((wc & ~0x7ffUL) == 0)
  {
  *su++ = (unsigned char)(0xc0 | wc >> 6);
  nextra = 1;
  }
 else if ((wc & ~0xffffUL) == 0)
  {
  *su++ = (unsigned char)(0xe0 | wc >> 12);
  nextra = 2;
  }
 else if ((wc & ~0x1fffffUL) == 0)
  {
  *su++ = (unsigned char)(0xf0 | wc >> 18);
  nextra = 3;
  }
 else if ((wc & ~0x3ffffffUL) == 0)
  {
  *su++ = (unsigned char)(0xf8 | wc >> 24);
  nextra = 4;
  }
 else
  {
  *su++ = (unsigned char)(0xfc | ((wc >> 30) & 0x03));
  nextra = 5;
  }

 for (; 0 < nextra; )
  *su++ = (unsigned char)(0x80 | ((wc >> 6 * --nextra) & 0x3f));
 return ((char *)su - s);
 }
