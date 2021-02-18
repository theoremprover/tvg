






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
  *su++ = (unsigned char)(0xfc | (wc >> 30) & 0x03);
  nextra = 5;
  }

 for (; 0 < nextra; )
  *su++ = (unsigned char)(0x80 | (wc >> 6 * --nextra) & 0x3f);
 return ((char *)su - s);
 }

