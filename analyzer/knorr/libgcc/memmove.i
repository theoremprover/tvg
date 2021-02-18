extern void * memmove (void *dest, const void *src, long unsigned int len);

void *
memmove (void *dest, const void *src, long unsigned int len)
{
  unsigned char *d = dest;
  const unsigned char *s = (const unsigned char *)src;

  if (d < s)
    {
      while (len--)
        *d++ = *s++;
    }
  else
    {
      s = s + (len-1);
      d = d + (len-1);
      while (len--)
        *d-- = *s--;
    }
  return dest;
}
