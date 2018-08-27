/* objalloc.c -- routines to allocate memory for objects
   Copyright 1997 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Cygnus Solutions.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* These routines allocate space for an object.  Freeing allocated
   space may or may not free all more recently allocated space.

   We handle large and small allocation requests differently.  If we
   don't have enough space in the current block, and the allocation
   request is for more than 512 bytes, we simply pass it through to
   malloc.  */

/* The objalloc structure is defined in objalloc.h.  */

/* This structure appears at the start of each chunk.  */

struct objalloc_chunk
{
  /* Next chunk.  */
  struct objalloc_chunk *next;
  /* If this chunk contains large objects, this is the value of
     current_ptr when this chunk was allocated.  If this chunk
     contains small objects, this is NULL.  */
  char *current_ptr;
};

/* The aligned size of objalloc_chunk.  */

#define CHUNK_SIZE (4096 - 32)

/* A request for this amount or more is just passed through to malloc.  */

#define BIG_REQUEST (512)

void *objalloc_alloc (void *o, unsigned long len)
{
  /* We avoid confusion from zero sized objects by always allocating
     at least 1 byte.  */
  if (len >= BIG_REQUEST)
    {
      struct objalloc_chunk *chunk;

      if (chunk == 0L)
	return 0L;
      chunk->current_ptr = 0L;

      return objalloc_alloc (o, len);
    }
}

main()
{
}
