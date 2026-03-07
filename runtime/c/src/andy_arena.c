#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "andy_arena.h"

typedef struct Buffer buffer_t;
struct Buffer {
  uint8_t*  data;		/* Start of Raw Memory Block */
  size_t    size;		/* Size of Block in Bytes */
  size_t    offset;		/* Allocation Offset */
  buffer_t* next;		/* Link to NEXT buffer */
};

struct Arena {
  buffer_t* head;		/* First Buffer in Arena */
  buffer_t* current;		/* Current Buffer for Allocation */
};

/* ------------------
   Private Functions
   ------------------ */

/* Create a Memory Buffer */
static buffer_t* create_buffer(size_t size) {

  buffer_t* b = (buffer_t*)malloc(sizeof(buffer_t));
  if (b == NULL) {
    perror("Andy Runtime Error: Failed to allocate buffer struct");
    exit(EXIT_FAILURE);
  }

  b->data   = (uint8_t*)malloc(size);
  if (b->data == NULL) {
    perror("Andy Runtime Error: Failed to allocate buffer memory");
    free(b);
    exit(EXIT_FAILURE);
  }

  b->size   = size;
  b->offset = 0;
  b->next   = NULL;

  return b;
}

/* Destroy Memory Buffers */
static void destroy_buffer(buffer_t* bptr) {

  /* Recursively destory buffers */
  if (bptr->next) destroy_buffer(bptr->next);

  /* Free Memory and Struct */
  free(bptr->data);
  free(bptr);
}

/* Add additional Buffer to Arena APTR */
static void expand_arena(arena_t* aptr, size_t data_size) {

  size_t buff_size = aptr->current->size * 2;
  if (buff_size < data_size)
    buff_size = data_size;

  buffer_t* bptr = create_buffer(buff_size);
  aptr->current->next = bptr;
  aptr->current = bptr;
}

/* ------------------
      Public API
   ------------------ */

/* Create an Arena allocation of size SIZE */
arena_t* arena_create(size_t size) {

  arena_t* a = malloc(sizeof(arena_t));
  if (!a) {
    perror("Andy Runtime Error: Failed to allocate arena");
    exit(EXIT_FAILURE);
  }  

  a->head = a->current = create_buffer(size);

  return a;
}


/* Reset an Arena */
void arena_reset(arena_t* aptr) {

    /* Guard against a NULL pointer */
    if (!aptr || !aptr->head) return;
  
    /* Recursively destory all but Head Buffer */
    if (aptr->head->next)
	destroy_buffer(aptr->head->next);

    /* Reset the Arena Head Buffer */
    aptr->head->offset = 0;
    aptr->head->next = NULL;
    aptr->current = aptr->head; 
}

/* Destroy a Arena */
void arena_destroy(arena_t* aptr) {

  /* Guard against a NULL pointer */
  if (!aptr) return;
  
  /* Destory all Buffers */
  if (aptr->head)
    destroy_buffer(aptr->head);

  free(aptr);
}

/* Allocate a Section of the Arena of SIZE, with alignment ALIGN */
void* arena_alloc(arena_t* aptr, size_t data_size, size_t align) {

    buffer_t* bptr = aptr->current;
    size_t offset = bptr->offset;

    /* Align the Offset */
    size_t misalign = offset % align;
    if (misalign != 0)
	offset += align - misalign;
  
    /* Generate new Buffer, if needed */
    if (offset + data_size > bptr->size) {
	expand_arena(aptr, data_size);
	bptr = aptr->current;
	offset = 0;
    }

    /* Update the Offset and return the Pointer*/
    void* ptr = bptr->data + offset;
    bptr->offset = offset + data_size;
    return ptr;
}
