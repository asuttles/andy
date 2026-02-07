#ifndef ANDY_ARENA_H
#define ANDY_ARENA_H

#include <stddef.h>
#include <stdint.h>

typedef struct Arena arena_t;

arena_t* arena_create(size_t size);
void* arena_alloc(arena_t* aptr, size_t size, size_t align);
void  arena_reset(arena_t* aptr);
void  arena_destroy(arena_t* aptr);

#endif
