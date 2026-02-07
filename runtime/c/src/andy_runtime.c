#include "andy_arena.h"
#include "andy_runtime.h"

#define INIT_BUFF_SZ 65536

static arena_t* ARENA_PTR;


void andy_runtime_init(void) {

  if (!ARENA_PTR)
    ARENA_PTR = arena_create( INIT_BUFF_SZ );  
}
  
void andy_runtime_shutdown(void) {

  if (ARENA_PTR) {
    arena_destroy(ARENA_PTR);
    ARENA_PTR = NULL;
  }
}

arena_t* andy_get_global_arena(void) {

  return ARENA_PTR;
}
