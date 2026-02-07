#ifndef ANDY_RUNTIME_H
#define ANDY_RUNTIME_H

#include "andy_arena.h"

void andy_runtime_init(void);
void andy_runtime_shutdown(void);
arena_t* andy_get_global_arena(void);

#endif
