#ifndef ANDY_IO_H
#define ANDY_IO_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "andy_types.h"
#include "andy_arena.h"
#include "andy_error.h"


void andy_set_sigDigits(int);
void andy_set_fieldsize(int);

void andy_print_newline(void);
void andy_print_int(int64_t x);
void andy_print_float(double x);
void andy_print_string(andy_string s);

andy_int     andy_read_int(void);
andy_float   andy_read_float(void);
andy_string* andy_read_string(arena_t* aptr);

void andy_press_enter(void);

#endif
