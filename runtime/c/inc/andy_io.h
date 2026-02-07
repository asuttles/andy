#ifndef ANDY_IO_H
#define ANDY_IO_H

#include "andy_types.h"

void andy_print_int(int64_t x);
void andy_print_float(double x);
void andy_print_string(andy_string s);
andy_string* andy_read_string(arena_t* aptr);

#endif
