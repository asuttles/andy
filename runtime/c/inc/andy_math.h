#ifndef ANDY_MATH_H
#define ANDY_MATH_H

#include <math.h>
#include <stdint.h>
#include "andy_types.h"

// Floating Point Functions
andy_float andy_sqrt(andy_float x);
andy_float andy_abs(andy_float x);
andy_float andy_neg(andy_float x);
andy_float andy_min(andy_float x, andy_float y);
andy_float andy_max(andy_float x, andy_float y);
andy_float andy_ceil(andy_float x);
andy_float andy_floor(andy_float x);
andy_float andy_pow(andy_float x, andy_float y);
andy_float andy_sin(andy_float x);
andy_float andy_cos(andy_float x);
andy_float andy_tan(andy_float x);
andy_float andy_exp(andy_float x);
andy_float andy_ln(andy_float x);
andy_float andy_i2f(andy_int x);

// Integer Functions
andy_int andy_f2i(andy_float x);

#endif
