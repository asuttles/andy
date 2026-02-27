#include "andy_math.h"

// Floating Point Functions
andy_float andy_sqrt(andy_float x) { return sqrt(x); }
andy_float andy_abs(andy_float x) { return (x < 0.0) ? -x : x; }
andy_float andy_neg(andy_float x) { return -1.0 * x; }
andy_float andy_min(andy_float x, andy_float y) { return (x < y) ? x : y; }
andy_float andy_max(andy_float x, andy_float y) { return (x > y) ? x : y; }
andy_float andy_ceil(andy_float x) { return ceil(x); }
andy_float andy_floor(andy_float x) { return floor(x); }
andy_float andy_pow(andy_float x, andy_float y) { return pow(x, y); }
andy_float andy_sin(andy_float x) { return sin(x); }
andy_float andy_cos(andy_float x) { return cos(x); }
andy_float andy_tan(andy_float x) { return tan(x); }
andy_float andy_exp(andy_float x) { return exp(x); }
andy_float andy_ln(andy_float x) { return log(x); }
andy_float andy_i2f(andy_int x) { return (andy_float)x; }

// Integer Functions
// note: f2i rounds instead of truncation
andy_int andy_f2i(andy_float x) { return (andy_int)llround(x); }
