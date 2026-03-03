#ifndef ANDY_RANDOM_H
#define ANDY_RANDOM_H

#include <stdint.h>
#include <time.h>

#include "andy_types.h"

void andy_seed(andy_int seed);
void andy_randomize(void);
andy_float andy_random(void);
andy_int andy_random_int(andy_int max);

#endif
