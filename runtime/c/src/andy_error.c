#include <stdio.h>
#include <stdlib.h>

#include "andy_error.h"

void andy_warn(const char *msg) {

  fprintf(stderr, "Andy Runtime Error: %s\n", msg);
}

void andy_panic(const char *msg) {

  fprintf(stderr, "Andy Runtime Error: %s\n", msg);
  exit(EXIT_FAILURE);
}

void andy_assert(int cond, const char *msg) {

  if (!cond)
    andy_panic(msg);
}
