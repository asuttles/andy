#ifndef ANDY_TYPES_H
#define ANDY_TYPES_H

#include <stddef.h>
#include <stdint.h>

/* Type tags (for unions) */
typedef enum {
    ANDY_TYPE_INT,
    ANDY_TYPE_FLOAT,
    ANDY_TYPE_STRING,
    ANDY_TYPE_ARRAY,
    ANDY_TYPE_FUNCTION
} andy_type_tag;

/* Scalar Types */
typedef int64_t andy_int;
typedef double  andy_float;

/* String Type (immutable) */
typedef struct {
  size_t length;
  char *data;
} andy_string;

/* Array */
typedef struct {
  andy_type_tag elem_type;
  size_t elem_size;
  size_t length;
  void*  data;
} andy_array;

typedef void* (*andy_fn_ptr)(void* env, ...);

typedef struct {
  andy_fn_ptr code;
  void* env;
} andy_function;

#endif /* ANDY_TYPES_H */
