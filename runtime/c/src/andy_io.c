#include <stdio.h>
#include <string.h>

#include "andy_arena.h"
#include "andy_error.h"
#include "andy_io.h"


#define BUFF_LEN 256

void andy_print_newline(void) {

    printf("/n");
}

void andy_print_int(int64_t x) {

  printf("%lli", x);
}

void andy_print_float(double x) {

  printf("%.3f", x);
}

void andy_print_string(andy_string s) {
  printf("%s", s.data);
}

/* Read input string from stdin */
andy_string* andy_read_string(arena_t* aptr) {

  char buff[BUFF_LEN];

  /* Allocate the Andy String data type */
  andy_string* str = arena_alloc(aptr, sizeof(andy_string), alignof(andy_string));
  if (!str)
    andy_panic("unable to allocate string struc");

  /* Read the input string */
  if (!fgets(buff, sizeof(buff), stdin))
    andy_panic("Error: Unable to read string input");

  size_t len = strlen(buff);

  /* Truncate long strings */
  if (len == BUFF_LEN - 1 && buff[len - 1] != '\n') {
    andy_warn("Input truncated: Input string is longer than string buffer");

    /* Consume remainder of input string */
    int c;
    while ((c = getchar()) != '\n' && c != EOF) {}    
  }

  /* Null terminate the string */
  if (len > 0 && buff[len - 1] == '\n') {
    buff[--len] = '\0';
  }

  /* Allocate the string data in the arena */
  str->length = len;
  str->data = arena_alloc(aptr, len + 1, alignof(char));

  if (!str->data)
    andy_panic("Error: Unable to allocate string data");

  /* Copy string buffer to allocation */
  memcpy(str->data, buff, len + 1);
  
  return str;
}
