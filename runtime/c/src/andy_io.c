#include "andy_io.h"


#define BUFF_LEN 256

// Module Private State Data
static int andy_field_size = 5;	        /* field size for numeric printing */
static int andy_sig_digits = 3;		/* significant digits for floats   */

// State Setters
void andy_set_fieldsize(int size) {

    andy_field_size = size;
}

void andy_set_sigDigits(int size) {

    andy_sig_digits = size;
}


// Write

void andy_print_newline(void) {

    printf("\n");
}

void andy_print_int(andy_int x) {

    printf("%*lld", andy_field_size, (long long)x);
}

void andy_print_float(andy_float x) {

    printf("%*.*f", andy_field_size, andy_sig_digits, (double)x);
}

void andy_print_string(andy_string s) {

    printf("%s", s.data);
}

// Read

/* Read an integer from stdin */
andy_int andy_read_int(void) {
    
    char buff[128];
    char *endPtr;		/* Point 1 char beyond numeric seq */

    if (!fgets(buff, sizeof(buff), stdin))
        andy_panic("error: failed to read integer from stdin");

    long long tmp = strtoll(buff, &endPtr, 10);

    if (endPtr == buff)          /* no digits */
	andy_panic("error: failed to read integer from stdin");

    return (andy_int)tmp;
}


/* Read a float from stdin */
andy_float andy_read_float() {
    
    char buff[128];
    char *endPtr;

    if (!fgets(buff, sizeof buff, stdin))
        andy_panic("error: failed to read float from stdin");

    double tmp = strtod(buff, &endPtr);

    if (endPtr == buff)
	andy_panic("error: failed to read float from stdin");

    return (andy_float)tmp;
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

void andy_press_enter(void) {

  puts("Press Enter to continue.");
  getchar();
}
