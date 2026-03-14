#include "andy_console.h"

void andy_clearScreen(void) {

  puts("\033[2J\033[H");
}
