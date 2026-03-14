#include "andy_console.h"

void andy_clearScreen(void) {

  puts("\033[2J\033[H");
}

void andy_resetConsole(void) {

  puts("\x1b[0m");
}

void andy_clearLine(void) {

  puts("\x1b[2K");
}

void andy_bold(void) {

  puts("\x1b[1m");
}

void andy_italic(void) {

  puts("\x1b[3m");
}

void andy_underline(void) {

  puts("\x1b[4m");
}

void andy_blink(void) {

  puts("\x1b[5m");
}

void andy_reverse(void) {

  puts("\x1b[7m");
}

void andy_setForegroundRGB(andy_int r, andy_int g, andy_int b) {

  printf("\x1b[38;2;%d;%d;%dm", (int)r, (int)g, (int)b);
}

void andy_setBackgroundRGB(andy_int r, andy_int g, andy_int b) {

  printf("\x1b[48;2;%d;%d;%dm", (int)r, (int)g, (int)b);
}

