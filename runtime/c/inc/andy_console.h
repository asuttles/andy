#ifndef ANDY_CONSOLE_H
#define ANDY_CONSOLE_H

#include <stdio.h>

#include "andy_types.h"

void andy_clearScreen(void);
void andy_resetConsole(void);
void andy_clearLine(void);
void andy_bold(void);
void andy_italic(void);
void andy_underline(void);
void andy_blink(void);
void andy_reverse(void);
void andy_setForegroundRGB(andy_int r, andy_int g, andy_int b);
void andy_setBackgroundRGB(andy_int r, andy_int g, andy_int b);

#endif
