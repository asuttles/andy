#ifndef ANDY_ERROR_H
#define ANDY_ERROR_H

void andy_warn(const char *msg);
void andy_panic(const char *msg);
void andy_assert(int cond, const char *msg);

#endif
