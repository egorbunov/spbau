#ifndef STRING_H_INCLUDED__
#define STRING_H_INCLUDED__

#include <stddef.h>
#include <stdint.h>

void memset(void *ptr, int value, size_t num);
void itoa (char *buf, char base, int d);

#endif