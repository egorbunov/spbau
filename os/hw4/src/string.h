#ifndef STRING_H_INCLUDED__
#define STRING_H_INCLUDED__

#include <stddef.h>
#include <stdint.h>

void memset(void *ptr, int value, size_t num);
void itoa_hex(int32_t val, char *dest, int32_t buf_size);
void ltoa_hex(int64_t val, char *dest, int32_t buf_size);

#endif