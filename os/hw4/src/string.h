#ifndef STRING_H_INCLUDED__
#define STRING_H_INCLUDED__

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

void memset(void *ptr, int value, size_t num);
void itoa (char *buf, char base, int d);
int strlen(const char* cstr);
int str_find(const char* str, const char* what, int from, int to);
bool strcmp(const char* str1, const char* str2, int from, int to);
#endif