#include "string.h"

void memset(void *ptr, int value, size_t num)
{
    int8_t *p = (int8_t*)ptr;
    for (int i = 0; i < num; i++)
    {
        *p++ = value;
    }
}