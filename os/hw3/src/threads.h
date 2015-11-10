#ifndef THREADS_H_INCLUDED__
#define THREADS_H_INCLUDED__

#include <stdint.h>

typedef void (*thread_ptr)(void);

// returns thread id
int32_t threads_add(thread_ptr pthread, void* stack);
__attribute__ ((fastcall)) int32_t threads_switch(int32_t);

#endif