#ifndef THREADS_H_INCLUDED__
#define THREADS_H_INCLUDED__

#include <stdint.h>

#define STACK_SIZE 1024

typedef struct stack_t {
	char data[STACK_SIZE];
} stack_t;

typedef struct thread_context_t {
	uint32_t eflags;
	uint32_t edi;
	uint32_t esi;
	uint32_t ebx;
	uint32_t ebp;
	uint32_t eip;
} thread_context_t;

typedef void (*thread_fun_ptr)(void);

// returns thread id
int32_t threads_add(thread_fun_ptr pthread);
void threads_switch();
void manual_thread_switch();

#endif