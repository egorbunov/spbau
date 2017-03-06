#ifndef UTIL_H_INCLUDED__
#define UTIL_H_INCLUDED__

#include <stdint.h>
#include <stddef.h>

#define CHECK_FLAG(flags,bit)   ((flags) & (1 << (bit)))

void outb(uint16_t port, uint8_t data);
void io_wait();
uint8_t inb(uint16_t port);


#endif