#ifndef UTIL_H_INCLUDED__
#define UTIL_H_INCLUDED__

#include <stdint.h>
#include <stddef.h>

void outb(uint16_t port, uint8_t data);
void io_wait();
uint8_t inb(uint16_t port);


#endif