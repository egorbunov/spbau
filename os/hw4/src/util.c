#include "util.h"


void outb(uint16_t port, uint8_t data)
{
    __asm__ __volatile__ ("outb %0,%1"::"a"(data), "Nd" (port));
}

void io_wait()
{
    __asm__ __volatile__ ("jmp 1f;1:jmp 2f;2:");
}

uint8_t inb(uint16_t port)
{
    unsigned char ret;
    __asm__ __volatile__ ("inb %1,%0":"=a"(ret):"Nd"(port));
    return ret;
}