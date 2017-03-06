#include "util.h"

static uint16_t *const video = (uint16_t*) 0xB8000;

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

void memset(void *ptr, int value, size_t num)
{
    int8_t *p = (int8_t*)ptr;
    for (int i = 0; i < num; i++)
    {
        *p++ = value;
    }
}

void putc(uint8_t x, uint8_t y, enum color fg, enum color bg, char c) 
{
    video[y * COLS + x] = (bg << 12) | (fg << 8) | c;
}

void clear(enum color bg) 
{
    uint8_t x, y;
    for (y = 0; y < ROWS; y++)
        for (x = 0; x < COLS; x++)
            putc(x, y, bg, bg, ' ');
}