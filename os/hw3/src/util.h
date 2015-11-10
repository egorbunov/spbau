#ifndef UTIL_H_INCLUDED__
#define UTIL_H_INCLUDED__

#include <stdint.h> 
#include <stddef.h>

enum color {
    BLACK = 0,
    BLUE, GREEN, CYAN, RED, MAGNETA, BROWN, GRAY, DARK_GRAY, BRIGHT_BLUE, BRIGHT_GREEN, BRIGHT_CYAN,
    BRIGHT_RED, BRIGHT_MAGNETA, YELLOW, WHITE
};

enum size {
    COLS = 80,
    ROWS = 25
};


void outb(uint16_t port, uint8_t data);
void io_wait();
uint8_t inb(uint16_t port);
void memset(void *ptr, int value, size_t num);
void putc(uint8_t x, uint8_t y, enum color fg, enum color bg, char c);
void clear(enum color bg);

#endif