#include "vga.h"

static uint16_t *const video = (uint16_t*) 0xB8000;

void putc(uint8_t x, uint8_t y, enum color fg, enum color bg, char c)  {
    video[y * COLS + x] = (bg << 12) | (fg << 8) | c;
}

void prints(uint8_t x, uint8_t y, enum color fg, enum color bg, const char *c) {
    int i = 0;
    while (c[i] != '\0') {
        if (c[i] == '\n') {
            y += 1;
            x = 0;
        }
        else {
            putc(x++, y, fg, bg, c[i]);
        }
        i++;
    }
}

void clear(enum color bg) 
{
    uint8_t x, y;
    for (y = 0; y < ROWS; y++)
        for (x = 0; x < COLS; x++)
            putc(x, y, bg, bg, ' ');
}