#ifndef VGA_H_INCLUDED__
#define VGA_H_INCLUDED__

#include <stdint.h>

enum color {
    BLACK = 0,
    BLUE, GREEN, CYAN, RED, MAGNETA, BROWN, GRAY, DARK_GRAY, BRIGHT_BLUE, BRIGHT_GREEN, BRIGHT_CYAN,
    BRIGHT_RED, BRIGHT_MAGNETA, YELLOW, WHITE
};

enum size {
    COLS = 80,
    ROWS = 25
};

void set_background_color(enum color bg);
void set_foreground_color(enum color fg);
void init_vga(void);
void putc(uint8_t x, uint8_t y, enum color fg, enum color bg, char c);
void clear(enum color bg);
void prints(uint8_t x, uint8_t y, enum color fg, enum color bg, const char *c);

#endif