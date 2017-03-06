#include "vga.h"
#include "string.h"

static uint16_t *const video = (uint16_t*) 0xB8000;
static enum color g_bg;
static enum color g_fg;

void set_background_color(enum color bg) {
    g_bg = bg;
}

void set_foreground_color(enum color fg) {
    g_fg = fg;
}

void init_vga(void) {
    clear(BLACK);
    set_foreground_color(WHITE);
    set_background_color(BLACK);
}


void putc_at(uint8_t x, uint8_t y, enum color fg, enum color bg, char c)  {
    video[y * COLS + x] = (bg << 12) | (fg << 8) | c;
}

void prints_at(uint8_t x, uint8_t y, enum color fg, enum color bg, const char *c) {
    int i = 0;
    while (c[i] != '\0') {
        if (c[i] == '\n') {
            y += 1;
            x = 0;
        }
        else {
            putc_at(x++, y, fg, bg, c[i]);
        }
        i++;
    }
}

/**
*  Print bytes from str in range: [from, to)
**/
void prints(const char* str, int from, int to) {
    for (int i = from; i < to; ++i) {
        putc(str[i]);
    }
}

// with new line
void printsn(const char* str, int from, int to) {
    for (int i = from; i < to; ++i) {
        putc(str[i]);
    }
    putc('\n');
}

void putc(char c) {
    static int xpos = 0;
    static int ypos = 0;

    if (c == '\n') {
        ypos += 1;
        xpos = 0;
    } else {
        putc_at(xpos++, ypos, g_fg, g_bg, c);
        if (xpos == COLS) {
            ypos += 1;
            xpos = 0;
        }
    }

    // TODO: change it
    if (ypos == ROWS) {
        ypos = 0;
        clear(g_bg);
    }
}

void printf(const char *format, ...) {
    char **arg = (char **) &format;
    int c;
    char buf[30];

    arg++;

    while ((c = *format++) != 0) {
        if (c != '%')
            putc(c);
        else {
            char *p;

            c = *format++;
            switch (c) {
                case 'd':
                case 'u':
                case 'x':
                    itoa (buf, c, *((int *) arg++));
                    p = buf;
                    goto string;
                    break;
                case 's':
                    p = *arg++;
                    if (!p)
                        p = "(null)";

                    string:
                        while(*p)
                            putc(*p++);
                        break;
                default:
                    putc(*((int *) arg++));
                break;
            }
        }
    }
}

void clear(enum color bg) 
{
    uint8_t x, y;
    for (y = 0; y < ROWS; y++)
        for (x = 0; x < COLS; x++)
            putc_at(x, y, bg, bg, ' ');
}