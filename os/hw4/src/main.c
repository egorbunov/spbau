#include <stdint.h> 
#include "util.h"
#include "vga.h"
#include "serial.h"

void cmain() {
    clear(BLACK);

    prints(0, 0, WHITE, BLACK, "Hello, world!\nHello world!");

    while(1) { };
}