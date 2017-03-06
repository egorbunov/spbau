#include <stdint.h> 
#include "util.h"
#include "serial.h"
#include "threads.h"
#include "interrupt.h"

#define BUFFER_SIZE 1024

extern void* stack_1; // stack ptr for first thread (not main one)

volatile int producer_pos = 0;
static volatile char buffer[BUFFER_SIZE];


void writer()
{
    int cons_pos = 0;
    static int x = 0;
    static int y = 0;

    // putc(x, y, RED, BLACK, 'x');
    // manual_thread_switch();

    for (;;) {
        if (producer_pos != cons_pos) {
            putc(x, y, WHITE, BLACK, buffer[cons_pos]);
            cons_pos++;
            cons_pos %= BUFFER_SIZE;
            x++;
            if (x == COLS) {
                x = 0;
                y++;
            }
        }
    }
}


void reader()
{
    // putc(1, 1, BLUE, BLACK, 'y');
    // manual_thread_switch();
    uint8_t byte = 0;
    for (;;)
    {
        byte = read_serial();
        buffer[producer_pos] = byte;
        producer_pos++;
        producer_pos %= BUFFER_SIZE;
    }
}

void cmain() {
    serial_init();
    clear(BLACK);

    threads_add(writer);
    threads_add(reader);

    // threads_add(stuff);
    // manual_thread_switch();
    // putc(2, 2, BLUE, BLACK, 'z');

    interrupts_on();

    while(1) { manual_thread_switch(); };
    // reader();
}