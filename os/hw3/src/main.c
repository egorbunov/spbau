#include <stdint.h> 
#include "util.h"
#include "serial.h"
#include "threads.h"
#include "interrupt.h"

#define BUFFER_SIZE 1024

extern void* stack_0; // stack ptr for main thread
extern void* stack_1; // stack ptr for second thread

volatile int producer_pos = 0;
static volatile char buffer[BUFFER_SIZE];

void writer()
{
    int cons_pos = 0;
    static int x = 0;
    static int y = 0;
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
    uint8_t byte = 0;
    for (;;)
    {
        byte = read_serial();
        buffer[producer_pos] = byte;
        producer_pos++;
        producer_pos %= BUFFER_SIZE;
    }
}

int __attribute__((noreturn)) cmain() 
{
    clear(BLACK);   
    serial_init();
    threads_add(writer, stack_1);
    interrupts_on();
    reader();
}