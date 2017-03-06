#include <stdint.h> 
#include <stddef.h>
#include <stdbool.h>
#include "pic.h"
#include "pit.h"
#include "util.h"
#include "interrupt.h"

#define CODE_SEGMENT_SELECTOR 0x8
#define MASTER_PIC_INTERRUPTS_OFFSET 0x20
#define SLAVE_PIC_INTERRUPTS_OFFSET 0x28
#define INTERRUPT_NUMBER 0x20
#define PIT_FREQUENCY 1193182

extern void isr_wrapper();

int next_color() {
    const int COLOR_NUM = 16;
    static int color = 0x06;
    color = (color + 0x01) % COLOR_NUM;
    color = (color == 0) ? 0x01 : color;
    return color;
}

void put_str(const char* str, int line_index, int shift, int color) {
    const int COLUMN_NUM = 0xA0; // length of line (80 symbols)

    char* video = (char*) 0xB8000 + COLUMN_NUM * line_index + shift * 2;
    int i;
    for (i = 0; str[i] != '\0'; i++) {
        video[i * 2] = str[i];
        video[i * 2 + 1] = color;
    }
}

void tick() {
    const int INIT_LINE_INDEX = 11;
    const int MAX_LINE_INDEX = 20;
    const int SHIFT = 31;
    const char str[] = "Hello, World!";
    static int line_index = INIT_LINE_INDEX;

    if (line_index != INIT_LINE_INDEX) {
        put_str(str, line_index - 1, SHIFT, 0); // black color
    } else {
        put_str(str, MAX_LINE_INDEX, SHIFT, 0);
    }
    put_str(str, line_index, SHIFT, next_color());

    line_index += 1;

    if (line_index > MAX_LINE_INDEX)
        line_index = INIT_LINE_INDEX;

}


void interrupt_handler() {
    /**
    * We need to draw text every second. But PIT freq. divisor can max. be only 65536
    * So we need to interrupt (PIT_FREQ / 65536) times and than interrupt after 
    * (PIT_FREQ % 65546) pulses
    */
    static uint32_t counter = 0;
    static const uint32_t MAX_FREQ_DIVISOR = 65536;
    static const uint32_t REMINDER = PIT_FREQUENCY % MAX_FREQ_DIVISOR;
    static const uint32_t ITER_CNT = PIT_FREQUENCY / MAX_FREQ_DIVISOR;

    PIC_sendEOI(INTERRUPT_NUMBER - MASTER_PIC_INTERRUPTS_OFFSET);

    counter += 1;
    if (counter == ITER_CNT) {
        // 0x30 is mode for single shot (interrupt) trigger (reload val not reset)
        set_PIT_reload_value(REMINDER, 0x30); 
    } else if (counter == ITER_CNT + 1) {
        tick();
        // 0x34 is mode, there reload value is reset every time it becomes 0 after decrement
        set_PIT_reload_value(0, 0x34); // 0 is treated as 2^16 (65536)
        counter = 0;
    }
}

void cmain(void) 
{   
    // make space for idt and tell cpu where that space is
    setup_idt();

    // init PICs
    PIC_remap(MASTER_PIC_INTERRUPTS_OFFSET, SLAVE_PIC_INTERRUPTS_OFFSET);

    // Adding interrupt service routine
    add_irs(INTERRUPT_NUMBER, isr_wrapper, CODE_SEGMENT_SELECTOR);

    // We need to unmask our interrupt in IMR to make PIC not ignore out IRQ
    const char IRQ_LINE = INTERRUPT_NUMBER - MASTER_PIC_INTERRUPTS_OFFSET;
    IRQ_clear_mask(IRQ_LINE); // we need our interruption (irq0) to be non masked
    
    set_PIT_reload_value(0, 0x34); // 0 is treated as 2^16
    
    interrupts_on();

    tick();

    for (;;) {}
}