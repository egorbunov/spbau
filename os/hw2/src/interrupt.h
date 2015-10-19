#ifndef INTERRUPT_H_INCLUDED__
#define INTERRUPT_H_INCLUDED__

#include <stdint.h>
#include <stddef.h>
#include "util.h"

// adds given handler to idt
void add_irs(size_t interrupt_number, void *interrupt_handler_ptr, uint16_t segment_selector);
void get_idt_ptr(void *idt_reg);
void setup_idt();
void get_flags();

// ...
void interrupts_on();
void interrupts_off();


#endif