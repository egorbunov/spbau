#ifndef INTERRUPT_H__
#define INTERRUPT_H__

#include <stdint.h>
#include <stddef.h>

void interrupts_on();
void interrupts_off();

void add_irs(size_t interrupt_number, void *interrupt_handler_ptr, uint16_t segment_selector);
void get_idt_ptr(void *idt_reg);

void setup_idt();


#endif