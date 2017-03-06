#ifndef PIC_FUNCTIONS_H__
#define PIC_FUNCTIONS_H__

#include <stdint.h>
#include "util.h"

void PIC_sendEOI(uint8_t irq);
void PIC_remap(int offset1, int offset2);
void IRQ_set_mask(unsigned char IRQline);
void IRQ_clear_mask(unsigned char IRQline);

#endif