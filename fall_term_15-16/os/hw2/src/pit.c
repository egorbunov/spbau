#include "pit.h"

#define CHANNEL_0_PORT 0x40
#define MODE_COMMAND_PORT 0x43

// set frequency divisor for mode, sotred in command
void set_PIT_reload_value(uint16_t value, uint8_t command) {
    interrupts_off();
    outb(MODE_COMMAND_PORT, command);
    outb(CHANNEL_0_PORT, value & 0xFF);
    outb(CHANNEL_0_PORT, value >> 8);
    interrupts_on();
}