#ifndef PIT_FINCTIONS_H__
#define PIT_FINCTIONS_H__

#include <stdint.h>
#include "util.h"
#include "interrupt.h"

void set_PIT_reload_value(uint16_t value, uint8_t command);

#endif