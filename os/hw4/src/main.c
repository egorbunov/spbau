#include <stdint.h> 
#include "util.h"
#include "vga.h"
#include "serial.h"
#include "string.h"
#include "multiboot.h"

void cmain(unsigned long magic, multiboot_info_t* pmbinfo) {
    init_vga();

    printf("Command line: %s\n", pmbinfo->cmdline);

    while(1) { };
}