ENTRY (loader)
LMA = 0x100000;
SECTIONS
{
    . = LMA + SIZEOF_HEADERS;
    .multiboot ALIGN(4096) : { loader.o(.text) }
    .text ALIGN(4096) : {*(.text)}
    .data ALIGN(4096) : { *(.data) }
    .rodata ALIGN(4096) : { *(.rodata) }
    .bss ALIGN(4096) : { *(.bss) }
}
