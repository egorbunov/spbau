OUTPUT_FORMAT(elf64-x86-64)
OUTPUT_ARCH(i386:x86-64)
ENTRY(main);
SECTIONS
{
    . = 0x7C00;
    .text : { *(.text); }
    .sig : AT(0x7DFE) { SHORT(0xaa55); }
} 