## MBR bootloader
To build run `make clean hello` from shell.

Most important pats are described below:

### Main C-file with 'Hello World!' printing code
```C
// writing code for real mode (16 bit memory addressing)
__asm__(".code16\n");                           

// unnecessary
__asm__("jmpl $0x0000, $main\n");

void main() {
     asm ("movb $'H' , %al\n"); // Character to print
     asm ("movb $0x0e, %ah\n"); // BIOS service code to print
     asm ("int  $0x10\n");      // interruption

     // printing remaining letters
}
```

### Linker script
Script, which describes contents of final output object file, which is linked from input object files. That script also specifies where (address in virtual address space) loadable sections of final object file will be loaded before program execution.
```LinkerScript
OUTPUT_FORMAT(elf64-x86-64)
OUTPUT_ARCH(i386:x86-64)
ENTRY(main);
SECTIONS
{
    . = 0x7C00;
    .text : { *(.text); }
    .boot : AT(0x7DFE) { SHORT(0xaa55); }
} 
```
* `. = 0x7C00` ‒ set, where program will be loaded (LMA) (sets output location); Address specified is address, where BIOS start to execute commands from; "." symbol is the location counter.
* `.text : { *(.text); } ` ‒ definition of output `.text` section: between `{` and `}` list of input sections is places, that sections will be places in output .text section; `*(.text)` ‒ means all `.text` input sections in all input files; The address of `.text` section will be `0x7C00`
* `boot : AT(0x7DFE) { SHORT(0xaa55); }` ‒ write at `0x7DFE` that strange number `0xAA55` (boot sector signature), which will tell BIOS, that sector is bootable! `0x7DFE` address of last 2 bytes = `0x7C00 + 0x1FE` (`0x1FE` = 510 in decimal system, so 510 and 511 bytes are last two bytes of sector with size of 512 bytes); 

## Multiboot bootloader