### MBR bootloader
To build run `make clean hello` from shell.

Most important pats are described below:

#### Main C-file with 'Hello World!' printing code
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

#### Linker script
```Linker Script
OUTPUT_FORMAT(elf64-x86-64)
OUTPUT_ARCH(i386:x86-64)
ENTRY(main);
SECTIONS
{
    . = 0x7C00;
    .text : { *(.text); }
    .sig : AT(0x7DFE) { SHORT(0xaa55); } //
} 
```
* `. = 0x7C00` -- set, where program will be loaded (sets output location); Address specified is address, where BIOS start to execute commands from
* `.text : { *(.text); } ` -- 
### Multiboot bootloader