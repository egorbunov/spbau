### MBR bootloader
To build run `make clean hello` from shell.

Most important pats are described below:
```C
// writing code for real mode (16 bit memory addressing)
__asm__(".code16\n");                           

// unnecessary
__asm__("jmpl $0x0000, $main\n");

void main() {
     asm ("movb $'H' , %al\n"); // Character to print
     asm ("movb $0x0e, %ah\n"); // BIOS service code to print
     asm ("int  $0x10\n");      // interruption
}
```
### Multiboot bootloader