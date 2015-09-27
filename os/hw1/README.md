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

### Building
1. `gcc -Werror -Wall -c hello.c -o hello.o` ‒ compilation
    * `-Werror` ‒ make all warnings into errors.
    * `-Wall` ‒ enable ALL (:)) warnings
    * `-ffreestanding` ‒ compilation takes place in freestanding environment, where standard library may not exist and some another stuff may happen.*Actually, it works without adding that flag*.
    * `-c` ‒ just compile ( without linking =) )
    * `-o` ‒ specify output file
2. `ld -m elf_i386 -T hello.ld -nostdlib -o hello.elf hello.o` ‒ linking
    * `-nostdlib` ‒ only search library directories explicitly specified on the command line (link only explicitly specified libs)
    * `-o` ‒ output
    * `-m elf_i386` ‒ specify linker `personality`, here if `elf_i386`, so linker knows something about target executable
3. `objcopy -O binary hello.elf hello` ‒ copies the contents of an object file to another
    * `-O binary` ‒ generate raw binary file (just raw binary file :D)

### Running
I've tested it only with QEMU using command: `qemu-system-i386 -hda hello`

### Links
* http://www.codeproject.com/Articles/664165/Writing-a-boot-loader-in-Assembly-and-C-Part
* http://www.lowlevel.eu/wiki/Teil_4_-_Hello_World


## Multiboot bootloader

### Differences from MBR

* In that case GRUB firstly started and it looks at program: it must satisfy [*multiboot specification*](https://www.gnu.org/software/grub/manual/multiboot/multiboot.html#Header-magic-fields) so besides main ('hello world' printing) code we need to add commands, which will setup multiboot headers.
* Final executable must be in **elf** format, not raw binary format
* Also in multiboot hello world app I use VGA buffer to print characters

### Loader code

```GAS
.text
    .global loader  # entry point visible to linker

    .set FLAGS,    0x0              
    .set MAGIC,    0x1BADB002        
    .set CHECKSUM, -(MAGIC + FLAGS) 

    # setting up multiboot header
    .align 4
    .long MAGIC
    .long FLAGS
    .long CHECKSUM

    .set STACKSIZE, 0x4000           # that is, 16k.
    .lcomm stack, STACKSIZE          # reserve 16k stack

loader:
    movl  $(stack + STACKSIZE), %esp # set up the stack (esp - stack top address)
    call  cmain                      # call C code
    cli
hang:
    hlt                              # halt machine should kernel return
    jmp   hang
```

**GAS commands and directives**
* `.set` defines some name and associates it's with value to substitute...
* `.align 4` force compiler to add additional bytes so next data will be landed on position, which address divisible by algnment value (here value is `4`)
* `.long` put on current address given value (reserves 4 bytes and current location)
* `.lcomm stack, STACKSIZE` reserves `STACKSIZE` bytes for `stack` area; memory allocated in *.bss* section
* `movl` mov with 32 bit longs
* `cli` switch off hardware interruptions 
* `hlt` stop processor till hardware interruption

So, due to multiboot spec. we place `0x1BADB002`, `0x0` and `-(0x1BADB002-0x0)` as first 3 bytes in out executable, to ensure that that bytes will be places at the very top of executable we specify it in linker script (see below).

### Linker script

```LinkerScript
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
```
Line `.multiboot ALIGN(4096) : { loader.o(.text) }` ensures that multiboot signature (that 3 bytes, described above) will be at the top of out program


### Hello World code
```C
int next_color() {
    const int COLOR_NUM = 16;
    static int color = 0x06;
    color = (color + 0x01) % COLOR_NUM;
    color = (color == 0) ? 0x01 : color;
    return color;
}

void cmain(void) {
    const int COLUMN_NUM = 0xA0; // length of line (80 symbols)
    const int LINE_INDEX = 11;
    const int SHIFT = 31;
    const char str[] = "Hello, World!";

    char* video = (char*) 0xB8000 + COLUMN_NUM * LINE_INDEX + SHIFT * 2;
    int i;
    for (i = 0; str[i] != '\0'; i++) {
        video[i * 2] = str[i];
        video[i * 2 + 1] = next_color();
    }
}
```

Here VGA buffer is used to print letters. BIOS during start sets VGA controller
in 7 mode (whatever it means) and buffer (if mode is 7) is starts from `0xB8000`. Every next 2 bytes if buffer are interpreted as ['character', 'style']. In that mode it is possible to access 80 columns and 25 lines, so every line has lengh of 80 characters. First line, for example is located between `0xB8000` and `0xB80A0`, so difference is `A0`<sub>16</sub>=`160`<sub>10</sub>, so 80 pairs of 2 bytes.


### Running
To test, how program is loaded and executed use: `qemu-system-i386 -kernel hello`

### Links
* http://habrahabr.ru/company/neobit/blog/173263/
* http://www.lowlevel.eu/wiki/Teil_4_-_Hello_World
* http://wiki.osdev.org/Bare_Bones