    .set FLAGS,    0x00000003
    .set MAGIC,    0x1BADB002 
    .set CHECKSUM, -(MAGIC + FLAGS)

    .set STACKSIZE, 0x1000
    .lcomm stack, STACKSIZE

multiboot:
    .align 4
    .long MAGIC
    .long FLAGS
    .long CHECKSUM

.section .text
    .global start

start:
    movl $(stack + STACKSIZE), %esp

    lgdt gdtdesc
    ljmp $0x08, $.continue
.continue: 
    # reloading segment registers
    movw $(0x10), %ax # 16 bytes off
    movw %ax, %ds 
    movw %ax, %es
    movw %ax, %ss
    movw $0, %ax
    movw %ax, %fs
    movw %ax, %gs

    call cmain
    jmp .

hang:
    hlt
    jmp hang

#include "asm.h"
# GDT
.p2align 2
gdt:
  SEG_NULLASM                             # null seg
  SEG_ASM(STA_X | STA_R, 0x0, 0xffffffff)   # code seg
  SEG_ASM(STA_W, 0x0, 0xffffffff)         # data seg    
gdtdesc:
    .word (gdtdesc - gdt - 1)   # length of GDT minus 1
    .long gdt                   # base address of GDT
