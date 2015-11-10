#include "asm.h"
.global  isr_wrapper
.globl start
.extern threads_switch
.global stack_1
.global stack_0

.p2align 2
.text
multiboot:
    .set FLAGS,    0x0              
    .set MAGIC,    0x1BADB002        
    .set CHECKSUM, -(MAGIC + FLAGS) 

    .align 4
    .long MAGIC
    .long FLAGS
    .long CHECKSUM

.set STACKSIZE, 0x4000           # that is, 16k.

.data
stack_1:
.long 0
stack_0:
.long 0

start:
    mov $(stack1 + STACKSIZE), %esp
    mov %esp, stack_1
    mov $(stack + STACKSIZE), %esp
    mov %esp, stack_0

    cli
    lgdt gdtdesc
    ljmp $(0x08), $go # 8 bytes off
go: 
    # reloading segment registers
    movw $(0x10), %ax # 16 bytes off
    movw %ax, %ds 
    movw %ax, %es
    movw %ax, %ss
    movw $0, %ax 
    movw %ax, %fs 
    movw %ax, %gs 

    call cmain

hang:
    hlt
    jmp hang

.align 4
isr_wrapper:
    pusha
    cld 
    mov %esp, %ecx
    call threads_switch
    mov %eax, %esp
    popa
    iret

# GDT
.p2align 2
gdt:
  SEG_NULLASM                             # null seg
  SEG_ASM(STA_X|STA_R, 0x0, 0xffffffff)   # code seg
  SEG_ASM(STA_W, 0x0, 0xffffffff)         # data seg    
gdtdesc:
    .word (gdtdesc - gdt - 1)   # length of GDT minus 1
    .long gdt                   # base address of GDT

.bss
.align 4
.lcomm stack, STACKSIZE          
stack0_ptr:
.lcomm stack1, STACKSIZE         
stack1_ptr: