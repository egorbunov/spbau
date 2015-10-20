.text
    # MULTIBOOT
    .set FLAGS,    0x0              
    .set MAGIC,    0x1BADB002        
    .set CHECKSUM, -(MAGIC + FLAGS) 

    .align 4
    .long MAGIC
    .long FLAGS
    .long CHECKSUM

    # ...
    .global  isr_wrapper
    .extern interrupt_handler
    .global loader  # entry point visible to linker

    .set STACKSIZE, 0x4000           # that is, 16k.
    .lcomm stack, STACKSIZE          # reserve 16k stack

# # ====== GLOBAL DESCRIPTOR TABLE (GDT) =====

NULL_DESCRIPTOR:
    .long 0            
    .long 0
CODE_SEGMENT_DESCRIPTOR:
    .word 0xFFFF       # limit low
    .word 0            # base low
    .byte 0x00         # base middle
    .byte 0x9A         # access byte
    .byte 0xCF         # 4 bits for flags, 4 bits for limit 
    .byte 0            # base high
DATA_SEGMENT_DESCRIPTOR:
    .word 0xFFFF      
    .word 0   
    .byte 0x10           
    .byte 0x92    
    .byte 0xCF    
    .byte 0       
gdtr:
    .word 15 # length of GDT minus 1 (a little bit of hardcode)
    .long NULL_DESCRIPTOR            # base address of GDT

# ==========================================

.align   4
isr_wrapper:
    pusha
    cld 
    call interrupt_handler
    popa
    iret

loader:
    mov $(stack + STACKSIZE), %esp
    cli
    # lgdt gdtr  # load gdt
    # reloading segment registers
    mov $0x10, %ax
    mov %ax, %ds 
    mov %ax, %es 
    mov %ax, %fs 
    mov %ax, %gs 
    ljmp $0x08, $go # code segment
go:
    call cmain
    cli

hang:
    hlt
    jmp hang
