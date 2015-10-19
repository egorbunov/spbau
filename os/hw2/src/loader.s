.text
    .global  isr_wrapper
    .extern interrupt_handler
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

.align   4
isr_wrapper:
    pusha
    cld 
    call interrupt_handler
    popa
    iret

# ====== GLOBAL DESCRIPTOR TABLE (GDT) =====

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
    .word gdtr - NULL_DESCRIPTOR - 1 # length of GDT minus 1
    .long NULL_DESCRIPTOR            # base address of GDT

# ==========================================

loader:
    mov $(stack + STACKSIZE), %esp
    cli
    lgdt gdtr  # load gdt
    # reloading segment registers
    mov DATA_SEGMENT_DESCRIPTOR - NULL_DESCRIPTOR, %ds 
    mov DATA_SEGMENT_DESCRIPTOR - NULL_DESCRIPTOR, %es 
    mov DATA_SEGMENT_DESCRIPTOR - NULL_DESCRIPTOR, %fs 
    mov DATA_SEGMENT_DESCRIPTOR - NULL_DESCRIPTOR, %gs 
    ljmp $0x08, $go
go:
    call cmain
    cli

hang:
    hlt
    jmp hang
