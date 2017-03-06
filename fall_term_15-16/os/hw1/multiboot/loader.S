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
