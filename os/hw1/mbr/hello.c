__asm__(".code16\n");

int cmain(void) {
     asm ("movb $'H' , %al\n"); // Character to print
     asm ("movb $0x0e, %ah\n"); // Bios service code to print
     asm ("int  $0x10\n");      // interruption

     asm("movb $'e' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'l' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'l' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'o' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $',' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $' ' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'W' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'o' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'r' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'l' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'d' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     asm("movb $'!' , %al\n");
     asm("movb $0x0e, %ah\n");
     asm("int  $0x10\n");

     return 0;
} 