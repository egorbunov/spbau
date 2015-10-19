#include "interrupt.h"

#define IDT_SIZE 256

// interrupt descriptor structure
// tells, where interrupt service routine (isr) is located
typedef struct IDTDescriptor {
   uint16_t routine_lo_offset; // offset bits 0..15
   uint16_t selector; // a code segment selector in GDT or LDT
   uint8_t zero;      // unused, set to 0
   uint8_t type_attr; // type and attributes, see below
   uint16_t routine_hi_offset; // offset bits 16..31
} IDTDescriptor;

static IDTDescriptor idt[IDT_SIZE];

typedef struct IDTReg {
    short limit;
    long idt_ptr;
} __attribute__ ((packed)) IDTReg;

static IDTReg idt_reg;


void setup_idt()
{
    memset(idt, 0, IDT_SIZE * sizeof(IDTDescriptor));

    idt_reg.idt_ptr = (long)idt;
    idt_reg.limit = IDT_SIZE * 8 - 1;
     __asm__ __volatile__ ( "lidt (%0);" :: "r"(&idt_reg) );
}

void get_idt_ptr(void *idt_reg)
{
     __asm__ __volatile__ ( "sidt (%0);" :: "r"(idt_reg) );
}

void add_irs(size_t interrupt_number, void *interrupt_handler_ptr, uint16_t segment_selector)
{
    IDTDescriptor descriptor;

    descriptor.routine_lo_offset = ((int) (interrupt_handler_ptr)) & 255; // 2 least signigicant bytes
    descriptor.selector = segment_selector;
    descriptor.zero = 0;
    descriptor.type_attr = 0b10001110; // 0x8E ( P=1, DPL=00b, S=0, type=1110b => type_attr=1000_1110b=0x8E)
    descriptor.routine_hi_offset = ((int) (interrupt_handler_ptr)) >> 16; // 2 most signigicant bytes

    idt[interrupt_number] = descriptor;
}

void interrupts_on()
{
    __asm__ __volatile__ ("sti");
}

void interrupts_off()
{
    __asm__ __volatile__ ("cli");
}
