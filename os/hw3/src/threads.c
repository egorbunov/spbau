#include "threads.h"
#include "interrupt.h"
#include "util.h"
#include "pic.h"
#include "pit.h"
#include "spinlock.h"

#define MAX_THREAD_NUM 10

#define MASTER_PIC_INTERRUPTS_OFFSET 0x20
#define SLAVE_PIC_INTERRUPTS_OFFSET 0x28
#define INTERRUPT_NUMBER 0x20
#define CODE_SEGMENT_SELECTOR 0x8

extern void isr_wrapper();


static void *g_threads_stack_ptrs[MAX_THREAD_NUM];
static uint16_t g_thread_num = 0;
static uint16_t g_cur_thread_id = 0;

spinlock g_lock;

void threads_prepare_scheduler()
{
    interrupts_off();

    initlock(&g_lock);

    setup_idt();
    add_irs(INTERRUPT_NUMBER, isr_wrapper, CODE_SEGMENT_SELECTOR);
    PIC_remap(MASTER_PIC_INTERRUPTS_OFFSET, SLAVE_PIC_INTERRUPTS_OFFSET);

    const char IRQ_LINE = INTERRUPT_NUMBER - MASTER_PIC_INTERRUPTS_OFFSET;
    IRQ_clear_mask(IRQ_LINE);

    g_thread_num = 1;
    g_cur_thread_id = 0;
}

__attribute__ ((fastcall)) int32_t threads_switch(int32_t current_stack)
{
    acquire(&g_lock);

    PIC_sendEOI(INTERRUPT_NUMBER - MASTER_PIC_INTERRUPTS_OFFSET);
    if (g_thread_num <= 1)
        return current_stack;

    g_threads_stack_ptrs[g_cur_thread_id] = (void*) current_stack;
    set_PIT_reload_value(10000, 0x30);
    g_cur_thread_id = (g_cur_thread_id + 1) % g_thread_num;

    release(&g_lock);

    return (int32_t) (g_threads_stack_ptrs[g_cur_thread_id]);
}


int32_t threads_add(thread_ptr pthread, void* stack) {
    if (g_thread_num == MAX_THREAD_NUM)
        return -1;
    if (0 == g_thread_num)
        threads_prepare_scheduler();


    int32_t *ptr = (int32_t*) (((char*)stack));
    ptr--; // push flags
    *ptr = 0x202;
    ptr--; // push cs
    *ptr = 0x8;
    ptr--; // push eip
    *ptr = (int32_t) pthread;
    ptr -= 4; // push eax ecx edx ebx
    ptr--; // push esp 
    *ptr = (int32_t) (( (char*) stack) - 12);
    
    ptr -= 3; // push ebp esi edi
    
    g_threads_stack_ptrs[g_thread_num] = (void*) (((int32_t) ptr));

    acquire(&g_lock);
    int new_id = g_thread_num;
    g_thread_num++;
    release(&g_lock);
    
    return new_id;
}

void manual_thread_switch() {
    __asm__ __volatile__("pusha");
    int32_t cur_esp;
    __asm__ __volatile__("movl %%esp, %0" :"=r"(cur_esp));
    if (g_thread_num <= 1)
        return;
    g_threads_stack_ptrs[g_cur_thread_id] = (void*) cur_esp;
    g_cur_thread_id = (g_cur_thread_id + 1) % g_thread_num;
    int32_t new_esp = (int32_t) (g_threads_stack_ptrs[g_cur_thread_id]);
    __asm__ __volatile__("mov %0, %%esp" : : "r"(new_esp) );
    __asm__ __volatile__("popa");
}
