#include "threads.h"
#include "interrupt.h"
#include "util.h"
#include "pic.h"
#include "pit.h"
#include "spinlock.h"
#include "x86.h"

#define MAX_THREAD_NUM 10

#define MASTER_PIC_INTERRUPTS_OFFSET 0x20
#define SLAVE_PIC_INTERRUPTS_OFFSET 0x28
#define INTERRUPT_NUMBER 0x20
#define CODE_SEGMENT_SELECTOR 0x8

extern void isr_wrapper();
extern void asm_thread_switch(thread_context_t** old_context, thread_context_t* new_context);
void cmain();

static thread_context_t* g_threads_contexts[MAX_THREAD_NUM];
static stack_t thread_stacks[MAX_THREAD_NUM + 1];

static uint16_t g_thread_num = 0; // number of registered threads
static uint16_t g_cur_thread_id = 0; // currently running thread id

void threads_prepare()
{
    // preparing main thread 
    setup_idt();
    add_irs(INTERRUPT_NUMBER, isr_wrapper, CODE_SEGMENT_SELECTOR);
    PIC_remap(MASTER_PIC_INTERRUPTS_OFFSET, SLAVE_PIC_INTERRUPTS_OFFSET);

    const char IRQ_LINE = INTERRUPT_NUMBER - MASTER_PIC_INTERRUPTS_OFFSET;
    IRQ_clear_mask(IRQ_LINE);

    g_cur_thread_id = threads_add(cmain);
}

void threads_switch()
{
    static int x = 2;
    // putc(x, 2, BLUE, BLACK, 's');
    // x += 1;
    // putc(2, 1, YELLOW, BLACK, 'y');

    PIC_sendEOI(INTERRUPT_NUMBER - MASTER_PIC_INTERRUPTS_OFFSET);
    set_PIT_reload_value(10000, 0x30);

    manual_thread_switch();
}


int32_t threads_add(thread_fun_ptr thread_fun) {
    pushcli();

    if (g_thread_num == MAX_THREAD_NUM)
        return -1;

    int new_thread_id = g_thread_num;
    thread_context_t* new_context = (thread_context_t*) (thread_stacks[new_thread_id + 1].data);
    new_context->eip = (uint32_t) thread_fun;
    new_context->eflags |= (1 << 9);
    new_context->edi = 0;
    new_context->esi = 0;
    new_context->ebx = 0;
    new_context->ebp = 0;
    g_threads_contexts[new_thread_id] = new_context;

    g_thread_num += 1;

    popcli();

    static int32_t x = 10;

    // putc(x, 2, BLUE, WHITE, '0' + new_thread_id);
    // putc(x+1, 2, GREEN, WHITE, '0' + g_thread_num);
    // x += 5;

    return new_thread_id;
}   

void manual_thread_switch() {
    pushcli();

    int32_t old_id = g_cur_thread_id;
    g_cur_thread_id = (g_cur_thread_id + 1) % g_thread_num;
    
    asm_thread_switch(&g_threads_contexts[old_id], g_threads_contexts[g_cur_thread_id]);
    popcli();
}   
    