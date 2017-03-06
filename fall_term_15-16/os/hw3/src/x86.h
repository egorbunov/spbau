#ifndef X86_H_INCLUDED__
#define X86_H_INCLUDED__


static inline uint32_t readeflags(void)
{
  uint32_t eflags;
   __asm__ __volatile__ ("pushfl; popl %0" : "=r" (eflags));
  return eflags;
}


static inline uint32_t xchg(volatile uint32_t *addr, uint32_t newval)
{
  uint32_t result;
  // The + in "+m" denotes a read-modify-write operand.
   __asm__ __volatile__ ("lock; xchgl %0, %1" :
               "+m" (*addr), "=a" (result) :
               "1" (newval) :
               "cc");
  return result;
}

#endif