#include "spinlock.h"
#include "x86.h"
#include "interrupt.h"

void initlock(spinlock *lk) {
  lk->locked = 0;
}

// Acquire the lock.
// Loops (spins) until the lock is acquired.
// Holding a lock for a long time may cause
// other CPUs to waste time spinning to acquire it.
void acquire(spinlock *lk) {
  pushcli(); // disable interrupts to avoid deadlock.
  if(holding(lk)) {
  	// bad
  }

  // The xchg is atomic.
  // It also serializes, so that reads after acquire are not
  // reordered before it. 
  while(xchg(&lk->locked, 1) != 0)
    ;
}

// Release the lock.
void release(spinlock *lk) {
  if(!holding(lk)) {
  	// bad
  }

  xchg(&lk->locked, 0);

  popcli();
}

int holding(spinlock *lock) {
  return lock->locked;
}

int g_ncli = 0;

void pushcli() {
  int eflags;
  
  eflags = readeflags();
  interrupts_off();
  g_ncli++;
}

void popcli(void) {
  if(readeflags() & 0x00000200) {
    // panic("popcli - interruptible");
  }
  if(--g_ncli < 0) {
    // panic("popcli");
  }
  if(g_ncli == 0)
    interrupts_on();
}
