#ifndef SPINLOCK_H_INCLUDED__
#define SPINLOCK_H_INCLUDED__

#include <stdint.h>

typedef struct spinlock {
  uint32_t locked;       // Is the lock held?
} spinlock;

void pushcli();
void popcli();
int holding(struct spinlock *lock);
void acquire(spinlock *lk);
void initlock(spinlock *lk);
void release(spinlock *lk);
int holding(spinlock *lock);

#endif