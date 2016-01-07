#! /bin/bash
import random
import itertools  # only for test

class shuffle:
    def __init__(self, it, n):
        self.it = iter(it)
        self.n = n
        self.cur_p = -1 # current pos
        self.buf = []
        self.is_shuffled = []

    def __iter__(self):
        return self

    def __len__(self):
        self.buf.extend(list(self.it))
        return len(self.buf)

    def prev(self):
        self.cur_p -= 1
        if self.cur_p < 0 or self.cur_p >= len(self.buf):
            raise StopIteration
        return self.buf[self.cur_p]

    def __next__(self):
        self.cur_p += 1
        try:
            while len(self.buf) - self.cur_p <= self.n:
                self.buf.append(next(self.it))
                self.is_shuffled.append(False)
        except StopIteration:
            pass
        if self.cur_p >= len(self.buf):
            raise StopIteration
        if self.is_shuffled[self.cur_p]:
            return self.buf[self.cur_p]
        k = random.randint(self.cur_p, self.cur_p + self.n)
        k = min(k, len(self.buf) - 1)
        self.buf[self.cur_p], self.buf[k] = self.buf[k], self.buf[self.cur_p]
        self.is_shuffled[k], self.is_shuffled[self.cur_p] = True, True
        return self.buf[self.cur_p]


# ======================================= test ====================================

sh = shuffle([1, 2, 3, 4, 5, 6, 7, 8, 9], 1)
for i in sh:
    print(i, end=' ')