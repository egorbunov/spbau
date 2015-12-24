#! /bin/bash
import random
import itertools  # only for test

class shuffle:
    def __init__(self, it, n):
        self.it = iter(it)
        self.n = n
        self.cur_p = -1 # current pos
        self.buf = []

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
        except StopIteration:
            pass
        if self.cur_p >= len(self.buf):
            raise StopIteration
        k = random.randint(self.cur_p, self.cur_p + self.n)
        k = min(k, len(self.buf) - 1)
        self.buf[self.cur_p], self.buf[k] = self.buf[k], self.buf[self.cur_p]
        return self.buf[self.cur_p]

sh = shuffle(itertools.count(0, 1), 10)

# print("len = {}".format(len(sh)))

for i in range(0, 10):
    print(next(sh))
print("......................")
for i in range(0, 9):
    print(sh.prev())