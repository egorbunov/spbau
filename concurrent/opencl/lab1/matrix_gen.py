#!/usr/bin/env python3

import sys
import random

def const_getter(val):
    def fun():
        return val
    return fun

def random_getter():
    return random.uniform(-10, 10)

def gen_matrix_file(n, m, out, get_fun=random_getter):
    out.write("{} {}\n".format(n, m))
    for i in range(n):
        for j in range(n):
            out.write("{} ".format(get_fun()))
        out.write("\n")
    for i in range(m):
        for j in range(m):
            out.write("{} ".format(get_fun()))
        out.write("\n")

if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("USAGE: /x.py <N> <M> <filename> <NUMBER|r>")
        print(" if NUMBER specified matrices will contain only that number")
        print(" if 'r' specified matrices will contain random nums")
        sys.exit(1)

    n = int(sys.argv[1])
    m = int(sys.argv[2])
    filename = sys.argv[3]
    get_fun = random_getter if sys.argv[4] == 'r' else const_getter(int(sys.argv[4]))

    with open(filename, 'w') as out:
        gen_matrix_file(n, m, out, get_fun)
