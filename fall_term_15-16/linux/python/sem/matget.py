#! /usr/bin/env python3

import sys

n = int(sys.argv[1])

'''
formatting: '{} {}'.format(1, 2)
''' 

print(n)
for i in range(n):
	print(' '.join(str(i - j)
				   for j in range(n)))

