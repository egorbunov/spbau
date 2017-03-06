#! /usr/bin/env python3

import sys

# IMPL: ~ 12 seconds on 3000x3000 matrix

# def read_mat(file_name):
# 	'''
# 	'with' construction helps not to care about closing 
# 	file (in case of errors too)
# 	'''
# 	with open(file_name, 'r') as f:
# 		# BAD FILE READING:
# 		stdin = sys.stdin
# 		sys.stdin = f
# 		n = int(input())
# 		mat = []
# 		for i in range(n):
# 			ts = input().split()
# 			mat.append([int(x) for x in ts])

# 		sys.stdin = stdin

# 	return mat


# def transpose(mat):
# 	n = len(mat)
# 	new_mat = [[0 for i in range(n)] 
# 				for j in range(n) ]

# 	for i in range(n):
# 		for j in range(n):
# 			new_mat[i][j] = mat[j][i]
# 	return new_mat


# def print_mat(mat):
# 	for row in mat:
# 		line = ''
# 		for x in row:
# 			line = line + str(x) + ' '
# 		line = line[:-1]
# 		print(line)


# Better implementation
def read_mat(file_name):
	'''
	'with' construction helps not to care about closing 
	file (in case of errors too)
	'''
	with open(file_name, 'r') as f:
		mat_lines = f.readlines()[1:]
	return [list(map(int, line.split())) for line in mat_lines]
	# return [[int(x) for x in line.split()] for line in mat_lines]


def transpose(mat):
	return list(zip(*mat)) # *mat -- unpacking mat to use as function args, 
						   # mat must be iterable


def print_mat(mat):
	print('\n'.join(' '.join(str(x) for x in r) for r in mat))
	# for row in mat:
	# 	line = ' '.join(str(x) for x in row)
	# 	print(line)


if __name__ == '__main__':
	mat = read_mat(sys.argv[1])
	mat = transpose(mat)
	print_mat(mat)