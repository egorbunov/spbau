#! /usr/bin/env python3

from sys import stdin


class Num:
    modA = int(1e9 + 9)  # without wrapping with int() it does't work!
    modB = int(1e9 + 7)

    def __init__(self, a, b=None):
        self.valModA = a
        self.valModB = a if b is None else b
        pass

    def __add__(self, other):
        return Num((self.valModA + other.valModA) % Num.modA,
                   (self.valModB + other.valModB) % Num.modB)

    def __sub__(self, other):
        return Num((self.valModA + Num.modA - other.valModA) % Num.modA,
                   (self.valModB + Num.modB - other.valModB) % Num.modB)

    def __mul__(self, other):
        return Num((self.valModA * other.valModA) % Num.modA,
                   (self.valModB * other.valModB) % Num.modB)

    def __eq__(self, other):
        return self.valModA == other.valModA and self.valModB == other.valModB

    def __repr__(self):
        return "(" + str(self.valModA) + ", " + str(self.valModB) + ")"

    def __str__(self):
        return self.__repr__()


class MatrixHasher:
    p = Num(239017)
    q = Num(238967)

    def __init__(self, mat):
        (rows, cols) = (len(mat), len(mat[0]))

        self.rowPow = [Num(1)] * rows
        self.colPow = [Num(1)] * cols
        for i in range(1, rows):
            self.rowPow[i] = self.rowPow[i - 1] * MatrixHasher.p
        for i in range(1, cols):
            self.colPow[i] = self.colPow[i - 1] * MatrixHasher.q

        self.h = [[Num(0)] * cols for _ in range(rows)]
        self.h[0][0] = Num(mat[0][0])
        for i in range(1, rows):
            self.h[i][0] = self.h[i - 1][0] + self.rowPow[i] * Num(mat[i][0])
        for i in range(1, cols):
            self.h[0][i] = self.h[0][i - 1] + self.colPow[i] * Num(mat[0][i])

        for i in range(1, rows):
            for j in range(1, cols):
                self.h[i][j] = self.h[i - 1][j] + self.h[i][j - 1] \
                    - self.h[i - 1][j - 1] + self.rowPow[i] \
                    * self.colPow[j] \
                    * Num(mat[i][j])

    def _get_matrix_hash(self, r1, r2, c1, c2):
        # print(r1, r2, c1, c2)
        res = self.h[r2][c2]
        if r1 > 0:
            res -= self.h[r1 - 1][c2]
        if c1 > 0:
            res -= self.h[r2][c1 - 1]
        if r1 > 0 and c1 > 0:
            res += self.h[r1 - 1][c1 - 1]
        return res

    def compare(self, x1, y1, h1, w1, x2, y2, h2, w2):
        return \
            self._get_matrix_hash(x1, x1 + h1 - 1, y1, y1 + w1 - 1) * \
            self.rowPow[x2] * self.colPow[y2] \
            == \
            self._get_matrix_hash(x2, x2 + h2 - 1, y2, y2 + w2 - 1) \
            * self.rowPow[x1] * self.colPow[y1]


def read_mat():
    (rows, cols) = map(int, stdin.readline().split())
    return [list(map(int, stdin.readline().split())) for _ in range(rows)]


def read_queries():
    num = int(stdin.readline())
    return [list(map(int, stdin.readline().split())) for _ in range(num)]


def main():
    mat = read_mat()
    qs = read_queries()
    hasher = MatrixHasher(mat)
    for q in qs:
        # print(q)
        print(1 if hasher.compare(*q) else 0)
    pass


if __name__ == '__main__':
    main()
