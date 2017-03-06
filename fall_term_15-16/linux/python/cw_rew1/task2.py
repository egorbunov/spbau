#! /bin/python3
import copy


class Node:
    def __init__(self, key):
        self.l = None
        self.r = None
        self.k = key


class BinTree:
    def __init__(self):
        self.__i = 0
        self.root = None

    def __iter__(self):
        if self.root:
            return self.dfs(self.root)

    def dfs(self, n):
        if n is None:
            return
        yield from self.dfs(n.l)
        yield n.k
        yield from self.dfs(n.r)

    @staticmethod
    def add(tree, i):
        cur = tree.root
        if cur is None:
            tree.root = Node(i)
            return
        while True:
            if i < cur.k:
                if cur.l is None:
                    cur.l = Node(i)
                    break
                cur = cur.l
            if i > cur.k:
                if cur.r is None:
                    cur.r = Node(i)
                    break
                cur = cur.r

    def __add__(self, other):
        new_tree = copy.deepcopy(self)
        if type(other) == type(self):
            for x in other:
                BinTree.add(new_tree, x)
        else:
            BinTree.add(new_tree, other)
        return new_tree

    def __contains__(self, item):
        cur = self.root
        while cur:
            if cur.k == item:
                return True
            if item < cur.k:
                cur = cur.l
            else:
                cur = cur.r
        return False


t = BinTree()
for i in range(10):
    t = t + i

for x in iter(t):
    print(x, end=" ")
print()

print(12 in t)


