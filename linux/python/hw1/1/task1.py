#! /usr/bin/env python3

from collections import namedtuple

Nil = namedtuple('Nil', '')
Cons = namedtuple('Cons', ['car', 'cdr'])


def null(a):
    """
    >>> null(Nil())
    True
    >>> null(Cons(0, Nil()))
    False
    """
    return type(a).__name__ == 'Nil'


def fromseq(seq):
    """
    >>> fromseq([])
    Nil()
    >>> fromseq(tuple())
    Nil()
    >>> fromseq([1, 2, 3])
    Cons(car=1, cdr=Cons(car=2, cdr=Cons(car=3, cdr=Nil())))
    """
    try:
        sec_iter = iter(seq)
        return Cons(car=next(sec_iter), cdr=fromseq(sec_iter))
    except StopIteration:
        return Nil()


def head(a):
    """
    >>> head(fromseq([1, 2, 3]))
    1
    >>> head(Nil())
    Traceback (most recent call last):
    ...
    AttributeError: 'Nil' object has no attribute 'car'
    """
    return a.car


def tail(a):
    """
    >>> tail(fromseq([1, 2, 3]))
    Cons(car=2, cdr=Cons(car=3, cdr=Nil()))
    >>> tail(fromseq([]))
    Traceback (most recent call last):
    ...
    AttributeError: 'Nil' object has no attribute 'cdr'
    """
    return a.cdr


def foldr(f, i, a):
    """
    >>> foldr(lambda x, y: x + y, 0, Nil())
    0
    >>> foldr(lambda x, y: x + y, 2, fromseq([1, 2, 3]))
    8
    >>> foldr(lambda x, y: x - y, 1, fromseq([3, 2, 1]))
    1
    """
    try:
        return f(a.car, foldr(f, i, tail(a)))
    except:
        return i


def foldl(f, i, a):
    """
    >>> foldl(lambda x, y: x + y, 0, Nil())
    0
    >>> foldl(lambda x, y: x + y, 2, fromseq([1, 2, 3]))
    8
    >>> foldl(lambda x, y: x - y, 1, fromseq([3, 2, 1]))
    -5
    """
    try:
        z = f(i, a.car)
        return foldl(f, z, tail(a))
    except:
        return i


def length(a):
    """
    >>> length(Nil())
    0
    >>> length(fromseq((1, 2)))
    2
    """
    try:
        return 1 + length(tail(a))
    except:
        return 0


def tolist(a):
    """
    >>> tolist(Nil())
    []
    >>> tolist(Cons(1, Nil()))
    [1]
    >>> tolist(fromseq([1, 2, 3]))
    [1, 2, 3]
    """
    try:
        return [a.car] + tolist(tail(a))
    except:
        return []


def map_(f, a):
    """
    >>> tolist(map_(lambda x: x, Nil()))
    []
    >>> tolist(map_(lambda x: x, fromseq([1, 2, 3])))
    [1, 2, 3]
    >>> tolist(map_(lambda x: str(x) + '0', fromseq([1, 2, 3])))
    ['10', '20', '30']
    """
    try:
        return Cons(car=f(a.car), cdr=map_(f, tail(a)))
    except:
        return Nil()


def append(a, b):
    """
    >>> append(Nil(), fromseq([]))
    Nil()
    >>> append(Nil(), Cons(0, Cons(1, Nil())))
    Cons(car=0, cdr=Cons(car=1, cdr=Nil()))
    >>> append(fromseq([1]), Nil())
    Cons(car=1, cdr=Nil())
    >>> append(fromseq([1, 2]), fromseq([3]))
    Cons(car=1, cdr=Cons(car=2, cdr=Cons(car=3, cdr=Nil())))
    """
    try:
        return Cons(car=a.car, cdr=append(tail(a), b))
    except:
        return b


def filter_(p, a):
    """
    >>> filter_(lambda x: True, Nil())
    Nil()
    >>> tolist(filter_(lambda x: True, fromseq([1, 2])))
    [1, 2]
    >>> tolist(filter_(lambda x: False, fromseq([1, 2])))
    []
    >>> tolist(filter_(lambda x: x % 2 == 0, fromseq(range(5))))
    [0, 2, 4]
    """
    try:
        if p(a.car):
            return Cons(car=a.car, cdr=filter_(p, tail(a)))
        else:
            return filter_(p, tail(a))
    except:
        return Nil()


def reverse(a):
    """
    >>> reverse(Nil())
    Nil()
    >>> tolist(reverse(fromseq(range(2))))
    [1, 0]
    >>> tolist(reverse(fromseq(range(3))))
    [2, 1, 0]
    """
    flip_concat = lambda x, y: Cons(car=y, cdr=x)
    return foldl(flip_concat, Nil(), a)


def elem(e, a):
    """
    >>> elem(10, Nil())
    False
    >>> elem(5, fromseq(range(5)))
    False
    >>> elem(5, fromseq(range(10)))
    True
    """
    try:
        return a.car == e or elem(e, tail(a))
    except:
        return False


if __name__ == '__main__':
    import doctest

    doctest.testmod()
