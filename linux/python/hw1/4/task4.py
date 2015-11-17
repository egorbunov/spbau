#! /usr/bin/env python3


class SkipList(object):

    def __init__(self):
        pass

    def insert(self, item):
        '''
        >>> sl = SkipList()
        >>> sl.insert(1), sl.insert(2)
        (True, True)
        >>> sl.insert(1)
        False
        '''
        pass

    def remove(self, item):
        '''
        >>> sl = SkipList()
        >>> sl.insert(1), sl.insert(2)
        (True, True)
        >>> sl.remove(1), sl.remove(1), sl.remove(3)
        (True, False, False)
        '''
        pass

    def contains(self, item):
        '''
        >>> sl = SkipList()
        >>> sl.insert(1), sl.insert(2)
        (True, True)
        >>> sl.contains(1), sl.contains(3)
        (True, False)
        '''
        pass

    def size(self):
        '''
        >>> sl = SkipList()
        >>> sl.size()
        0
        >>> sl.insert(1), sl.insert(1)
        (True, False)
        >>> sl.size()
        1
        >>> sl.insert(2)
        True
        >>> sl.size()
        2
        '''
        pass

    def __iter__(self):
        pass


def make_skip_list(seq):
    '''
    >>> make_skip_list([]).size()
    0
    >>> make_skip_list(range(10)).size()
    10
    >>> make_skip_list([10] * 10).size()
    1
    >>> sum(make_skip_list(range(10**6)))
    499999500000
    '''
    pass


def make_list(skip_list):
    '''
    >>> make_list(SkipList())
    []
    >>> make_list(make_skip_list(range(3, 0, -1)))
    [1, 2, 3]
    >>> sum(make_list(make_skip_list(range(10**6))))
    499999500000
    '''
    return list(skip_list)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
