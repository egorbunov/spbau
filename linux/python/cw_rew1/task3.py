#! /bin/python3
import datetime


def spy(method):
    spy.info = dict()

    def decorate(*args, **kw):
        res = method(*args, **kw)
        if not (method.__name__ in spy.info.keys()):
            spy.info[method.__name__] = []
        spy.info[method.__name__].append((datetime.datetime.now(), (args, kw)))
        return res
    decorate.__name__ = method.__name__
    return decorate


def bond(method):
    print("X")
    return spy.info[method.__name__]


@spy
def foo(x):
    print(x)


