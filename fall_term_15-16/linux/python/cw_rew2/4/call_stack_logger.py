from collections import defaultdict

def test1(num):
    return num == 1


def test2(num):
    return num > 3


def foo(num):
    print("Original")
    test1(num)


def foo2(num, arg1=0, arg2=0):
    foo(num + arg1 + arg2)
    test1(num)


class CallStackLogger(object):
    history = []
    cur_indent = 0

    @classmethod
    def get_history(cls):
        return cls.history

    @classmethod
    def init(cls):
        def wrapper(func):
            def dummy(*args, **kwargs):
                cls.history.append(
                    (func.__name__, list(args), list(kwargs.items()), cls.cur_indent))
                cls.cur_indent += 2
                res = func(*args, **kwargs)
                cls.cur_indent -= 2
                return res
            return dummy

        for name, item in globals().items():
            if hasattr(item, '__call__') and name != 'printStack' and name != 'get_history':
                globals()[name] = wrapper(item)


def printStack():
    for r in CallStackLogger.get_history():
        kwargs = map(lambda x : x[0] + '=' + str(x[1]), r[2])
        args = map(lambda x: str(x), r[1])
        print((' ' * r[3]) + r[0] + '(' + ','.join(list(args) + list(kwargs)) + ')')

CallStackLogger.init()
foo2(1, arg1=0, arg2=0)
foo(2)
printStack()