#! bin/python3


class UnknownSignatureException(Exception):
    def __init__(self, message):
        super(UnknownSignatureException, self).__init__(message)


class poly(object):
    funcs = {}

    def __init__(self, *args):
        if args[0] == polyDefault.default_tag:
            self.args = polyDefault.default_tag
        else:
            self.args = args
        self.f_name = ''

    def __call__(self, f):
        self.f_name = f.__name__
        if self.f_name not in poly.funcs:
            poly.funcs[self.f_name] = {}
        poly.funcs[self.f_name][str(self.args)] = f

        def wrapped_f(*args):
            signature = str(tuple(map(type, args)))
            if signature in poly.funcs[self.f_name]:
                poly.funcs[self.f_name][signature](*args)
            elif polyDefault.default_tag in poly.funcs[self.f_name]:
                poly.funcs[self.f_name][polyDefault.default_tag](*args)
            else:
                raise UnknownSignatureException("error: there is no "
                                                "overloaded function [{}] "
                                                "with signature [{}]"
                                                .format(self.f_name,
                                                        signature))

        return wrapped_f


class polyDefault(object):
    default_tag = 'DEFAULT'

    def __call__(self, f):
        decorator = poly(polyDefault.default_tag)(f)

        def wrapped_f(*args):
            decorator(*args)

        return wrapped_f


@poly(int, int)
def fun(x, y):
    print(x+y)


@poly(int)
def fun(x):
    print(x)


@polyDefault()
def fun(*args):
    print("DEFAULT!")


@poly(str, str)
def g(str1, str2):
    print(str1 + " --> " + str2)


@polyDefault()
def g(*args):
    print("g({}) default".format(args))

fun(1, 2)
fun(1)
fun(1, 2, 3)
fun("sadsa")

g("Hello", "World!")
g(10)
g()