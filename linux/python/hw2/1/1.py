def final(f):
    f.__is_final__ = True
    return f


class FinalViolationError(Exception):
    def __init__(self, message):
        super(FinalViolationError, self).__init__(message)


class WithFinals(type):
    def __init__(cls, name, bases, d):
        super(WithFinals, cls).__init__(name, bases, d)

        parents = filter(lambda x: x != cls and x != object, cls.mro())
        finals = ((p.__name__, k)
                  for p in parents
                  for k, v in p.__dict__.items()
                  if callable(v) and
                  '__is_final__' in v.__dict__ and v.__is_final__)

        for p_name, f_name in finals:
            if f_name in cls.__dict__:
                raise FinalViolationError('Override of final function [{}] '
                                          'from class [{}] '
                                          'in parent class [{}]'
                                          .format(f_name, p_name, name))


# ============================ TEST ===============================

class A(metaclass=WithFinals):
    def f(self):
        pass

    def g(self):
        pass

    @final
    def fun(self):
        pass


class C(metaclass=WithFinals):
    @final
    def c_fun(self):
        pass


class FromA(A, C):
    def x(self):
        pass


class FromFromA(FromA):
    def f(self):
        pass

    def c_fun(self):
        pass



