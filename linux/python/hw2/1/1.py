def final(f):
    f.__is_final__ = True
    return f


class FinalViolationError(Exception):
    def __init__(self, message):
        super(FinalViolationError, self).__init__(message)


class WithFinals(type):
    """
    Metaclass, which provides final constraint checking.
    """
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

class B0(object, metaclass=WithFinals):
    @final
    def foo(self):
        pass


class B1(B0):
    pass

class B2(B0):
    pass

class B3(B1, B2):
    def foo(self):
        pass


