#!/bin/python3


class NotRegisterAnnotatedClassError(Exception):
    def __init__(self, message):
        super(NotRegisterAnnotatedClassError, self).__init__(message)


def register(cls):
    if not hasattr(register, 'registry'):
        register.registry = {}
    register.registry[cls] = [0, 0]  # second is lock counter
    init_save = cls.__init__

    def init_wrapper(self, *args, **kwargs):
        reg = register.registry
        bases = [c for c in type(self).mro() if c in reg and reg[c][1] == 0]
        for b in bases:
            reg[b][0] += 1
            reg[b][1] += 1  # locking

        init_save(self, *args, **kwargs)  # user def. constructor

        for b in bases:
            reg[b][1] -= 1  # unlocking

    def inst_cnt(clazz):
        if clazz not in register.registry:
            raise NotRegisterAnnotatedClassError("error: class {} not"
                                                 "annotated with @register"
                                                 .format(clazz.__name__))
        return register.registry[cls][0]

    setattr(cls, '__init__', init_wrapper)
    setattr(cls, 'instance_count', classmethod(inst_cnt))

    return cls


# ====================== tests ==============================


def test1():
    @register
    class A:
        pass

    class B(A):
        pass

    @register
    class C(B):
        pass

    A(); A(); B(); B(); B(); C(); C()
    assert (A.instance_count() == 7)
    assert (C.instance_count() == 2)
    try:
        B.instance_count()
    except NotRegisterAnnotatedClassError as e:
        pass
    else:
        assert (1 == 2)


def test2():
    @register
    class A:
        pass

    class B:
        pass

    @register
    class C(A, B):
        pass

    A();
    A();
    B();
    B();
    B();
    C();
    C()
    assert (A.instance_count() == 4)
    assert (C.instance_count() == 2)


def test3():
    @register
    class A:
        pass

    @register
    class B:
        pass

    @register
    class C(A, B):
        pass

    A();
    A();
    B();
    B();
    B();
    C();
    C()
    assert (A.instance_count() == 4)
    assert (B.instance_count() == 5)
    assert (C.instance_count() == 2)


def test4():
    @register
    class A:
        pass

    @register
    class B(A):
        def __init__(self):
            print("in B()")

    @register
    class C(B, A):
        pass

    C()
    assert(A.instance_count() == 1)
    assert(B.instance_count() == 1)
    assert(C.instance_count() == 1)


try:
    test1()
    test2()
    test3()
    test4()
except AssertionError:
    print("FAILED")
else:
    print("PASSED")
