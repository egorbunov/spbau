#!/bin/python3

class NotRegisterAnnotatedClassError(Exception):
    def __init__(self, message):
        super(NotRegisterAnnotatedClassError, self).__init__(message)

def register(cls):
    class RegisterWrapper(cls):
        __inst_cnt__ = 0
        __name__ = cls.__name__

        def __init__(self, *args, **kwargs):
            RegisterWrapper.__inst_cnt__ += 1
            super(RegisterWrapper, self).__init__(*args, **kwargs)

        @classmethod
        def instance_count(cls):
            if cls.__name__ != RegisterWrapper.__name__:
                raise NotRegisterAnnotatedClassError("error: class {} not" 
                    "annotated with @register".format(cls.__name__))
            return RegisterWrapper.__inst_cnt__

    return RegisterWrapper


# =================================================== tests ===================================================

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
    assert(A.instance_count() == 7)
    assert(C.instance_count() == 2)
    try:
        B.instance_count()
    except NotRegisterAnnotatedClassError as e:
        pass
    else:
        assert(1 == 2)



def test2():
    @register
    class A:
        pass

    class B:
        pass

    @register
    class C(A, B):
        pass

    A(); A(); B(); B(); B(); C(); C()
    assert(A.instance_count() == 4)
    assert(C.instance_count() == 2)


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

    A(); A(); B(); B(); B(); C(); C()
    assert(A.instance_count() == 4)
    assert(B.instance_count() == 5)
    assert(C.instance_count() == 2)



try:
    test1()
    test2()
    test3()
except AssertionError:
    print("FAILED")
else:
    print("PASSED")