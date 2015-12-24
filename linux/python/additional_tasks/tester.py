#!/bin/python3
import traceback


class Tester(type):
    testable_classes = []

    @staticmethod
    def testAll():
        for cls in Tester.testable_classes:
            print("{} Tests from class {} {}".format('='*10,
                                                     cls.__name__, '='*10))
            Tester.test(cls)

    @staticmethod
    def test(cls):
        d = cls.__dict__
        for k in d:
            if callable(d[k]) and hasattr(d[k], '__test__'):
                try:
                    d[k]()
                except Exception as e:
                    trace_str = "\n" + traceback.format_exc()
                    trace_str = trace_str.replace("\n", "\n" + ' '*10)
                    print("-----> FAILED test [{}.{}] with: {}".
                          format(cls.__name__, k, trace_str))

                else:
                    print("-----> PASSED test [{}.{}]".format(cls.__name__, k))

    def __init__(cls, name, bases, d):
        Tester.testable_classes.append(cls)
        super(Tester, cls).__init__(name, bases, d)


class WrongTestDecoratorUsage(Exception):
    def __init__(self, message):
        super(WrongTestDecoratorUsage, self).__init__(message)


def test(st_method):
    if not isinstance(st_method, staticmethod):
        raise WrongTestDecoratorUsage("error: can't decorate not static "
                                      "method with @test")

    def wrapper(*args, **kwargs):
        return st_method.__func__(*args, **kwargs)
    setattr(wrapper, '__test__', True)
    return wrapper


class A(metaclass=Tester):
    @test
    @staticmethod
    def simple_test_A():
        print("In test")
        assert (2 + 2) == 4

    @test
    @staticmethod
    def test_except_A():
        assert 2 == 4

    @test
    @staticmethod
    def one_more_test_A():
        pass


class B(metaclass=Tester):
    @test
    @staticmethod
    def simple_test_B():
        print("In test")
        assert (2 + 2) == 4

    @test
    @staticmethod
    def test_except_B():
        assert 2 == 4

    @test
    @staticmethod
    def one_more_test_B():
        pass

Tester.test(A)
Tester.testAll()
