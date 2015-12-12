#!/bin/python3
import pickle


class Singleton(type):
    """
    Extended Singleton class. Same constructor arguments => one instance.
    """
    object_pool = {}

    def __call__(cls, *args, **kwargs):
        def serialize_args():
            byte_args = pickle.dumps(args)
            byte_kwargs = pickle.dumps(kwargs)
            return byte_args + byte_kwargs

        args_id = serialize_args()
        if args_id not in cls.object_pool:
            cls.object_pool[args_id] = super(Singleton, cls).__call__(
                    *args, **kwargs)
        return cls.object_pool[args_id]


class A(object, metaclass=Singleton):
    def __init__(self, x=1):
        self.value = x

    def __call__(self, *args, **kwargs):
        super.__call__(args, kwargs)
        print("from A")


a = A(1)
b = A(2)
c = A(1)

print(a is b)
print(a is c)
print(c is b)


class B(object, metaclass=Singleton):
    def __init__(self, d, lst, lOfl):
        pass

d1 = {'a': {'c': {'d': "str", 'x': 42}, 'z': 65}, 'y': 123}
d2 = {'g': {'c': {'d': 32131, 'x': 4212}, 'h': 65}, 'y': 123}
l1 = [1, 2, 3, 4, 5]
l2 = [["asd"], [1, 2, 3, 4] , [123, 123, 54], ["42"]]

o1 = B(d1, l1, l2)
o2 = B(d1, l1, l2)
o3 = B(d2, l1, l2)

print(o1 is o2)
print(o2 is o3)
print(o3 is o1)


