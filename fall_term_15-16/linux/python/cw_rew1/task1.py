class ClassBase(type):
    def __init__(cls, name, bases, d):
        super(ClassBase, cls).__init__(name, bases, d)
        with open(cls.__name__) as file:
            for x, y in [s.strip().split(':', 1) for s in file.readlines()]:
                setattr(cls, x.strip(), y.strip())


class A(metaclass=ClassBase):
    pass

print(A.x)
print(A.y)





