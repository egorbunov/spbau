import pickle


class LoadError(Exception):
    def __init__(self, message):
        super(LoadError, self).__init__(message)


class InvalidClassStorageError(Exception):
    def __init__(self, message):
        super(InvalidClassStorageError, self).__init__(message)


class ClassStorer(type):
    @staticmethod
    def store(self, file_name):
        try:
            with open(file_name, "wb") as f:
                name = self.__class__.__name__
                to_store = (name, self)
                pickle.dump(to_store, f)
        except FileNotFoundError:
            raise LoadError("error: can't find file [{}]".format(file_name))

    def restore(cls, file_name):
        try:
            with open(file_name, "rb") as f:
                name, obj = pickle.load(f)
                if name != cls.__name__:
                    raise InvalidClassStorageError(
                            "error: can't load class [{}] "
                            "from file for class [{}]".
                            format(cls.__name__, name))
                else:
                    return obj
        except FileNotFoundError:
            raise LoadError("error: can't find file [{}]".format(file_name))

    def __call__(cls, *args, **kwargs):
        # super(ClassStorer, cls).__call__(*args, **kwargs)
        if len(args) == 1 and len(kwargs) == 0 and isinstance(args[0], str):
            return cls.restore(args[0])
        return super(ClassStorer, cls).__call__(*args, **kwargs)

    def __init__(cls, name, bases, d):
        super(ClassStorer, cls).__init__(name, bases, d)
        cls.store = ClassStorer.store


class A(metaclass=ClassStorer):
    pass


class B(metaclass=ClassStorer):
    pass

# test ok


def fun(y):
    print(y)

a = A()
a.x = 5
a.y = [1, 2, 3]
a.f = fun

a.store("x.txt")
b = A("x.txt")
print(b.__dict__)
b.f("Hello world!")

# test except

b = B("x.txt")