import pickle


class LoadError(Exception):
    def __init__(self, message):
        super(LoadError, self).__init__(message)


class InvalidClassStorageError(Exception):
    def __init__(self, message):
        super(InvalidClassStorageError, self).__init__(message)


class SuperStorer(type):
    def store(self, file_name):
        # get instances dict
        inst_dict = {}
        for k in globals():
            if globals()[k] in self.instances:
                inst_dict[k] = globals()[k]
        # print(inst_dict)
        try:
            with open(file_name, "wb") as f:
                name = self.__name__
                # print(name)
                to_store = (name, self, inst_dict)
                pickle.dump(to_store, f)
        except FileNotFoundError:
            raise LoadError("error: can't find file [{}]".format(file_name))

    @staticmethod
    def restore(file_name):
        try:
            with open(file_name, "rb") as f:
                # class BaseClass(object):
                #     def __init__(self, classtype):
                #         self._type = classtype
                name, cls, instances = pickle.load(f)
                globals()[name] = cls
                for k in instances:
                    globals()[k] = instances[k]
                # print(name)
        except FileNotFoundError:
            raise LoadError("error: can't find file [{}]".format(file_name))

    def __call__(cls, *args, **kwargs):
        if len(args) == 1 and len(kwargs) == 0 and isinstance(args[0], str):
            return cls.restore(args[0])
        inst = super(SuperStorer, cls).__call__(*args, **kwargs)
        cls.instances.append(inst)
        return inst

    def __init__(cls, name, bases, d):
        cls.instances = []
        super(SuperStorer, cls).__init__(name, bases, d)


class A(metaclass=SuperStorer):
    def __init__(self, x):
        self.x = x

    def __str__(self):
        return str(self.x)

# class B(metaclass=SuperStorer):
#     pass


a = A(10)
b = A(100)

A.store("file.txt")

del a
del b

SuperStorer.restore("file.txt")

print(a)
print(b)
