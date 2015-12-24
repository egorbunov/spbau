#!/bin/python3


class register(object):
    registers = {}   # cls -> register

    @staticmethod
    def instance_count(cls):
        return register.registers[cls].inst_cnt

    def __init__(self, cls, *args, **kwargs):
        register.registers[cls] = self
        self.cls = cls
        self.inst_cnt = 0

    def __call__(self, *args, **kwargs):
        self.inst_cnt += 1
        return self.cls.__call__(*args, **kwargs)


@register
class A:
    def __init__(self):
        print("init A")
    pass

class B(A):
    pass

a = A()
b = B()

print(register.instance_count(A))
print(register.instance_count(B))
