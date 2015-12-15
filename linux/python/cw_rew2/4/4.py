import sys


def printStack():
    if not hasattr(printStack, 'call_stack'):
        printStack.call_stack = []
    for dep, fun, args in printStack.call_stack:
        print(" "*(dep * 7) + fun + "(" + str(args).strip("[]") + ")")


def trace_function(frame, event, arg):
    if not hasattr(trace_function, 'dep'):
        trace_function.dep = 0

    if not hasattr(printStack, 'call_stack'):
        printStack.call_stack = []

    if event == "call":
        arg_cnt = frame.f_code.co_argcount
        printStack.call_stack.append((trace_function.dep,
                                      frame.f_code.co_name,
                                      [frame.f_locals[
                                           frame.f_code.co_varnames[i]]
                                       for i in range(arg_cnt)]))
        trace_function.dep += 1
    elif event == "return":
        trace_function.dep -= 1
    return trace_function

sys.settrace(trace_function)


def test1(num):
    return (num == 1)


def test2(num):
    return (num > 3)


def foo(num):
    print("Original")
    test1(num)


def foo2(num):
    foo(num)
    test1(num)

foo2(1)
foo(2)

printStack()
