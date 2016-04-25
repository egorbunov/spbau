#include <iostream>

#include "fn.h"
#include "gtest/gtest.h"

int fun_to_bind_2(int x, int y) {
    return (x + y);
}

void fun_to_bind_0() {
    std::cout << "Wheeee!" << std::endl;
}

TEST(bind, dummy_test)
{
    bind(fun_to_bind_2, 1, 2);
}