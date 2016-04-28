#include <iostream>

#include "fn.h"
#include "gtest/gtest.h"

using namespace fn;

int sum_function_1(int x, int y) {
    return x + y;
}

int sum_function_2(const int& x, const int& y) {
    return x + y;
}

int& sum_to_x(int& x, int& y) {
    x += y;
    return x;
}

TEST(bind, bind_simple_no_placeholder_call)
{
    auto binder_1 = bind(sum_function_1, 42, 8);
    ASSERT_EQ(50, binder_1.call());

    int x = 42;
    int y = 8;
    auto binder_2 = bind(sum_function_1, x, y);
    ASSERT_EQ(50, binder_2.call());

    auto binder_3 = bind(sum_function_2, 42, 8);
    ASSERT_EQ(50, binder_3.call());

    auto binder_4 = bind(sum_function_2, x, 8);
    ASSERT_EQ(50, binder_4.call());
}

TEST(bind, bind_reference_params_call)
{
    int rx = 42;
    int ry = 8;
    auto binder_5 = bind(sum_to_x, rx, ry);
    ASSERT_EQ(50, binder_5.call());
    ASSERT_EQ(50, rx);
}