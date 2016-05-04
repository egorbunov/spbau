#include <iostream>

#include "fn.h"
#include "gtest/gtest.h"

using namespace fn;

int sum_function_1(int x, int y)
{
    return x + y;
}

int sum_function_2(const int& x, const int& y)
{
    return x + y;
}

int& sum_to_x(int& x, int& y)
{
    x += y;
    return x;
}

TEST(bind, bind_simple_no_placeholder_call)
{
    auto binder_1 = bind(sum_function_1, 42, 8);
    ASSERT_EQ(50, binder_1());

    int x = 42;
    int y = 8;
    auto binder_2 = bind(sum_function_1, x, y);
    ASSERT_EQ(50, binder_2());

    auto binder_3 = bind(sum_function_2, 42, 8);
    ASSERT_EQ(50, binder_3());

    auto binder_4 = bind(sum_function_2, x, 8);
    ASSERT_EQ(50, binder_4());
}

TEST(bind, bind_reference_params_no_placeholders_call)
{
    int rx = 42;
    int ry = 8;
    auto binder_5 = bind(sum_to_x, rx, ry);
    ASSERT_EQ(50, binder_5());
    ASSERT_EQ(50, rx);
}

TEST(placeholder, is_placeholder_test)
{
    ASSERT_TRUE(is_placeholder<decltype(_1)>::value);
    ASSERT_TRUE(is_placeholder<decltype(_2)>::value);
    ASSERT_TRUE(is_placeholder<decltype(_3)>::value);
    ASSERT_TRUE(is_placeholder<decltype(_4)>::value);
    ASSERT_TRUE(is_placeholder<decltype(_5)>::value);
    ASSERT_TRUE(is_placeholder<const fn::placeholder_t<1ul>>::value);
    ASSERT_FALSE(is_placeholder<int>::value);
    ASSERT_FALSE(is_placeholder<double>::value);
}

void my_print(float x) {
    std::cout << x << std::endl;
}

TEST(bind, simple_placeholder_test) {
    auto b = bind(my_print, _1);
    b(1.0);
}