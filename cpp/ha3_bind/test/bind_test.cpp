#include <iostream>
#include <string>

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

template<class T>
auto sfinae_placeholder_test(T &&arg) -> typename std::enable_if<!is_placeholder<typename std::decay<T>::type>::value, std::string>::type
{
    return "not placeholder";
}

template<class T>
auto sfinae_placeholder_test(T &&arg) -> typename std::enable_if<is_placeholder<typename std::decay<T>::type>::value, std::string>::type
{
    return "placeholder";
}

TEST(placeholder, is_placeholder_sfinae_test) {
    ASSERT_EQ(sfinae_placeholder_test(_1), "placeholder");
    ASSERT_EQ(sfinae_placeholder_test(1), "not placeholder");
    ASSERT_EQ(sfinae_placeholder_test(placeholder_t<2>()), "placeholder");
}

TEST(bind, simple_bind_placeholder_test_1) {
    auto sum_bind = bind(sum_function_1, _1, _2);
    ASSERT_EQ(10, sum_bind(7, 3));

    int x = 3, y = 7;
    ASSERT_EQ(10, sum_bind(x, y));
}

TEST(bind, simple_bind_placeholder_test_2) {
    auto sum_bind = bind(sum_function_2, _1, _2);
    ASSERT_EQ(10, sum_bind(7, 3));

    int x = 3, y = 7;
    ASSERT_EQ(10, sum_bind(x, y));
}

TEST(bind, ref_bind_placeholder_test) {
    auto sum_bind = bind(sum_to_x, _1, _2);

    int x = 3, y = 7;
    ASSERT_EQ(10, sum_bind(x, y));
    ASSERT_EQ(10, x);
}

int sub_function_1(int x, int y) {
    return x - y;
}

TEST(bind, test_placeholders_positioning_1)
{
    auto sub_bind_1 = bind(sub_function_1, _1, _2);
    auto sub_bind_2 = bind(sub_function_1, _2, _1);
    ASSERT_EQ(1, sub_bind_2(2, 3));
    ASSERT_NE(sub_bind_2(10, 100), sub_bind_1(10, 100));
}

TEST(bind, test_parameter_number_doesent_matter)
{
    auto sub_bind_1 = bind(sub_function_1, _1, _2);
    ASSERT_EQ(10, sub_bind_1(10, 0, "Hello!", "Random!", 42, 42, 123123));

    auto sub_bind_2 = bind(sub_function_1, _3, _6);
    ASSERT_EQ(10, sub_bind_2("_", "_", 100, "_", "_", 90, "_"));
}

std::string complex_fun(std::string x, std::string& y, std::string&& z, const std::string& o)
{
    return x + y + z + o;
}

TEST(bind, partly_binded_test)
{
    std::string s = "2";

    auto fb = bind(complex_fun, _1, s, _2, "4");
    ASSERT_EQ("1234", fb("1", "3"));
}