#include <iostream>
#include <string>
//#include <functional>

#include "fn.h"
#include "gtest/gtest.h"

//using namespace std;
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

// mr. Lesin tests

int global_var;
static bool post_increment_global_var(int amount=1)
{
    int old_val = global_var;
    global_var += amount;
    return old_val;
}

TEST(bind, test_1_arg_func_bind)
{
    global_var = 0;
    ASSERT_TRUE(bind(post_increment_global_var, 777)() == 0);
    ASSERT_TRUE(global_var == 777);
    global_var = 0;
    ASSERT_TRUE(bind(post_increment_global_var, _1)(777) == 0);
    ASSERT_TRUE(global_var == 777);
}

static int take_first(int first, int second)
{
    return first;
}

static int take_second(int first, int second)
{
    return second;
}

TEST(bind, test_2_arg_func_bind)
{
    ASSERT_TRUE(bind(take_first, 111, 222)() == 111);
    ASSERT_TRUE(bind(take_first, _1, 222)(111) == 111);
    ASSERT_TRUE(bind(take_first, 111, _1)(222) == 111);
    ASSERT_TRUE(bind(take_first, _1, _2)(111, 222) == 111);
    ASSERT_TRUE(bind(take_first, _2, _1)(111, 222) == 222);

    ASSERT_TRUE(bind(take_second, 111, 222)() == 222);
    ASSERT_TRUE(bind(take_second, _1, 222)(111) == 222);
    ASSERT_TRUE(bind(take_second, 111, _1)(222) == 222);
    ASSERT_TRUE(bind(take_second, _1, _2)(111, 222) == 222);
    ASSERT_TRUE(bind(take_second, _2, _1)(111, 222) == 111);
}

template<class T>
typename std::decay<T>::type func_arb_arg(T &&arg)
{
    return arg;
}

static int arg_by_val(int i)
{
    return i;
}

TEST(bind, test_passing_by_value)
{
    int i = 777;
    auto binder = bind(arg_by_val, i);
    ASSERT_TRUE(i == binder());
}

static int arg_by_ref(int &i)
{
    return i;
}

TEST(bind, test_passing_by_ref)
{
    int i = 777;
    auto binder = bind(arg_by_ref, i);
    ASSERT_TRUE(i == binder());
}

static int arg_by_const_ref(const int &i)
{
    return i;
}

TEST(bind, test_passing_by_const_ref)
{
    int i = 777;
    auto binder = bind(arg_by_const_ref, i);
    ASSERT_TRUE(i == binder());
}


TEST(bind, test_passing_by_rvalue_ref)
{
    struct test_struct
    {
        test_struct()
                : was_moved_(false)
        {}

        test_struct(const test_struct &src)
                : was_moved_(false)
        {
        }

        test_struct(test_struct &&src)
                : was_moved_(false)
        {
            src.was_moved_ = true;
        }

        bool was_moved_;
    };

    test_struct arg;
    auto src = bind(func_arb_arg<test_struct&&>, std::move(arg));
    ASSERT_TRUE(arg.was_moved_);
    (void)src;
}

TEST(bind, test_placeholder_validity)
{
    auto _1_copy(_1);
    _1_copy = _1;
    auto _1_ptr = &_1;
    bind(take_second, *_1_ptr, 2)(1);
}

TEST(bind, test_binder_move_constructor)
{
    struct test_struct
    {
        test_struct()
                : was_moved_(false)
        {}

        test_struct(const test_struct &src)
                : was_moved_(src.was_moved_)
        {}

        test_struct(test_struct &&src)
                : was_moved_(true)
        {
            src.was_moved_ = true;
        }

        bool was_moved_;
    };

    test_struct arg;
    auto src = bind(func_arb_arg<test_struct&>, arg);
    src();
    auto moved = std::move(src);
    ASSERT_TRUE(moved().was_moved_);
}

TEST(bind, test_binder_copy_constructor)
{
    auto src = bind(take_first, 1, _1);
    auto copy(src);
    ASSERT_TRUE(src(2) == copy(2));
}


TEST(bind, test_binder_move_constructor1)
{
    struct test_struct
    {
        int& copy_cnt;
        int& move_cnt;
        test_struct(int& copy_cnt, int& move_cnt): copy_cnt(copy_cnt), move_cnt(move_cnt)
        {}

        test_struct(const test_struct &src): copy_cnt(src.copy_cnt), move_cnt(src.move_cnt)
        {
            ++copy_cnt;
        }

        test_struct(test_struct &&src): copy_cnt(src.copy_cnt), move_cnt(src.move_cnt)
        {
            ++move_cnt;
        }
    };

    int copy_cnt = 0;
    int move_cnt = 0;
    test_struct arg(copy_cnt, move_cnt);
    std::cout << "constructor\n";
    auto src = bind(func_arb_arg<test_struct&>, arg); // 1st copy
    auto res = src(); // 2nd copy
    auto moved = std::move(src); // 1st move
    ASSERT_TRUE(move_cnt == 1);
    ASSERT_TRUE(copy_cnt == 2);
    (void)moved;
    (void)res;
}

TEST(bind, test_assignment)
{
    /* There is no assignment in standard bind. */
    /* So ignore this task. */
    auto inst1 = bind(take_first, 1, _1);
    auto inst2 = bind(take_second, 2, _1);
    ASSERT_TRUE(inst1(2) == 1);
    ASSERT_TRUE(inst2(1) == 1);
    inst2 = inst1;
    ASSERT_TRUE(inst1(2) == 1);
}