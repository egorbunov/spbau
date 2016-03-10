#include <vector>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <algorithm>

#define TEST_TASK1
#define TEST_TASK2
#define TEST_TASK3

#ifdef TEST_TASK1
#include "ordered_task_queue.h"

void test_ordered_empty()
{
    ordered_task_queue queue{};
    assert(queue.empty());
    assert(queue.run_one() == 0);
    assert(queue.run() == 0);
}

void test_ordered_one_element()
{
    ordered_task_queue queue{};

    bool executed = false;
    queue.push([&executed]{ executed = true; });
    assert(!queue.empty());
    assert(queue.run_one() == 1);
    assert(executed);
    assert(queue.empty());
    assert(queue.run() == 0);

    executed = false;
    queue.push([&executed]{ executed = true; });
    assert(queue.run() == 1);
    assert(executed);
    assert(queue.empty());
}

static void test_ordered_ordering()
{
    std::vector<std::string> dst;
    ordered_task_queue queue;
    auto push_str = [&dst](std::string str) {
        dst.push_back(str);
    };
    queue.push(std::bind(push_str, "2"));
    queue.push(std::bind(push_str, "1"));
    queue.push(std::bind(push_str, "3"));
    while(queue.run_one()) {}
    assert(dst[0] == "2");
    assert(dst[1] == "1");
    assert(dst[2] == "3");
}

void test_ordered_many_elements()
{
    ordered_task_queue queue{};
    int result_loop = 0;
    int result_queue = 0;
    size_t iterations = 100;
    for (size_t i = 0; i < iterations; ++i)
    {
        if (i % 2 == 0)
        {
            queue.push([&result_queue, i] {result_queue *= i; });
            result_loop *= i;
        } else
        {
            queue.push([&result_queue, i] {result_queue += i; });
            result_loop += i;
        }
    }
    assert(iterations == queue.run());
    assert(queue.empty());
    assert(result_loop = result_queue);
}

void test_ordered_move_constructor()
{
    std::vector<std::string> dst;
    ordered_task_queue queue_src;
    auto push_str = [&dst](std::string str) {
        dst.push_back(str);
    };
    queue_src.push(std::bind(push_str, "2"));
    queue_src.push(std::bind(push_str, "1"));
    queue_src.push(std::bind(push_str, "3"));

    ordered_task_queue queue = std::move(queue_src);
    queue.run();
    assert(dst.size() == 3);
}

void test_copy_constructor()
{
    // Shouldn't compile
    /*
    std::vector<std::string> dst;
    ordered_task_queue queue_src;
    auto push_str = [&dst](std::string str) {
        dst.push_back(str);
    };
    queue_src.push(std::bind(push_str, "2"));
    queue_src.push(std::bind(push_str, "1"));
    queue_src.push(std::bind(push_str, "3"));

    ordered_task_queue queue = queue_src;
    queue.run();
    assert(dst.size() == 3);
    */
}
#endif // TEST_TASK1

#ifdef TEST_TASK2
#include "priority_task_queue.h"

void test_priority_two_elements()
{
    priority_task_queue<short, std::greater<short>> queue{};
    int initial = 1;
    int result = initial;
    queue.push([&result]{ result += 2; }, 1);
    queue.push([&result]{ result *= 3; }, 2);
    assert(!queue.empty());
    queue.run();
    assert(result == ((initial + 2) * 3));

    assert(queue.empty());
    result = initial;
    queue.push([&result]{ result += 2; }, 2);
    queue.push([&result]{ result *= 3; }, 1);
    queue.run_one();
    queue.run_one();
    assert(result == ((initial * 3) + 2));
}

static void test_priority_ordering()
{
    std::vector<std::string> dst;
    priority_task_queue<> queue;
    auto push_str = [&dst](std::string str) {
        dst.push_back(str);
    };
    queue.push(std::bind(push_str, "3"), 2);
    queue.push(std::bind(push_str, "2"), 3);
    queue.push(std::bind(push_str, "1"), 1);
    while(queue.run_one()) {}
    assert(dst[0] == "2");
    assert(dst[1] == "3");
    assert(dst[2] == "1");
}

void test_priority_custom_struct()
{
    struct foo
    {
        int a;
    };

    struct foo_compare
    {
        bool operator()(const foo& lhs, const foo& rhs) const {
            return lhs.a < rhs.a;
        }
    };
    
    priority_task_queue<foo, foo_compare> queue{foo_compare()};
    int initial = 1;
    int result = 0;
    queue.push([&result, initial]{ result = initial; }, foo{ 3 });
    queue.push([&result]{ result += 2; }, foo{ 2 });
    queue.push([&result]{ result *= 3; }, foo{ 1 });
    assert(!queue.empty());
    queue.run();
    assert(result == ((initial + 2) * 3));

    assert(queue.empty());
    queue.push([&result, initial]{ result = initial; }, foo{ 3 });
    queue.push([&result]{ result += 2; }, foo{ 1 });
    queue.push([&result]{ result *= 3; }, foo{ 2 });
    queue.run();
    assert(result == ((initial * 3) + 2));
}

void test_priority_many_elements()
{
    std::vector<int> data(100);
    srand(42);
    generate(data.begin(), data.end(), rand);
    priority_task_queue<int, std::greater<int>> queue;
    int result = 0;
    for (size_t num : data)
    {
        queue.push([&result, num]{ result = num; }, num);
    }
    sort(data.begin(), data.end());
    for (size_t i = 0; i < data.size(); ++i)
    {
        queue.run_one();
        assert(result == data[i]);
    }
    assert(queue.empty());
}
#endif // TEST_TASK2

#ifdef TEST_TASK3
#include "priority_task_queue.h"

template<class fwd_it>
void sort_and_compare(fwd_it beg, fwd_it end)
{
    std::vector<typename fwd_it::value_type> reference(beg, end);
    std::sort(reference.begin(), reference.end());
    sleep_sort(beg, end);

    assert(std::equal(reference.begin(), reference.end(), beg));
}

template<class T>
void sort_and_compare(std::vector<T> vector)
{
    sort_and_compare(vector.begin(), vector.end());
}

void test_sleep_sorting()
{
    sort_and_compare(std::vector<int>());

    std::vector<int> data = { 1 };
    sort_and_compare(data);

    data = { 1, 1 };
    sort_and_compare(data);

    data = { 1, 2, 3, 4, 5 };
    sort_and_compare(data);

    data = { 5, 4, 3, 2, 1 };
    sort_and_compare(data);

    srand(42);
    data.resize(100);
    generate(data.begin(), data.end(), rand);
    sort_and_compare(data);
}
#endif // TEST_TASK3

int main()
{
#ifdef TEST_TASK1
    test_ordered_empty();
    test_ordered_one_element();
    test_ordered_ordering();
    test_ordered_many_elements();
    test_ordered_move_constructor();
#endif

#ifdef TEST_TASK2
    test_priority_two_elements();
    test_priority_ordering();
    test_priority_custom_struct();
    test_priority_many_elements();
#endif

#ifdef TEST_TASK3
    test_sleep_sorting();
#endif
}
