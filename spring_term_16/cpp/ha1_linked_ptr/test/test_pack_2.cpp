#include <iostream>
#include <set>
#include "gtest/gtest.h"
#include "linked_ptr.h"

using namespace std;
using namespace smart_ptr;

TEST(LinkedPtr_p2, emptyTests) {
    linked_ptr<int> a;
    linked_ptr<int> copy(a);
    linked_ptr<int> assign = copy;

    ASSERT_FALSE(static_cast<bool>(a));
    ASSERT_EQ(a, copy);
    ASSERT_EQ(a, assign);
    ASSERT_EQ(assign, copy);
}

TEST(LinkedPtr_p2, intTest) {
    linked_ptr<int> a(new int());
    linked_ptr<int> b(new int());
    auto assigned = a;
    auto copy(a);

    ASSERT_EQ(a, copy);
    ASSERT_EQ(a, assigned);
    ASSERT_NE(b, a);

    *a = 5;
    ASSERT_EQ(*a, 5);
    ASSERT_EQ(*copy, 5);
    ASSERT_EQ(*assigned, 5);
}

TEST(LinkedPtr_p2, comparatorsTest) {
    set<linked_ptr<double>> awesome;

    for (int i = 0; i < 1000; ++i)
    {
        awesome.insert(linked_ptr<double>(new double(i)));
    }

    auto dummy_object = linked_ptr<double>(new double(42));
    for (int i = 0; i < 1000; ++i)
    {
        set<linked_ptr<double>> strange_set;
        for (int j = 0; j < 100; ++j)
        {
            strange_set.insert(dummy_object);
        }
    }

    ASSERT_EQ(*dummy_object, 42);
}

struct Mock {
    static int link_counter;

    Mock()
    {
        ++link_counter;
    }

    Mock(Mock const&)
    {
        ++link_counter;
    }

    ~Mock()
    {
        --link_counter;
    }
};

int Mock::link_counter = 0;

TEST(LinkedPtr_p2, refsTest) {
    {
        auto dummy_object = linked_ptr<Mock>(new Mock());

        for (int i = 0; i < 1000; ++i)
        {
            set<linked_ptr<Mock>> strange_set;
            for (int j = 0; j < 100; ++j)
            {
                strange_set.insert(dummy_object);
            }
        }

        ASSERT_EQ(Mock::link_counter, 1);
    }
    assert(Mock::link_counter == 0);

    {
        auto dummy_object = linked_ptr<Mock>(new Mock());
        for (int i = 0; i < 1000; ++i)
        {
            dummy_object = dummy_object;
        }
        ASSERT_EQ(Mock::link_counter, 1);
    }

    {
        set<linked_ptr<Mock>> strange_set;
        for (int j = 0; j < 100; ++j)
        {
            strange_set.insert(linked_ptr<Mock>(new Mock()));
        }

        ASSERT_EQ(Mock::link_counter, 100);
    }
    ASSERT_EQ(Mock::link_counter, 0);

}

TEST(LinkedPtr_p2, testEmptyLptr) {
    linked_ptr<int> lpi_empty;
    ASSERT_FALSE(lpi_empty);
    lpi_empty.reset();
    ASSERT_FALSE(lpi_empty);
    ASSERT_TRUE(!lpi_empty.get());
    ASSERT_TRUE(!lpi_empty.unique());
}

TEST(LinkedPtr_p2, testOneLptr) {
    linked_ptr<int> lpi_single(new int(777));
    ASSERT_TRUE(lpi_single);
    lpi_single.reset(new int(666));
    ASSERT_TRUE(lpi_single);
    ASSERT_TRUE(lpi_single.get());
    ASSERT_TRUE(lpi_single.unique());
}

TEST(LinkedPtr_p2, testSimpleAssignment) {
    linked_ptr<int> ptr1(new int(5));
    linked_ptr<int> ptr2(new int(6));
    ASSERT_EQ(*ptr1, 5);
    ASSERT_EQ(*ptr2, 6);
    ptr1 = ptr2;
    ASSERT_EQ(*ptr1, 6);
    ASSERT_EQ(*ptr2, 6);
}

TEST(LinkedPtr_p2, testManyLptrs) {
    linked_ptr<int> lp;
    lp = linked_ptr<int>(new int(777));
    auto lp1(lp);
    auto lp2(lp1);
    auto lp3(lp2);
    auto lp4(lp);
    auto lp5(lp1);
    auto lp6(lp1);
    auto lp7(lp3);
    auto lp8(lp7);
    auto lp9(lp5);

    lp7.reset();
    auto lp10(lp7);
    auto lp11(lp6);
    auto lp12(lp1);
    auto lp13(lp2);
    lp13.reset(new int(666));
    auto lp14(lp3);
    auto lp15(lp13);
    auto lp16(lp12);
    auto lp17(lp15);
}

TEST(LinkedPtr_p2, testLptrConversions) {
    struct base
    {
        virtual bool is_base() { return true; }
        virtual ~base() {}
    };
    struct derived : base
    {
        virtual bool is_base() { return false; }
    };
    linked_ptr<base> lptr(new base());
    ASSERT_TRUE(lptr->is_base());
    lptr = linked_ptr<base>(new derived());
    ASSERT_TRUE(!lptr->is_base());
}

TEST(LinkedPtr_p2, testLptrDelete) {
    struct foo;
    linked_ptr<foo> lptr{};
    struct foo {}; //compiler error if not defined here
}

struct Base
{
    virtual ~Base() {}
};

struct Derived
    : public Base
{};

TEST(LinkedPtr_p2, resetTests) {
    linked_ptr<Derived> a(new Derived());
    linked_ptr<Base> b(new Base());

    b.reset(new Derived());

    a.swap(a);
    linked_ptr<Derived> c(new Derived());
    c.swap(a);
}