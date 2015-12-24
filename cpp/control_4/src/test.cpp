#include "maybe.h"
#include <string>
#include <assert.h>

using namespace utils;

namespace
{
    struct test_obj
    {
        test_obj(const char* s)
                : str(s)
        {
            ++counter();
        }

        test_obj(test_obj const& other)
                : str(other.str)
        {
            ++counter();
        }

        ~test_obj()
        {
            --counter();
        }

        static void verify_counter()
        {
            assert(counter() == 0);
        }

        std::string const& to_str() const
        {
            return str;
        }

    private:
        test_obj& operator=(test_obj const&);

        static size_t& counter()
        {
            static size_t counter = 0;
            return counter;
        }

    private:
        std::string str;
    };



    bool operator==(test_obj const& lhs, test_obj const& rhs)
    {
        return lhs.to_str() == rhs.to_str();
    }

    //////////////////////////////////////////////////////////////////////////

    // Task 1

    void test_construction_empty()
    {
        maybe<test_obj> os_uninit1;
        assert(!os_uninit1);

        maybe<test_obj> os_uninit2(empty);
        assert(!os_uninit2);
    }

    void test_construction_value()
    {
        maybe<test_obj> mto1("abra");
        assert(mto1);
    }

    void test_get()
    {
        maybe<test_obj> mto1("abra");
        assert(mto1);

        test_obj to2("abra");
        assert(mto1.get() == to2);

        const maybe<test_obj> mto2("abra");
        assert(mto2);
        assert(mto2.get() == to2);
    }

    void test_exception_on_value()
    {
        maybe<int> x;
        bool caught = false;

        try
        {
            int y = x.get();
            (void)y;
        }
        catch (std::exception const&)
        {
            caught = true;
        }

        assert(caught);
        x = 5;

        int y = x.get();
        (void)y;
    }


    // Task 2

    void test_copy_construction()
    {
        maybe<test_obj> os_uninit1;
        assert(!os_uninit1);

        maybe<test_obj> os_copy_val1("foobar");
        assert(os_copy_val1);

        maybe<test_obj> os_copy_uninit1(os_uninit1);
        assert(!os_copy_uninit1);

        maybe<test_obj> os_copy_init1(os_copy_val1);
        assert(os_copy_init1);
        assert(os_copy_init1.get() == os_copy_val1.get());

        test_obj to("hello");

        maybe<test_obj> x("hello");
        maybe<test_obj> y = to;

        assert(x.get() == y.get());
    }

    void test_assignment()
    {
        maybe<test_obj> os_uninit1;
        maybe<test_obj> os_uninit2;

        maybe<test_obj> os_copy_val1("foobar");
        assert(!os_uninit2);
        os_uninit2 = os_uninit1;
        assert(!os_uninit2);
        os_uninit2 = os_copy_val1;
        assert(os_uninit2);

        assert(os_uninit2.get() == os_copy_val1.get());
    }


    void test_accessors()
    {
        maybe<test_obj> os_uninit3;
        assert(!os_uninit3);
        maybe<test_obj> os_val1("111");
        assert(*os_val1 == "111");
        os_val1 = "foobar1";
        assert(*os_val1 == "foobar1");
        assert(os_val1->to_str() == "foobar1");
        os_uninit3 = "foobar2";
        assert(*os_uninit3 == "foobar2");
    }

} // 'anonymous'

int main()
{
    try
    {
        // task 1
        test_construction_empty();
        test_construction_value();
        test_get();
        test_exception_on_value();

        // task 2
        test_copy_construction();
        test_assignment();
//
//        // task 3
        test_accessors();
    }
    catch (...)
    {
        assert(false);
    }

    test_obj::verify_counter();
    return 0;
}