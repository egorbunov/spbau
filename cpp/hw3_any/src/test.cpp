#include <iostream>
#include <cassert>
#include <string>

#include "any.h"
using utils::any;
using utils::any_cast;
using utils::bad_any_cast;
using std::string;
using std::cout;
using std::cerr;
using std::endl;


void construct_test() {
    // value construction
    any a(42);
    any b(42.0);
    any c("42");

    string str = "You!";
    any d(str);

    // copy construction
    any x(d);

    // empty
    any emp;
    assert(emp.empty());
}

void assignment_test() {
    any a(42);
    any b(50);
    a = b;
    b = a;
    any c = "string";
    a = b = c;

    int v = 100;
    a = v;
    b = 123.0;
    c = "new string";
}


template <class T>
void check_cast(any &a, bool should_throw) {
    bool thrown = false;
    try {
        double res = any_cast<T>(a);
        std::cout << res;
    } catch (bad_any_cast const &err) {
        thrown = true;
        std::cerr << err.what() << std::endl;
    }
    assert(should_throw == thrown);
}

void retrieve_value_test() {
    any ia(42);

    int& t = any_cast<int&>(ia);
    std::cout << t; // segfault

    auto res = any_cast<double>(&ia);
    assert(res == nullptr);
    check_cast<double>(ia, true);
    check_cast<int>(ia, false);
}

void swap_test(any &a, any &b) { 
    swap(a, b); 
}

void complex_test() {
    any empty;
    any str(std::string("foobar"));
    empty = str;
    assert(any_cast<string&>(empty) == any_cast<string&>(str));

    any x(123);
    const int cx = any_cast<const int>(x);
    cout << cx;
    const int& rcx = any_cast<const int&>(x);
    cout << rcx << endl;


    const any const_any(42);
    // int *p = any_cast<int>(&const_any);
    // cout << *p << endl;

    const int& val2 = any_cast<const int&>(const_any);
    // val2 += 1;
    cout << val2 << endl;

    int const * pval1 = new int(100);
    any any_p(pval1);
    const int* pval2 = any_cast<const int*>(any_p);
    // *pval2 += 1;
    cout << *pval2 << endl;
    delete pval1;

    int val1 = any_cast<int>(const_any);
    cout << val1 << endl;
}

void complex_test_2() {
    any c(123);
    int& n = any_cast<int&>(c);
    std::cout << n << std::endl;

    any empty;
    any str(std::string("foobar"));
    empty = str;
    (void) any_cast<string&>(empty);

    const any cc(243);
    const int* ptr = any_cast<int>(&cc);
    cout << *ptr << endl;

    any tm(42);
    int tmx = any_cast<int>(std::move(tm));
    cout << tmx << endl;
}

int main(int argc, const char *argv[]) {
    construct_test();
    retrieve_value_test();

    any a(42), b(string("b"));
    swap_test(a, b);
    complex_test();
    complex_test_2();
    return 0;
}