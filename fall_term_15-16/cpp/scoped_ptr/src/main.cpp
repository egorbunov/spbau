#include <iostream>
#include <string>
#include <utility>
#include "scoped_ptr.hpp"
#include "unique_ptr.hpp"

using namespace std;

struct foo
{
	int x;
	int y;
	string str;

	foo(): x(0), y(0), str("Hello, world!") {
	}
};

ostream& operator<<(ostream& out, const foo& obj) {
	out << "[ " << obj.x << ", " << obj.y << "; " << obj.str << " ]";
	return out;
}

void bool_fun(bool x) {
	return;
}

void bar() {
	scoped_ptr<foo> foo_ptr(new foo());
	cout << "Asteric op : " << (*foo_ptr) << endl 
		 << "Arrow op   : " << foo_ptr->str << endl
		 << "get()      : " << foo_ptr.get() << endl;

	if (foo_ptr)
		foo_ptr->x += 1;

	cout << "Change test: " << *foo_ptr << endl;

	foo_ptr.reset();
	if (!foo_ptr) {
		cout << "OK" << endl;
	}

	foo_ptr.reset(new foo());
	cout << foo_ptr->str << endl;
	// cout << "T1" << endl;
	// cout << foo_ptr->x;

	// scoped_ptr<foo> foo_ptr2;
	// cout << "T1" << endl;
	// cout << foo_ptr2->x;

	// foo_ptr = std::move(foo_ptr);
	// auto foo_ptr2(foo_ptr);
	// foo_ptr = foo_ptr;
	// bool_fun(foo_ptr);

	decltype(foo_ptr)::element_type foo_on_stack;
	cout << "ON STACK: " << foo_on_stack.str << endl;

	// UNIQUE PTR
	unique_ptr<foo> foo_uptr0(new foo());
	unique_ptr<foo> foo_uptr1 = std::move(foo_uptr1);
}

int main() {
	bar();
	return 0;
}