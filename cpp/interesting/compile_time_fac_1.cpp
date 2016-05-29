#include <iostream>

template<int N>
struct factorial {
	enum {
		value = factorial<N - 1>::value + factorial<N - 2>::value
	};
};

template<>
struct factorial<0> {
	enum {
		value = 0
	};
};

template<>
struct factorial<1> {
	enum {
		value = 1
	};
};

using namespace std;

int main() {
	static_assert(factorial<10>::value == 55, "");
	return 0;
}