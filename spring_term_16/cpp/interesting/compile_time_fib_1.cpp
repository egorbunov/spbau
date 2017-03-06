#include <iostream>

template<int N>
struct fib {
	enum {
		value = fib<N - 1>::value + fib<N - 2>::value
	};
};

template<>
struct fib<0> {
	enum {
		value = 0
	};
};

template<>
struct fib<1> {
	enum {
		value = 1
	};
};

using namespace std;

int main() {
	static_assert(fib<10>::value == 55, "");
	return 0;
}