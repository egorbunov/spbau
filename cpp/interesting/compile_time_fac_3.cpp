#include <iostream>
#include <cassert>

template<int N>
struct factorial {
	constexpr static int value = factorial<N - 1>::value + factorial<N - 2>::value;
};

template<>
struct factorial<0> {
	constexpr static int value = 0;
};

template<>
struct factorial<1> {
	constexpr static int value = 1;
};

using namespace std;

int main() {
	static_assert(factorial<10>::value == 55, "");
	return 0;
}