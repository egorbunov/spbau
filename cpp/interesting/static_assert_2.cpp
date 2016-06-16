#include <iostream>
#include <type_traits>

using namespace std;

template<bool> struct my_static_assert;
template<> struct my_static_assert<true>
{};

int main() {
	my_static_assert<2 != 2>();
	my_static_assert<3 == 3>();

	return 0;
}