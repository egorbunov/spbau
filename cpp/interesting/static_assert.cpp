#include <iostream>

using namespace std;

template<bool C>
struct my_static_assert 
{
	int arr[(C) ? 1 : -1];
};

int main() {
	my_static_assert<2 == 2>();
	my_static_assert<2 == 1>();
	return 0;
}