#include "my_ostream_con.h"

#include <iostream>

auto mystream::my_ostream_con::operator<<(int x) -> my_ostream& {
	std::cout << x;
	return *this;
}

auto mystream::my_ostream_con::operator<<(double x) -> my_ostream& {
	std::cout << x;
	return *this;
}

auto mystream::my_ostream_con::operator<<(const char* str) -> my_ostream& {
	std::cout << str;
	return *this;
}
