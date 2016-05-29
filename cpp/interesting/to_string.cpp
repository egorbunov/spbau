#include <iostream>
#include <type_traits>
#include <string>
#include <sstream>

using namespace std;

template<typename T>
auto to_string(const T& x) -> typename enable_if<is_integral<T>::value, string>::type {
	stringstream s;
	s >> x;
	return s.str();
}


int main() {
	cout << to_string(10);
	return 0;
}