#include "linq.h"
#include <functional>
#include <vector>
#include <iostream>
#include <iterator>
#include <string>

template <typename T>
std::ostream& operator<< (std::ostream& out, const std::vector<T>& v) {
  if (!v.empty()) {
    out << '[';
    std::copy (v.begin(), v.end(), std::ostream_iterator<T>(out, ", "));
    out << "\b\b]";
  }
  return out;
}

int main() {
	std::vector<int> vec = {1, 2, 3, 7, 6, 5};
	auto evec = linq::from(vec);
	auto filtered = evec
		.where([](int x){ return x % 2 == 0; })
		.select<double>([](int x){ return (double) x / 2; });


    std::cout << "input  = " << evec.to_vector() << std::endl;
    std::cout << "count even = " << evec.count([](int x){ return x % 2 == 0; }) << std::endl;
	std::cout << "output = " << filtered.to_vector() << std::endl;
    std::cout << "is empty = " << filtered.any() << std::endl;
    std::cout << "has any zero = " << filtered.any([](double x) { return x == 0; }) << std::endl;

	return 0;
}