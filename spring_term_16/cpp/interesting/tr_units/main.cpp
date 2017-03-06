#include <iostream>
#include "h1.h"
#include "h2.h"

int main() {
	extern int global_a;
	extern int static_a;
	extern int const_a;
	extern int constexpr_a;

	std::cout << "HELLO!" << std::endl;
	std::cout << "GLOBAL = " << global_a << std::endl;
	// std::cout << "STATIC = " << static_a << std::endl; // ERROR!
	// std::cout << "CONST = " << const_a << std::endl; // ERROR! Const is internally linked
	// std::cout << "CONSTEXPR = " << constexpr_a << std::endl; // ERROR! Constexpr is internally linked!
	return 0;
}