#include <iostream>

#include "test.h"

void fun_to_bind(int a, int b) {
	std::cout << "I'am called =) with args: [" << a << ", " << b << " ]" << std::endl;
}

int main() {
	bind(fun_to_bind, 1, 2);
}