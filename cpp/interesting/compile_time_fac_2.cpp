#include <iostream>

using namespace std;

constexpr int f(int n) {
	return (n <= 1) ? n : (f(n - 2) + f(n - 1));
}

int main() {
	static_assert(f(10) == 55, "");
	return 0;
}