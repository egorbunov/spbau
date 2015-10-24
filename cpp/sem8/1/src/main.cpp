#include <iostream>
#include "lazy_string.h"

int main() {
	using namespace std;
	
	lazy::lazy_string str("foobar");

	size_t res = find(str, "bar", 0);

	cout << res;

	return 0;
}