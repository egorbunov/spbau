#include "my_ostream_con.h"
#include <iostream>
int main() {
	using namespace mystream;

	my_ostream_con *ps = new my_ostream_con();
	my_ostream *pms = ps;

	std::cout << &ps << " " << &pms << std::endl;

	my_ostream_con my_cout;

	my_cout << 0 << " " << 0.5 << "!!!" << "\n";

	return 0;
}