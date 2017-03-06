#include <iostream>
#include "shared_buffer.h"


using namespace std;

int main () {
	shared_buffer sb(100);
	// shared_buffer sb1(200);
	std::cout << "-----------" << std::endl;
	shared_buffer x = sb;
	x = sb;
	// shared_buffer y = sb;
    // x = sb1;

	// cout << "sb: " << sb.get_ref_cnt() << endl;
	// cout << "x : " << x.get_ref_cnt() << endl;
	// cout << "y : " << y.get_ref_cnt() << endl;

	return 0;
}