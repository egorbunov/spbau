#include <fstream>
#include <iostream>

using namespace std;

int main() {
	while (true) {
		ofstream out("f.txt", ofstream::out | ofstream::trunc);
		out << "HELLO WORLD" << endl;
	}

	return 0;
}