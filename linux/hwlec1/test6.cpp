#include <iostream>
#include <string>

void println(std::string s) {
	std::cout << "That is string: " << s << "\t\t \"Very good!\"" << "\n";
}

// So-called "programming" starts below:
int main() {
	using namespace std;
	string s = "super_string";
	println(s + " - bla bla bla");
	println("Stop \"breaking\" my brain");
	std::cout << "Bad stuff"; /*
			"We must prepare for his return."
	*/
	return 0;
}
