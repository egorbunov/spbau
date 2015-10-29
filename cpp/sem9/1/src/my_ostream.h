#pragma once

namespace mystream {

class my_ostream {
public:
	virtual ~my_ostream() {};
	virtual my_ostream& operator<<(double) = 0;
	virtual my_ostream& operator<<(int) = 0;
	virtual my_ostream& operator<<(const char*) = 0;
};

}