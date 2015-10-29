#pragma once

#include "my_ostream.h"

namespace mystream {

class my_ostream_con : public my_ostream {
public:
	my_ostream& operator<<(double x) override;
	my_ostream& operator<<(int x) override;
	my_ostream& operator<<(const char* str) override;
};

}