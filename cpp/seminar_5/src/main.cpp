#include <memory>
#include <vector>
#include "shapes.h"
#include "double_dispatcher.hpp"

void test_shape() {
	std::vector<std::shared_ptr<shape>> shapes = {
		std::make_shared<point>(0, 0),
		std::make_shared<rectangle>(0, 0, 0, 0),
		std::make_shared<circle>(0, 0, 0)
	};

	for (auto sptr1 : shapes) {
		for (auto sptr2: shapes) {
			intersect(*sptr1, *sptr2);
		}
	}
}

void test_dispatch() {
	double_dispatcher<shape, int>::reg<point, rectangle>(
		[] (point p, rectangle r) { std::cout << "fun_int(point, rectangle)" << std::endl; }
	);

	double_dispatcher<shape, double>::reg<point, rectangle>(
		[] (point p, rectangle r) { std::cout << "fun_double(point, rectangle)" << std::endl; }
	);

	std::shared_ptr<shape> s1 = std::make_shared<point>(0, 0);
	std::shared_ptr<shape> s2 = std::make_shared<rectangle>(0, 0, 0, 0);

	double_dispatcher<shape, int>::call(*s1, *s2);
	double_dispatcher<shape, double>::call(*s1, *s2);
}

int main() {
	test_dispatch();

	return 0;
}