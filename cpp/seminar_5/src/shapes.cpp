#include "shapes.h"

#include <iostream>

void intersect(shape &a, shape& b) {
	a.intersect(b);
}

// ----------- rectangle -------------

void rectangle::intersect(shape &other) {
	other.intersect_impl(*this);
}

void rectangle::intersect_impl(point &other) {
	std::cout << "intersecting rectangle with point" << std::endl;
}

void rectangle::intersect_impl(rectangle &other) {
	std::cout << "intersecting rectangle with rectangle" << std::endl;
}

void rectangle::intersect_impl(circle &other) {
	std::cout << "intersecting rectangle with circle" << std::endl;
}

// ----------- circle -------------

void circle::intersect(shape &other) {
	other.intersect_impl(*this);
}

void circle::intersect_impl(point &other) {
	std::cout << "intersecting circle with point" << std::endl;
}

void circle::intersect_impl(rectangle &other) {
	std::cout << "intersecting circle with rectangle" << std::endl;
}

void circle::intersect_impl(circle &other) {
	std::cout << "intersecting circle with circle" << std::endl;
}

// ----------- point -------------

void point::intersect(shape &other) {
	other.intersect_impl(*this);
}

void point::intersect_impl(point &other) {
	std::cout << "intersecting point with point" << std::endl;
}

void point::intersect_impl(rectangle &other) {
	std::cout << "intersecting point with rectangle" << std::endl;
}

void point::intersect_impl(circle &other) {
	std::cout << "intersecting point with circle" << std::endl;
}