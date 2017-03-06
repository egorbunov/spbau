#pragma once

#include <utility>
#include "scoped_ptr.hpp"


template<class T>
class unique_ptr : public scoped_ptr<T> {
public:
	using scoped_ptr<T>::scoped_ptr;
	unique_ptr(unique_ptr &&src);
	unique_ptr& operator=(unique_ptr &&src);
};

template<class T>
unique_ptr<T>::unique_ptr(unique_ptr<T> &&src) {
	swap(*this, src);
}

template<class T>
unique_ptr<T>& unique_ptr<T>::operator=(unique_ptr<T> &&src) {
	if ((*this).get() != src.get()) {
		(*this).reset();
		swap(*this, src);
	}
	return *this;
}

// what about arrow operator?