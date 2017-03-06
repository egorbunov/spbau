#pragma once

#include <iostream>
#include <typeinfo>

namespace playgr {

template<class T> struct remove_reference      {typedef T type;};
template<class T> struct remove_reference<T&>  {typedef T type;};
template<class T> struct remove_reference<T&&> {typedef T type;};

template<typename T> 
void print_type() {
	std::cout << typeid(T).name();
}

template<typename T>
class what {
public:
	T& t_ref;
	what(T* t_ptr) : t_ref(*t_ptr) {

	}
};

}