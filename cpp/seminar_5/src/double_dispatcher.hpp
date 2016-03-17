#pragma once

#include <functional>
#include <map>
#include <utility>
#include <typeindex>
#include <iostream>
#include <cassert>

using std::map;
using std::pair;
using std::type_index;
using std::function;

/**
 * T -- base class type
 * ID -- multimethod identifier
 */
template<class T, typename ID>
class double_dispatcher {
public:
	template<class ARG1, class ARG2>
	static void reg(std::function<void(ARG1, ARG2)> fun) {
		auto key = std::make_pair(
			type_index(typeid(ARG1)), 
			type_index(typeid(ARG2))
		);

		methods[key] = [fun](T& a, T&b) {
			fun(dynamic_cast<ARG1&>(a), dynamic_cast<ARG2&>(b));
		};
	}

	static void call(T& a, T& b) {
		auto key_a = std::make_pair(type_index(typeid(a)), type_index(typeid(b)));

		auto pair_it = methods.find(key_a);
		if (pair_it != methods.end()) {
			pair_it->second(a, b);
		} else {
			auto key_b = std::make_pair(type_index(typeid(b)), type_index(typeid(a)));
			pair_it = methods.find(key_b);
			if (pair_it != methods.end()) {
				pair_it->second(b, a); // HMMMM
			} else {
				std::cout << "Method [ void(" << typeid(a).name() 
						  << ", " << typeid(b).name() << " ] not found!" << std::endl;
				// ERROR
			}
		}
	}
private:
	static map<pair<type_index, type_index>, function<void(T&, T&)>> methods; 
};

template<class T, typename ID>
map<pair<type_index, type_index>, function<void(T&, T&)>> double_dispatcher<T, ID>::methods;