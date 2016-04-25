#pragma once

#include <iostream>
#include <tuple>
#include <cstddef>
#include <string>
#include <utility>

template <std::size_t... Is>
struct indices {};

template <std::size_t N, std::size_t... Is>
struct build_indices: build_indices<N-1, N-1, Is...> {};

template <std::size_t... Is>
struct build_indices<0, Is...> : indices<Is...> {};

template<typename T>
using Bare = typename std::remove_cv<typename std::remove_reference<T>::type>::type;

template <typename Tuple>
using IndicesFor = build_indices<std::tuple_size<Bare<Tuple>>::value>;

template<class F, class Tuple>
struct binder {
	F fun;
	Tuple tuple;

	binder(F&& fun, Tuple&& tuple): fun(std::forward<F>(fun)), tuple(std::forward<Tuple>(tuple))
	{}

	void call() {
		
	}
};

template<class F, class... Ts>
binder<F, int> bind(F&& fun, Ts&&... args) {
	return binder<F, int>(fun, 1);
}