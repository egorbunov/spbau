#pragma once

#include <functional>
#include <utility>
#include <algorithm>
#include <vector>

namespace utils {

template<class>
class event;

template<class R, class ... A>
class event<R(A...)> {
public:
	typedef std::function<R(A...)> handler_t;

	event():id(0) {};
	event(event const &) = delete;
	event(event&&) = default;
	event& operator=(event const &) & = delete;
	event& operator=(event&&) & = default;

	void add(handler_t handler) {
		handlers.push_back(std::move(handler));
		return std::bind(&event::handler_deletor, this, handlers.end() - 1)
	}

	void fire(A... args) {
		std::for_each(handlers.begin(), handlers.end(), [&args...](handler_t& h) { 
			h(args...); 
		});
	}

private:
	int id;
	std::vector<handler_t> handlers;

	void handler_deletor(int id) {
	}
};

}