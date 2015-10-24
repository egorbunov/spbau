#pragma once

#include <string>
#include <cstring>

namespace lazy {
	class lazy_string {
		std::string s;
	public:
		lazy_string(const char* str) {
			s = std::string(str);
		}

		const char* get_str() const {
			return s.c_str();
		}

		size_t get_size() const {
			return s.length();
		}
	};

	size_t find(const lazy_string &in, const char* what, size_t start_ix = 0);
}