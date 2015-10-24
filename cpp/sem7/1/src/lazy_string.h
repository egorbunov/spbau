#pragma once

#include <iostream>
#include "shared_buffer.h"

namespace lazy {

	struct lazy_string {
		lazy_string(const char *src);
		lazy_string(const lazy_string &src);

		bool empty() const;
		size_t get_size() const;
		char get_at(size_t ix) const;

	private:
		friend void print(std::ostream& os, const lazy_string &str);
		friend lazy_string concat(const lazy_string &str1, const lazy_string &str2);

		size_t size;
		shared_buffer buf;

	};

}
