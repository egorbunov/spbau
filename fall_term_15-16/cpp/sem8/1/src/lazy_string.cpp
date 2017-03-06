#include "lazy_string.h"

size_t lazy::find(const lazy_string &in, const char* what, size_t start_ix) {

	const char* str = in.get_str();
	size_t what_size = strlen(what);
	for (size_t i = start_ix; i < in.get_size() - what_size + 1; ++i) {
		bool is_eq = true;
		for (size_t j = 0; j < what_size; ++j) {
			if (str[i + j] != what[j]) {
				is_eq = false;
				break;
			}
		}
		if (is_eq)
			return i;

	}
	return -1;
}