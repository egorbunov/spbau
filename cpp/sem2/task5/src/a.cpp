#include "a.h"

inline int& get_global_a() {
	static int global_a = 0;
	return global_a;
}