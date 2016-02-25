#include <iostream>
#include <vector>
#include "my_algorithms.h"

template<class Container>
void print(const Container &cont) {
	std::cout << "[ ";
	std::for_each(cont.begin(), cont.end() - 1, 
		[](const typename Container::value_type& x){ std::cout << x << ", "; });
	std::cout << *(cont.end() - 1) << " ]" << std::endl;
}

void test_merge_sort() {
	std::vector<int> vec1 = {6, 5, 2, 10, 3, 4, 4, 5};
	print(vec1);
	merge_sort(vec1.begin(), vec1.end());
	print(vec1);
}

void test_heap_sort() {
	std::vector<int> vec1 = {6, 5, 2, 10, 3, 4, 4, 5};
	print(vec1);
	heap_sort(vec1.begin(), vec1.end());
	print(vec1);
}

int main() {
	// std::vector<int> vec = {1, 2, 3, 4, 5, 6};
	// std::cout << elem_num_in_sorted(vec.begin(), vec.end(), 3) << std::endl;

	// std::cout << *(set_add(vec, 7)) << std::endl;

	// print(vec);
	// erase_if(vec, [](int x){ return x == 3; });
	// print(vec);

	test_merge_sort();
	test_heap_sort();
	return 0;
}