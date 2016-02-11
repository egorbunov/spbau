#include <vector>
#include <set>
#include <iostream>
#include <iterator>
#include <functional>
#include <unordered_set>

// ===================== task 1 =========================

template<typename fwd_it>
void print(fwd_it begin, fwd_it end) {
	for (auto it = begin; it != end; ++it) {
		std::cout << *it << " ";
	}
	std::cout << std::endl;
}

void task1() {
	std::vector<int> vec = {10, -1, 0, 23, 11, 1, 0, 100501 };
	print(std::begin(vec), std::end(vec));
}

// ===================== task 2 =========================

template<typename fwd_it>
void tree_sort(fwd_it begin, fwd_it end) {
	print(begin, end);
}

struct greater {
    bool operator() (const int& lhs, const int& rhs) const{
        return lhs > rhs;
    }
};

void task2() {
	int myints[]= { 1, 100, 2, 3, 123, 32, -100, 32 };
	std::set<int, greater> int_set(myints, myints + 8);
	tree_sort(int_set.begin(), int_set.end());
}

// ===================== task 3 =========================

void task3() {

}

int main() {
	task3();
	return 0;
}