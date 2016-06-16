#pragma once

#include <iterator>
#include <algorithm>

template<class ForwardIterator, class T>
int elem_num_in_sorted(ForwardIterator begin, ForwardIterator end, const T& elem) {
	ForwardIterator it;
	int cnt = 0;
	for (auto it = begin; it < end; ++it) {
		if (*it == elem) {
			cnt += 1;
		}
	}	
	return cnt;
} 

template<class Container, class T>
typename Container::iterator set_add(Container& cont, const T& val) {
	if (!std::binary_search(cont.begin(), cont.end(), val)) {
		return cont.insert(std::upper_bound(cont.begin(), cont.end(), val), val);
	} else {
		return cont.end();
	}
}

template<class Container, class UnaryPredicate>
void erase_if(Container& cont, UnaryPredicate pred) {
	auto it = std::remove_if(cont.begin(), cont.end(), pred);
	cont.erase(it, cont.end());
}

template<class BidirIt>
void merge_sort(BidirIt begin, BidirIt end) {
	if (std::distance(begin, end) <= 1) {
		return;
	}
	auto mid = begin;
	std::advance(mid, std::distance(begin, end) / 2);
	merge_sort(begin, mid);
	merge_sort(mid, end);
	std::inplace_merge(begin, mid, end);
}

template<class RaIt>
void heap_sort(RaIt begin, RaIt end) {
	std::make_heap(begin, end);
	while (begin < end) {
		std::pop_heap(begin, end--);
	}
}