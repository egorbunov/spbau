#pragma once

#include <vector>
#include <cmath>
#include <list>
#include <cstdint>
#include <iostream>
#include <stdexcept>
#include <cassert>

struct au_allocator {
    au_allocator(size_t max_order = 7): max_size((size_t) (1 << (max_order - 1))) {
    	if (max_order == 0 || N < max_size) {
    		throw std::logic_error("Max order must be > 0 and < log(2, 4096)");
    	}

    	for (size_t order = 0; order < max_order; ++order) {
    		chunks_for_order.push_back(chunks_for_order_t(order));
    	}
    }

    void* allocate(size_t size) {
    	if (size > max_size) {
    		return malloc(size);
    	}
    	size_t c_idx = get_chunk_idx(size);
    	return chunks_for_order[c_idx].allocate();
    }

    void deallocate(void* ptr, size_t size) {
    	if (size > max_size) {
    		free(ptr);
    	} else {
    		size_t c_idx = get_chunk_idx(size);
    		chunks_for_order[c_idx].deallocate(ptr);
    	}
    }

    template<class T, class... Types>
    T* allocate(Types&&... params) {
    	void* mem = allocate(sizeof(T));
    	if (mem == nullptr) {
    		return nullptr;
    	}
    	return new(mem) T(std::forward<Types>(params)...);
    }

    template<class T>
    void deallocate(T* const ptr) {
    	ptr->~T();
    	deallocate((void*) ptr, sizeof(T));
    }

private:
	static const size_t ORDER_LIMIT = 11;
	static const size_t N = 2 << ORDER_LIMIT;

	struct chunks_for_order_t {
		size_t order;
		std::vector<uint8_t*> chunks;
		std::list<void*> free_bins;

		chunks_for_order_t(size_t order): order(order), bin_cnt(N / (1 << order)), bin_size(1 << order) {
			assert(au_allocator::N % (1 << order) == 0);
		}

        ~chunks_for_order_t() {
            for (uint8_t* ptr : chunks) {
                delete ptr;
            }
        }

		void* allocate() {
			if (free_bins.empty()) {
				add_chunk();
			}
			auto free_it = free_bins.begin();
			void* res = *free_it;
			free_bins.erase(free_it);
			return res;
		}

		void deallocate(void* ptr) {
			free_bins.push_back(ptr);
		}

	private:
		const size_t bin_cnt;
		const size_t bin_size;
		void add_chunk() {
			uint8_t* new_chunk = new uint8_t[N];
			chunks.push_back(new_chunk);
			for (size_t i = 0; i < N; i += bin_size) {
				free_bins.push_back((void*) &(new_chunk[i]));
			}
		}
	};

	size_t max_size;
	std::vector<chunks_for_order_t> chunks_for_order;

	size_t get_chunk_idx(size_t size) {
    	size_t c_idx = 0;
    	while (size > (size_t) (1 << c_idx)) {
    		c_idx++;
    	}
    	return c_idx;
	}
};
