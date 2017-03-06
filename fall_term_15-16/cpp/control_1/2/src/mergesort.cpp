#include "mergesort.h"

namespace {
    /**
     * merge [from, mid) and [mid, to)
     */
    void merge(int *array, size_t from, size_t to, size_t mid) {
        size_t a_it = from;
        size_t b_it = mid;

        int *tmp_array = new int[to - from];
        size_t i = 0;

        while (a_it < mid && b_it < to) {
            if (array[a_it] < array[b_it]) {
                tmp_array[i] = array[a_it];
                a_it++;
            } else {
                tmp_array[i] = array[b_it];
                b_it++;
            }
            ++i;
        }
        while (a_it < mid) {
            tmp_array[i++] = array[a_it++];
        }
        while (b_it < to) {
            tmp_array[i++] = array[b_it++];
        }

        for (size_t j = from; j < to; ++j) {
            array[j] = tmp_array[j - from];
        }

        delete[] tmp_array;
    }

    /**
     * recursive merge sort of [from, to) array part
     */
    void sort(int* array, size_t from, size_t to) {
        if (to - from == 1) {
            return;
        }
        size_t mid = (from + to) / 2;
        sort(array, from, mid);
        sort(array, mid, to);
        merge(array, from, to, mid);
    }
}

void mergesort(int* array, size_t size) {
    if (size < 2) {
        return;
    }
    sort(array, 0, size);
}