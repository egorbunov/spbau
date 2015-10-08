#include <iostream>
#include <cstdio>
#include <vector>

namespace {

    using namespace std;

    /**
     * merge [from, mid) and [mid, to)
     */
    long long merge(vector<int> &array, size_t from, size_t to, size_t mid) {
        size_t a_it = from;
        size_t b_it = mid;

        int *tmp_array = new int[to - from];
        size_t i = 0;

        long long invCnt = 0;

        while (a_it < mid && b_it < to) {
            if (array[a_it] <= array[b_it]) {
                tmp_array[i] = array[a_it];
                a_it++;
            } else {
                invCnt += (mid - a_it);
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

        return invCnt;
    }

    /**
     * recursive merge sort of [from, to) array part
     */
    long long sort(vector<int> &array, size_t from, size_t to) {
        if (to - from == 1) {
            return 0;
        }
        size_t mid = (from + to) / 2;
        long long x = sort(array, from, mid);
        long long y = sort(array, mid, to);
        long long z = merge(array, from, to, mid);
        return x + y + z;
    }

    long long mergesort(vector<int> &array) {
        if (array.size() < 2) {
            return 0;
        }
        return sort(array, 0, array.size());
    }

    long long getInversionNumberFast(vector<int> &array) {
        return mergesort(array);
    }
}