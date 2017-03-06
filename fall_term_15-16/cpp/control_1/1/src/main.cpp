#include <iostream>
#include "mergesort.h"

int main(int argc, char* argv[]) {
    const int sz = 100;
    // random permutation of nat. in range [1,...,100]
    int array[sz] = {87, 2, 51, 93, 77, 74, 27, 78, 90, 63, 46, 7, 61, 13, 97, 21, 82, 22, 62, 41, 1, 14, 73, 5, 33, 91, 100, 59, 31, 6, 10, 57, 39, 32, 79, 34, 98, 69, 85, 67, 88, 95, 71, 96, 4, 40, 44, 18, 80, 25, 43, 76, 66, 19, 24, 84, 68, 75, 55, 15, 47, 92, 36, 65, 11, 29, 35, 60, 81, 9, 50, 54, 3, 53, 48, 89, 20, 58, 8, 42, 52, 17, 38, 23, 30, 94, 28, 70, 37, 16, 49, 86, 12, 26, 56, 72, 64, 99, 45, 83};
    mergesort(array, sz);
    for (int i = 0; i < sz; ++i) {
        std::cout << array[i] << " ";
    }
    std::cout << std::endl;
}
