#include "vector.h"

void au::vector::add(int x) {
    if (size == capacity) {
        capacity = (capacity == 0) ? 1 : capacity * 2;

        int *tmp = data;
        data = new int[capacity];
        for (int i = 0; i < size; ++i)
            data[i] = tmp[i];
        delete [] tmp;
    }

    data[size++] = x;
}

int au::vector::at(size_t ind) {
    return data[ind];
}

size_t au::vector::get_size() {
    return size;
}

int* au::vector::get_raw_ptr() {
    return data;
}

au::vector::~vector() {
    if (data != nullptr) {
        delete [] data;
    }
}
