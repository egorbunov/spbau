//
// Created by egorbunov on 10/17/15.
//

#ifndef FIXED_CAPACITY_ARRAY_H_INCLUDED__
#define FIXED_CAPACITY_ARRAY_H_INCLUDED__

#include <cstdio>
#include <stdexcept>

namespace au {
    template<typename T>
    class FixedCapacityArray {
    public:
        FixedCapacityArray(size_t capacity) : capacity(capacity), size(0) {
            array = new T[capacity];
        }

        ~FixedCapacityArray() {
            delete [] array;
            array = nullptr;
        }

        T operator[](size_t i) const {
            if (i >= size) throw std::out_of_range("given index out of bounds!");
            return array[i];
        }

        T &operator[](size_t i) {
            if (i >= size) throw std::out_of_range("given index out of bounds!");
            return array[i];
        }

        /**
         * add element to the end of the array
         */
        void push(T e) {
            if (size == capacity) throw std::overflow_error("capacity overflow!");
            array[size++] = e;
        }

        /**
         * remove element from the end of the array
         */
        void pop() {
            if (size > 0) size -= 1;
        }


        size_t getCapacity() {
            return capacity;
        }

        /**
         * returns actual array size (number of added and not deleted elements)
         */
        size_t getSize() {
            return size;
        }

        void copyTo(FixedCapacityArray& dest) {
            if (dest.capacity < size) throw std::out_of_range("too little destination array!");
            dest.size = size;
            for (size_t i = 0; i < size; ++i) {
                dest[i] = array[i];
            }
        }

    private:
        size_t capacity;
        size_t size;
        T *array;
    };
}

#endif //FIXED_CAPACITY_ARRAY_H_INCLUDED__
