#ifndef VEC_H_INCLUDED__
#define VEC_H_INCLUDED__

#include <cstdio>

namespace au {
    /**
     * Resizing array
     */
    class vector {
        size_t size;
        size_t capacity;
        int *data;
    public:
        vector() : size(0), capacity(0), data(nullptr) { }
        ~vector();

        void add(int x);

        int at(size_t ind);

        size_t get_size();

        int *get_raw_ptr();
    };
}

#endif //VEC_H_INCLUDED__
