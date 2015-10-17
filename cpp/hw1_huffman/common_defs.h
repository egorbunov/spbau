#ifndef COMMON_DEFS_H_INCLUDED__
#define COMMON_DEFS_H_INCLUDED__

#include <cstdint>
#include <cstdio>

namespace au {
    typedef uint8_t byte_t;
    const size_t BITS_IN_BYTE = 8;
    const size_t BYTE_NUMBER = 1 << (BITS_IN_BYTE - 1);
}

#endif //COMMON_DEFS_H_INCLUDED__
