#pragma once
#ifndef HUFFMAN_H_INCLUDED__
#define HUFFMAN_H_INCLUDED__

#include <fstream>
#include <cassert>
#include <queue>
#include <cstdint>
#include "common_defs.h"
#include "Result.h"

namespace au {
    /**
     * @return Result<int> - Result object. It's template argument payload is just for...nothing. Use it only
     *         for checking if no errors occurred during function call
     */
    Result<int> huffmanEncode(FILE* inFile, FILE* outFile);
    Result<int> huffmanDecode(FILE* inFile, FILE* outFile);
}


#endif //HUFFMAN_H_INCLUDED__
