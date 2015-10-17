#pragma once
#ifndef HUFFMAN_H_INCLUDED__
#define HUFFMAN_H_INCLUDED__

#include <fstream>
#include <cassert>
#include <queue>
#include <cstdint>
#include "common_defs.h"

namespace au {
    void huffmanEncode(FILE* inFile, FILE* outFile);
    void huffmanDecode(FILE* inFile, FILE* outFile);
}




#endif //HUFFMAN_H_INCLUDED__
