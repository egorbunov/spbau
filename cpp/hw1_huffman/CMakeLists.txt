cmake_minimum_required(VERSION 2.8)
project(hw1_huffman)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp huffman.cpp huffman.h FixedCapacityArray.h bit_io.cpp bit_io.h common_defs.h)
add_executable(hw1_huffman ${SOURCE_FILES})