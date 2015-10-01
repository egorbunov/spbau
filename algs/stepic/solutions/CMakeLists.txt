cmake_minimum_required(VERSION 3.3)
project(solutions)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES greedy/point_cover.cpp greedy/uniform_backpack.cpp greedy/n_to_max_sum.cpp greedy/huffman_encode.cpp greedy/huffman_decode.cpp greedy/priority_queue.cpp)
add_executable(solutions ${SOURCE_FILES})