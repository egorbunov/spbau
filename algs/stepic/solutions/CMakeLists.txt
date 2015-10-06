cmake_minimum_required(VERSION 3.3)
project(solutions)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES sortsearch/inversioncount.cpp)
add_executable(solutions ${SOURCE_FILES})