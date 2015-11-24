cmake_minimum_required(VERSION 3.3)
project(hw2_lint)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp lint.cpp lint.h BigInt.h)
add_executable(hw2_lint ${SOURCE_FILES})