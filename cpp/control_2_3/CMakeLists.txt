cmake_minimum_required(VERSION 3.5)
project(control_2_3)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    src/au_allocator.h
    src/test.cpp)

add_executable(control_2_3 ${SOURCE_FILES})