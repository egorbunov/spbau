cmake_minimum_required(VERSION 3.3)
project(control_3)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES main.cpp src/au_list.h)
add_executable(control_3 ${SOURCE_FILES})