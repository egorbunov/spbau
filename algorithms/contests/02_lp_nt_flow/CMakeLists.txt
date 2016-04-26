cmake_minimum_required(VERSION 3.5)
project(02_lp_nt_flow)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

add_executable(a_puzzle src/a_puzzle.cpp)
add_executable(b_inverse src/b_inverse.cpp)