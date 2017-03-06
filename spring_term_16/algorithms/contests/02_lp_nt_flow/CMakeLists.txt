cmake_minimum_required(VERSION 3.5)
project(02_lp_nt_flow)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

add_executable(a_puzzle src/a_puzzle.cpp)
add_executable(b_inverse src/b_inverse.cpp)
add_executable(c_pairs src/c_pairs.cpp)
add_executable(d_birthday src/d_birthday.cpp)
add_executable(e_taxi src/e_taxi.cpp)
add_executable(g_flow src/g_flow.cpp)
add_executable(h_min_cut src/h_min_cut.cpp)
add_executable(f_snails src/f_snails.cpp)
add_executable(i_molecule src/i_molecule.cpp)