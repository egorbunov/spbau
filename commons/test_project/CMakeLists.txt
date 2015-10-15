cmake_minimum_required(VERSION 3.3)
project(test_project)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES src/artic_points_search.cpp)
add_executable(test_project ${SOURCE_FILES})