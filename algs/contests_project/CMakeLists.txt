cmake_minimum_required(VERSION 3.3)
project(contests_project)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES contest1/F.cpp)
add_executable(contests_project ${SOURCE_FILES})