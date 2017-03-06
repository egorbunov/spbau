find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS} ${ALGO_SOURCE_DIR}/src)
add_executable (Tests tests.cpp)
target_link_libraries (Tests
					   # ToTest
                       ${GTEST_MAIN_LIBRARIES}
                       ${GTEST_LIBRARIES}
                       pthread
                       )