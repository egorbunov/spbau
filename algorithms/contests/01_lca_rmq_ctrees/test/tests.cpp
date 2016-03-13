#include <gtest/gtest.h>
 
TEST(exampleTestClass, ExampleTest) { 
    ASSERT_EQ(42, 42);
}
 
int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}