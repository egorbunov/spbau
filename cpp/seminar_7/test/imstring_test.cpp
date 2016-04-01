#include "gtest/gtest.h"
#include "imstring.h"

#include <iostream>

TEST(imstring, construct_test) {
	imstring s("Hello, world!");
	imstring s1 = "Hello, world!";
	imstring s2 = s1;
}

TEST(imstring, str_size) {
	imstring s("42");
	imstring p = s;
	ASSERT_EQ(2, (int) s.size());
	ASSERT_EQ(2, (int) p.size());
}

TEST(imstring, buffer_is_shared) {
	imstring s("Hello!");
	imstring s1 = s;
	imstring s2 = s1;

	ASSERT_TRUE(reinterpret_cast<const void*>(s1.c_str()) == reinterpret_cast<const void*>(s.c_str()));
	ASSERT_TRUE(reinterpret_cast<const void*>(s2.c_str()) == reinterpret_cast<const void*>(s.c_str()));
	ASSERT_EQ(s1.c_str(), s.c_str());
	ASSERT_EQ(s.c_str(), s2.c_str());
}

TEST(imstring, iterator) {
	
}
