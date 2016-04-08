#include <gtest/gtest.h>
#include <cstring>
#include <sstream>

#include "lazy_string.h"


using namespace std_utlis;

TEST(lazy_string, construct_test) {
	lazy_basic_string<char> s1;
	lazy_basic_string<char> s2 = "Hello";
	lazy_basic_string<char> s3(5, 'x');

	ASSERT_TRUE(s1 == "");
	ASSERT_TRUE(s2 == "Hello");
	ASSERT_TRUE(s3 == "xxxxx");
}

TEST(lazy_string, lazy_construct_test) {
	lazy_basic_string<char> s("Hello!");
	lazy_basic_string<char> s1 = s;

	ASSERT_EQ(s.c_str(), s1.c_str()); // pointer comparison
}

TEST(lazy_string, is_empty_test) {
	lazy_basic_string<char> s1;
	lazy_basic_string<char> s2("42");

	ASSERT_TRUE(s1.empty());
	ASSERT_FALSE(s2.empty());
}

TEST(lazy_string, get_size_test) {
	lazy_basic_string<char> s("42");
	
	ASSERT_EQ(s.size(), (size_t) 2);
}

TEST(lazy_string, simple_get_at_test) {
	const lazy_basic_string<char> s("0123456789");

	char x = s[0];
	ASSERT_EQ(x, '0');
	x = s[5];
	ASSERT_EQ(x, '5');
}

TEST(lazy_string, test_concat) {
	lazy_string s1("x");
	lazy_string x = s1 + s1;

	ASSERT_TRUE(s1 == "x");
	ASSERT_TRUE(x == "xx");

	x += "x";
	ASSERT_TRUE(x == "xxx");

	lazy_string s2("2");
	lazy_string y = "4" + s2;
	ASSERT_TRUE(y == "42");

	lazy_string z = s2 + "4";
	ASSERT_TRUE(z == "24");
}

TEST(lazy_string, test_char_concat) {
	lazy_string s("4");
	s += '2';
	ASSERT_TRUE(s == "42");

	s = s + '4';
	ASSERT_TRUE(s == "424");
}

TEST(lazy_string, non_const_at) {
	lazy_string s("01");

	auto x = s[0];

	ASSERT_TRUE(x == '0');

	x = '1';

	ASSERT_EQ(s, "11");
}

TEST(lazy_basic_string, test_read) {
	std::istringstream in("hello");

	lazy_string s;
	in >> s;

	ASSERT_EQ(s, "hello");

	std::istringstream in1("H");
	in1 >> s[0];
	ASSERT_EQ(s, "Hello");
}

TEST(lazy_basic_string, testx)
{
	lazy_string str("Hello");
	lazy_string str1 = str;

	ASSERT_EQ(str.c_str(), str1.c_str());

	str[0] = 'Y';
	ASSERT_EQ(str, "Yello");
	ASSERT_EQ(str1, "Hello");
}


TEST(lazy_basic_string, testy)
{
	lazy_istring s("Hello");
	ASSERT_EQ(s, "HeLLo");
}

