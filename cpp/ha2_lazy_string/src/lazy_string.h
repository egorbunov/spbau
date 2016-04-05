#pragma once

#include <memory>

namespace std_utlis 
{

	template<class CharT, class Traits = std::char_traits<CharT>>
	class lazy_basic_string {
	public:
		typedef Traits traits_type;
		typedef Traits::char_type value_type;
		typedef size_t size_type; 
		typedef std::ptrdiff_t difference_type;

		lazy_basic_string();
		lazy_basic_string(size_type cnt, CharT sym);
		lazy_basic_string(const lazy_basic_string& other);
		lazy_basic_string(const CharT* s);
		lazy_basic_string(lazy_basic_string&& other) = default;

		~lazy_basic_string();

		lazy_basic_string& operator=(const lazy_basic_string& other);
		lazy_basic_string& operator=(lazy_basic_string&& other);
		lazy_basic_string& operator=(const CharT* s);

		lazy_basic_string& operator+=(const lazy_basic_string& str);
		lazy_basic_string& operator+=(CharT ch);
		lazy_basic_string& operator+=(const CharT* s);

		const CharT& operator[](size_type pos) const;
		void operator[](size_type pos); // TODO

		void empty() const;
		size_type size() const;
		const CharT* c_str() const;
		void clear();
		void swap(lazy_basic_string& other);

	private:
		// state
	};

	lazy_basic_string operator+(const lazy_basic_string& lhs, const lazy_basic_string& rhs);
	lazy_basic_string operator+(const lazy_basic_string& lhs, char rhs);
	lazy_basic_string operator+(char lhs, const lazy_basic_string& rhs);
	lazy_basic_string operator+(const lazy_basic_string& lhs, const char* rhs);
	lazy_basic_string operator+(const char* lhs, const lazy_basic_string& rhs);

	bool operator== (const lazy_basic_string& lhs, const lazy_basic_string& rhs);
	bool operator== (const char*   lhs, const lazy_basic_string& rhs);
	bool operator== (const lazy_basic_string& lhs, const char*   rhs);

	bool operator!= (const lazy_basic_string& lhs, const lazy_basic_string& rhs);
	bool operator!= (const char*   lhs, const lazy_basic_string& rhs);
	bool operator!= (const lazy_basic_string& lhs, const char*   rhs);

	bool operator<  (const lazy_basic_string& lhs, const lazy_basic_string& rhs);
	bool operator<  (const char*   lhs, const lazy_basic_string& rhs);
	bool operator<  (const lazy_basic_string& lhs, const char*   rhs);

	bool operator<= (const lazy_basic_string& lhs, const lazy_basic_string& rhs);
	bool operator<= (const char*   lhs, const lazy_basic_string& rhs);
	bool operator<= (const lazy_basic_string& lhs, const char*   rhs);

	bool operator>  (const lazy_basic_string& lhs, const lazy_basic_string& rhs);
	bool operator>  (const char*   lhs, const lazy_basic_string& rhs);
	bool operator>  (const lazy_basic_string& lhs, const char*   rhs);

	bool operator>= (const lazy_basic_string& lhs, const lazy_basic_string& rhs);
	bool operator>= (const char*   lhs, const lazy_basic_string& rhs);
	bool operator>= (const lazy_basic_string& lhs, const char*   rhs);
	

}