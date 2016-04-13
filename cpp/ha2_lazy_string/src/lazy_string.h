#pragma once

#include <memory>
#include <string>
#include <algorithm>
#include <cctype>
#include <cstddef>

namespace std_utils
{
	template<class CharT, class Traits = std::char_traits<CharT>>
	class lazy_basic_string {
	public:
        using traits_type = Traits;
        using value_type = typename Traits::char_type;
        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer =  value_type*;
        using const_pointer = const value_type*;
        using difference_type = ptrdiff_t;
        using size_type = size_t;

        using buf_type = std::basic_string<CharT, Traits>;

		lazy_basic_string(): pbuf_(std::make_shared<buf_type>()) 
		{}

		lazy_basic_string(size_type n, CharT sym): pbuf_(std::make_shared<buf_type>(n, sym))
		{}

		lazy_basic_string(const CharT* s): pbuf_(std::make_shared<buf_type>(s))
		{}

		lazy_basic_string(const lazy_basic_string& other): pbuf_(other.pbuf_)
		{}

		lazy_basic_string(lazy_basic_string&& other) = default;

		~lazy_basic_string() 
		{}

		lazy_basic_string& operator=(const lazy_basic_string& other)
		{
	        auto copy(other);
	        swap(copy);
	        return *this;
		}

		lazy_basic_string& operator=(lazy_basic_string&& other) 
		{
	        swap(other);
        	return *this;
		}

		lazy_basic_string& operator=(const CharT* s) 
		{
			pbuf_ = std::make_shared<buf_type>(s);
			return *this;
		}

		bool empty() const {
			return pbuf_->empty();
		}

		size_type size() const {
			return pbuf_->size();
		}
		
		const CharT* c_str() const {
			return pbuf_->c_str();
		}

		void clear() {
			pbuf_ = std::make_shared<buf_type>();
		}

		void swap(lazy_basic_string& other) {
			pbuf_.swap(other.pbuf_);
		}

		// Concat and other operators 	

		lazy_basic_string& operator+=(const lazy_basic_string& str)
		{
			pbuf_ = std::make_shared<buf_type>(*pbuf_ + *(str.pbuf_));
			return *this;
		}

		lazy_basic_string& operator+=(CharT ch)
		{
			pbuf_ = std::make_shared<buf_type>(*pbuf_ + ch);
			return *this;
		}
		
		lazy_basic_string& operator+=(const CharT* s)
		{
			pbuf_ = std::make_shared<buf_type>(*pbuf_ + s);
			return *this;
		}

		const CharT& operator[](size_type pos) const 
		{
			return (*pbuf_)[pos];
		}

		// char_proxy for non const at operator
		class char_proxy {
		public:
			char_proxy(const char_proxy&) = default;
			char_proxy(char_proxy&&) = default;

			char_proxy& operator=(const CharT& c) 
			{
				if (str_.pbuf_.use_count() > 1) {
					str_.pbuf_ = std::make_shared<buf_type>(str_.c_str());
				}
				(*str_.pbuf_)[pos_] = c;

				return *this;
			}

			operator CharT() const
			{
				return (*str_.pbuf_)[pos_];
			}

			friend std::istream& operator>>(std::istream& in, char_proxy p) 
			{
				CharT ch;
				in >> ch;
				p = ch;
				return in;
			}
		private:
			friend class lazy_basic_string;
			char_proxy(size_type pos, lazy_basic_string& str): pos_(pos), str_(str)
			{}
			size_type pos_;
			lazy_basic_string &str_;
		};

		char_proxy operator[](size_type pos) {
			return char_proxy(pos, *this);
		}

		// Friend operators ...
		friend lazy_basic_string operator+(const lazy_basic_string& lhs, const lazy_basic_string& rhs)
		{
			lazy_basic_string res = lhs;
			res += rhs;
			return res;
		}

		friend bool operator==(const lazy_basic_string& lhs, const lazy_basic_string& rhs) 
		{
			return lhs.size() == rhs.size() 
			   	   && traits_type::compare(lhs.c_str(), rhs.c_str(), lhs.size()) == 0;
		}

		friend bool operator<(const lazy_basic_string& lhs, const lazy_basic_string& rhs) 
		{
			int res = traits_type::compare(lhs.c_str(), rhs.c_str(), std::min(lhs.size(), rhs.size()));
			return res < 0 || (res == 0 && lhs.size() < rhs.size());
		}

		friend bool operator<=(const lazy_basic_string& lhs, const lazy_basic_string& rhs) 
		{
			int res = traits_type::compare(lhs.c_str(), rhs.c_str(), std::min(lhs.size(), rhs.size()));
			return res < 0 || (res == 0 && lhs.size() <= rhs.size());
		}

		friend bool operator>(const lazy_basic_string& lhs, const lazy_basic_string& rhs) 
		{
			return !(lhs <= rhs);
		}

		friend bool operator>=(const lazy_basic_string& lhs, const lazy_basic_string& rhs) 
		{
			return !(lhs < rhs);
		}

		friend bool operator!=(const lazy_basic_string& lhs, const lazy_basic_string& rhs) 
		{
			return !(lhs == rhs);
		}

		friend std::ostream& operator<<(std::ostream& out, const lazy_basic_string& str) {
			return out << str.c_str();
		}

		friend std::istream& operator>>(std::istream& in, lazy_basic_string& str) {
			if (str.pbuf_.use_count() > 1) {
				str.clear();
			}
			return in >> *str.pbuf_;
		}

	private:
		// state
		std::shared_ptr<buf_type> pbuf_;
	};

	template<class C, class T>
	lazy_basic_string<C, T> operator+(const lazy_basic_string<C, T>& lhs, C rhs)
	{
		return lhs + lazy_basic_string<C, T>(1, rhs);
	}

	template<class C, class T>
	lazy_basic_string<C, T> operator+(C lhs, const lazy_basic_string<C, T>& rhs)
	{
		return lazy_basic_string<C, T>(1, lhs) + rhs;
	}

	/**
	 * Case-insensitive traits
	 */
	struct i_traits: std::char_traits<char> {
	  static bool eq(char c, char d) 
	  {
	  	  return std::tolower(c) == std::tolower(d); 
	  }
	  
	  static bool lt(char c, char d) 
	  { 
	  	  return std::tolower(c) < std::tolower(d); 
	  }

	  static int compare(const char* p, const char* q, std::size_t n) 
	  {
	      while (n > 0) {
	      	if (!eq(*p,*q)) {
	      		return lt(*p,*q) ? -1 : 1; 
	      	}
	      	++p; 
	      	++q;
	      	--n;
	      }
	      return 0;
	  }
	};

	// ============== typedefs ========================

	typedef lazy_basic_string<char> lazy_string;
	typedef lazy_basic_string<wchar_t> lazy_wstring;
	typedef lazy_basic_string<char, i_traits> lazy_istring;
}