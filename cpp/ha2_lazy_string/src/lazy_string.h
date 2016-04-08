#pragma once

#include <memory>
#include <string>
#include <algorithm>
#include <cctype>

namespace std_utlis
{
	template<class CharT, class Traits = std::char_traits<CharT>>
	class lazy_basic_string {
	public:
		typedef Traits traits_type;
		typedef typename Traits::char_type value_type;
		typedef size_t size_type; 
		typedef std::ptrdiff_t difference_type;
		typedef std::basic_string<CharT, Traits> buf_type;

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
			pbuf_ = other.pbuf_;
			return *this;
		}

		lazy_basic_string& operator=(lazy_basic_string&& other) 
		{
			pbuf_ = std::move(other.pbuf_);
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

		// Proxy for non const at operator
		class Proxy {
		public:
			Proxy(const Proxy&) = default;
			Proxy(Proxy&&) = default;

			Proxy& operator=(const CharT& c) {
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

			friend std::istream& operator>>(std::istream& in, Proxy p) {
				CharT ch;
				in >> ch;
				p = ch;
				return in;
			}
		private:
			friend class lazy_basic_string;
			Proxy(size_type pos, lazy_basic_string& str): pos_(pos), str_(str)
			{}
			size_type pos_;
			lazy_basic_string &str_;
		};

		Proxy operator[](size_type pos) {
			return Proxy(pos, *this);
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
			return !(lhs = rhs);
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

	// ============== typedefs ========================
	
	typedef lazy_basic_string<char> lazy_string;
	typedef lazy_basic_string<wchar_t> lazy_wstring;

	struct i_traits: std::char_traits<char> {
	  static bool eq (char c, char d) 
	  {
	  	  return std::tolower(c) == std::tolower(d); 
	  }
	  
	  static bool lt (char c, char d) 
	  { 
	  	  return std::tolower(c) < std::tolower(d); 
	  }

	  static int compare (const char* p, const char* q, std::size_t n) 
	  {
	      while (n--) {
	      	if (!eq(*p,*q)) {
	      		return lt(*p,*q) ? -1 : 1; 
	      	}
	      	++p; ++q;
	      }
	      return 0;
	  }
	};

	typedef lazy_basic_string<char, i_traits> lazy_istring;


	// ============== additional operators =============

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
}