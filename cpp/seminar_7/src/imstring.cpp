#include "imstring.h"

imstring::imstring(const char* s): pstr(std::make_shared<std::string>(s)) 
{
}

imstring::imstring(const imstring& other): pstr(other.pstr), pconcat_strs(other.pconcat_strs)
{
}

imstring::imstring(imstring&& other): pstr(other.pstr), pconcat_strs(other.pconcat_strs)
{
}


auto imstring::begin() const -> const_iterator
{
	return pstr->cbegin();
}

auto imstring::end() const -> const_iterator
{
	return pstr->cend();
}

size_t imstring::size() const 
{
	return pstr->length();
}

const char& imstring::operator[](size_t pos) const 
{
	return (*pstr)[pos];
}

const char* imstring::c_str() const 
{
	return pstr->c_str();
}

std::ostream& operator<<(std::ostream& out, const imstring& s) {
	out << s.c_str();
	for (auto istr : *s.pconcat_strs) {
		out << istr;
	}
	return out;
}

