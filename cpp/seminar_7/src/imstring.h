#pragma once

#include <string>
#include <memory>
#include <vector>
#include <ostream>

class imstring {
public:
	typedef std::string::const_iterator const_iterator;

	imstring(const char*);
	imstring(const imstring&);
	imstring(imstring&&);

	const_iterator begin() const;
	const_iterator end() const;
	size_t size() const;
	const char* c_str() const;

	imstring& operator=(const imstring&) = delete;
	friend std::ostream& operator<<(std::ostream&, const imstring&);
	friend istring operator+(const istring& s1, const istring& s2);

	const char& operator[](size_t pos) const;
private:
	std::shared_ptr<std::string> pstr;
	std::shared_ptr<std::vector<imstring>> pconcat_strs;
};