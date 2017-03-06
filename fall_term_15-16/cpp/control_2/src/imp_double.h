#pragma once

#include <iostream>

namespace math {
	class imp_double {
	public:
		static const double EPS;

		imp_double();
		imp_double(double val);
		imp_double(double val, double abs_error);

		void mul(const imp_double &x); // multiply
		void sub(const imp_double &x); // subtract
		void add(const imp_double &x); // add
		void div(const imp_double &x); // divide

		double get_delta() const; 
		double get_value() const;

    	imp_double& operator+=(const imp_double& other);
    	imp_double& operator*=(const imp_double& other);
    	imp_double& operator/=(const imp_double& other);
    	imp_double& operator-=(const imp_double& other);
    	imp_double operator-() const;

		friend std::ostream &operator<<(std::ostream &output, const imp_double &x);
		friend std::istream &operator>>(std::istream  &input, imp_double &x);

	private:
		double val;
		double err;
	};

	imp_double operator+(const imp_double& x, const imp_double& y);
	imp_double operator*(const imp_double& x, const imp_double& y);
	imp_double operator/(const imp_double& x, const imp_double& y);
	imp_double operator-(const imp_double& x, const imp_double& y);

    bool operator==(const imp_double& x, const imp_double& y);
    bool operator<(const imp_double& x, const imp_double& y);
    bool operator<=(const imp_double& x, const imp_double& y);
    bool operator>(const imp_double& x, const imp_double& y);
    bool operator>=(const imp_double& x, const imp_double& y) ;
    bool operator!=(const imp_double& x, const imp_double& y);
}