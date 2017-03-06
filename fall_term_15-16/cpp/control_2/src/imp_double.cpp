#include "imp_double.h"

#include <cmath>

const double math::imp_double::EPS = 1e-15;

math::imp_double::imp_double() : imp_double(0) {
}

math::imp_double::imp_double(double val) : imp_double(val, fabs(EPS * val)) {
}

math::imp_double::imp_double(double val, double abs_error) : val(val) {
	err = std::fmax(abs_error, EPS * fabs(val));
}

void math::imp_double::mul(const imp_double &x) {
	double tmp_val = val * x.val;
	double tmp_err = err * std::fabs(x.val) + x.err * std::fabs(val);
	val = tmp_val;
	err = std::fmax(tmp_err, EPS * fabs(val));
}

void math::imp_double::sub(const imp_double &x) {
	double tmp_val = val - x.val;
	double tmp_err = err + x.err;
	val = tmp_val;
	err = std::fmax(tmp_err, EPS * fabs(val));
}

void math::imp_double::add(const imp_double &x) {
	double tmp_val = val + x.val;
	double tmp_err = err + x.err;
	val = tmp_val;
	err = std::fmax(tmp_err, EPS * fabs(val));
}

void math::imp_double::div(const imp_double &x) {
	double tmp_val = val / x.val;
	double tmp_err = err / std::fabs(x.val) + (std::fabs(val) * x.err / (x.val * x.val));
	val = tmp_val;
	err = std::fmax(tmp_err, EPS * fabs(val));
}

double math::imp_double::get_delta() const {
	return err;
}

double math::imp_double::get_value() const {
	return val;
}


math::imp_double& math::imp_double::operator+=(const imp_double& other) {
	add(other);
	return *this;
}

math::imp_double& math::imp_double::operator-=(const imp_double& other) {
	sub(other);
	return *this;
}


math::imp_double& math::imp_double::operator*=(const imp_double& other) {
	mul(other);
	return *this;
}

math::imp_double& math::imp_double::operator/=(const imp_double& other) {
	div(other);
	return *this;
}

math::imp_double math::imp_double::operator-() const {
	imp_double x(*this);
	x.val = -x.val;
	return x;
}

namespace math {
	math::imp_double operator+(const imp_double& x, const imp_double& y) {
		imp_double tmp(x);
		tmp.add(y);
		return tmp;
	}

	math::imp_double operator-(const imp_double& x, const imp_double& y) {
		imp_double tmp(x);
		tmp.sub(y);
		return tmp;
	}

	math::imp_double operator/(const imp_double& x, const imp_double& y) {
		imp_double tmp(x);
		tmp.div(y);
		return tmp;
	}

	math::imp_double operator*(const imp_double& x, const imp_double& y) {
		imp_double tmp(x);
		tmp.mul(y);
		return tmp;
	}

	bool operator==(const imp_double& x, const imp_double& y) {
		return x.get_value() == y.get_value() && x.get_delta() == y.get_delta();
	}

	bool operator<(const imp_double& x, const imp_double& y) {
		return (x.get_value() + x.get_delta()) < (y.get_value() - y.get_delta());
	}

	bool operator<=(const imp_double& x, const imp_double& y) {
		return !(x > y);
	}

	bool operator>(const imp_double& x, const imp_double& y) {
		return (x.get_value() - x.get_delta()) > (y.get_value() + y.get_delta());
	}

	bool operator>=(const imp_double& x, const imp_double& y) {
		return !(x < y);
	}

	bool operator!=(const imp_double& x, const imp_double& y) {
		return !(x == y);
	}

	std::ostream &operator<<(std::ostream &output, const math::imp_double &x)
	{
		output << std::scientific << x.val << "\u00b1" << x.err;
	    return output;
	}

	std::istream &operator>>(std::istream  &input, math::imp_double &x)
	{
		char tmp;
		input >> x.val >> tmp >> tmp >> x.err;
	    return input;
	}
}
