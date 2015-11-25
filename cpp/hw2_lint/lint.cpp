//
// Created by egorbunov on 22.11.15.
//

#include "lint.h"

#include <cmath>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <stdexcept>
#include <limits>

size_t apa::lint::DIG_LEN = std::to_string(lint::BASE - 1).length();

apa::lint::lint() : lint(0) {
}

apa::lint::lint(std::string snum) {
    int i;
    sign = 1;
    if (snum[0] == '-')
        sign = -1;
    if (snum[0] == '+' || snum[0] == '-')
        snum = snum.substr(1);
    for (i = (int) (snum.length() - DIG_LEN); i >= 0; i -= DIG_LEN) {
        std::string s = snum.substr((size_t) i, DIG_LEN);
        num.push_back(std::stoull(s));
    }
    if (i != -DIG_LEN) {
        std::string s = snum.substr(0, DIG_LEN + i);
        num.push_back(std::stoull(s));
    }

    del_lead_zeros();

    if (num.size() == 1 && num[0] == 0) {
        sign = 0;
    }

}

apa::lint::lint(int inum) {
    sign = 0;
    if (inum < 0)
        sign = -1;
    else if (inum > 0)
        sign = 1;

    long long ln = (sign * (long long) inum);

    if (ln != 0) {
        while (ln > 0) {
            num.push_back((digit_t) (ln % BASE));
            ln /= BASE;
        }
    } else {
        num.push_back(0);
    }
}

apa::lint::lint(double dnum) {
    if (dnum == 0) {
        sign = 0;
        num.push_back(0);
    } else if (dnum < 0)
        sign = - 1;
    else
        sign = 1;
    dnum *= sign;
    dnum = floor(dnum);


    while (dnum > 0) {
//        std::cout << "XXX = " << (dnum - floor(dnum / BASE) * BASE) << std::endl;
        num.push_back((digit_t) ((dnum - floor(dnum / BASE) * BASE) + 0.5));
        dnum = floor(dnum / 10);
    }
}

apa::lint::operator int() const {
    if (*this < std::numeric_limits<int>::max() && *this > std::numeric_limits<int>::min()) {
        return atoi(this->to_string().c_str());
    } else {
        throw std::overflow_error("Can't convert to int!");
    }
}

apa::lint::operator bool() const {
    return *this != 0;
}

apa::lint& apa::lint::operator+=(const apa::lint &rhs) {
    int init_sign = sign;
    if (sign == rhs.sign || sign == 0 || rhs.sign == 0) {
        u_add(rhs);
    } else {
        u_sub(rhs);
        if (init_sign < 0)
            sign *= -1;
    }
    return *this;
}

apa::lint &apa::lint::operator*=(const apa::lint &rhs) {
    sign = sign * rhs.sign;

    // zero case
    if (sign == 0) {
        num.resize(1);
        num[0] = 0;
        return *this;
    }

    lint cpy(*this);

    std::fill(num.begin(), num.end(), 0);

    for (int i = 0; i < rhs.num.size(); ++i) {
        digit_t rem = 0;
        for (int j = 0; j < cpy.num.size() || rem != 0; ++j) {
            if (i + j >= num.size())
                num.push_back(0);
            num[i + j] += (j < cpy.num.size() ? rhs.num[i] * cpy.num[j] : 0) + rem;
            rem = num[i + j] / BASE;
            num[i + j] %= BASE;
        }
    }

    del_lead_zeros();

    return *this;
}

apa::lint &apa::lint::operator-=(const apa::lint &rhs) {
    int init_sign = sign;
    if (sign != rhs.sign || sign == 0 || rhs.sign == 0) {
        u_add(rhs);
        if (init_sign == 0)
            sign *= -1;
    } else {
        u_sub(rhs);
        if (init_sign < 0)
            sign *= -1;
    }
}

apa::lint &apa::lint::operator/=(const apa::lint &rhs) {
    if (rhs == 0)
        throw std::overflow_error("Division by zero exception");
    if (*this == 0)
        return *this;
    if (u_cmp(rhs) < 0) {
        sign = 0;
        num.resize(1);
        num[0] = 0;
        return *this;
    }

    sign *= rhs.sign;

    lint tmp(0);
    tmp.num.resize(num.size(), 0);

    bool is_sum = rhs < 0;

    for (int i = (int) (num.size() - 1); i >= 0; --i) {
        tmp *= BASE;
        tmp += num[i];
        num[i] = 0;
        while (tmp.u_cmp(rhs) >= 0) {
            if (is_sum)
                tmp += rhs;
            else
                tmp -= rhs;
            num[i] += 1;
        }
    }

    del_lead_zeros();

    if (num.size() == 1 && num[0] == 0)
        sign = 0;

    return *this;
}

apa::lint apa::lint::operator+() const {
    return lint(*this);
}

apa::lint apa::lint::operator-() const {
    lint res(*this);
    res.sign *= -1;
    return res;
}


void apa::lint::u_add(const lint &x) {
    if (x.num.size() > num.size())
        num.resize(x.num.size(), 0);

    digit_t rem = 0;
    int i;
    for (i = 0; i < num.size(); ++i) {
        num[i] += (i < x.num.size() ? x.num[i] : 0) + rem;
        if (num[i] >= lint::BASE) {
            rem = num[i] / lint::BASE;
            num[i] %= lint::BASE;
        } else {
            rem = 0;
        }
    }
    if (rem > 0)
        num.push_back(rem);

    if (sign == 0 && x.sign != 0)
        sign = x.sign;
}

void apa::lint::u_sub(const lint &x) {
    sign = 1;
    if (u_cmp(x) < 0) { // this less than x
        lint tmp(x);
        tmp.u_sub(*this);
        tmp.sign = 1;
        *this = tmp;
        sign = -1;
        return;
    }

    // *this is greater
    digit_t rem = 0;
    int i;
    long long new_digit;
    for (i = 0; i < num.size(); ++i) {
        new_digit = num[i] - (i < x.num.size() ? x.num[i] : 0) - rem;
        if (new_digit < 0) {
            new_digit += BASE;
            rem = 1;
        } else {
            rem = 0;
        }
        num[i] = (digit_t) new_digit;
    }

    del_lead_zeros();

    // zero case
    if (num.size() == 1 && num[0] == 0) {
        sign = 0;
    }

}

int apa::lint::u_cmp(const lint &x) const {
    bool isLess = true;
    bool isEq = false;
    if (num.size() > x.num.size()) {
        isLess = false;
    }
    else if (num.size() == x.num.size()) {
        isEq = true;
        for (int i = (int) num.size() - 1; i >= 0; --i) {
            if (x.num[i] != num[i])
                isEq = false;
            if (x.num[i] > num[i]) {
                isLess = true;
                break;
            } else if (x.num[i] < num[i]) {
                isLess = false;
                break;
            }
        }
    }

    return (isEq ? 0 : (isLess ? -1 : 1));
}

void apa::lint::del_lead_zeros() {
    if (*this == 0)
        return;

    int zeroCnt = 0;
    for (int i = (int) num.size() - 1; i > 0; --i) {
        if (num[i] == 0) {
            zeroCnt += 1;
        } else {
            break;
        }
    }
    num.resize(num.size() - zeroCnt);
}

namespace apa {
    lint operator+(const lint& a, const lint& b) {
        lint tmp(a);
        tmp += b;
        return tmp;
    }

    lint operator-(const lint& a, const lint& b) {
        lint tmp(a);
        tmp -= b;
        return tmp; // TODO: why 'return lint(a)-=b' does't work?!!!???1
    }

    lint operator*(const lint& a, const lint& b) {
        lint tmp(a);
        tmp *= b;
        return tmp;
    }

    lint operator/(const lint& a, const lint& b) {
        lint tmp(a);
        tmp /= b;
        return tmp;
    }

    lint operator++(lint& x, int) {
        lint cpy(x);
        x += 1;
        return cpy;
    }

    lint operator--(lint& x, int) {
        lint cpy(x);
        x -= 1;
        return cpy;
    }

    lint& operator--(lint& x) {
        x -= 1;
        return x;
    }

    lint& operator++(lint& x) {
        x += 1;
        return x;
    }

    std::string lint::to_string() const {
        if (sign == 0) {
            return "0";
        } else {
            std::stringstream ss;
            if (sign < 0) ss << "-";
            ss << num[num.size() - 1];
            for (int i = (int) num.size() - 2; i >= 0; --i) {
                ss << std::setw(DIG_LEN) << std::setfill('0') <<num[i];
            }
            return ss.str();
        }
    }

    bool operator==(const lint& a, const lint& b) {
        return a.sign == b.sign && a.num == b.num;
    }

    bool operator<(const lint& a, const lint& b) {
        if (a == b)
            return false;
        if (a.sign < b.sign)
            return true;
        if (b.sign < a.sign)
            return false;

        bool isLess = (a.u_cmp(b) < 0);

        if (a.sign < 0)
            isLess = !isLess;

        return isLess;
    }

    bool operator<=(const lint& a, const lint& b) {
        return !(a > b);
    }

    bool operator>(const lint& a, const lint& b) {
        return !(a < b) && (a != b);
    }

    bool operator>=(const lint& a, const lint& b) {
        return !(a < b);
    }

    bool operator!=(const lint& a, const lint& b) {
        return !(a == b);
    }

    std::ostream &operator<<(std::ostream &output, const lint &x) {
        return output << x.to_string();
    }

    std::istream &operator>>(std::istream  &input, lint &x) {
        std::string s;
        input >> s;
        x = lint(s);
        return input;
    }

    lint abs(const lint &x) {
        if (x < 0)
            return -x;
        else
            return lint(x);
    }

    lint pow(const lint &x, int pow) {
        lint result = 1;
        lint value(x);
        while(pow > 0)
        {
            if(pow & 1)
                result = result * value;
            value = value * value;
            pow /= 2;
            //power >>= 1;
        }
        return result;
    }
}