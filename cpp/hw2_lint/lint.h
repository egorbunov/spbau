//
// Created by egorbunov on 22.11.15.
//

#pragma once

#include <string>
#include <vector>
#include <iostream>

namespace apa {

    typedef long long digit_t;

    class lint {
    public:
        lint();
        explicit lint(std::string snum);
        lint(int inum);
        explicit lint(double dnum);

        std::string to_string() const;

        // binary arithmetic operators
        lint& operator+=(const lint& x);
        lint& operator-=(const lint& x);
        lint& operator*=(const lint& x);
        lint& operator/=(const lint& x);

        // friends =(
        friend bool operator==(const lint& a, const lint &b);
        friend bool operator<(const lint& a, const lint &b);

        // unary
        lint operator+() const;
        lint operator-() const;

        // convertions
        explicit operator int() const;
        explicit operator bool() const;

    private:
        static const digit_t BASE = 10; // MUST BE POWER OF 10
        static size_t DIG_LEN;

        std::vector<digit_t> num;
        char sign; // 0, -1 or 1

        void del_lead_zeros();
        int u_cmp(const lint& x) const;
        void u_add(const lint& x);
        void u_sub(const lint& x);
    };

    // unary operators
    lint& operator--(lint& x);
    lint& operator++(lint& x);
    lint operator++(lint& x, int);
    lint operator--(lint& x, int);


    // binary arithmetic operators
    lint operator+(const lint& a, const lint& b);
    lint operator-(const lint& a, const lint& b);
    lint operator*(const lint& a, const lint& b);
    lint operator/(const lint& a, const lint& b);

    // comparison operators
    bool operator==(const lint& a, const lint& b);
    bool operator<(const lint& a, const lint& b);
    bool operator<=(const lint& a, const lint& b);
    bool operator>(const lint& a, const lint& b);
    bool operator>=(const lint& a, const lint& b) ;
    bool operator!=(const lint& a, const lint& b);

    // in out
    std::ostream &operator<<(std::ostream &output, const lint &x);
    std::istream &operator>>(std::istream  &input, lint &x);

    // additional
    lint abs(const lint &x);
    lint pow(const lint &x, int pow);
}


