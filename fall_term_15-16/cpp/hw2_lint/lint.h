//
// Created by egorbunov on 22.11.15.
//

#pragma once

#include <string>
#include <iostream>
#include <stdexcept>

#include <cstdio>

// MY VECTOR

namespace au {
    /**
     * Resizing array
     */
    template<typename T>
    class vector {
        size_t _size;
        size_t _capacity;
        T *data;
        void swap(vector<T>& other);
    public:
        vector() : _size(0), _capacity(0), data(nullptr) { }
        vector(const vector<T>& other);
        ~vector();

        void push_back(T x);
        T at(size_t ind);
        size_t size() const;
        void resize(size_t new_size, const T &val = T());
        void fill(const T &val);

        vector<T>& operator=(const vector& other);
        bool operator==(const vector& other) const;
        T operator[](size_t i) const;
        T & operator[](size_t i);
    };
}


template<typename T>
void au::vector<T>::push_back(T x) {
    if (_size == _capacity) {
        _capacity = (_capacity == 0) ? 1 : _capacity * 2;

        T* tmp = data;
        data = new T[_capacity];
        for (size_t i = 0; i < _size; ++i)
            data[i] = tmp[i];
        delete [] tmp;
    }
    data[_size++] = x;
}

template<typename T>
T au::vector<T>::at(size_t ind) {
    return data[ind];
}

template<typename T>
size_t au::vector<T>::size() const {
    return _size;
}

template<typename T>
au::vector<T>& au::vector<T>::operator=(const vector& other) {
    vector<T> cpy(other);
    swap(cpy);
    return *this;
}

template<typename T>
au::vector<T>::~vector() {
    delete [] data;
}

template<typename T>
au::vector<T>::vector(const au::vector<T> &other) {
    data = nullptr;
    _size = other._size;
    _capacity = other._capacity;
    data = new T[_capacity];
    for (size_t i = 0; i < _size; ++i) {
        data[i] = other.data[i];
    }
}

template<typename T>
void au::vector<T>::swap(au::vector<T> &other) {
    std::swap(data, other.data);
    std::swap(_size, other._size);
    std::swap(_capacity, other._capacity);
}



template<typename T>
T au::vector<T>::operator [](size_t i) const {
    if (i >= _size)
        throw std::out_of_range("au::vector out of range");
    return data[i];
}

template<typename T>
T& au::vector<T>::operator [](size_t i) {
    if (i >= _size)
        throw std::out_of_range("au::vector out of range");
    return data[i];
}

template<typename T>
bool au::vector<T>::operator==(const au::vector<T> &other) const {
    if (other.size() != _size)
        return false;
    for (size_t i = 0; i < _size; ++i) {
        if (data[i] != other.data[i])
            return false;
    }
    return true;
}

template<typename T>
void au::vector<T>::fill(const T &val) {
    for (size_t i = 0; i < _size; ++i) {
        data[i] = val;
    }
}
// LINT

template<typename T>
void au::vector<T>::resize(size_t new_size, const T &val) {
    if (new_size < _capacity) {
        for (size_t i = _size; i < new_size; ++i)
            data[i] = val;
        _size = new_size;
        return;
    }
    T *tmp = data;
    data = new T[new_size];
    for (size_t i = 0; i < _size; ++i)
        data[i] = tmp[i];
    for (size_t i = _size; i < new_size; ++i)
        data[i] = val;
    _size = new_size;
    _capacity = new_size;
    delete [] tmp;
}


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

        au::vector<digit_t> num;
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


