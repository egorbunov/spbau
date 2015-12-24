#pragma once

#include <iostream>
#include <stdexcept>
#include <cstddef>

namespace utils {
    namespace {
        struct empty_t {
        };
        empty_t empty;
    }

    template<typename T>
    class maybe {
    public:
        maybe() : maybe(utils::empty) {
        }

        maybe(empty_t empty): is_empty(true) {
        }

        maybe(T value) : is_empty(false) {
            new (v_place) T(value);
        }

        maybe(const maybe<T>& other) {
            is_empty = other.is_empty;
            if (!is_empty) {
                new (v_place) T(other.get());
            }
        }

        maybe<T>& operator=(const maybe<T>& other) {
            maybe<T> cpy(other);
            swap(cpy);
            return *this;
        }

        maybe<T>& operator=(const T& other) {
            maybe<T> cpy(other);
            swap(cpy);
            return *this;
        }

        maybe<T>& operator=(const empty_t& other) {
            maybe<T> tmp;
            swap(tmp);
            return *this;
        }

        ~maybe() {
            if (!is_empty) {
                T* p_val = reinterpret_cast<T*>(v_place);
                p_val->~T();
            }
        }

        T& get() {
            if (is_empty)
                throw std::runtime_error("error: empty!");
            return *reinterpret_cast<T*>(v_place);
        }

        const T& get() const {
            if (is_empty)
                throw std::runtime_error("error: empty!");
            return *reinterpret_cast<const T*>(v_place);
        }

        explicit operator bool() const {
            return !is_empty;
        }

        bool operator!() const {
            return is_empty;
        }

        T* operator->() {
            return reinterpret_cast<T*>(v_place);
        }

        const T* operator->() const {
            return reinterpret_cast<const T*>(v_place);
        }

        T& operator*() {
            return *reinterpret_cast<T*>(v_place);
        }

        T const& operator*() const {
            return *reinterpret_cast<const T*>(v_place);
        }

    private:
        bool is_empty;
        uint8_t v_place[sizeof(T)];

        void swap(maybe<T> &other) {
            std::swap(is_empty, other.is_empty);
            std::swap(v_place, other.v_place);
        }
    };
}

