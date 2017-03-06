#pragma once

#ifndef ANY_H_INCLUDED__
#define ANY_H_INCLUDED__

#include <typeinfo>

namespace utils {
    struct bad_any_cast : public std::bad_cast {
        bad_any_cast(const std::type_info &from, const std::type_info &to);
        virtual const char *what() const noexcept override;
    private:
        const std::type_info &from;
        const std::type_info &to;
        std::string error_msg;
    };

    class any {
    public:
        // friend cast functions
        template<typename ValueType>
        friend ValueType* any_cast(any*);

        template<typename ValueType>
        friend const ValueType* any_cast(const any*);

        template<typename T>
        friend T any_cast(any&);

        template<typename T>
        friend T any_cast(const any&);

        // interface
        template<typename T> any(const T& value);

        any();

        any(const any& other);

        any(any&& other);

        ~any();

        any& swap(any &rhs);

        any& operator=(const any &rhs);

        template<typename T>
        any& operator=(const T& rhs);

        bool empty() const;

        const std::type_info &type_info() const;
    private:

        struct value_holder_t {
            virtual ~value_holder_t() {}
            virtual const std::type_info &type_info() const = 0;
            virtual value_holder_t *clone() const = 0;
        };

        template<typename T>
        struct holder_t : value_holder_t {
            holder_t(const T &value): value(value) {}
            virtual const std::type_info &type_info() const override {
                return typeid(T);
            }
            virtual value_holder_t *clone() const override {
                return new holder_t(value);
            }

            T value;
        };

        value_holder_t *value_holder;
    };

    void swap(any &, any &);
}


namespace utils {
    template<typename T>
    any::any(const T &value): value_holder(new holder_t<typename std::decay<const T>::type>(value)) {
    }

    template<typename T>
    any& any::operator=(const T &rhs) {
        any tmp = any(rhs);
        return swap(tmp);
    }


    // casts

    template<typename ValueType>
    const ValueType* any_cast(const any* any_ptr) {
        if (any_ptr->type_info() == typeid(ValueType)) {
            return &(static_cast<any::holder_t<typename std::decay<ValueType>::type>  const *>(
                any_ptr->value_holder)->value
            );
        } else {
            return nullptr;
        }
    }

    template<typename ValueType>
    ValueType* any_cast(any* any_ptr) {
        if (any_ptr->type_info() == typeid(ValueType)) {
            return &(static_cast<any::holder_t<typename std::decay<ValueType>::type> *>(
                any_ptr->value_holder)->value
            );
        } else {
            return nullptr;
        }
    }

    template<typename T>
    T any_cast(const any& any_ref) {
        if (any_ref.type_info() == typeid(T)) {
            return static_cast<any::holder_t<typename std::decay<T>::type> const *>(
                any_ref.value_holder)->value;
        } else {
            throw bad_any_cast(any_ref.type_info(), typeid(T));
        }
    }

    template<typename T>
    T any_cast(any& any_ref) {
        if (any_ref.type_info() == typeid(T)) {
            return static_cast<any::holder_t<typename std::decay<T>::type> *>(
                any_ref.value_holder)->value;
        } else {
            throw bad_any_cast(any_ref.type_info(), typeid(T));
        }
    }
}

#endif