#include <typeinfo>
#include <iosfwd>
#include <sstream>
#include "any.h"


namespace utils {

    const char* bad_any_cast::what() const noexcept {
        return error_msg.c_str();
    }

    bad_any_cast::bad_any_cast(const std::type_info &from, const std::type_info &to) 
    : from(from), to(to) {
        std::ostringstream err;
        err << "error: can't convert from type [ " <<
                from.name() << " ] to [ " <<
                to.name() << " ]";
        error_msg = err.str();
    }

    // any
    utils::any::any(): value_holder(nullptr) { }

    any& any::swap(any &rhs) {
        std::swap(value_holder, rhs.value_holder);
        return *this;
    }

    any& any::operator=(const any &rhs) {
        any tmp = any(rhs);
        return swap(tmp);
    }

    any::any(const any& other) : value_holder(other.value_holder ? other.value_holder->clone() : nullptr) {
    }
    
    any::~any() {
        delete value_holder;
    }

    const std::type_info& any::type_info() const {
        if (value_holder) {
            return value_holder->type_info();
        } else {
            return typeid(void);
        }
    }

    bool any::empty() const {
        return value_holder == nullptr;
    }

    void swap(any &a, any& b) {
        a.swap(b);
    }
}

