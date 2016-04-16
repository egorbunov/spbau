#pragma once
#include <type_traits>

namespace reflection
{
    template<class processor, class type>
    void reflect_type(processor& proc, type& obj)
    {
        typename std::enable_if<false, type>::type
            REFLECT_FUNC_SHOULD_BE_DEFINED_FOR_PROCESSOR_AND_TYPE;
        /*
         * In your reflect_type() implementation you should
         * call reflect_field() on each field of your type
         */
    }

    template<class processor, class type>
    void reflect_field(processor& proc, type& obj, const char* key)
    {
        proc(obj, key);
    }
} // reflection
