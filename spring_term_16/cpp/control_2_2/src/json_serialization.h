#pragma once

#include <string>
#include <type_traits>
#include <cstdint>
#include <istream>
#include <ostream>
#include <cstring>
#include <iostream>
#include <map>
#include <sstream>

namespace serialization
{
    struct json_value_t
    {
        /*
         * json value can be a string
         * or a mapping of (string key => json_value_t).
         */
        std::string value_;
        std::map<std::string, json_value_t> mapping_;
    };

    /**
     * is arithmetic type serialization  
    **/
    template<class type>
    typename std::enable_if<std::is_arithmetic<type>::value>::type write(json_value_t& jvalue, type& obj)
    {
        std::stringstream ss;
        ss << obj;
        jvalue.value_ = ss.str();
    }

    /**
     * arithmetic type deserialization  
    **/
    template<class type>
    typename std::enable_if<std::is_arithmetic<type>::value>::type read(json_value_t& jvalue, type& obj)
    {     
        std::stringstream ss(jvalue.value_);
        ss >> obj;
    }

    struct json_write_proc_t {
        json_write_proc_t(json_value_t& json): json_(json) 
        {}

        template<class field_type>
        void operator()(field_type& value, const char* key)
        {
            json_value_t json;
            write(json, value);
            json_.mapping_[key] = json;
        }
    private:
        json_value_t& json_;
    };

    /**
     * not arithmetic type serialization  
    **/
    template<class type>
    typename std::enable_if<!std::is_arithmetic<type>::value>::type write(json_value_t& jvalue, type& obj)
    {
        using namespace reflection;
        json_write_proc_t proc(jvalue);
        reflect_type(proc, obj);
    }

    struct json_read_proc_t {
        json_read_proc_t(json_value_t& json): json_(json) 
        {}

        template<class field_type>
        void operator()(field_type& value, const char* key)
        {
            try {
                auto json = json_.mapping_.at(key);
                read(json, value);
            } catch(...) {
            }
        }
    private:
        json_value_t& json_;
    };

    /**
     * not arithmetic type deserialization  
    **/
    template<class type>
    typename std::enable_if<!std::is_arithmetic<type>::value>::type read(json_value_t& jvalue, type& obj)
    {
        using namespace reflection;
        json_read_proc_t proc(jvalue);
        reflect_type(proc, obj);
    }
} // serialization
