#pragma once

#include <type_traits>
#include <cstdint>
#include <istream>
#include <ostream>
#include <cstring>
#include <iostream>

#include "reflection.h"

namespace serialization
{
	/**
	 * POD type serialization  
	**/
	template<class type>
	typename std::enable_if<std::is_pod<type>::value>::type write(std::ostream& os, type& obj)
	{
		os.write(reinterpret_cast<const char*>(&obj), sizeof(type));
	}

	/**
	 * POD type deserialization  
	**/
	template<class type>
	typename std::enable_if<std::is_pod<type>::value>::type read(std::istream& in, type& obj)
	{
		in.read(reinterpret_cast<char*>(&obj), sizeof(type));
	}


	struct non_pod_write_proc_t {
		non_pod_write_proc_t(std::ostream& os): os_(os) 
		{}

		template<class field_type>
		void operator()(field_type& value, const char* key)
		{
			write(os_, value);
		}
	private:
		std::ostream& os_;
	};

	struct non_pod_read_proc_t {
		non_pod_read_proc_t(std::istream& in): in_(in) 
		{}

		template<class field_type>
		void operator()(field_type& value, const char* key)
		{
			read(in_, value);
		}
	private:
		std::istream& in_;
	};

	/**
	 * Non-POD type serialization
	**/
	template<class type>
	typename std::enable_if<!std::is_pod<type>::value>::type write(std::ostream& os, type& obj)
	{
    	using namespace reflection;
		non_pod_write_proc_t proc(os);
		reflect_type(proc, obj);
	}

	/**
	 * Non-POD type deserialization  
	**/
	template<class type>
	typename std::enable_if<!std::is_pod<type>::value>::type read(std::istream& in, type& obj)
	{
    	using namespace reflection;
		non_pod_read_proc_t proc(in);
		reflect_type(proc, obj);
	}

} // serialization
