#pragma once

#include <ostream>
#include <istream>
#include <type_traits>
#include <cstdint>
#include <cstring>

namespace serialization {

	/**
	 * Serialize POD object
	 */
	template<typename T>
	void serialize(std::ostream& out, T& obj, ...) 
	{
		static_assert(std::is_pod<T>::value, "Type is not serializable");

		uint8_t dst[sizeof(T)];
		std::memcpy(dst, &obj, sizeof(T));

		out << dst;
	}

	/**
	 * Deserialize POD object
	 */
	template<typename T>
	void deserialize(std::istream& in, T& obj, ...) 
	{
		static_assert(std::is_pod<T>::value, "Type is not deserializable");

		uint8_t bytes[sizeof(T)];
		in >> bytes;

		std::memcpy(&obj, bytes, sizeof(T));
	}

	template<class Cont>
	void serialize(std::ostream& out, Cont& obj, 
		typename Cont::iterator check_it = Cont().begin()) 
	{

	}

	template<class Cont>
	void deserialize(std::ostream& out, Cont& obj, 
		typename Cont::iterator check_it = Cont().begin()) 
	{
		
	}
}