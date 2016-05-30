#pragma once

#include <vector>
#include <functional>
#include <algorithm>

namespace linq 
{
	template<class T>
	struct enumerable 
	{
		using value_type = T;
		using holder_type = std::vector<T>;

		/**
		 * friend due to constructor privacy
		 */
		template<class container_t>
		friend enumerable<typename container_t::value_type> from(container_t container);

		/*
		  constructors and assign operators ... 
		  TODO: reconsider!!!
		*/
		enumerable(const enumerable&) = default;
		enumerable(enumerable&&) = default;
		enumerable& operator=(const enumerable&) = default;
		enumerable& operator=(enumerable&&) = default;

		/**
		 * Generates vector from this enumerable
		 */
		std::vector<T> to_vector() const;

		/**
		 * Filters elements with given predicate and copies to new enumerable
		 * @param  pred predicate (condition)
		 * @return new enumerable with filtered elements
		 */
		enumerable where(std::function<bool(T)> pred) const;

		/**
		 * Maps enumerable from T to R
		 * @param transform funtion for transforming elemnt of T to R
		 * @return new enumerable<R>
		 */
		template<class R>
		enumerable<R> select(std::function<R(T)> transform) const;

		/**
		 * @return number of elements in enumerable
		 */
		size_t count() const;

		/**
		 * @param  pred predicate
		 * @return      number of elements in enumerable 
		 *              which satisfy given predicate
		 */
		size_t count(std::function<bool(T)> pred) const;

		/**
		 * @return true, if collection is not empty, else false
		 */
		bool any() const;

		/**
		 * @param  pred predicate
		 * @return      true if there is more than zero elements which 
		 *              satisfy predicate in collection, else false
		 */
		bool any(std::function<bool(T)> pred) const;
 	private:
 		template<class U>
 		friend class enumerable;

		enumerable() {}

		/**
		 * interface for adding elements...if vector will be replaced
		 */
		inline void add(T element) {
			data.push_back(element);
		}

		/**
		 * all elements are stored in vector
		 */
		holder_type data;
	};

	/**
	 * function, which creates enumerable from iterable container
	 */
	template<class container_t>
	enumerable<typename container_t::value_type> from(container_t container) 
	{
		enumerable<typename container_t::value_type> res;
		for (auto val : container) {
			res.add(val);
		}
		return res;
	}


	// ------------- enumerable implementation -------------
	
	template<class T>
	std::vector<T> enumerable<T>::to_vector() const {
		return data; // copying!
	}

	template<class T>
	enumerable<T> enumerable<T>::where(std::function<bool(T)> pred) const {
		enumerable<T> result;
		std::copy_if(data.cbegin(), data.cend(), back_inserter(result.data), pred);
		return result;
	}

	template<class T>
	template<class R>
	enumerable<R> enumerable<T>::select(std::function<R(T)> transform) const {
		enumerable<R> result;
		std::transform(data.cbegin(), data.cend(), back_inserter(result.data), transform);
		return result;
	}

	template<class T>
	size_t enumerable<T>::count() const {
		return data.size();
	}

	template<class T>
	size_t enumerable<T>::count(std::function<bool(T)> pred) const {
		return std::count_if(data.cbegin(), data.cend(), pred);
	}

	template<class T>
	bool enumerable<T>::any() const {
		return any([](const T& x){ return true; });
	}

	template<class T>
	bool enumerable<T>::any(std::function<bool(T)> pred) const {
		return std::any_of(data.cbegin(), data.cend(), pred);
	}


}
