#pragma once

#include <utility>


template<class T>
class scoped_ptr;

template<class T>
void swap<T>(scoped_ptr<T> &ptr1, scoped_ptr<T> &ptr2);

template<class T> class scoped_ptr {
	public:
		using element_type = T;

		scoped_ptr(T* ptr) : ptr(ptr) {
		}

		scoped_ptr() : ptr(nullptr) {
		}

		virtual ~scoped_ptr() {
			delete ptr;
		}

		T * const get() const {
			return ptr;
		}

		void reset() {
			delete ptr;
			ptr = nullptr;
		}

		void reset(T* ptr) {
			reset();
			this->ptr = ptr;
		}

		T& operator*() const {
			return *ptr;
		}

		T * const operator->() const {
			return get();
		}

		explicit operator bool() const {
			return !(ptr == nullptr);
		}

		scoped_ptr(const scoped_ptr&) = delete;
		scoped_ptr(scoped_ptr&&) = delete;
		scoped_ptr& operator=(const scoped_ptr &other) = delete;

		template<class T>
		friend void swap<T>(scoped_ptr<T> &ptr1, scoped_ptr<T> &ptr2) {
			std::swap(ptr1.ptr, ptr2.ptr);
		}

	private:
		T * ptr;
};