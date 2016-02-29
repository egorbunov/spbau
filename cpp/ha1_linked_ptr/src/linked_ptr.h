#pragma once

#include <cassert>

namespace smart_ptr {

namespace {
	class linked_ptr_node {
	public:
		linked_ptr_node();
		void swap(linked_ptr_node& other);
		bool unique() const;
		void insert(linked_ptr_node* node);
		template<class T>
		bool release(T* ptr);
	private:
		linked_ptr_node* prev;
		linked_ptr_node* next;			
	};
}

template<class T>
class linked_ptr {
public:
	/**
	 * Constructs linked_ptr with no managed object
	 */
	linked_ptr();

	/**
	 * Constructs linked_ptr with ptr as pointer to managed object;
	 * ptr must be convertible to T*
	 */
	template<class U>
	explicit linked_ptr(U* ptr);

	/**
	 * Constructs linked_ptr which shares the object ownership, managed by r.
	 */
	linked_ptr(const linked_ptr& r);

	/**
	 * Constructs linked_ptr which shares the object ownership, managed by r.
	 * U must be implicitly convertible to T
	 */
	template<class U>
	explicit linked_ptr(const linked_ptr<U>& r);

	~linked_ptr();

	/**
	 * Replaces managed object with object, managed by r
	 */
	linked_ptr& operator=(const linked_ptr& r);

	template<class U>
	linked_ptr& operator=(const linked_ptr<U>& r);

	/**
	 * Releases the ownership of managed object
	 */
	void reset();

	/**
	 * Replaces managed object with given
	 */
	template<class U>
	void reset(U* ptr);

	void swap(linked_ptr& r);

	/**
	 * Checks if this linked_ptr is the only one who holds 
	 * reference to it's managed object
	 */
	bool unique() const;

	/**
	 * Returns stored pointer
	 */
	T* get() const;

	/**
	 * Dereferencing operators; Undefined behaviour if nullptr is stored
	 */
	T& operator*() const;
	T* operator->() const;

	operator bool() const;

private:
	T* ptr;
	mutable linked_ptr_node node;

	template<typename> friend class linked_ptr;
};

// Implementation

// -------------- linked_ptr_node ---------------------

linked_ptr_node::linked_ptr_node(): prev(nullptr), next(nullptr) {
}

void linked_ptr_node::swap(linked_ptr_node& other) {
	bool l = (this == other.prev);
	bool r = (this == other.next);
	std::swap(next, other.next);
	std::swap(prev, other.prev);

	if (l) {
		other.next = this;
		prev = &other;
	}
	if (r) {
		next = &other;
		other.prev = this;
	}
	if (prev != nullptr) {
		prev->next = this;
	}
	if (next != nullptr) {
		next->prev = this;
	}
	if (other.prev != nullptr) {
		other.prev->next = &other;
	}
	if (other.next != nullptr) {
		other.next->prev = &other;
	}
}

bool linked_ptr_node::unique() const {
	return next == nullptr && prev == nullptr;
}

void linked_ptr_node::insert(linked_ptr_node* node) {
	node->prev = this;
	node->next = next;
	if (next != nullptr) {
		next->prev = node;
	}
	next = node;
}

template<class T>
bool linked_ptr_node::release(T* ptr) {
	if (unique()) {
		delete ptr;
	}
	if (prev != nullptr) {
		prev->next = next;
	}
	if (next != nullptr) {
		next->prev = prev;
	}
	next = nullptr;
	prev = nullptr;
	return false;
}

// ---------------- linked_ptr --------------------

template<class T>
linked_ptr<T>::linked_ptr(): ptr(nullptr) {
}

template<class T>
template<class U>
linked_ptr<T>::linked_ptr(U* ptr): ptr(ptr) {
}

template<class T>
linked_ptr<T>::linked_ptr(const linked_ptr& r): ptr(r.get()) {
	r.node.insert(&node);
}

template<class T>
template<class U>
linked_ptr<T>::linked_ptr(const linked_ptr<U>& r): ptr(r.get()) {
	r.node.insert(&node);
}

template<class T>
linked_ptr<T>::~linked_ptr() {
	typedef char type_must_be_complete[sizeof(T)? 1: -1];
    (void) sizeof(type_must_be_complete);

    node.release(ptr);
    ptr = nullptr;
}

template<class T>
linked_ptr<T>& linked_ptr<T>::operator=(const linked_ptr& r) {
	linked_ptr tmp(r);
	swap(tmp);
	return *this;
}

template<class T>
template<class U>
linked_ptr<T>& linked_ptr<T>::operator=(const linked_ptr<U>& r) {
	linked_ptr<T> tmp(r);
	swap(tmp);
	return *this;	
}

template<class T>
void linked_ptr<T>::reset() {
	linked_ptr().swap(*this);
}

template<class T>
template<class U>
void linked_ptr<T>::reset(U* ptr) {
	linked_ptr<T>(ptr).swap(*this);
}

template<class T>
void linked_ptr<T>::swap(linked_ptr& r) {
	node.swap(r.node);
	std::swap(ptr, r.ptr);
}

template<class T>
bool linked_ptr<T>::unique() const {
	return ptr != nullptr && node.unique();
}

template<class T>
T* linked_ptr<T>::get() const {
	return ptr;
}

template<class T>
T& linked_ptr<T>::operator*() const {
	return *ptr;
}

template<class T>
T* linked_ptr<T>::operator->() const {
	return ptr;
}

template<class T>
linked_ptr<T>::operator bool() const {
	return ptr != nullptr;	
}


/**
 * Comparison operators
 */

template<class T>
bool operator==(const linked_ptr<T>& lhs, const linked_ptr<T>& rhs) {
	return lhs.get() == rhs.get();
}

template<class T>
bool operator!=(const linked_ptr<T>& lhs, const linked_ptr<T>& rhs) {
	return !(lhs == rhs);
}

template<class T>
bool operator<(const linked_ptr<T>& lhs, const linked_ptr<T>& rhs) {
	return std::less<T*>()(lhs.get(), rhs.get());
}

template<class T>
bool operator>(const linked_ptr<T>& lhs, const linked_ptr<T>& rhs) {
	return rhs < lhs;
}

template<class T>
bool operator<=(const linked_ptr<T>& lhs, const linked_ptr<T>& rhs) {
	return !(lhs > rhs);
}

template<class T>
bool operator>=(const linked_ptr<T>& lhs, const linked_ptr<T>& rhs) {
	return !(lhs < rhs);
}

/**
 * nullptr comparisons
 */

template<class T>
bool operator==(std::nullptr_t lhs, const linked_ptr<T>& rhs) {
	return !rhs;
}

template<class T>
bool operator==(const linked_ptr<T>& lhs, std::nullptr_t rhs) {
	return !lhs;
}

template<class T>
bool operator!=(std::nullptr_t lhs, const linked_ptr<T>& rhs) {
	return static_cast<bool>(rhs);
}

template<class T>
bool operator!=(const linked_ptr<T>& lhs, std::nullptr_t rhs) {
	return static_cast<bool>(lhs);
}

template<class T>
bool operator<(std::nullptr_t lhs, const linked_ptr<T>& rhs) {
	return std::less<T*>()(nullptr, rhs.get());
}

template<class T>
bool operator<(const linked_ptr<T>& lhs, std::nullptr_t rhs) {
	return std::less<T*>()(lhs.get(), nullptr);
}

template<class T>
bool operator>(std::nullptr_t lhs, const linked_ptr<T>& rhs) {
	return rhs < nullptr;
}

template<class T>
bool operator>(const linked_ptr<T>& lhs, std::nullptr_t rhs) {
	return nullptr < lhs;
}

template<class T>
bool operator<=(std::nullptr_t lhs, const linked_ptr<T>& rhs) {
	return !(nullptr > rhs);
}

template<class T>
bool operator<=(const linked_ptr<T>& lhs, std::nullptr_t rhs) {
		return !(lhs > nullptr);
}

template<class T>
bool operator>=(std::nullptr_t lhs, const linked_ptr<T>& rhs) {
	return !(nullptr < rhs);
}

template<class T>
bool operator>=(const linked_ptr<T>& lhs, std::nullptr_t rhs) {
	return !(lhs < nullptr);
}

}
