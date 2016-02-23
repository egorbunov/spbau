#pragma once

// Policy class
template<typename T>
class NewCreator {
public:
	T* create() {
		return new T;
	}

	T* create(const T& arg) {
		return new T(arg);
	}
};

// Policy class user
template<template <class Created> class CreationPolicy>
class IntManager : public CreationPolicy<int> {
};