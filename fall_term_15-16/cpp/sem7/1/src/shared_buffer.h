#pragma once

#include <cstring>
#include <utility>
#include <iostream>

struct shared_buffer {
	explicit shared_buffer(size_t size);
	shared_buffer(const shared_buffer &src);
	shared_buffer& operator=(shared_buffer src);
	~shared_buffer();
	char* get_data();
	const char* get_data() const;
	size_t get_size() const;

	int get_ref_cnt() {
		return *ref_cnt;
	}
private:
	char* data;
	size_t size;
	size_t* ref_cnt; // number of references to 'data'

	void swap(shared_buffer &obj);
	void try_destruct();
};