#include "shared_buffer.h"

shared_buffer::shared_buffer(size_t size) : size(size) {
	data = new char[size];
	ref_cnt = new size_t(1);
}

size_t shared_buffer::get_size() const {
	return size;
}

char* shared_buffer::get_data() {
	return data;
}

const char* shared_buffer::get_data() const {
	return data;
}

void shared_buffer::swap(shared_buffer &obj) {
	std::swap(data, obj.data);
	std::swap(size, obj.size);
	std::swap(ref_cnt, obj.ref_cnt);
}

shared_buffer& shared_buffer::operator=(shared_buffer src) {
	std::cout << "ASSIGN" << std::endl;
	try_destruct();
	swap(src);
	return *this;
}

shared_buffer::~shared_buffer() {
	std::cout << "DESTRUCTION" << std::endl;
	try_destruct();
}

shared_buffer::shared_buffer(const shared_buffer &src) {

	std::cout << "COPY" << std::endl;

	*src.ref_cnt += 1;
	ref_cnt = src.ref_cnt;
	data = src.data;
	size = src.size;
}

void shared_buffer::try_destruct() {
	*ref_cnt -= 1;
	if (*ref_cnt == 0) {
		delete [] data;
		data = nullptr;
		delete ref_cnt;
		ref_cnt = nullptr;
	}
}

