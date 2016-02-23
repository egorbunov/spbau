#include "gtest/gtest.h"
#include "linked_ptr.h"

struct obj {
	static int new_cnt;
	static int del_cnt;
	
	int v;

	obj(int v) : v(v) {}

	void* operator new(size_t size) {
		new_cnt += 1;
		void *storage = malloc(size);
	    if (storage == nullptr) {
	        throw "allocation fail : no free memory";
	    }
	    return storage;
	}

	void operator delete(void* data) {
		del_cnt += 1;
		free(data);
	}

};

int obj::new_cnt = 0;
int obj::del_cnt = 0;

struct A_t {
	int x;
};

struct B_t : A_t {
	int y;
};


TEST(LinkedPtr, compileTest) {
	using smart_ptr::linked_ptr;
	// default constructor
	linked_ptr<int> int_sptr1;
	// construct from ptr
	int* int_ptr1 = new int(42);
	linked_ptr<int> int_sptr2(int_ptr1);
	// construct from other smart ptr
	linked_ptr<int> int_sptr3(int_sptr2);
	// assign
	int_sptr1 = int_sptr3;

	// construct from convertible ptr
	linked_ptr<A_t> sptrA1(new B_t);
	// construct from convertible sptr
	linked_ptr<B_t> sptrB1(new B_t);
	linked_ptr<A_t> sptrA2(sptrB1);
	// assign convertible
	sptrA1 = sptrB1;

	// simple reset
	int_sptr1.reset();

	// reset convertible
	sptrA1.reset(new B_t);
}

TEST(LinkedPtr, testDestruct) {
	{
		smart_ptr::linked_ptr<obj> obj_ptr1(new obj(42));
	}
	ASSERT_EQ(obj::new_cnt, obj::del_cnt);
	ASSERT_EQ(obj::new_cnt, 1);
}