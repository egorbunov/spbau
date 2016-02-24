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

	static void reset() {
		new_cnt = 0;
		del_cnt = 0;
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

	// assign construct
	linked_ptr<int> int_sptr4 = int_sptr3;

	// simple reset
	int_sptr1.reset();

	// reset convertible
	sptrA1.reset(new B_t);
}

TEST(LinkedPtr, testDestruct) {
	using smart_ptr::linked_ptr;

	{
		smart_ptr::linked_ptr<obj> obj_ptr1(new obj(42));
	}
	ASSERT_EQ(obj::new_cnt, obj::del_cnt);
	ASSERT_EQ(obj::new_cnt, 1);
}

TEST(LinkedPtr, swapTest1) {
	using smart_ptr::linked_ptr;

	obj::reset();
	{
		linked_ptr<obj> obj_ptr0(new obj(42));
		{
			linked_ptr<obj> obj_ptr1(obj_ptr0);
			linked_ptr<obj> obj_ptr2(obj_ptr1);
			linked_ptr<obj> obj_ptr3(obj_ptr2);
			linked_ptr<obj> obj_ptr4(obj_ptr3);
			obj_ptr2.swap(obj_ptr3);
		}
		ASSERT_EQ(obj::new_cnt, 1);
		ASSERT_EQ(obj::del_cnt, 0);
	}
	ASSERT_EQ(obj::new_cnt, obj::del_cnt);
	ASSERT_EQ(obj::new_cnt, 1);
}

TEST(LinkedPtr, swapTest2) {
	using smart_ptr::linked_ptr;

	obj::reset();
	obj* ptr1 = new obj(42);
	obj* ptr2 = new obj(43);

	{
		linked_ptr<obj> sptr1(ptr1);
		linked_ptr<obj> sptr1_1 = sptr1;
		linked_ptr<obj> sptr1_2 = sptr1_1;

		linked_ptr<obj> sptr2(ptr2);
		linked_ptr<obj> sptr2_1 = sptr2;
		linked_ptr<obj> sptr2_2 = sptr2_1;

		sptr1_1.swap(sptr2_1);
	}
	ASSERT_EQ(obj::new_cnt, obj::del_cnt);
	ASSERT_EQ(obj::new_cnt, 2);
}

TEST(LinkedPtr, testResetWithNullptr) {
	using smart_ptr::linked_ptr;
	obj::reset();

	obj* ptr1 = new obj(42);
	linked_ptr<obj> sptr1(ptr1);
	sptr1.reset();

	ASSERT_EQ(obj::new_cnt, obj::del_cnt);
	ASSERT_EQ(obj::new_cnt, 1);
}

TEST(LinkedPtr, testResetWithPtr) {
	using smart_ptr::linked_ptr;
	obj::reset();

	{
		obj* ptr1 = new obj(42);
		obj* ptr2 = new obj(43);
		linked_ptr<obj> sptr1(ptr1);
		sptr1.reset(ptr2);
		ASSERT_EQ(obj::del_cnt, 1);
	}

	ASSERT_EQ(obj::new_cnt, obj::del_cnt);
	ASSERT_EQ(obj::new_cnt, 2);
}

TEST(LinkedPtr, testComparisons) {
	using smart_ptr::linked_ptr;


	int* int_ptr_1 = new int(42);
	linked_ptr<int> sptr1(int_ptr_1);
	linked_ptr<int> sptr2 = sptr1;

	ASSERT_TRUE(sptr1 == sptr2);
	ASSERT_FALSE(sptr1 < sptr2);
	ASSERT_FALSE(sptr1 > sptr2);
	ASSERT_FALSE(sptr1 != sptr2);
	ASSERT_TRUE(sptr1 <= sptr2);
	ASSERT_TRUE(sptr1 >= sptr2);
}

TEST(LinkedPtr, testNullComparisons) {
	using smart_ptr::linked_ptr;


	int* int_ptr = new int(42);
	linked_ptr<int> sptr1(int_ptr);

	ASSERT_TRUE(sptr1 != nullptr);
	ASSERT_TRUE(nullptr < sptr1);
	ASSERT_FALSE(nullptr > sptr1);
	ASSERT_TRUE(nullptr <= sptr1);
	ASSERT_FALSE(nullptr >= sptr1);
	ASSERT_FALSE(sptr1 == nullptr);

	ASSERT_TRUE(linked_ptr<int>() == nullptr);
}

TEST(LinkedPtr, testBoolCast) {
	using smart_ptr::linked_ptr;

	int* int_ptr = new int(42);
	linked_ptr<int> sptr1(int_ptr);

	ASSERT_TRUE(sptr1);
	ASSERT_FALSE(linked_ptr<obj>());
}

TEST(LinkedPtr, testDereferencing) {
	using smart_ptr::linked_ptr;

	struct int_holder {
		int val;
		int_holder(int val): val(val) {}
	};

	auto ptr = new int_holder(42);
	linked_ptr<int_holder> sptr(ptr);

	ASSERT_EQ(ptr->val, sptr->val);
	ASSERT_EQ((*ptr).val, (*sptr).val);
}

TEST(LinkedPtr, uniqueTest) {
	using smart_ptr::linked_ptr;

	int* int_ptr_1 = new int(42);
	linked_ptr<int> sptr1(int_ptr_1);

	ASSERT_TRUE(sptr1.unique());

	linked_ptr<int> sptr2 = sptr1;

	ASSERT_FALSE(sptr1.unique());
	ASSERT_FALSE(sptr2.unique());
}