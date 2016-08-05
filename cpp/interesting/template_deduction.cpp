#include <iostream>
#include <typeinfo>

int fun()
{
	return 0;
}

typedef int (*pfun_t)();

template<class T>
void foo(T* p)
{
	std::cout << "T : " << typeid(T).name() << std::endl;
	std::cout << "T*: " << typeid(p).name() << std::endl;
}

int main() {
	pfun_t pf = &fun;
	foo(pf);
	int **a = new int*[10];
	foo(&a);
	return 0;
}