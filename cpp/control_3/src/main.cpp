#include <iostream>
#include <cassert>
#include "au_list.h"

using namespace std;
using containers::au_list;

void task0()
{
    using namespace containers;
    int a = 0;
    au_list<int>::value_type vt = a;
    au_list<int>::reference ref = a;
    au_list<int>::const_reference cref = a;
    au_list<int>::pointer ptr = &a;
    au_list<int>::const_pointer cptr = &a;
    au_list<int>::size_type size = 0;
    (void)vt;
    (void)ref;
    (void)cref;
    (void)ptr;
    (void)cptr;
    (void)size;
}

void task1()
{
    using namespace containers;
    au_list<int>::node node;
    const au_list<int>::node cnode;
    au_list<int>::reference ref = node.get_value();
    assert(ref == int());
    au_list<int>::const_reference cref = cnode.get_value();
    assert(cref == int());

    int a = 5;
    node.set_value(a);
    node.get_value()++;
    assert(a == 5);
    assert(node.get_value() == a + 1);
}

void task2()
{
    using namespace containers;
    au_list<int> list;
    assert(list.begin() == list.end());
    assert(list.empty());
    assert(list.size() == 0);

    const au_list<int> clist;
    assert(clist.begin() == clist.end());
    assert(clist.empty());
    assert(clist.size() == 0);
}

void task3()
{
    using namespace containers;
    au_list<int> list;
    list.insert(list.end(), 5);
    assert(list.begin()->get_value() == 5);
    assert(!list.empty());
    assert(list.size() == 1);
    int arr[] = {1, 2, 3, 4};
    list.insert(list.begin(), &arr[0], &arr[3]);
    assert(!list.empty());
    assert(list.size() == 5);

    au_list<int> list_copy(list);
    assert(!list_copy.empty());
    assert(list_copy.size() == 5);
}

void task4()
{
    using namespace containers;
    au_list<int> list;
    int arr[] = {1, 2, 3, 4};
    list.insert(list.begin(), &arr[0], &arr[3]);
    auto after_erased = list.erase(list.begin());
    assert(list.size() == 3);
    assert(after_erased->get_value() == 2);
    after_erased = list.erase(list.begin(), list.end());
    assert(list.size() == 0);
    assert(after_erased == list.end());
}


int main() {
    task0();
    task1();
    task2();
    task3();
    task4();

    return 0;
}