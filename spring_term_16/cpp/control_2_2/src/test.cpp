#include <cmath>
#include <iostream>
#include <cassert>
#include <type_traits>
#include <sstream>

#include "reflection.h"
#include "binary_serialization.h"
#include "json_serialization.h"

//#define TEST_BIN_SERIALIZATION
//#define TEST_JSON_SERIALIZATION

// === binary serialization tests ===
struct pod_struct
{
    int a;
    double b;
};

static void test_binary_pod()
{
    static_assert(std::is_pod<pod_struct>::value,
            "pod_struct not pod!?");
#ifdef TEST_BIN_SERIALIZATION
    std::stringstream stream;
    std::ostream &os = stream;
    std::istream &is = stream;

    pod_struct ps{ 1, 3.14 };
    serialization::write(os, ps);

    pod_struct ps_read;
    serialization::read(is, ps_read);

    assert(ps.a == ps_read.a);
    assert(ps.b == ps_read.b);
#endif
}

struct not_pod_struct
{
    not_pod_struct()
        : not_pod_struct(0, 0, {0 , 0})
    {}

    not_pod_struct(int a, double b, pod_struct pod)
        : a(a),
          b(b),
          pod(pod)
    {}

    int a;
    double b;
    pod_struct pod;

    virtual ~not_pod_struct() {}
};

template<class proc>
void reflect_type(proc& p, not_pod_struct& nps)
{
    using namespace reflection;
    reflect_field(p, nps.a, "a");
    reflect_field(p, nps.b, "b");
    reflect_field(p, nps.pod, "pod");
}

void test_binary_not_pod()
{
    static_assert(!std::is_pod<not_pod_struct>::value,
            "not_pod_struct is pod!?");
#ifdef TEST_BIN_SERIALIZATION
    std::stringstream stream;
    std::ostream &os = stream;
    std::istream &is = stream;

    not_pod_struct ps{ 1, 3.14, { 'P', 'O' } };
    serialization::write(os, ps);

    not_pod_struct ps_read;
    serialization::read(is, ps_read);

    assert(ps.a == ps_read.a);
    assert(std::abs(ps.b - ps_read.b) < 0.001);
    assert(ps.pod.a == ps_read.pod.a);
    assert(ps.pod.b == ps_read.pod.b);
#endif
}

void test_binary_serialization()
{
    test_binary_pod();
    test_binary_not_pod();
}

// === json serialization tests ===
struct small_record
{
    char letter;
    bool flag;

    small_record()
        : letter(0)
        , flag(0)
    {}
};

template<class proc>
void reflect_type(proc& p, small_record& smr)
{
    using namespace reflection;
    reflect_field(p, smr.letter, "letter");
    reflect_field(p, smr.flag, "flag");
}

struct custom_record
{
    custom_record()
        : dvalue(0)
        , ivalue(0)
    {}

    double dvalue;
    int    ivalue;
    small_record small;
};

template<class proc>
void reflect_type(proc& p, custom_record& cr)
{
    using namespace reflection;
    reflect_field(p, cr.dvalue, "dvalue");
    reflect_field(p, cr.ivalue, "ivalue");
    reflect_field(p, cr.small, "small");
}

void test_json_arithmetic()
{
#ifdef TEST_JSON_SERIALIZATION
    serialization::json_value_t jvalue;

    int i = 42;
    write(jvalue, i);

    int o;
    read(jvalue, o);

    assert(i == o);
#endif
}

void test_json_struct()
{
#ifdef TEST_JSON_SERIALIZATION
    serialization::json_value_t jvalue;

    custom_record cr;
    cr.dvalue = 3.14;
    cr.small.letter = 'P';
    write(jvalue, cr);

    custom_record cr2;
    read(jvalue, cr2);

    assert(abs(cr2.dvalue - 3.14) < 0.001);
    assert(cr2.small.letter == 'P');
#endif
}

struct ver0
{
    int a;
    short b;
    char c;
};

template<class proc>
void reflect_type(proc& p, ver0& v0)
{
    using namespace reflection;
    reflect_field(p, v0.a, "a");
    reflect_field(p, v0.b, "b");
    reflect_field(p, v0.c, "c");
}

struct ver1
{
    int a;
    short b;
};

template<class proc>
void reflect_type(proc& p, ver1& v1)
{
    using namespace reflection;
    reflect_field(p, v1.a, "a");
    reflect_field(p, v1.b, "b");
}

void test_json_field_removed()
{
#ifdef TEST_JSON_SERIALIZATION
    serialization::json_value_t jvalue;
    ver0 v0{ 1, 2, 'c' };
    write(jvalue, v0);

    ver1 v1;
    read(jvalue, v1);

    assert(v0.a == v1.a);
    assert(v0.b == v1.b);
#endif
}

void test_json_field_added()
{
#ifdef TEST_JSON_SERIALIZATION
    serialization::json_value_t jvalue;
    auto v1 = ver1{ 3, 4 };
    write(jvalue, v1);

    auto v0 = ver0{};
    read(jvalue, v0);

    assert(v0.a == v1.a);
    assert(v0.b == v1.b);
    assert(v0.c == 0);
#endif
}

void test_json_serialization()
{
    test_json_arithmetic();
    test_json_struct();
    test_json_field_added();
    test_json_field_removed();
}

int main()
{
    test_binary_serialization();
    test_json_serialization();
    std::cout << "Ok!" << std::endl;
    return 0;
}
