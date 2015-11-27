#include <iostream>
#include <sstream>
#include <iomanip>
#include <fstream>
#include <limits>
#include <random>
#include "lint.h"

using namespace std;

void test_ll_construct() {
    string test_name = "{ LL_CONSTRUCT_TEST }";
    cout << "-------------------" << test_name << "-------------------------" << endl;
    bool isFailed = false;
    long long nums[] = {0, -0, 1, -1, 123124, 1000000, -10000001};

    for (int x : nums) {
        string s = to_string(x);
        apa::lint lx(x);
        if (lx.to_string() != s) {
            cout << "FAILED on [ " << s << " ]" << " actual = " << lx.to_string() << endl;
            isFailed = true;
        }
    }

    if (isFailed)
        cout << "       FAILED: " << test_name << endl;
    else
        cout << "       PASSED: " << test_name << endl;
}

void test_to_string() {
    string test_name = "{ TO_STRING_TEST }";
    cout << "-------------------" << test_name << "-------------------------" << endl;
    bool isFailed = false;
    std::string strs[] = {"0",
                          "100002",
                          "1000000000000000000000000000000",
                          "12231124231231212314324103841092384014573948572093813208561938247",
                          "-1230172498134698237641987236419238746123893120298120372134982364",
                          "-1",
                          "1",
                          "-123123000000000134123400000023120123912803982040900001459340502054280435820344025234052034502405234502345023405203450243502043502130210310"};
    for (string s : strs) {
        apa::lint x(s);
        if (x.to_string() != s) {
            cout << "FAILED on [ " << s << " ]" << " actual = " << x.to_string() << endl;
            isFailed = true;
        }
    }

    apa::lint testNZ("-0");
    if (testNZ.to_string() != "0") {
        cout << "FAILED on [ -0 ]" << " actual = " << testNZ.to_string() << endl;
        isFailed = true;
    }

    if (isFailed)
        cout << "       FAILED: " << test_name << endl;
    else
        cout << "       PASSED: " << test_name << endl;
}

void test_small_compare() {
    string test_name = "{ COMPARE_TEST }";
    cout << "-------------------" << test_name << "-------------------------" << endl;

    vector<int> numsA = {0, 1, 2, 3, -10, -20, -30, 123123, 1000000, -100000000};
    vector<int> numsB = {-0, -1000, 2200, 12312, 31231, 123123, 999999, 123, 111, -100000000};

    bool isFailed = false;
    for (int i = 0; i < numsA.size(); ++i) {
        int expected;
        if (numsA[i] < numsB[i])
            expected = -1;
        else if (numsA[i] > numsB[i])
            expected = 1;
        else
            expected = 0;

        int actual;
        apa::lint la(numsA[i]), lb(numsB[i]);
        if (la < lb)
            actual = -1;
        else if (la > lb)
            actual = +1;
        else
            actual = 0;

        if (actual != expected) {
            cout << "COMPARE FAILED: expected = " << expected << " actual = " << actual << endl;
            isFailed = true;
        }
    }
    if (isFailed)
        cout << "       FAILED: " << test_name << endl;
    else
        cout << "       PASSED: " << test_name << endl;
}

void test_add() {
    string fname = "test_data/sum.test";
    string test_name = "{ SUM_TEST }";

    cout << "-------------------" << test_name << "-------------------------" << endl;

    ifstream in(fname, ifstream::in);
    if (!in) {
        cout << "Cannot open test file!";
        return;
    }
    int n;
    in >> n;
    string ex, ey, esum;
    bool isFailed = false;
    for (int i = 0; i < n; ++i) {
        in >> ex >> ey >> esum;
        apa::lint ax(ex), ay(ey), lsum(esum);
        apa::lint asum = ax + ay;
        if (ax + ay != lsum) {
            cout << i << " | FAILED.      Expected: " << ex << " + " << ey << " = " << esum << endl;
            cout << "                     Actual:   " << ax << " + " << ay << " = " << asum << endl;
            isFailed = true;
        }
    }
    if (isFailed)
        cout << "       FAILED: " << test_name << endl;
    else
        cout << "       PASSED: " << test_name << endl;
}

void test_sub() {
    string fname = "test_data/sub.test";
    string test_name = "{ SUB_TEST }";

    cout << "-------------------" << test_name << "-------------------------" << endl;

    ifstream in(fname, ifstream::in);
    if (!in) {
        cout << "Cannot open test file!";
        return;
    }
    int n;
    in >> n;
    string ex, ey, esub;
    bool isFailed = false;
    for (int i = 0; i < n; ++i) {
        in >> ex >> ey >> esub;
        apa::lint ax(ex), ay(ey), lsub(esub);
        apa::lint asub = ax - ay;
//        cout << "TESTING : " << ex << " - " << ey << " = " << esub << endl;
        if (asub != lsub) {
            cout << i << " | FAILED.      Expected: " << ex << " - " << ey << " = " << esub << endl;
            cout << "                     Actual:   " << ax << " - " << ay << " = " << asub << endl;
            isFailed = true;
        }
    }
    if (isFailed)
        cout << "       FAILED: " << test_name << endl;
    else
        cout << "       PASSED: " << test_name << endl;
}

void mul_test() {
    string fname = "test_data/mul.test";
    string test_name = "{ MUL_TEST }";

    cout << "-------------------" << test_name << "-------------------------" << endl;

    ifstream in(fname, ifstream::in);
    if (!in) {
        cout << "Cannot open test file!";
        return;
    }
    int n;
    in >> n;
    string ex, ey, emul;
    bool isFailed = false;
    for (int i = 0; i < n; ++i) {
        in >> ex >> ey >> emul;
        apa::lint ax(ex), ay(ey), lmul(emul);
        apa::lint amul = ax * ay;
        if (amul != lmul) {
            cout << i << " | FAILED.      Expected: " << ex << " * " << ey << " = " << emul << endl;
            cout << "                     Actual:   " << ax << " * " << ay << " = " << amul << endl;
            isFailed = true;
        }
    }
    if (isFailed)
        cout << "       FAILED: " << test_name << endl;
    else
        cout << "       PASSED: " << test_name << endl;
}

void div_test() {
    string test_name = "{ DIV_TEST }";
    cout << "-------------------" << test_name << "-------------------------" << endl;
    using namespace apa;

    std::random_device rd;
    std::mt19937 gen(rd());
    long long from = numeric_limits<int>::min();
    long long to = numeric_limits<int>::max();
    std::uniform_int_distribution<int> dis(from, to);

    // long long test

    int num = 100000;

    bool isFailed = false;
    for (int i = 0; i < num; ++i) {
        int a = dis(gen);
        int b = 0;
        while (b == 0) b = dis(gen);

        lint la(a);
        lint lb(b);
        int expected = a / b;
        lint actual = la / lb;
//
//        cout << "          expected [ " << a << " / " << b << " = " << a / b << " ] " << endl;
//        cout << "            actual [ " << la << " / " << lb << " = " << la / lb << " ] " << endl;
//        cout << endl;

        if (expected != actual) {
            cout << " FAILED : expected [ " << a << " / " << b << " = " << a / b << " ] " << endl;
            cout << "            actual [ " << la << " / " << lb << " = " << la / lb << " ] " << endl;
            isFailed = true;
        }
    }

    if (isFailed)
        cout << "       FAILED: " << test_name << endl;
    else
        cout << "       PASSED: " << test_name << endl;

//    long long a = 6;
//    long long b = -3;
//    lint x(a);
//    lint y(b);
//    lint d = x / y;
//
//    cout << x / y << " [ " << a / b << " ] " << endl;
}

void x_test() {
    using namespace apa;
//    double dx = -123e3;
//    lint x(dx);
//    cout << x << " " << (int) dx;
//    int p = 20;
//    lint x("123123");
//    cout << x << "^" << p << "=" << pow(x, p) << endl;
//    cout << abs(lint("-321111111111111111112231000000000000002312")) << endl;
//    cout << pow(lint(132131), 0) << endl;

//    char str[] = "0123456789x";
//
//    lint y = 0;
//    cout << str[(int) y++];
//    cout << str[(int) y];
//    cout << str[(int) ++y];
//    cout << endl;
//
//    for (lint x = 0; x < 10; ++x) {
//        cout << x << " ";
//    }
//    cout << endl;
//    for (lint x = 0; x < 10; x++) {
//        cout << x << " ";
//    }
//    cout << endl;
//    for (lint x = 10; x >= 0; x--) {
//        cout << x << " ";
//    }
//    cout << endl;
//    for (lint x = 10; x >= 0; --x) {
//        cout << x << " ";
//    }
//    cout << endl;

    lint x(numeric_limits<int>::max());
    cout << x;
}

int main() {
    apa::lint x("-100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001");
    apa::lint y("-100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001");

    x -= y;

    cout << x;
//    x_test();
//    div_test();
//    mul_test();
//    test_add();
//    test_sub();
//    test_ll_construct();
//    test_to_string();
//    test_small_compare();
    return 0;
}