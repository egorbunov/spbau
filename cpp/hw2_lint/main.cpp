#include <iostream>
#include <sstream>
#include <iomanip>
#include <fstream>
#include "lint.h"

using namespace std;

void test_ll_construct() {
    long long nums[] = {0, -0, 1, -1, 123124, 1000000, -10000001};

    for (long long x : nums) {
        string s = to_string(x);
        apa::lint lx(x);
        if (lx.to_string() != s) {
            cout << "FAILED on [ " << s << " ]" << " actual = " << lx.to_string() << endl;
        }
    }
}

void test_to_string() {
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
        }
    }

    apa::lint testNZ("-0");
    if (testNZ.to_string() != "0") {
        cout << "FAILED on [ -0 ]" << " actual = " << testNZ.to_string() << endl;
    }
}

void test_small_compare() {
    vector<long long> numsA = {0, 1, 2, 3, -10, -20, -30, 123123, 1000000, -100000000};
    vector<long long> numsB = {-0, -1000, 2200, 12312, 31231, 123123, 999999, 123, 111, -100000000};

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

        if (actual != expected)
            cout << "COMPARE FAILED: expected = " << expected << " actual = " << actual << endl;
    }


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

int main() {
    test_add();
    test_sub();
    test_ll_construct();
    test_to_string();
    test_small_compare();
    return 0;
}