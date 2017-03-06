#include <iostream>
#include <bitset>

using namespace std;

int main() {

    string bitStr1, bitStr2;
    cin >> bitStr1 >> bitStr2;

    int n = (int) bitStr1.length();
    for (int i = 0; i < n; ++i) {
        string tmp(bitStr1);
        cout << tmp << " ";
        tmp[i] = '1' - tmp[i] + '0';
        cout << tmp << " ";
        for (int j = i + 1; j < n; ++j) {
            if (tmp[j] != bitStr2[j]) {
                tmp[j] = bitStr2[j];
                cout << tmp << " ";
            }
        }
        for (int j = 0; j <= i; ++j) {
            if (tmp[j] != bitStr2[j]) {
                tmp[j] = bitStr2[j];
                cout << tmp << " ";
            }
        }
        cout << endl;
    }

    return 0;
}
