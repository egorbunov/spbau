//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <string>

/**
 * Task D
 */
namespace contest1D {
    using namespace std;

    int cnts[10];

    void solve() {
        char c;
        int n;

        cin >> n;
        for (int i = 0; i < n; ++i) {
            cin >> c;
            cnts[(size_t) (c - '0')] += 1;
        }

        for (int i = 1; i < 10; ++i) {
            if (cnts[i] > 0) {
                cout << i;
                cnts[i] -= 1;
                break;
            }
        }
        for (int i = 0; i < 10; ++i) {
            while (cnts[i] > 0) {
                cout << i;
                cnts[i] -= 1;
            }
        }
        cout << endl;
    }
}