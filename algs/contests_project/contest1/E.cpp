//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <string>

/**
 * Task E
 */
namespace contest1E {
    using namespace std;

    void solve() {
        int questTime;
        int d; // number of things in dragons chest
        int a; // number of things in alch. cchest

        cin >> questTime >> d >> a;

        if (questTime > 120 || (7.0 / d) > 1) {
            cout << "Hideout" << endl;
            return;
        }

        int questNum = 120 / questTime;

        if (questNum * d > a) {
            cout << "Fear not" << endl;
        } else {
            cout << "Hideout" << endl;
        }
    }
}
