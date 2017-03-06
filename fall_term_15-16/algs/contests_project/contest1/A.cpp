//
// Created by egorbunov on 26.10.15.
//
#include <algorithm>
#include <iostream>

namespace contest1A {
    using namespace std;

    bool isInM[10002];
    int* D[2];
    int* DM[2];

    /**
     * Main idea: calculate whole number of partitions of N and subtract number
     * of partitions, where no any m from M occurred.
     *
     * Here I store only 2 columns of table to calculate
     */
    void solve() {
        const int MOD = 100111001;

        int n;
        cin >> n;

        D[0] = new int[n+1];
        D[1] = new int[n+1];
        DM[0] = new int[n+1];
        DM[1] = new int[n+1];
        for (int i = 0; i <= n; ++i) {
            D[0][i] = DM[0][i] = 0;
            D[1][i] = DM[1][i] = 0;
        }

        int m;
        cin >> m;
        int x;
        for (int i = 0; i < m; ++i) {
            cin >> x;
            if (x <= 10001)
                isInM[x] = true;
        }

        D[1][0] = DM[1][0] = 0;
        D[1][1] = DM[1][1] = 1;
        if (isInM[1])
            DM[1][1] = 0;

        for (int i = 1; i <= n; ++i) {

            for (int j = 2; j <= n; ++j) {
                D[1][j] = (D[0][j] + ((j < i) ? 0 : D[1][j-i])) % MOD ;
                DM[1][j] = (DM[0][j] + (( (j < i) || (isInM[i]) ) ? 0 : DM[1][j-i])) % MOD ;
            }

            swap(D[0], D[1]);
            swap(DM[0], DM[1]);

            D[1][0] = DM[1][0] = 1;
            D[1][1] = DM[1][1] = 1;

            if (isInM[1])
                DM[1][1] = 0;
            if (isInM[i+1])
                DM[1][0] = 0;

        }

        cout << (D[0][n] - DM[0][n]) % MOD;
    }
}