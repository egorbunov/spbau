//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <string>

/**
 * Task C
 */
namespace contest1C {
    using namespace std;

    int D[2001][2001];

    void solve() {
        string s;
        cin >> s;
        string scpy = s;

        int k;
        for (int i = 0; i < s.length(); ++i) {
            for (int j = i + 1; j < s.length(); ++j) {
                k = j - i - 1;
                D[k][j] = 1000000000;
                if (s[k] == s[j]) {
                    D[k][j] = D[k+1][j-1];
                }
                if (D[k + 1][j - 1] + 2 < D[k][j]) {
                    D[k][j] = D[k + 1][j - 1] + 2;
                }
                if (D[k+1][j] + 1 < D[k][j]){
                    D[k][j] = D[k+1][j] + 1;
                }
                if (D[k][j-1] + 1 < D[k][j]){
                    D[k][j] = D[k][j-1] + 1;
                }
            }
        }

//        for (int i = 0; i < s.length(); ++i) {
//            for (int j = 0; j < s.length(); ++j) {
//                cout << D[i][j] << " ";
//            }
//            cout << endl;
//        }

        int l = 0;
        int r = s.length() - 1;
        while (r - l > 0) {
            if (D[l][r] == D[l+1][r] + 1) {
                scpy[l] = '-';
                l += 1;
            } else if (D[l][r] == D[l][r-1] + 1) {
                scpy[r] = '-';
                r -= 1;
            } else if (D[l][r] == D[l+1][r-1] + 2) {
                scpy[l] = scpy[r] = '-';
                r -= 1; l += 1;
            } else {
                r -= 1; l += 1;
            }
        }

        for (int i = 0; i < scpy.length(); ++i) {
            if (scpy[i] != '-') cout << scpy[i];
        }
    }

}