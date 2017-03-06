//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>

/**
 * Task B
 */
namespace contest1B {
    using namespace std;

    const int MAX_M = (int) 1e4;
    int price[MAX_M + 1];
    bool *D[2];

    /**
     * Main idea: dynamic solution, matrix D[w][k]:
     *
     * D[w][k] = is it possible to split 'k' first presents so the price difference will be 'w' (true ot false)
     * and recurrent formula is:
     *      D[w][k] = D[abs(w - w_k)][k - 1] || D[w + w_k][k - 1]
     * That is because if u have 2 sets of presents: S1 and S2. U can add k'th present with price w_k to S1 or to S2
     * and due to your choice result difference will be (S1 + w_k) - S2 = (S1-S2)+w_k or (S1-S2)-w_k...
     */
    void solve() {
        int m;
        cin >> m;
        int sum = 0;
        price[0] = 0;
        for (int i = 1; i <= m; ++i) {
            cin >> price[i];
            sum += price[i];
        }

        D[0] = new bool[sum + 1];
        D[1] = new bool[sum + 1];

        D[0][0] = true;
        for (int i = 1; i <= sum; ++i) {
            D[0][i] = false;
        }

        for (int i = 1; i <= m; ++i) {
            for (int j = 0; j <= sum; ++j) {
                D[1][j] = (D[0][abs(j - price[i])])
                          || ((j + price[i] <= sum) ? (D[0][j + price[i]]) : false);
            }
            swap(D[0], D[1]);
        }

        for (int i = 0; i <= sum; ++i) {
            if (D[0][i]) {
                cout << i;
                return;
            }
        }
    }

}

int main() {
    contest1B::solve();
    return 0;
}