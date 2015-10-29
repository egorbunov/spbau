//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <string>

/**
 * Task F
 */
namespace contest1F {
    using namespace std;

    void solve() {
        int n;
        cin >> n;

        vector<int> heap(n);

        heap[0] = 1;
        int cnt = 0;
        for (int i = 1; i < n; ++i) {
            heap[i - 1] = i + 1;
            heap[i] = 1;

            int j = i - 1;
            while (j > 0) {
                int parent = (j - 1) / 2;
                swap(heap[parent], heap[j]);
                j = parent;
                cnt += 1;
            }

        }

        cout << cnt << endl;
        for (int i = 0; i < n; ++i) {
            cout << heap[i] << " ";
        }
        cout << endl;
    }
}

int main() {
    contest1F::solve();
    return 0;
}