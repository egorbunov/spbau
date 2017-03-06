//
// Created by egorbunov on 10/20/15.
//

#include <iostream>
#include <vector>

int main() {
    using namespace std;

    size_t n;
    cin >> n;
    vector<int> arr(n);
    for (int i = 0, x; i < (int) n; ++i) {
        cin >> x;
        arr[i] = x;
    }

    vector<int> D(n, 1);

    int max = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            if (arr[i] % arr[j] == 0)
                D[i] = std::max(D[j] + 1, D[i]);
        }
        max = std::max(D[i], max);
    }

    cout << max;

    return 0;
}