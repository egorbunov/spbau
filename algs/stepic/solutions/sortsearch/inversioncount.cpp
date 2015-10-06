//
// Created by egorbunov on 06.10.15.
//

#include <iostream>
#include <vector>

namespace {
    using namespace std;

    int getInversionNumber(vector<int> &arr) {
        int cnt = 0;
        for (int i = 0; i < arr.size(); ++i) {
            for (int j = i; j < arr.size(); ++j) {
                if (arr[j] < arr[i])
                    cnt += 1;
            }
        }
        return cnt;
    }

    void solve() {
        int n, tmp;
        cin >> n;
        vector<int> arr;

        while (n-- > 0) {
            cin >> tmp;
            arr.push_back(tmp);
        }

        cout << getInversionNumber(arr);
    }
}

int main() {
    solve();
}