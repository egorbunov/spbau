//
// Created by egorbunov on 10/20/15.
//

#include <iostream>
#include <vector>
#include <algorithm>
#include <limits.h>

namespace {
/**
 * Input array must be sorted descending!
 */
    int firstElementLessThan(std::vector<int> &arr, int key) {
        // every iteration performs search in [l, r) segment
        int l = 0;
        int r = static_cast<int>(arr.size());
        int m;
        while (r > l) {
            m = l + (r - l) / 2;
            if (arr[m] >= key) {
                l = m + 1;
            } else {
                r = m;
            }
        }
        return l;
    }

    template<typename  T> void print_array(std::vector<T> &vec) {
        std::cout << "[ ";
        for (int i = 0; i < vec.size(); ++i) {
            std::cout << vec[i] << " ";
        }
        std::cout << "]" << std::endl;
    }

    void solve(std::vector<int> &arr, std::vector<int> &ans) {
        using namespace std;

        // D[i] --> max element arr[k] in arr[0..n), that the increasing subsequence with length i ends on a[k]
        vector<int> D(arr.size(), -1);
        vector<int> prev(arr.size(), -1);
        D[0] = arr[0];
        ans.push_back(0);
        int sz = 1;
        int firstLowIdx;
        for (int i = 1; i < arr.size(); ++i) {
//            print_array(D);
//            cout << "PREV = "; print_array(prev);
//            cout << "IND = "; print_array(ans);
            firstLowIdx = firstElementLessThan(D, arr[i]);
            D[firstLowIdx] = arr[i];

            if (firstLowIdx == sz) {
                ans.push_back(i);
                sz += 1;
            } else {
                ans[firstLowIdx] = i;
            }

            if (firstLowIdx > 0)
                prev[i] = ans[firstLowIdx - 1];
            else
                prev[i] = -1;
        }
//        print_array(D);
//        cout << "PREV = "; print_array(prev);
//        cout << "IND = "; print_array(ans);

        sz -= 1;
        while (sz != 0) {
            sz -= 1;
            ans[sz] = prev[ans[sz + 1]];
        }

    }
}

int main() {
    using namespace std;

    size_t n;
    cin >> n;
    vector<int> arr(n);
    for (int i = 0, x; i < (int) n; ++i) {
        cin >> x;
        arr[i] = x;
    }

    vector<int> ans;
    ans.reserve(n);
    solve(arr, ans);

    cout << ans.size() << endl;
    for (int i = 0; i < ans.size(); ++i) {
        cout << ans[i] + 1 << " ";
    }

    return 0;
}