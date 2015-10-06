#include <iostream>
#include <vector>

namespace {
    using namespace std;

    /**
     * Input array must be sorted ascending!
     */
    int binarySearch(vector<int> &arr, int key) {
        // every iteration performs search in [l, r) segment
        int l = 0;
        int r = static_cast<int>(arr.size());
        int m;
        while (r > l) {
            m = l + (r - l) / 2;

            if (arr[m] == key) {
                return m;
            }
            if (arr[m] < key) {
                l = m + 1;
            } else {
                r = m;
            }
        }
        return -2;
    }

    void solve(vector<int> &array, vector<int> keys) {
        for (size_t i = 0; i < keys.size(); ++i) {
            cout << binarySearch(array, keys[i]) + 1 << " ";
        }
    }

}

int main() {
    int tmp;

    int n;
    cin >> n;
    vector<int> array;
    while (n-- > 0) {
        cin >> tmp;
        array.push_back(tmp);
    }

    int k;
    cin >> k;
    vector<int> keys;
    while (k-- > 0) {
        cin >> tmp;
        keys.push_back(tmp);
    }

    solve(array, keys);

    return 0;
}

