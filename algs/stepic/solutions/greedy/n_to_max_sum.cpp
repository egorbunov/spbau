#include <iostream>
#include <cmath>

using namespace std;

namespace {
  void solve() {
    int n;
    cin >> n;

    int k = floor((sqrt(8.0 * n + 1) - 3) / 2.0);
    cout << k + 1 << endl;
    for (int i = 1; i <= k; ++i) {
      cout << i << " ";
    }
    cout << n - (k * (k + 1)) / 2;
  }
}