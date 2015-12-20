#include <iostream>
#include <vector>

using namespace std;

namespace B {
    int solve() {
        int n, m;
        cin >> n >> m;

        int v = 1;
        int w = n - 1;
        int from = 1;
        int to = 2;
        int cnt = 0;
        for (int i = 0; i < m; ++i) {
            if (cnt == n - from) {
                from += 1;
                to = from + 1;
                w = n - from;
                cnt = 0;
            }
            cout << from << " " << to << " " << ((to == from + 1) ? 1 : w) << endl;
            w++;
            to++;
            cnt++;
        }

        return 0;
    }
}