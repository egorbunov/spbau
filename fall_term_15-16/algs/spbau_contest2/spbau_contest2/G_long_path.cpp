#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

namespace G {
    const int MAX_N = 10000;
    vector<int> graph[MAX_N];
    int D[MAX_N]; // max path starting at
    int n, m;

    void read() {
        cin >> n >> m;
        int from, to;
        for (int i = 0; i < m; ++i) {
            cin >> from >> to;
            from -= 1;
            to -= 1;
            graph[from].push_back(to);
        }
    }

    bool used[MAX_N];

    void dfs(int v) {
        used[v] = true;
        int max_path_len = D[v];
        for (auto &to : graph[v]) {
            if (!used[to]) dfs(to);
            max_path_len = max(max_path_len, D[to] + 1);
        }
        D[v] = max_path_len;
    }

    void solve() {
        read();

        for (int i = 0; i < n; ++i) {
            if (!used[i])
                dfs(i);
        }

        int max = 0;
        for (int i = 0; i < n; ++i) {
            if (D[i] > max) {
                max = D[i];
            }
        }
        cout << max << endl;
    }
}


//int main() {
//    G::solve();
//    return 0;
//}