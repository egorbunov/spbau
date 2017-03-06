#include <iostream>
#include <vector>
#include <algorithm>

namespace J {
    const int MAX_N = 100;
    const long long INF = 10000000; // > 100 * 1000
    long long graph[MAX_N][MAX_N];
    int n;

    void read() {
        using namespace std;
        cin >> n;
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                cin >> graph[i][j];
                if (graph[i][j] == -1) {
                    graph[i][j] = INF;
                }
            }
        }
    }

    void solve() {
        using namespace std;
        read();

        // run Floyd–Warshall algorithm
        for (int k = 0; k < n; ++k) {
            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < n; ++j) {
                    if (i == j) continue;
                    graph[i][j] = std::min(graph[i][j], graph[i][k] + graph[k][j]);
                }
            }
        }

        long long diametr = 0;
        long long radius = INF;
        for (int i = 0; i < n; ++i) {
            long long e = 0;
            for (int j = 0; j < n; ++j) {
                if (i == j) continue;
                e = std::max(e, graph[i][j]);
            }
            diametr = std::max(diametr, e);
            radius = std::min(radius, e);
        }

        cout << diametr << endl;
        cout << radius << endl;
    }
}

int main() {
    J::solve();
    return 0;
}
