#include <iostream>
#include <vector>
#include <cassert>
#include <tuple>

using namespace std;

struct graph {
    graph(int n, int m): from_a_to_b(n, vector<int>()), visited(n, false), max_match(m, -1) {
    }

    void add_a_to_b(int a, int b) {
        assert(a < from_a_to_b.size());

        from_a_to_b[a].push_back(b);
    }

    tuple<size_t, const vector<int>&> get_max_match() {
        size_t sz = 0;
        for (int i = 0; i < from_a_to_b.size(); ++i) {
            std::fill(visited.begin(), visited.end(), false);
            if (max_match_dfs(i)) {
                sz += 1;
            }
        }
        return tie(sz, max_match);
    }

private:
    vector<vector<int>> from_a_to_b;
    vector<bool> visited;
    vector<int> max_match;

    /**
     * v in A part
     */
    bool max_match_dfs(int v) {
        if (visited[v]) return false;
        visited[v] = true;
        for (int u : from_a_to_b[v]) {
            if (max_match[u] == -1 || max_match_dfs(max_match[u])) {
                max_match[u] = v;
                return true;
            }
        }
        return false;
    }

};

int main() {
    int n, m;

    cin >> n >> m;

    graph g(n, m);

    for (int i = 0; i < n; ++i) {
        int b;
        for (;;) {
            cin >> b;
            if (b == 0) break;
            g.add_a_to_b(i, b - 1);
        }
    }

    auto t = g.get_max_match();
    cout << get<0>(t) << endl;
    auto match = get<1>(t);
    for (int i = 0; i < match.size(); ++i) {
        if (match[i] != -1) {
            cout << match[i] + 1 << " " << i + 1 << endl;
        }
    }
}