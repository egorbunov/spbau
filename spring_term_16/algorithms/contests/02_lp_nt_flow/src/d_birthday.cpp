#include <iostream>
#include <vector>
#include <tuple>
#include <utility>

using namespace std;

struct graph {
    graph(int n, int m): n(n), m(m), from_a_to_b(n, vector<int>()), visited(n, false), max_match(m, -1) {
    }

    void add_a_to_b(int a, int b) {
        from_a_to_b[a].push_back(b);
    }

    tuple<size_t, const vector<int>&> get_max_match() {
        std::fill(max_match.begin(), max_match.end(), -1);
        size_t sz = 0;
        for (int i = 0; i < from_a_to_b.size(); ++i) {
            std::fill(visited.begin(), visited.end(), false);
            if (max_match_dfs(i)) {
                sz += 1;
            }
        }
        return tie(sz, max_match);
    }

    const pair<vector<bool>, vector<bool>>& get_min_cover() {
        get_max_match();

        vector<bool> is_cov_by_match(n, false);
        for (int i = 0; i < max_match.size(); ++i) {
            if (max_match[i] != -1) {
                is_cov_by_match[max_match[i]] = true;
            }
        }

        reachable.first = vector<bool>(n, false);
        reachable.second = vector<bool>(m, false);
        for (int i = 0; i < is_cov_by_match.size(); ++i) {
            if (is_cov_by_match[i] == false) {
                min_cover_dfs(i);
            }
        }

        min_cover.first = vector<bool>(n, false);
        min_cover.second = vector<bool>(m, false);
        for (int i = 0; i < n; ++i) {
            min_cover.first[i] = !reachable.first[i];
        }
        for (int i = 0; i < m; ++i) {
            min_cover.second[i] = reachable.second[i];
        }

        return min_cover;
    }

    const pair<vector<bool>, vector<bool>>& get_max_ind_set() {
        get_min_cover();

        max_ind_set.first = vector<bool>(n, false);
        max_ind_set.second = vector<bool>(m, false);
        for (int i = 0; i < n; ++i) {
            max_ind_set.first[i] = !min_cover.first[i];
        }
        for (int i = 0; i < m; ++i) {
            max_ind_set.second[i] = !min_cover.second[i];
        }
        return max_ind_set;
    }

private:
    int n;
    int m;
    vector<vector<int>> from_a_to_b;
    vector<bool> visited;
    vector<int> max_match;
    pair<vector<bool>, vector<bool>> min_cover;
    pair<vector<bool>, vector<bool>> max_ind_set;
    pair<vector<bool>, vector<bool>> reachable;

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

    /**
     * v in A part
     */
    void min_cover_dfs(int v) {
        if (reachable.first[v]) return;
        reachable.first[v] = true;
        for (int u : from_a_to_b[v]) {
            if (max_match[u] != -1 && max_match[u] != v) {
                reachable.second[u] = true;
                min_cover_dfs(max_match[u]);
            }
        }
    }
};

int main() {
    int query_num;
    cin >> query_num;
    for (int q = 0; q < query_num; ++q) {

        int n, m;
        cin >> n >> m;
        graph g(n, m);

        for (int i = 0; i < n; ++i) {
            int b;
            vector<bool> to(m, false);
            for (;;) {
                cin >> b;
                if (b == 0) break;
                to[b - 1] = true;
            }
            for (int j = 0; j < m; ++j) {
                if (to[j] == false) {
                    g.add_a_to_b(i, j);
                }
            }
        }

        auto t = g.get_max_ind_set();
        size_t cntA = 0;
        size_t cntB = 0;
        for (bool x : t.first) {
            if (x) cntA += 1;
        }
        for (bool x : t.second) {
            if (x) cntB += 1;
        }

        cout << cntA + cntB << endl;
        cout << cntA << " " << cntB << endl;
        for (int i = 0; i < t.first.size(); ++i) {
            if (t.first[i]) {
                cout << i + 1 << " ";
            }
        }
        cout << endl;
        for (int i = 0; i < t.second.size(); ++i) {
            if (t.second[i]) {
                cout << i + 1 << " ";
            }
        }
        cout << endl;
        cout << endl;
    }
}
