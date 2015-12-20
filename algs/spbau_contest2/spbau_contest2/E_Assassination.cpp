#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

namespace E {
    const int MAX_N = 100000;
    const int MAX_M = 300000;

    struct Edge {
        int from;
        int to;
        bool is_rev;
        size_t rev;
        bool is_deleted;
    };

    vector<Edge> graph[MAX_N];
    bool is_cut_point[MAX_N];
    int cut_point_cnt;
    bool is_t_reachable_from[MAX_N];
    bool is_visited[MAX_N];
    int t_in[MAX_N];
    int f_up[MAX_N];
    int timer;
    int s;
    int t;
    int n;
    int m;

    bool visited[MAX_N];
    bool delete_dead_ends(int v, int from = -1) {
        if (v == t)
            return true;

        bool is_reachable_from_v = false;

        visited[v] = true;
        for (auto &e : graph[v])  {
            if (e.is_rev) continue;

            bool is_reachable;
            if (visited[e.to]) {
                is_reachable = is_t_reachable_from[e.to];
            }
            else {
                is_reachable = delete_dead_ends(e.to, v);;
            }
           
            if (is_reachable) {
                e.is_deleted = false;
                graph[e.to][e.rev].is_deleted = false;
                is_reachable_from_v = true;
            }

        }

        is_t_reachable_from[v] = is_reachable_from_v;
        return is_t_reachable_from[v];
    }


    void dfs(int v, int parent = -1) {
        is_visited[v] = true;
        t_in[v] = f_up[v] = timer++;

        for (auto &e : graph[v]) {
            int to = e.to;
            if (to == parent ||  e.is_deleted) continue;
            if (is_visited[to]) {
                f_up[v] = min(f_up[v], t_in[to]);
            }
            else {
                dfs(to, v);
                f_up[v] = min(f_up[v], f_up[to]);
                if (f_up[to] >= t_in[v] && parent != -1 && v != t) {
                    is_cut_point[v] = true;
                    cut_point_cnt += 1;
                }
            }
        }
    }

    void read() {
        cin >> n >> m >> s >> t;
        --s;
        --t;
        int from, to;
        for (int i = 0; i < m; ++i) {
            cin >> from >> to;
            --from;
            --to;
            graph[from].push_back({ from, to, false, graph[to].size(), true });
            graph[to].push_back({ to, from, true, graph[from].size() - 1, true });
        }

        for (int i = 0; i < n; ++i) {
            f_up[i] = numeric_limits<int>::max();
        }

        is_t_reachable_from[t] = true;
    }

    void solve() {
        read();
        delete_dead_ends(s);
        dfs(s);

        cout << cut_point_cnt << endl;
        for (int i = 0; i < n; ++i) {
            if (is_cut_point[i] && is_t_reachable_from[i]) {
                cout << i + 1 << " ";
            }
        }
    }
}

//int main() {
//    E::solve();
//    return 0;
//}