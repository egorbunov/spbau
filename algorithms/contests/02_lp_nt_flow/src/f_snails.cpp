#include <iostream>
#include <vector>
#include <limits>
#include <iomanip>
#include <cassert>
#include <random>

using namespace std;

/**
 * Network, based on undirected graph
 */
struct network_t {
    struct edge_t {
        int next; // next edge in dfs
        int from;
        int to;
        int flow;    // flow
        int c;    // capacity
        int id;
        bool used; // for k paths searching
        edge_t(int next, int from, int to, int f, int c):
                next(next), from(from), to(to), flow(f), c(c), used(false)
        {}
    };

    vector<edge_t> edges;
    vector<int> head; // head[a] -- first edge to start dfs from a
    vector<int> is_used; // if is_used[i] == fl --> i already used
    int used_flag;

    int s; // sink
    int t; // target

    vector<vector<int>> paths;

    network_t(int n, int m, int s, int t): head(n, -1), is_used(n, 0), used_flag(1), s(s), t(t) {
    }

    void add_d_edge(int a, int b, int capacity) {
        edges.push_back(edge_t(head[a], a, b, 0, capacity));
        head[a] = (int) edges.size() - 1;
        edges.push_back(edge_t(head[b], b, a, 0, 0));
        head[b] = (int) edges.size() - 1;
    }

    int calc_max_flow() {
        int res = 0, f;
        while ((f = inc_flow(s, std::numeric_limits<int>::max())) > 0) {
            res += f;
            used_flag += 1;
        }
        return res;
    }

    int get_k_paths(int k) {
        int res = 0;
        int f;
        while ((f = inc_flow(s, std::numeric_limits<int>::max())) > 0) {
            res += f;
            used_flag += 1;
            if (res >= k) {
                break;
            }
        }

        if (res < k) {
            return 0;
        }

        paths = vector<vector<int>>(k, vector<int>(1, s));
        for (int i = 0; i < k; ++i) {
            used_flag += 1;
            get_path(s, paths[i]);
            assert(paths[i][paths[i].size() - 1] == t);
        }

        return 1;
    }

private:
    void get_path(int v, vector<int>& path) {
        if (v == t) return;

        for (int i = head[v]; i != -1; i = edges[i].next) {
            if (edges[i].flow > 0 && edges[i].c > 0 && !edges[i].used) {

                edges[i].used = true;
                path.push_back(edges[i].to);

                get_path(edges[i].to, path);
                return;
            }
        }
    }

    /**
     * Finds augmented path recursively and tries to increases flow by delta
     */
    int inc_flow(int v, int delta) {
        if (v == t) return delta;
        if (is_used[v] == used_flag) return 0;
        is_used[v] = used_flag;
        for (int e_ind = head[v]; e_ind != -1; e_ind = edges[e_ind].next) {
            int flow = 0;
            if (edges[e_ind].flow < edges[e_ind].c
                && (flow = inc_flow(edges[e_ind].to, min(delta, edges[e_ind].c - edges[e_ind].flow))) > 0) {
                edges[e_ind].flow += flow;
                edges[e_ind ^ 1].flow -= flow;
                return flow;
            }
        }
        return 0;
    }
};


int main() {
    int n, m, s, t;

    cin >> n >> m >> s >> t;

    network_t net(n, m, s - 1, t - 1);

    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        if (a == b) {
            continue; // loops doesn't matter
        }
        net.add_d_edge(a - 1, b - 1, 1);
    }

    int k = 2;
    int ans = net.get_k_paths(k);
    if (ans) {
        cout << "YES" << endl;
        for (int i = 0; i < k; ++i) {
            for (int v : net.paths[i]) {
                cout << v + 1 << " ";
            }
            cout << endl;
        }
    } else {
        cout << "NO" << endl;
    }

    return 0;
}

