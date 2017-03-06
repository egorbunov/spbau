#include <iostream>
#include <vector>
#include <climits>
#include <limits>

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
        edge_t(int next, int from, int to, int f, int c):
                next(next), from(from), to(to), flow(f), c(c)
        {}
    };

    vector<edge_t> edges;
    vector<int> head; // head[a] -- first edge to start dfs from a
    vector<int> is_used; // if is_used[i] == fl --> i already used
    int used_flag;

    int s; // sink
    int t; // target

    int cut_weight;
    vector<int> cut;
    vector<bool> is_reachable;

    network_t(int n, int m): head(n, -1), is_used(n, 0), used_flag(1), s(0), t(n - 1) {
    }

    void add_directed_edge(int a, int b, int capacity) {
        edges.push_back(edge_t(head[a], a, b, 0, capacity));
        head[a] = (int) edges.size() - 1;
    }

    /**
     * Add undirected edge
     */
    void add_edge(int a, int b, int capacity) {
        add_directed_edge(a, b, capacity);
        add_directed_edge(b, a, capacity);
    }

    int calc_max_flow() {
        int res = 0, f;
        while ((f = inc_flow(s, std::numeric_limits<int>::max())) > 0) {
            res += f;
            used_flag += 1;
        }
        return res;
    }

    int calc_min_cut() {
        cut_weight = 0;
        used_flag += 1;
        is_reachable = vector<bool>(edges.size(), false);
        mark_reachable_edges(s);
        used_flag += 1;
        calc_min_cut(s);
        return cut_weight;
    }
private:
    /**
     * Finds augmented path recursively and tries to increases flow by delta
     */
    int inc_flow(int v, int delta) {
        if (v == t) return delta;
        if (is_used[v] == used_flag) return 0;
        is_used[v] = used_flag;
        for (int i = head[v]; i != -1; i = edges[i].next) {
            int flow = 0;
            if (edges[i].flow < edges[i].c
                && (flow = inc_flow(edges[i].to, min(delta, edges[i].c - edges[i].flow))) > 0) {
                edges[i].flow += flow;
                edges[i ^ 1].flow -= flow;
                return flow;
            }
        }
        return 0;
    }

    void mark_reachable_edges(int v) {
        if (is_used[v] == used_flag) return;
        is_used[v] = used_flag;
        for (int i = head[v]; i != -1; i = edges[i].next) {
            if (edges[i].flow < edges[i].c) {
                is_reachable[i] = true;
                is_reachable[i ^ 1] = true;
                mark_reachable_edges(edges[i].to);
            }
        }
    }

    void calc_min_cut(int v) {
        if (is_used[v] == used_flag) return;
        is_used[v] = used_flag;
        for (int i = head[v]; i != -1; i = edges[i].next) {
            if (is_reachable[i]) {
                calc_min_cut(edges[i].to);
            } else {
                cut_weight += edges[i].c;
                cut.push_back(i);
            }
        }
    }
};

int main() {
    int n, m;

    cin >> n >> m;

    network_t net(n, m);

    for (int i = 0; i < m; ++i) {
        int a, b, c;
        cin >> a >> b >> c;
        net.add_edge(a - 1, b - 1, c);
    }

    net.calc_max_flow();
    int cut_w = net.calc_min_cut();

    cout << net.cut.size() << " " << cut_w << endl;
    for (int e : net.cut) {
        cout << e / 2 + 1 << " ";
    }
    cout << endl;

    return 0;
}

