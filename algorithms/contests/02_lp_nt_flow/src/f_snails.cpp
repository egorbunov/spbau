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

    network_t(int n, int m): head(n, -1), is_used(n, 0), used_flag(1), s(0), t(n - 1) {
    }

    /**
     * Add undirected edge
     */
    void add_edge(int a, int b, int capacity) {
        edges.push_back(edge_t(head[a], a, b, 0, capacity));
        head[a] = (int) edges.size() - 1;
        edges.push_back(edge_t(head[b], b, a, 0, capacity));
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

private:
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
    int n, m;

    cin >> n >> m;

    network_t net(n, m);

    for (int i = 0; i < m; ++i) {
        int a, b, c;
        cin >> a >> b >> c;
        net.add_edge(a - 1, b - 1, c);
    }

    int flow = net.calc_max_flow();

    cout << flow << endl;
    for (int i = 0; i < m; ++i) {
        cout << net.edges[2 * i].flow << endl;
    }

    return 0;
}

