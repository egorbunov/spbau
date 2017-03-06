#include <iostream>
#include <vector>
#include <limits>
#include <iomanip>
#include <cassert>
#include <random>
#include <string>

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

    network_t(int n, int s, int t): head(n, -1), is_used(n, 0), used_flag(1), s(s), t(t) {
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

int get_valence(char c) {
    switch (c) {
        case '.': return 0;
        case 'H': return 1;
        case 'O': return 2;
        case 'N': return 3;
        case 'C': return 4;
        default: assert(true);
    }
}

int main() {
    int rows, cols;
    cin >> rows >> cols;

    struct edge {
        int a, b;
    };

    vector<edge> edges;
    vector<int> vs;
    vector<vector<int>> ids(rows, vector<int>(cols, -1));

    int target_flow = 0;

    for (int i = 0; i < rows; ++i) {
        string str;
        cin >> str;
        for (int j = 0; j < str.length(); ++j) {
            int valence = get_valence(str[j]);
            if (valence == 0) {
                ids[i][j] = -1;
                continue;
            }

            target_flow += valence;
            int id = (int) vs.size();
            vs.push_back(valence);

            // vertical edge
            if (i > 0 && ids[i - 1][j] >= 0) {
                edges.push_back({ids[i - 1][j], id});
            }
            // horizontal edge
            if (j > 0 && ids[i][j -1] >= 0) {
                edges.push_back({ids[i][j - 1], id});
            }
            ids[i][j] = id;
        }
    }

    int n = (int) (2 * vs.size());
    int s = n++;
    int t = n++;

    network_t net(n, s, t);

    for (int v = 0; v < vs.size(); ++v) {
        net.add_d_edge(s, v, vs[v]);
        net.add_d_edge((int) (vs.size() + v), t, vs[v]);
    }

    // from sink to every edge node
    // and edges from edge to vertices
    for (int e = 0; e < edges.size(); ++e) {
        net.add_d_edge(edges[e].a, (int) (vs.size() + edges[e].b), 1);
        net.add_d_edge(edges[e].b, (int) (vs.size() + edges[e].a), 1);
    }

    int flow = net.calc_max_flow();

//    cout << flow << endl;

    if (flow == target_flow && target_flow > 0) {
        cout << "Valid" << endl;
    } else {
        cout << "Invalid" << endl;
    }

    return 0;
}

