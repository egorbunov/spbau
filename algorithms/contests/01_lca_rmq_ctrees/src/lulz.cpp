#include <iostream>
#include <vector>
#include <limits>
#include <algorithm>
#include <stack>
#include <cassert>
#include <array>

using std::vector;
using std::stack;

typedef int idx_t;

/**graph
 * Undirected graph
 */
struct Graph {
    struct Edge {
        idx_t from;
        idx_t to;
        bool isDeleted;
        size_t rev; // reverse edge index
        Edge(idx_t from, idx_t to, size_t rev): from(from), to(to), isDeleted(false), rev(rev) {}
    };

    vector<vector<Edge>> graph;
    vector<idx_t> labels;
    vector<bool> isVisited;
    vector<Edge*> bridges;
    vector<idx_t> components;
    vector<idx_t> minIdxInComponent;

    Graph(size_t vNum): graph(vNum), labels(vNum), isVisited(vNum, false), components(vNum, -1)
            , minIdxInComponent(vNum, std::numeric_limits<int>::max()) {}

    /**
     * from, to in [0, vNum)
     */
    void addEdge(idx_t from, idx_t to) {
        graph[from].push_back(Edge(from, to, graph[to].size()));
        graph[to].push_back(Edge(to, from, graph[from].size() - 1));
    }

    void deleteBridges() {
        dfsDeleteBridges(labels.size(), 0);
    }

    int getComponent(idx_t v) {
        return components[v];
    }

    void markComponents() {
        int mark = 0;
        for (idx_t v = 0; v < (int) graph.size(); ++v) {
            if (components[v] < 0) {
                dfsMarkComponents(v, mark);
                mark += 1;
            }
        }
    }


private:
    int dfsDeleteBridges(idx_t from, idx_t v) {
        if (from >= (int) labels.size()) {
            labels[v] = 0;
        } else {
            labels[v] = labels[from] + 1;
        }
        isVisited[v] = true;
        int minUp = std::numeric_limits<int>::max();
        for (Edge &e : graph[v]) {
            if (e.isDeleted)
                continue;
            if (isVisited[e.to]) {
                if (e.to != from) {
                    minUp = std::min(minUp, labels[e.to]);
                }
            } else {
                int minUpTo = dfsDeleteBridges(v, e.to);
                minUp = std::min(minUp, minUpTo);
                if (minUpTo > labels[v]) {
                    bridges.push_back(&e);
                    delEdge(e);
                }
            }
        }
        return minUp;
    }

    void delEdge(Edge& e) {
        e.isDeleted = true;
        graph[e.to][e.rev].isDeleted = true;
    }

    void dfsMarkComponents(idx_t v, int compMark) {
        if (components[v] >= 0)
            return;
        if (minIdxInComponent[compMark] > v) {
            minIdxInComponent[compMark] = v;
        }
        components[v] = compMark;
        for (Edge &e : graph[v]) {
            if (!e.isDeleted && components[e.to] < 0)
                dfsMarkComponents(e.to, compMark);
        }
    }
};


int main() {
    std::ios_base::sync_with_stdio(false);

    int n; // number of cities
    int m; // number of roads

    std::cin >> n >> m;

    Graph g(n);

    // reading graph
    for (int i = 0; i < m; ++i) {
        int a, b;
        std::cin >> a >> b;
        g.addEdge(a, b);
    }

    g.deleteBridges();
    g.markComponents();

    for (int i = 0; i < n; ++i) {
        std::cout << g.minIdxInComponent[g.components[i]] << std::endl;
    }


    return 0;
}
