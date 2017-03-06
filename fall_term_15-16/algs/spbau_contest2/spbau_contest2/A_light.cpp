#include <iostream>
#include <vector>

using namespace std;

namespace A {
    struct Edge {
        int from;
        int to;
        long long w;

        Edge() : from(-1), to(-1), w(0) {}
        Edge(int from, int to) : from(from), to(to), w(0) {}
        Edge(int from, int to, int w) : from(from), to(to), w(w) {}
    };

    struct Vertex {
        int id;
        int deg;

        Vertex() : id(-1) {}
        Vertex(int id) : id(id), deg(0) {}
    };

    struct Graph {
        vector<Vertex> V;
        vector<Edge> E;

        Graph(int v, int e) {
            V.resize(v);
            for (int i = 0; i < v; ++i) {
                V[i] = Vertex(i);
            }
            E.resize(e);
        }

        void addEdge(Edge e) {
            static int i = 0;
            if (i >= E.size()) throw std::runtime_error("ERROR: too much edges added!");
            E[i++] = e;
            V[e.from - 1].deg += 1;
            V[e.to - 1].deg += 1;
        }

    };

    int solve() {

        int n, m;
        cin >> n >> m;
        Graph g(n, m);
        for (int i = 0; i < m; ++i) {
            int from, to;
            cin >> from >> to;
            g.addEdge(std::move(Edge(from, to)));
        }

        for (auto &v : g.V) {
            cout << v.deg << " ";
        }

        return 0;
    }
}