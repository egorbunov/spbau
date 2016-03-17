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

struct Tree {
	static const int NO_PARENT;
	static const std::array<int, 17> DEGREES_OF_TWO;
	size_t vNum;
	vector<int> parents;
	vector<int> depths;
    idx_t root;
	vector<int[DEGREES_OF_TWO.size()]> jumps;

	Tree(size_t vNum): vNum(vNum), parents(vNum, NO_PARENT), depths(vNum, 0), root(-1), jumps(vNum) {}
	
	/**
	 * p, x in [0, vNum)
	 *
	 * 1) p is treated is root if no nodes in tree
	 * 2) if tree is not empty, p must have parent in tree
	 */
	void hang(idx_t p, idx_t x) {
		if (root == -1) {
			root = p;
			parents[root] = -1;
            jumps[root][0] = -1;
            depths[0] = 0;
		}
		parents[x] = p;
		jumps[x][0] = p; // jum to 2^0
        depths[x] = depths[p] + 1;
	}

	int parent(idx_t x) {
		return parents[x];
	}

	int dep(idx_t x) {
		return depths[x];
	}

	void preprocess() {
		for (size_t i = 1; i < DEGREES_OF_TWO.size(); ++i) {
			for (size_t v = 0; v < vNum; ++v) {
				if (jumps[v][i - 1] == -1) {
					jumps[v][i] = -1;
				} else {
					jumps[v][i] = jumps[jumps[v][i - 1]][i - 1];
				}
			}
		}
	}

	// level of {b} ancestor of {a}, level(a) > level(b)
    idx_t LA(idx_t a, idx_t b) {
        idx_t cur = a;
		int deg = (int) DEGREES_OF_TWO.size() - 1;
		while (depths[cur] != depths[b]) {
			auto jmp = jumps[cur][deg];
			if (jmp < 0 || depths[jmp] < depths[b]) {
				deg--;
			} else {
				cur = jmp;
			}
		}
		return cur;
	}

    idx_t LCA(idx_t a, idx_t b) {
		if (depths[a] > depths[b]) {
            a = LA(a, b);
        } else {
            b = LA(b, a);
        }

        int deg = (int) DEGREES_OF_TWO.size() - 1;
        while (a != b) {
        	if (jumps[a][deg] == jumps[b][deg]) {
        		deg--;
        		if (deg < 0)
        			return jumps[a][0];
        	} else {
        		a = jumps[a][deg];
        		b = jumps[b][deg];
        	}

        }
        return a;
	}
};
const idx_t Tree::NO_PARENT = -1;
const std::array<int, 17>  Tree::DEGREES_OF_TWO = { 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536 };

/**
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

    Graph(size_t vNum): graph(vNum), labels(vNum), isVisited(vNum, false), components(vNum, -1) {}
    
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

    /**
     * use after deleteBridges()
     */
	Tree buildBridgeComponentTree(idx_t root) {
        size_t componentNum = bridges.size() + 1; // V(T) = E(T) + 1
		Tree t = Tree(componentNum);
        dfsBuildComponentTree(root, 0, t);
		return t;
	}

	int getComponent(idx_t v) {
		return components[v];
	}


private:
    void dfsBuildComponentTree(idx_t v, int mark, Tree &tree) {
        static int nextMark = mark;
        components[v] = mark;
        for (Edge &e : graph[v]) {
            if (components[e.to] >= 0)
                continue;
            if (e.isDeleted) {
                nextMark += 1;
                tree.hang(components[v], nextMark);
                dfsBuildComponentTree(e.to, nextMark, tree);
                continue;
            }
            dfsBuildComponentTree(e.to, mark, tree);
        }
    }

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
};


int main() {
	std::ios_base::sync_with_stdio(false);

	int n; // number of cities
	int m; // number of roads
	int root; // conference city

	std::cin >> n >> m >> root;
    root--; // all in [0, ...) !


	Graph g(n);

	// reading graph
	for (int i = 0; i < m; ++i) {
		int a, b;
		std::cin >> a >> b;
		g.addEdge(a - 1, b - 1);
	}

	// prepare graph and build 2-connect component tree
	g.deleteBridges();
	Tree t = g.buildBridgeComponentTree(root);
	t.preprocess();

	int queryNum;
	std::cin >> queryNum;

	for (int i = 0; i < queryNum; ++i) {
		// answering query
		int a, b;
		std::cin >> a >> b;
		idx_t lca = t.LCA(g.getComponent(a - 1), g.getComponent(b - 1));
		std::cout << t.dep(lca) << std::endl;
	}

	return 0;
}