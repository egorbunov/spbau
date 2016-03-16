#include <iostream>
#include <vector>
#include <limits>
#include <algorithm>
#include <cassert>
#include <array>

using std::vector;

struct Tree {
	static const int NO_PARENT;
	static const std::array<int, 17> DEGREES_OF_TWO;
	size_t vNum;
	vector<int> parents;
	vector<int> depths;
	int root;
	vector<int[10]> jumps;

	Tree(size_t vNum): vNum(vNum), parents(vNum, NO_PARENT), depths(vNum, 0), root(-1) {}
	
	/**
	 * p, x in [0, vNum)
	 */
	void hang(size_t p, size_t x) {
		if (root == -1) {
			root = p;
			parents[root] = -1;
			depths[root] = 0;
		}
		parents[x] = p;
		jumps[x][0] = p; // jum to 2^0
		depths[x] = depths[p] + 1;
	}

	int parent(size_t x) {
		return parents[x];
	}

	int dep(size_t x) {
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
	size_t LA(size_t a, size_t b) {
		size_t cur = a;
		int deg = DEGREES_OF_TWO.size() - 1;
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

	size_t LCA(size_t a, size_t b) {
		if (depths[a] > depths[b]) {
            a = LA(a, b);
        } else {
            b = LA(b, a);
        }

        int deg = DEGREES_OF_TWO.size() - 1;
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
const int Tree::NO_PARENT = -1;
const std::array<int, 17>  Tree::DEGREES_OF_TWO = { 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536 };

/**
 * Undirected graph
 */
struct Graph {
	struct Edge {
		size_t from;
		size_t to;
		bool isDeleted;
		size_t rev; // reverse edge index
		Edge(int from, int to, size_t rev): from(from), to(to), isDeleted(false), rev(rev) {}
	};

	vector<vector<Edge>> graph;
	vector<size_t> labels;
	vector<bool> isVisited;
	vector<Edge*> bridges;
	vector<int> components;
	size_t componentNum;
    
    Graph(size_t vNum): graph(vNum), labels(vNum), isVisited(vNum, false), components(vNum, -1), componentNum(1) {}
    
	/**
	 * from, to in [0, vNum)
	 */
	void addEdge(size_t from, size_t to) {
		graph[from].push_back(Edge(from, to, graph[to].size()));
		graph[to].push_back(Edge(to, from, graph[from].size() - 1));
	}

	void deleteBridges() {
		dfsDeleteBridges(-1, 0);
	}

	void markComponents() {
		int mark = 0;
		for (size_t v = 0; v < graph.size(); ++v) {
			if (components[v] < 0) {
				dfsMarkComponents(v, mark++);
			}
		}
		componentNum = mark;
	}

	Tree buildBridgeComponentTree(int root) {
		Tree t = Tree(componentNum);
		int skip = -1;
		for (size_t i = 0; i < bridges.size(); ++i) {
			if (bridges[i]->to == (size_t) root) {
				t.hang(components[bridges[i]->to], components[bridges[i]->from]); // makes root component root in tree
				skip = i;
				break;
			}
			if (bridges[i]->from == (size_t) root) {
				t.hang(components[bridges[i]->from], components[bridges[i]->to]); 
				skip = i;
				break;
			}
		}
		for (size_t i = 0; i < bridges.size(); ++i) {
			if (i == (size_t) skip)
				continue;
			Edge* e = bridges[i];
			if (t.parent(components[e->to]) == Tree::NO_PARENT) {
				t.hang(components[e->from], components[e->to]);
			} else if (t.parent(components[e->from]) == Tree::NO_PARENT) {
				t.hang(components[e->to], components[e->from]);
			} else {
				assert(true);
			}
		}
		return t;
	}

	size_t getComponent(size_t v) {
		return components[v];
	}


private:
	void dfsMarkComponents(size_t v, int mark) {
		components[v] = mark;
		for (Edge &e : graph[v]) {
			if (components[e.to] >= 0 || e.isDeleted) 
				continue;
			dfsMarkComponents(e.to, mark);
		}
	}

	size_t dfsDeleteBridges(size_t from, size_t v) {
		labels[v] = labels[from] + 1;
		isVisited[v] = true;
		size_t minUp = std::numeric_limits<size_t>::max();
		for (Edge &e : graph[v]) {
			if (e.isDeleted)
				continue;
			if (isVisited[e.to] && e.to != from) {
				minUp = std::min(minUp, labels[e.to]);
				continue;
			}
			size_t minUpTo = dfsDeleteBridges(v, e.to);
			minUp = std::min(minUp, minUpTo);
			if (minUpTo > labels[v]) {
				bridges.push_back(&e);
				delEdge(e);
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
	int n = 10; // number of cities
	int root = 1; // conference city
	int a = 0, b = 1; // magicians

	Graph g(n);

	// fill graph here
	

	// prepare graph
	g.deleteBridges();
	g.markComponents();
	Tree t = g.buildBridgeComponentTree(root);

	t.preprocess();

	// answering query
	size_t lca = t.LCA(g.getComponent(a), g.getComponent(b));
	std::cout << t.dep(lca) << std::endl;

	return 0;
}