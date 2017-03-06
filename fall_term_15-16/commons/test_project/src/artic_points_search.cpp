#include <iostream>
#include <vector>

namespace {
	struct Vertex {
		int tin;	// time, when started processing
		int tout;   // time, when end processing
		size_t id;
		Vertex(size_t id) : id(id), tin(-1), tout(-1) {}
	};

	struct Graph {
		std::vector<Vertex*> vertices;
		std::vector<bool> isUsed;
		std::vector<int> minRevTimes;
		std::vector<std::vector<Vertex*> > graph;
        std::vector<bool> isArticPoint;
		int _time = 0;


		size_t addVertex() {
			vertices.push_back(new Vertex(vertices.size()));
			graph.push_back(std::vector<Vertex*>());
			isUsed.push_back(false);
            isArticPoint.push_back(false);
			minRevTimes.push_back(0);

			return vertices.size();
		}

		void addEdge(size_t id1, size_t id2) {
			if (vNum() <= id1 || vNum() <= id2)
				return; // ERROR
			// TODO: check if exists

			graph[id1].push_back(vertices.at(id2));
			graph[id2].push_back(vertices.at(id1));
		}

		size_t vNum() {
			return vertices.size();
		}

		void dfs(size_t id, size_t prevId) {
			// min start processing time, which we met during dfs from given vertex
			_time += 1;
			minRevTimes[id] = _time;
			vertices[id]->tin = _time;

			isUsed[id] = true;
			int children = -1;
			for (int i = 0; i < graph[id].size(); ++i) {
				size_t to = graph[id][i]->id;
				if (to == prevId) continue;
				if (isUsed[to]) {
					minRevTimes[id] = std::min(minRevTimes[id], vertices[to]->tin);
				} else {
					dfs(graph[id][i]->id, id);
					minRevTimes[id] = std::min(minRevTimes[id], minRevTimes[to]);
					if (minRevTimes[to] >=  vertices[id]->tin && prevId != vNum() + 1)
                        isArticPoint[id] = true;
					++children;
				}
			}

			if (prevId == vNum() + 1 && children > 1)
                isArticPoint[id] = true;

			vertices[id]->tout = ++_time;
		}

        ~Graph() {
            for (int i = 0; i < vertices.size(); ++i) {
                delete vertices[i];
            }
        }

	};

}

int main() {
	using namespace std;

	Graph graph = Graph();

	size_t v, u;
	while (cin >> v >> u) {
		while (graph.vNum() <= v || graph.vNum() <= u)
			graph.addVertex();
		graph.addEdge(v, u);
	}

	graph.dfs(0, graph.vNum() + 1);

	for (int i = 0; i < graph.vNum(); ++i) {
        if (graph.isArticPoint[i]) {
            cout << i << " ";
        }
	}

	return 0;
}