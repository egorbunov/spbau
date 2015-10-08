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
		std::vector<std::vector<Vertex*> > graph;
		std::vector<size_t> articulationPoints;
		int _time = 0;


		size_t addVertex() {
			vertices.push_back(new Vertex(vertices.size()));
			graph.push_back(std::vector<Vertex*>());
			isUsed.push_back(false);
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

		int dfs(size_t id, size_t prevId) {
			// min start processing time, which we met during dfs from given vertex
			_time += 1;
			int minRevTime = _time; 
			vertices[id]->tin = _time;

			isUsed[id] = true;
			for (int i = 0; i < graph[id].size(); ++i) {
				if (!isUsed[graph[id][i]->id]) {
					int tmp = dfs(graph[id][i]->id, id);
					if (tmp < minRevTime)
						minRevTime = tmp;
				}
			}

			if (minRevTime >= vertices[id]->tin && graph[id].size() != 1)
				articulationPoints.push_back(id);

			vertices[id]->tout = ++_time;

			return minRevTime;
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
	int tmp = 0;
	cin >> tmp;
	while (cin >> v >> u) {
		tmp -= 1;
		while (graph.vNum() <= v || graph.vNum() <= u)
			graph.addVertex();
		graph.addEdge(v, u);

		if (tmp == 0)
			break;
	}

	graph.dfs(0, graph.vNum() + 1);

	for (int i = 0; i < graph.articulationPoints.size(); ++i) {
		cout << graph.articulationPoints[i] << " ";
	}

	return 0;
}