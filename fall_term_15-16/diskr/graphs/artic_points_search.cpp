#include <iostream>
#include <vector>

namespace {
	struct Vertex {
		int tin;	// time, when started processing
		int tout;   // time, when end processing
		int id;
		Vertex(int id) : id(id) {}
	};

	struct Graph {
		std::vector<Vertex> vertices;
		std::vector<bool> isUsed;
		std::vector<std::vector<Vertex*> > graph;
		std::vector<int> articulationPoints;
		int _time = 0;


		int addVertex() {
			vertices.push_back(Vertex(vertices.size()));
			graph.push_back(std::vector());
			isUsed.push_back(false);
			return vertices.size();
		}

		void addEdge(int id1, int id2) {
			if (vertices.size() >= id1 || vertices.size() >= id2)
				return; // ERROR
			// TODO: check if exists
			graph[id1].push_back(&vertices[id2]);
			graph[id2].push_back(&vertices[id1]);
		}

		int vNum() {
			return vertices.size();
		}

		int dfs(int id) {
			// min start processing time, which we met during dfs from given vertex
			_time += 1;
			int minRevTime = _time; 
			vertices[id].tin = _time;
			;

			isUsed[id] = true;
			for (int i = 0; i < graph[id].size(); ++i) {
				if (isUsed[graph[id][i]]) {
					if (graph[id][i]->tin < minRevTime)
						minRevTime = graph[id][i]->tin;
				} else {
					int tmp = dfs(graph[id][i]->id)
					if (tmp < minRevTime)
						minRevTime = tmp;
				}
			}

			if (minRevTime >= vertices[id].tin)
				articulationPoints.push_back(id);

			vertices[id].tout = ++_time;

			return minRevTime;
		}

	};

}

int main() {
	using namespace std;

	Graph graph;

	int v, u;
	while (cin >> v >> u) {
		while (graph.vNum() < v - 1 || graph.vNum() < u - 1)
			graph.addVertex();
		graph.addEdge(v, u);
	}

	graph.dfs(0);

	for (int i = 0; i < graph.articulationPoints.size(); ++i) {
		cout << graph.articulationPoints[i] << " ";
	}
}