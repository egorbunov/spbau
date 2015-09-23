#include <iostream>
#include <vector>
#include <stack>

using namespace std;

struct Edge {
    int a;
    int b;
    bool isDeleted;
    int rev;
};

void remove(Edge &e, vector<vector<Edge> > &graph) {
    e.isDeleted = true;
    graph[e.b][e.rev].isDeleted = true;
}

void euler(vector<vector<Edge>> &edges) {
    stack<int> st;
    st.push(0);
    while (!st.empty()) {
        int v = st.top();

        bool doStep = true;
        for (int i = 0; i < edges[v].size(); ++i) {
            if (!edges[v][i].isDeleted) {
                doStep = false;
                st.push(edges[v][i].b);
                // delete edge
                remove(edges[v][i], edges);
                break;
            }
        }

        if (doStep) {
            st.pop();
            cout << v + 1 << " ";
        }
    }
}

//int dfs(int v, vector<vector<Edge> >& edges, vector<int>& cycle) {
//    for (int i = 0; i < edges[v].size(); ++i) {
//        if (!edges[v][i].isDeleted) {
//            remove(edges[v][i], edges);
//            dfs(edges[v][i].b, edges, cycle);
//        }
//    }
//    cycle.push_back(v+1);
//}

int main() {
    int v, e;
    cin >> v >> e;

    vector<vector<Edge> > edges;
    edges.resize((size_t) v);

    int a, b;
    Edge edge, redge;
    for (int i = 0; i < e; ++i) {

        cin >> a >> b;
        --a; --b;
        edge = {a, b, false, (int) edges[b].size()};
        redge = {b, a, false, (int) edges[a].size()};

        edges[a].push_back(edge);
        edges[b].push_back(redge);
    }

    for (int i = 0; i < v; ++i) {
        if (edges[i].size() % 2 == 1 || edges[i].size() == 0) {
            cout << "NONE" << endl;
            return 0;
        }
    }

    euler(edges);
//    vector<int> cycle;
//    dfs(0, edges, cycle);
//    for (int i = 0; i < cycle.size() - 1; ++i) {
//        cout << cycle[i] << " ";
//    }
    return 0;
}