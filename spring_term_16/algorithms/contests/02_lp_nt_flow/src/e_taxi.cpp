#include <iostream>
#include <vector>
#include <cassert>
#include <ostream>
#include <tuple>
#include <cmath>

using namespace std;

struct graph {
    graph(int n, int m): from_a_to_b(n, vector<int>()), visited(n, false), max_match(m, -1) {
    }

    void add_a_to_b(int a, int b) {
        assert(a < from_a_to_b.size());

        from_a_to_b[a].push_back(b);
    }

    tuple<size_t, const vector<int>&> get_max_match() {
        size_t sz = 0;
        for (int i = 0; i < from_a_to_b.size(); ++i) {
            std::fill(visited.begin(), visited.end(), false);
            if (max_match_dfs(i)) {
                sz += 1;
            }
        }
        return tie(sz, max_match);
    }

private:
    vector<vector<int>> from_a_to_b;
    vector<bool> visited;
    vector<int> max_match;

    /**
     * v in A part
     */
    bool max_match_dfs(int v) {
        if (visited[v]) return false;
        visited[v] = true;
        for (int u : from_a_to_b[v]) {
            if (max_match[u] == -1 || max_match_dfs(max_match[u])) {
                max_match[u] = v;
                return true;
            }
        }
        return false;
    }

};

struct pos_t {
    int x, y;
    friend istream& operator>>(istream& in, pos_t& pos) {
        return in >> pos.x >> pos.y;
    }
};

int dist(pos_t& a, pos_t& b) {
    return (abs(b.x - a.x) + abs(b.y - a.y));
}



struct order_t {
    int time;
    pos_t from;
    pos_t to;
    order_t(int t, pos_t from, pos_t to): time(t), from(from), to(to) {
    }
};

int main() {
    int order_num;
    cin >> order_num;

    vector<order_t> orders;

    for (int i = 0; i < order_num; ++i) {
        int h, m;
        char skip;
        cin >> h >> skip >> m;
        pos_t from, to;
        cin >> from >> to;
        orders.push_back(order_t(60 * h + m, from, to));
    }

    graph g = graph(orders.size(), orders.size());

    for (size_t a = 0; a < orders.size(); ++a) {
        for (size_t b = 0; b < orders.size(); ++b) {
            if (orders[a].time
                + dist(orders[a].from, orders[a].to)
                + dist(orders[a].to, orders[b].from) < orders[b].time) {

                g.add_a_to_b(a, b);
            }
        }
    }

    auto t = g.get_max_match();

    cout << orders.size() - get<0>(t) << endl;
}