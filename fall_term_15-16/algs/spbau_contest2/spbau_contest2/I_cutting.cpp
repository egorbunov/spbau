#include <iostream>
#include <algorithm>
#include <vector>
#include <string>

namespace I {
    struct UnionFind {
        std::vector<int> rank;
        std::vector<int> parent;
       
        UnionFind(int n) {
            rank.resize(n);
            parent.resize(n);
            for (int i = 0; i < n; ++i) {
                make_set(i);
            }
        }

        int find(int v) {
            if (v == parent[v])
                return v;
            parent[v] = find(parent[v]);
            return parent[v];
        }

        void merge(int a, int b) {
            a = find(a);
            b = find(b);
            if (a != b) {
                if (rank[a] < rank[b])
                    std::swap(a, b);
                parent[b] = a;
                if (rank[a] == rank[b])
                    ++rank[a];
            }
        }

        bool are_connected(int a, int b) {
            return find(a) == find(b);
        }

    private:
        void make_set(int x) {
            parent[x] = x;
            rank[x] = 0;
        }
    };


    struct query_t {
        enum TYPE {
            ASK,
            CUT
        };

        TYPE t;
        int a;
        int b;

        friend std::istream& operator>>(std::istream& in, query_t& q) {
            std::string str;
            in >> str;
            if (str == "ask")
                q.t = ASK;
            else
                q.t = CUT;
            return in >> q.a >> q.b;
        }
    };

    std::vector<query_t> queries;
    int n, m;

    void read() {
        int q_num;
        std::cin >> n >> m >> q_num;
        queries.resize(q_num);
        int tmp;
        for (int i = 0; i < m; ++i) {
            std::cin >> tmp >> tmp;
        }
        for (int i = 0; i < q_num; ++i) {
            std::cin >> queries[q_num - i - 1];
        }
    }

    void solve() {
        read();

        UnionFind uf(n);

        std::vector<bool> answers;
        answers.reserve(queries.size());
        for (auto &q : queries) {
            if (q.t == query_t::ASK) {
                answers.push_back(uf.are_connected(q.a - 1, q.b - 1));
            }
            else {
                uf.merge(q.a - 1, q.b - 1);
            }
        }

        std::vector<bool>::reverse_iterator rit = answers.rbegin();
        for (; rit != answers.rend(); ++rit) {
            if (*rit) {
                std::cout << "YES";
            }
            else {
                std::cout << "NO";
            }
            std::cout << std::endl;
        }
    }
}

//int main() {
//    I::solve();
//    return 0;
//}