#include <iostream>
#include <string>
#include <queue>

namespace D {
    const int MAX_COL = 502;
    const int MAX_ROW = 502;
    const int INF = std::numeric_limits<int>::max();

    enum CELL_TYPE {
        WALL,
        CLIN, // clean :D 
        DIRT
    };

    const int NUM_DIRS = 8;
    const int DIR_VEC[NUM_DIRS][2] = { {-1, 0}, {1, 0}, {0, 1}, {0, -1}, {-1, 1}, {-1, -1}, {1, -1}, {1, 1} };


    struct Node {
        int r;
        int c;
    };

    bool operator==(const Node& lhs, const Node& rhs) {
        return lhs.r == rhs.r && lhs.c == rhs.c;
    }

    bool operator!=(const Node& lhs, const Node& rhs) {
        return !(lhs == rhs);
    }

    int w_dist[MAX_COL][MAX_ROW];
    int e_dist[MAX_COL][MAX_ROW];
    CELL_TYPE field[MAX_COL][MAX_ROW];


    struct my_queue {
        Node q[MAX_COL * MAX_ROW * 2];
        int s;
        int e;
        void push(Node x) {
            q[e++] = x;
        }
        Node pop() {
            if (empty())
                throw std::runtime_error("AAA");
            return q[s++];
        }
        my_queue() : s(0), e(0) {}
        bool empty() {
            return e == s;
        }
    };

    my_queue q1;
    my_queue q2;


    int get_edge_weight(int from_r, int from_c, int to_r, int to_c) {
        if (from_r > 0 && from_c > 0 && to_r > 0 && to_c > 0) {
            if ((field[from_r][from_c] == CLIN && field[to_r][to_c] == DIRT)
                || (field[from_r][from_c] == DIRT && field[to_r][to_c] == CLIN))
            {
                return 1;
            }
        }
        else {
            return 1;
        }

        return 0;

    }

    void go(Node node) {
        // edge weight
        for (int i = 0; i < NUM_DIRS; ++i) {
            int nr = DIR_VEC[i][0] + node.r;
            int nc = DIR_VEC[i][1] + node.c;

            if (field[nr][nc] == WALL) {
                continue;
            }

            // relaxing
            bool isRelaxed = false;

            int cew = get_edge_weight(node.r, node.c, nr, nc);
            if (w_dist[nr][nc] > w_dist[node.r][node.c] + cew) {
                isRelaxed = true;
                w_dist[nr][nc] = w_dist[node.r][node.c] + cew;
            }

            if (isRelaxed || w_dist[nr][nc] == w_dist[node.r][node.c] + cew) {
                if (e_dist[nr][nc] > e_dist[node.r][node.c] + 1) {
                    isRelaxed = true;
                    e_dist[nr][nc] = e_dist[node.r][node.c] + 1;
                }
            }

            if (isRelaxed && cew == 0) {
                q1.push({ nr, nc });
            }
            else if (isRelaxed && cew != 0) {
                q2.push({ nr, nc });
            }
        }
    }

    void solve() {
        using namespace std;

        // reading 

        int cols; // width (cols)
        int rows; // height (rows)
        cin >> rows >> cols;

        int sr, sc; // source row and column
        int dr, dc; // dest row and column

        cin >> sr >> sc;
        cin >> dr >> dc;

        string str;
        getline(cin, str);
        for (int i = 1; i <= rows; ++i) {
            getline(cin, str);
            for (int j = 1; j <= cols; ++j) {
                field[i][j] = (CELL_TYPE) ((int) (str[j - 1] - '0'));
            }
        }

        // initializing data 

        for (int i = 0; i < MAX_ROW; ++i) {
            for (int j = 0; j < MAX_COL; ++j) {
                w_dist[i][j] = INF;
                e_dist[i][j] = INF;
            }
        }

        w_dist[sr][sc] = 0;
        e_dist[sr][sc] = 0;

        // BFS

        Node start = { sr, sc };
        q1.push(start);

        while (!q1.empty() || !q2.empty()) {
            Node q;
            if (!q1.empty()) {
                q = q1.pop();
            }
            else {
                q = q2.pop();
            }
            go(q);
        }

        // result
        if (e_dist[dr][dc] == INF || w_dist[dr][dc] == INF) {
            cout << 0 << " " << 0 << endl;
        }
        else {
            cout << e_dist[dr][dc] + 1 << " " << w_dist[dr][dc] << endl;
        }
    }
}

//int main() {
//    D::solve();
//    return 0;
//}