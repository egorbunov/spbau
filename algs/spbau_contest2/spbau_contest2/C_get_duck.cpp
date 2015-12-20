#include <iostream>
#include <string>

using namespace std;

namespace C {
    struct pos_t {
        int x;
        int y;
        pos_t(int x, int y) : x(x), y(y) {}
        pos_t() : x(-1), y(-1) {}
    };

    enum DIRECTION {
        UP,
        DOWN,
        RIGHT,
        LEFT,
        UNKNOWN
    };

    struct state {
        pos_t p;
        DIRECTION d;
        state(int x, int y, DIRECTION d) : p(x, y), d(d) {}
        state() : d(UNKNOWN) {}
    };

    const int MAX_N = 1000; // height
    const int MAX_M = 1000; // width
    char field[2 * MAX_N + 3][2 * MAX_M + 3];
    bool visited[2 * MAX_N + 3][2 * MAX_M + 3][5];



    struct my_stack {
        state arr[(2 * MAX_M + 5) * (2 * MAX_N + 5)];
        int sz = 0;

        void push(state st) {
            arr[sz++] = st;
        }

        state pop() {
            return arr[--sz];
        }

        bool empty() {
            return sz == 0;
        }

        my_stack() {}

    };
    my_stack dfs_stack;

    bool isOkToGo(char x) {
        return x == 'S' || x == ' ' || x == 'D' || x == 'X';
    }

    void add_next_to_stack(pos_t p) {
        if (field[p.x - 1][p.y] == '-' && isOkToGo(field[p.x + 1][p.y]) && !visited[p.x + 1][p.y][DOWN]) {
            dfs_stack.push(state(p.x + 1, p.y, DOWN));
        } else if (field[p.x + 1][p.y] == '-' && isOkToGo(field[p.x - 1][p.y]) && !visited[p.x - 1][p.y][UP]) {
            dfs_stack.push(state(p.x - 1, p.y, UP));
        }
        if (field[p.x][p.y - 1] == '|' && isOkToGo(field[p.x][p.y + 1]) && !visited[p.x][p.y + 1][RIGHT]) {
            dfs_stack.push(state(p.x, p.y + 1, RIGHT));
        } else if (field[p.x][p.y + 1] == '|' && isOkToGo(field[p.x][p.y - 1]) && !visited[p.x][p.y - 1][LEFT]) {
            dfs_stack.push(state(p.x, p.y - 1, LEFT));
        }
    }

    void dfs(pos_t p) {
        add_next_to_stack(p);

        while (!dfs_stack.empty()) {
            state st = dfs_stack.pop();
            visited[st.p.x][st.p.y][st.d] = true;
            if (field[st.p.x][st.p.y] == 'D') {
                field[st.p.x][st.p.y] = 'X';
            }
            add_next_to_stack(st.p);
            int nx = st.p.x, ny = st.p.y;
            switch (st.d)
            {
            case UP:
                nx -= 1;
                break;
            case DOWN:
                nx += 1;
                break;
            case RIGHT:
                ny += 1;
                break;
            case LEFT:
                ny -= 1;
                break;
            }
            if (isOkToGo(field[nx][ny]) && visited[nx][ny][st.d] == false) {
                dfs_stack.push(state(nx, ny, st.d));
            }
        }
    }

    void solve() {
        int n, m;

        string str = "";
        cin >> n >> m;
        getline(cin, str);

        pos_t s;

        for (int i = 1; i <= 2 * n + 1; ++i) {
            getline(cin, str);
            if (str.length() != 2 * m + 1)
                throw std::runtime_error("!!!");
            for (int j = 1; j <= 2 * m + 1; ++j) {
                field[i][j] = str[j - 1];
                if (field[i][j] == 'S') {
                    s.x = i;
                    s.y = j;
                }
            }

        }

        dfs(s);

        for (int i = 1; i <= 2 * n + 1; ++i) {
            for (int j = 1; j <= 2 * m + 1; ++j) {
                if (field[i][j] == 'D') {
                    cout << ' ';
                }
                else if (field[i][j] == 'X') {
                    cout << 'D';
                }
                else {
                    cout << field[i][j];
                }
            }
            cout << endl;
        }
    }
}

//int main() {
//    C::solve();
//    return 0;
//}
