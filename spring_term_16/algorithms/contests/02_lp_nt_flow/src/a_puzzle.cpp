#include <iostream>
#include <vector>
#include <bitset>

using namespace std;

const int MAX_N = 201;

void print(vector<bitset<MAX_N>>& matrix, size_t n, size_t m) {
    cout << endl;
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < m; ++j) {
            cout << matrix[i][j] << " ";
        }
        cout << endl;
    }
    cout << "------------------" << endl;
}

int solve(vector<bitset<MAX_N>>& matrix, size_t n, size_t m, bitset<MAX_N>& ans) {
    vector<int> where(m, -1);
    for (int col = 0, row = 0; col < m && row < n; ++col) {

//        print(matrix, n, m + 1);

        for (int i = row; i < n; ++i) {
            if (matrix[i][col] == 1) {
                swap(matrix[i], matrix[row]);
                break;
            }
        }
        if (matrix[row][col] == 0) {
            continue;
        }
        where[col] = row;

        for (int i = 0; i < n; ++i) {
            if (i != row && matrix[i][col] == 1) {
                matrix[i] ^= matrix[row];
            }
        }

        row++;
    }
//    print(matrix, n, m + 1);


    ans.reset();
    for (int i = 0; i < m; ++i) {
        if (where[i] >= 0) {
            ans[i] = matrix[where[i]][m];
        }
    }
    for (int i = 0; i < n; ++i) {
        int sum = 0;
        for (int j = 0; j < m; ++j) {
            sum ^= ans[j] * matrix[i][j];
        }
        if (sum != matrix[i][m]) {
            return 0;
        }
    }
    for (int i = 0; i < m; ++i) {
        if (where[i] == -1) {
            return -1;
        }
    }
    return 1;

}

int main() {
    size_t n;
    cin >> n;

    vector<bitset<MAX_N>> matrix(n, bitset<MAX_N>());

    for (size_t i = 0; i < n; ++i) {
        size_t k;
        cin >> k;
        matrix[i][i] = 1;
        for (size_t j = 0; j < k; ++j) {
            int id;
            cin >> id;
            matrix[i][id - 1] = 1;
        }
    }
    for (size_t i = 0; i < n; ++i) {
        int x;
        cin >> x;
        matrix[i][n] = x;
    }

    // SOLVING

    vector<bitset<MAX_N>> copy = matrix;
    bitset<MAX_N> ans;

    int acnt = solve(matrix, n, n, ans);

    if (acnt == 0) {
        cout << -1 << endl;
        return 0;
    }

    cout << ans.count() << endl;
    for (int i = 0; i < n; ++i) {
        if (ans[i] != 0) {
            cout << i + 1 << " ";
        }
    }
    cout << endl;
    return 0;
}