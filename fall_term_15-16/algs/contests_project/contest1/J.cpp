//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <string>

/**
 * Task J
 */
namespace contest1J {

    using namespace std;

    const int MAX_N = 200;

    int pcounts[2 * MAX_N];
    int palindromes[2 * MAX_N][2 * MAX_N];

    // [from, to)
    void findPalindromes(string s, int from, int to) {
        int curLen;
        int curStart;
        int k;
        for (int i = 0; i < 2 * MAX_N; ++i) {
            pcounts[i] = 1;
            palindromes[i][0] = 1;
        }
        for (int i = from; i < to; ++i) {
            k = 1;
            // odd palindrome
            while ((i - k >= from)
                   && (i + k < to)
                   && s[i - k] == s[i + k]) {
                curStart = i - k;
                curLen = 2 * k + 1;
                palindromes[curStart][pcounts[curStart]++] = curLen;
                k += 1;
            }
            // even palindrome
            if (i >= 1 && s[i] == s[i - 1]) {
                palindromes[i-1][pcounts[i-1]++] = 2;
                k = 2;
                while ((i - k >= from)
                       && (i + k - 1 < to)
                       && s[i - k] == s[i + k - 1]) {
                    curStart = i - k;
                    curLen = 2 * k;
                    palindromes[curStart][pcounts[curStart]++] = curLen;
                    k += 1;
                }
            }
        }
    }

    int D[2 * MAX_N + 1];
    int ANS[MAX_N + 2][2 * MAX_N + 1];

    // [from, to)
    int palindromeSplit(string &s, int from, int to) {
        findPalindromes(s, from, to);

//        for (int i = from; i < to; ++i) {
//            cout << s[i];
//        }
//        cout << endl;

        D[to - 1] = 1;
        ANS[from][to - 1] = 1;
        for (int i = to - 2; i >= from; --i) {
            int min = 10000000;
            int len = 1;
            for (int j = 0; j < pcounts[i]; ++j) {
                if (min > 1 + D[i + palindromes[i][j]])
                    len = palindromes[i][j];
                min = std::min(min, 1 + D[i + palindromes[i][j]]);
            }
            ANS[from][i] = len;
            D[i] = min;
        }

        return D[from];
    }

    void solve() {

        string s;
        int tmp;
        cin >> tmp;
        cin >> s;

        int len = s.length();
        s = s + s;

//        cout << s << endl;

        int min = 1000000;
        int from = -1;
        for (int i = 0; i < len; ++i) {
            int ans = palindromeSplit(s, i, i + len);
            if (ans < min) {
                min = ans;
                from = i;
            }
        }

        cout << from << " " << min << endl;
        int k = from;
        int to = from + len;
        while (k < to) {
            for (int i = k; i < k + ANS[from][k]; ++i) {
                cout << s[i];
            }
            cout << " ";
            k += ANS[from][k];
        }
        cout << endl;
    }
}

int main() {
    contest1J::solve();
    return 0;
}