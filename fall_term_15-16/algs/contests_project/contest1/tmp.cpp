//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <string>

namespace contest1TMP {
    using namespace std;

    void solve() {
        string s;
        cin >> s;

        int maxPStart = 0; // max palinrome start pos
        int maxPLen = 1; // max palindrome len
        int curLen;
        int k;
        for (int i = 0; i < s.length(); ++i) {
            k = 1;

            // odd palindrome
            while ((i - k >= 0)
                   && (i + k <= s.length())
                   && s[i - k] == s[i + k]) {
                k += 1;
            }
            k -= 1;
            curLen = 2 * k + 1;
            if (maxPLen < curLen) {
                maxPLen = curLen;
                maxPStart = i - k;
            }

            // even palindrome
            if (i >= 1 && s[i] == s[i - 1]) {
                k = 2;
                while ((i - curLen >= 0)
                       && (i + k - 1 <= s.length())
                       && s[i - k] == s[i + k - 1]) {
                    k += 1;
                }
                k -= 1;
                curLen = 2 * k + 1;
                if (maxPLen < curLen) {
                    maxPLen = curLen;
                    maxPStart = i - k;
                }
            }
        }

        for (int i = maxPStart; i < maxPLen + maxPStart; ++i) {
            cout << s[i];
        }
        cout << endl;
    }

}