#include <iostream>

namespace testcontestA {
    void solve() {
        using namespace std;
        int x;
        cin >> x;
        if (x == 42)
            cout << "YES";
        else
            cout << "NO";
    }
}

int main() {
    testcontestA::solve();
}
