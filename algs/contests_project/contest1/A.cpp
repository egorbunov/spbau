#include <iostream>

namespace contest1A {
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
    contest1A::solve();
}
