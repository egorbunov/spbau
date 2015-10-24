#include <iostream>

namespace contest1B {
    void solve() {
        using namespace std;
        char x;
        cin >> x;
        if (x == 'x')
            cout << 'y';
        else
            cout << 'x';
    }
}

int main() {
    contest1B::solve();
}
