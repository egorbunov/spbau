#include <iostream>

namespace testcontestB {
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
    testcontestB::solve();
}
