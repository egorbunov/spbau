#include <iostream>
#include <string>
#include <queue>

namespace {
    int solve() {
        using namespace std;

        int op_num;
        cin >> op_num;

        string op;
        int num;

        priority_queue<int> heap;
        while (op_num-- > 0) {
            cin >> op;
            if (op == "ExtractMax") {
                cout << heap.top() << endl;
                heap.pop();
            } else {
                cin >> num;
                heap.push(num);
            }
        }
    }
}