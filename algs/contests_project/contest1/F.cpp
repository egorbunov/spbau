//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <string>

/**
 * Task F
 */
namespace contest1F {
    using namespace std;

    int cnt = 0;

    void siftUp(vector<int> &heap, int i) {
        int parent = i / 2;
        if (heap[i] > heap[parent]) {
            swap(heap[i], heap[parent]);
            cnt += 1;
            siftUp(heap, parent);
        }
    }

    int cnt1 = 0;
    void siftDown(vector<int> &heap, int i, int size) {
        int c1 = 2 * i + 1;
        if (c1 >= size)
            c1 = i;
        int c2 = 2 * i + 2;
        if (c2 >= size)
            c2 = i;
        int max = c1;
        if (heap[c2] > heap[c1])
            max = c2;
        if (heap[i] < heap[max]) {
            swap(heap[i], heap[max]);
            cnt1 += 1;
            siftDown(heap, max, size);
        }
    }

    void hsort(vector<int> &heap, int size) {
        if (size <= 1)
            return;
        swap(heap[0], heap[size - 1]);
        siftDown(heap, 0, size - 1);
        hsort(heap, size - 1);
    }

    void solve() {
        int n;
        cin >> n;

        vector<int> heap(n);

        int size = 1;
        heap[0] = 1;
        while (size != n) {
            heap[size - 1] = size + 1;
            siftUp(heap, size - 1);
            size += 1;
            heap[size - 1] = 1;
        }

        cout << cnt << endl;
        for (int i = 0; i < size; ++i) {
            cout << heap[i] << " ";
        }
        cout << endl;

        hsort(heap, size);
        cout << cnt1 << endl;
        for (int i = 0; i < size; ++i) {
            cout << heap[i] << " ";
        }
        cout << endl;
    }
}

int main() {
    contest1F::solve();
    return 0;
}