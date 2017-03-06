#include <algorithm>
#include <map>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <bits/stl_set.h>

struct Treap {

    Treap(): Treap(int()) {}

    Treap(int data): size(1), data(data), prior(rand()), l(NULL), r(NULL) {
    }

    size_t size; // size of subtree (including this node)
    int data; // data
    int prior;
    Treap * l;
    Treap * r;

    inline void update() {
        size = 1;
        if (l != NULL) {
            size += l->size;
        }
        if (r != NULL) {
            size += r->size;
        }
    }

    inline size_t pos() {
        return (l == NULL) ? 0 : l->size;
    }

    static void split(Treap * node, size_t from, size_t to, Treap * &tl, Treap * &tm, Treap * &tr) {
        split(node, from, tl, tm);
        split(tm, to + 1 - from, tm, tr);
    }

    static void split(Treap * node, size_t idx, Treap * &resL, Treap * &resR) {
        if (node == NULL) {
            resL = resR = NULL;
            return;
        }
        size_t pos = node->pos();
        if (pos < idx) {
            split(node->r, idx - pos - 1, node->r, resR);
            resL = node;
        } else {
            split(node->l, idx, resL, node->l);
            resR = node;
        }
        node->update();
    }

    static void merge(Treap * l, Treap * r, Treap * &res) {
        if (l == NULL || r == NULL) {
            res = (l == NULL) ? r : l;
        } else if (l->prior > r->prior) {
            merge(l->r, r, l->r);
            res = l;
        } else {
            merge(l, r->l, r->l);
            res = r;
        }
        res->update();
    }

    static void insert(Treap* &t, Treap * newNode, size_t idx) {
        if (t == NULL) {
            t = newNode;
        } else {
            Treap *l = NULL, *r = NULL;
            split(t, idx, l, r);
            merge(l, newNode, l);
            merge(l, r, t);
        }
        t->update();
    }

    static void insert(Treap* &t, Treap* newNode) {
        insert(t, newNode, (t == NULL) ? 0 : t->size);
    }

    static Treap* find(Treap* t, size_t pos) {
        if (t == NULL) {
            return NULL;
        }
        size_t tpos = t->pos();
        if (tpos == pos) {
            return t;
        } else if (tpos < pos) {
            return find(t->r, pos - tpos - 1);
        } else {
            return find(t->l , pos);
        }
    }

    static void erase(Treap* &t, size_t pos) {
        Treap *l = NULL, *r = NULL, *m = NULL;
        split(t, pos + 1, l, r);
        split(l, pos, l, m);
        merge(l, r, t);
        t->update();
    }

    static void print(Treap*& t) {
        if (t == NULL) {
            std::cout << "[]" << std::endl;
            return;
        }
        for (size_t j = 0; j < (size_t) t->size; ++j) {
            Treap *node = Treap::find(t, j);
            std::cout << node->data << " ";
        }
        std::cout << std::endl;
    }
};


void test() {

}

const size_t MAX_M = 132000;
Treap NODES[270000];
std::vector<size_t> arr;
std::set<size_t> zeroPoses;

Treap *treap = NULL;

int main() {
    using namespace std;

    cin.sync_with_stdio(false);
    cin.tie(NULL);

    int max = -1;
    size_t n, m;
    cin >> n >> m;
    arr.resize(n);

    for (size_t i = 0; i < n; ++i) {
        cin >> arr[i];
        max = std::max(max, (int) arr[i]);
        arr[i]--;
    }

    size_t ndi = 0;
    for (size_t i = 0; i < (size_t) max; ++i) {
        zeroPoses.insert(i);
        NODES[ndi].prior = rand();
        Treap::insert(treap, &NODES[ndi++]);
    }

    for (size_t i = 1; i <= n; ++i) {
        NODES[ndi].data = (int) i;
        Treap *node = Treap::find(treap, arr[i - 1]);
        if (node->data == 0) {
            node->data = (int) i;
            zeroPoses.erase(arr[i - 1]);
        } else {
            auto it = zeroPoses.lower_bound(arr[i - 1]);
            if (it != zeroPoses.end()) {
                Treap::erase(treap, *it);
                zeroPoses.erase(it);
            }
            NODES[ndi].prior = rand();
            Treap::insert(treap, &NODES[ndi++], arr[i - 1]);
        }
    }
    cout << treap->size << endl;
    for (size_t i = 0; i < (size_t) treap->size; ++i) {
        cout << Treap::find(treap, i)->data << " ";
    }
    cout << endl;

    return 0;
}
