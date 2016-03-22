#include <algorithm>
#include <cstdio>
#include <cstdlib>

// ================ Implicit Cartesian Tree ==================

struct Treap {

    Treap(): Treap(int()) {}

    Treap(int data): size(1), data(data), prior(rand()), l(NULL), r(NULL) {
    }

    size_t size; // size of subtree (including this node)
    int data; // data
    int prior;
    long long sum;
    Treap * l;
    Treap * r;

    inline void update() {
        sum = data;
        size = 1;
        if (l != NULL) {
            sum += l->sum;
            size += l->size;
        }
        if (r != NULL) {
            sum += r->sum;
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
            Treap *l, *r;
            split(t, idx, l, r);
            merge(l, newNode, l);
            merge(l, r, t);
        }
        t->update();
    }

    static void insert(Treap *&t, Treap * newNode) {
        insert(t, newNode, (t == NULL) ? 0 : t->size);
    }

    static long long getSum(Treap * t, size_t from, size_t to) {
        Treap *l, *m, *r;
        l = m = r = NULL;
        Treap::split(t, from, l, r);
        Treap::split(r, to - from + 1, m, r);
        long long ans = m->sum;
        Treap::merge(l, m, l);
        Treap::merge(l, r, t);
        return ans;
    }

};

// ===================================================================

typedef Treap treap_t;

treap_t NODES[200001];

treap_t *oddT = NULL;
treap_t *evenT = NULL;

size_t n;
size_t qNum;
int ind = 1;
int q, a, b;
int oddL, oddR, evenL, evenR;
treap_t *to1 = NULL, *to2 = NULL, *to3 = NULL;
treap_t *te1 = NULL, *te2 = NULL, *te3 = NULL;
long long sum = 0;


int main() {
    using namespace std;

    while (true) {
        oddT = NULL;
        evenT = NULL;
        scanf("%lu %lu", &n, &qNum);
        if (n == 0 && qNum == 0) {
            break;
        }
        printf("Swapper %d:\n", ind++);
        for (size_t i = 0; i < n; ++i) {
            int x;
            scanf("%d", &x);
            NODES[i] = treap_t(x);
        }
        for (size_t i = 0; i < n - 1; i += 2) {
            treap_t::insert(evenT, &NODES[i]);
            treap_t::insert(oddT, &NODES[i + 1]);
        }
        if ((n - 1) % 2 == 0) {
            treap_t::insert(evenT, &NODES[n - 1]);
        }
        for (size_t i = 0; i < qNum; ++i) {
            scanf("%d %d %d", &q, &a, &b);
            a--;
            b--;
            oddR = evenR = b >> 1;
            oddL = evenL = a >> 1;
            if (a % 2 != 0)
                evenL += 1;
            if (b % 2 == 0)
                oddR -= 1;
            if (q == 1) { // swap
                treap_t::split(oddT, (size_t) oddL, (size_t) oddR, to1, to2, to3);
                treap_t::split(evenT, (size_t) evenL, (size_t) evenR, te1, te2, te3);
                treap_t::merge(to1, te2, te2);
                treap_t::merge(te2, to3, oddT);
                treap_t::merge(te1, to2, to2);
                treap_t::merge(to2, te3, evenT);
            } else if (q == 2) { // sum
                sum = 0;
                if (oddL <= oddR && oddL >= 0) {
                    sum += treap_t::getSum(oddT, (size_t) oddL, (size_t) oddR);
                }
                if (evenL <= evenR && evenL >= 0) {
                    sum += treap_t::getSum(evenT, (size_t) evenL, (size_t) evenR);
                }
                printf("%lli\n", sum);
            }
        }
        printf("\n");
    }

    return 0;
}
