#include <iostream>
#include <stdexcept>
#include <algorithm>
#include <vector>
#include <assert.h>
#include <limits>
#include <iterator>
#include <random>

// ================ Implicit Cartesian Tree ==================

template<typename T>
struct ImplicitTreap {
    struct Node;

    ImplicitTreap(): root(nullptr) {}

    T get(size_t pos);

    size_t getSize();

    void pushBack(Node* node);

    /**
     * idx(resL) < idx
     * idx(resR) >= idx
     */
    void split(size_t idx, ImplicitTreap &resL, ImplicitTreap &resR);

    /**
     * Merge this tree with given
     */
    ImplicitTreap<T>& merge(ImplicitTreap &t);

    /**
     * Splits tree into 3:
     *      [0..from), [from, to], [to + 1, ...)
     */
    void splitSegment(size_t from, size_t to,
                      ImplicitTreap &l,
                      ImplicitTreap &m,
                      ImplicitTreap &r);

    // sum [from, to]
    T sum(size_t from, size_t to);

	struct Node {
		friend class ImplicitTreap;

        Node(): Node(T()) {}

        Node(T data): size(1), data(data), sum(data), l(nullptr), r(nullptr) {
        }

    private:
        size_t size; // size of subtree (including this node)
        T data; // priority
        T sum;
        Node* l;
        Node* r;

		void update() {
			sum = data;
			size = 1;
			if (l != nullptr) {
				sum += l->sum;
				size += l->size;
			}
			if (r != nullptr) {
				sum += r->sum;
				size += r->size;
			}
		}

        size_t pos() {
            return (l == nullptr) ? 0 : l->size;
        }

        static void split(Node* node, size_t idx, Node* &resL, Node* &resR) {
            if (node == nullptr) {
                resL = resR = nullptr;
                return;
            }
            if (node->pos() < idx) {
                split(node->r, idx - node->pos() - 1, node->r, resR);
                resL = node;
            } else {
                split(node->l, idx, resL, node->l);
                resR = node;
            }
            if (resL != nullptr) {
                resL->update();
            }
            if (resR != nullptr) {
                resR->update();
            }
        }

        /**
         * Merging
         *     l             r
         *   /   \         /   \
         * l->l  l->r    r->l  r->r
         */
        static void merge(Node* l, Node* r, Node* &res) {
            if (l == nullptr || r == nullptr) {
                res = (l == nullptr) ? r : l;
            } else if (l->data > r->data) {
                merge(l->r, r, l->r);
                res = l;
            } else {
                merge(l, r->l, r->l);
                res = r;
            }
            if (res != nullptr) {
                res->update();
            }
        }

        /**
         * Inserts newNode before current idx node if exists, or adds to the end
         */
        static void insert(Node* &t, Node* newNode, size_t idx) {
            if (t == nullptr) {
                t = newNode;
            } else {
                Node *l, *r;
                split(t, idx, l, r);
                merge(l, newNode, l);
                merge(l, r, t);
            }
            t->update();
        }

        static Node* find(Node* t, size_t pos) {
            if (t == nullptr) {
                return nullptr;
            } else if (t->pos() == pos) {
                return t;
            } else if (t->pos() < pos) {
                return find(t->r, pos - t->pos() - 1);
            } else {
                return find(t->l , pos);
            }
        }
	};

private:
	Node* root;
};

template<class T>
void ImplicitTreap<T>::split(size_t idx, ImplicitTreap &resL, ImplicitTreap &resR) {
    Node::split(root, idx, resL.root, resR.root);
}

template<class T>
ImplicitTreap<T>& ImplicitTreap<T>::merge(ImplicitTreap &t) {
    Node::merge(root, t.root, root);
    return *this;
}

template<class T>
void ImplicitTreap<T>::splitSegment(size_t from,
                                    size_t to,
                                    ImplicitTreap &l,
                                    ImplicitTreap &m,
                                    ImplicitTreap &r) {
    split(from, l, m);
    m.split(to - from + 1, m, r);
}


template<class T>
void ImplicitTreap<T>::pushBack(Node *node) {
    Node::insert(root, node, getSize());
}

template<class T>
T ImplicitTreap<T>::get(size_t pos) {
    Node* node = Node::find(root, pos);
    if (node != nullptr) {
        return node->data;
    } else {
        throw std::out_of_range("Treap index out of bounds.");
    }
}

template<class T>
size_t ImplicitTreap<T>::getSize() {
    return (root == nullptr) ? 0 : root->size;
}

template<class T>
T ImplicitTreap<T>::sum(size_t from, size_t to) {
    Node *l, *m, *r;
    l = m = r = nullptr;
    Node::split(root, from, l, r);
    Node::split(r, to - from + 1, m, r);
    T ans = m->sum;
    Node::merge(l, m, l);
    Node::merge(l, r, root);
    return ans;
}

// ===================================================================



int main() {
    std::ios_base::sync_with_stdio(false);
    using namespace std;
    typedef ImplicitTreap<long long>::Node node_t;

    size_t n;
    size_t qNum;



    cout << "Swapper 1:" << endl;
    while (true) {
        ImplicitTreap<long long> oddT;
        ImplicitTreap<long long> evenT;
        vector<node_t> nodes;
        nodes.reserve(100000);

        cin >> n >> qNum;
        if (n == 0 && qNum == 0) {
            break;
        }

        // filling cart trees
        for (size_t i = 0; i < n; ++i) {
            int x;
            cin >> x;
            nodes.push_back(node_t(x));
            if (i % 2 == 0) {
                evenT.pushBack(&nodes[i]);
            } else {
                oddT.pushBack(&nodes[i]);
            }
        }

        for (size_t i = 0; i < qNum; ++i) {
            int q, a, b;
            cin >> q >> a >> b;
            a--;
            b--;

            int oddL, oddR, evenL, evenR;
            oddR = evenR = (b / 2);
            oddL = evenL = (a / 2);
            if (a % 2 != 0)
                evenL += 1;
            if (b % 2 == 0)
                oddR -= 1;

            if (q == 1) { // swap
                ImplicitTreap<long long> to1, to2, to3;
                ImplicitTreap<long long> te1, te2, te3;

                oddT.splitSegment((size_t) oddL, (size_t) oddR, to1, to2, to3);
                evenT.splitSegment((size_t) evenL, (size_t) evenR, te1, te2, te3);

                oddT = std::move(to1.merge(te2).merge(to3));
                evenT = std::move(te1.merge(to2).merge(te3));
            } else if (q == 2) { // sum
                long long sum = 0;
                if (oddL <= oddR && oddL >= 0) {
                    sum += oddT.sum((size_t) oddL, (size_t) oddR);
                }

                if (evenL <= evenR && evenL >= 0) {
                    sum += evenT.sum((size_t) evenL, (size_t) evenR);
                }

                cout << sum << endl;
            }
        }
        cout << endl;
    }

    return 0;
}
