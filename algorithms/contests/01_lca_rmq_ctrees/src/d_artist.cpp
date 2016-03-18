#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <limits>
//#include <random>

using std::vector;

/**
 * T must have default contructor and (+), (-) operators must be defined
 */
template<typename T>
struct SegmentTree {
    static const size_t INF = std::numeric_limits<size_t>::max();

	SegmentTree(size_t size): root(0), arraySize(size) {
        nodes.reserve(2 * size);
        nodes.push_back(Node(0, size - 1));
        build(nodes[0]);
	}

	T get(size_t idx) {
//        assert(idx < arraySize);
        return sum(idx, idx);
	}

	// sum on segment [from, to]
	T sum(size_t from, size_t to) {
//        assert(from <= to && to < arraySize);
        return sum(nodes[root], from, to);
    }

	/**
	 * Assigns value to array[idx]
	 */
	void set(size_t idx, T value) {
//        assert(idx < arraySize);
		set(idx, idx, value);
	}

	/**
	 * Assigns value to array segment [from, to]
	 */
	void set(size_t from, size_t to, T value) {
//        assert(from <= to && to < arraySize);
        set(nodes[root], from, to, value);
	}

    void print() {
        for (size_t i = 0; i < arraySize; ++i) {
            std::cout << get(i) << " ";
        }
        std::cout << std::endl;
    }

private:
	struct Node {
		T val;
        T sum;
		bool isPushed;
        size_t l, r, lc, rc;
		Node(): val(), sum(), isPushed(false), l(INF), r(INF), lc(INF), rc(INF) {}
        Node(size_t l, size_t r): val(), sum(), isPushed(false), l(l), r(r), lc(INF), rc(INF) {}
    };

    void build(Node &node) {
        if (node.l == node.r)
            return;
        size_t m = (node.l + node.r) / 2;
        nodes.push_back(Node(node.l, m));
        nodes.push_back(Node(m + 1, node.r));
        node.lc = (nodes.size() - 2);
        node.rc = (nodes.size() - 1);
        build(nodes[node.lc]);
        build(nodes[node.rc]);
    }

	size_t root;
	size_t arraySize;
	vector<Node> nodes; // segment tree nodes

	void push(Node &node) {
        if (!node.isPushed) {
            node.isPushed = true;
            auto& lc = nodes[node.lc];
            auto& rc = nodes[node.rc];
            lc.val = rc.val = node.val;
            lc.sum = lc.val * (lc.r - lc.l + 1);
            rc.sum = rc.val * (rc.r - rc.l + 1);
            lc.isPushed = rc.isPushed = false;
        }
    }

	void set(Node &node, size_t from, size_t to, T value) {
		if (from > to) {
			return;
		}
		if (node.l == from && node.r == to) {
			node.isPushed = false;
            node.val = value;
            node.sum = (to - from + 1) * value;
		} else {
			push(node);
			size_t m = (size_t) ((node.l + node.r) / 2);
			set(nodes[node.lc], from, std::min(to, m), value);
			set(nodes[node.rc], std::max(from, m + 1), to, value);

//            assert(node.lc < nodes.size() && node.lc >= 0);
//            assert(node.rc < nodes.size() && node.rc >= 0);
            node.sum = nodes[node.lc].sum + nodes[node.rc].sum;
		}
	}

	T sum(Node& node, size_t from, size_t to) {
		if (from > to) {
			return 0;
		}
		if (node.l == from && node.r == to) {
			return node.sum;
		} else {
			push(node);
			size_t m = (size_t) ((node.l + node.r) / 2);
			return sum(nodes[node.lc], from, std::min(to, m))
				 + sum(nodes[node.rc], std::max(from, m + 1), to);
		}
	}
};

struct NaiveSolver {
    vector<int> arr;
    NaiveSolver(size_t arraySize): arr(arraySize) {

    }

    void paint(int color, size_t from, size_t to) {
        std::fill_n(arr.begin() + from, to - from + 1, color);
    }

    int len() {
        return std::accumulate(arr.begin(), arr.end(), 0);
    }

    int segCnt() {
        int ans = 0;
        for (size_t i = 0; i < arr.size() - 1; ++i) {
            if (arr[i + 1] - arr[i] == 1) {
                ans += 1;
            }
        }
        if (arr[0] == 1) {
            ans += 1;
        }
        return ans;
    }
};

struct SegmentTreeSolver {
    SegmentTree<int> treeSegments;
    /**
     * segment tree below array like:
     *  0  1  2  3  4  5  6  7  8
     * [0, 0, 1, 0, 0, 2, 0, 1, 2]
     * means, that there is segment on painting [2, 5) and [7, 8)
     */
    SegmentTree<int> treeEnds;
    size_t arraySize;
    const int L_MARK = 1;
    const int R_MARK = 2;

    SegmentTreeSolver(size_t arraySize): treeSegments(arraySize), treeEnds(arraySize), arraySize(arraySize) {
    }

    void paint(int color, size_t from, size_t to) {
        if (color == 0) {
            treeSegments.set(from, to, 0);

            treeEnds.set(from, to, 0);
            if (treeEnds.get(to + 1) == R_MARK) {
                treeEnds.set(to + 1, 0);
            }
            if (from > 0 && treeEnds.sum(0, from - 1) % 3 != 0) {
                treeEnds.set(from, R_MARK);
            }
            if (treeEnds.sum(to + 1, arraySize - 1) % 3 != 0) {
                treeEnds.set(to + 1, L_MARK);
            }
        } else if (color == 1) {
            treeSegments.set(from, to, 1);

            treeEnds.set(from, to, 0);
            if (from == 0 || treeEnds.sum(0, from - 1) % 3 == 0) {
                treeEnds.set(from, 1);
            }
            if (treeEnds.sum(to + 1, arraySize - 1) % 3 == 0) {
                if (treeEnds.get(to + 1) == L_MARK) {
                    treeEnds.set(to + 1, 0);
                } else {
                    treeEnds.set(to + 1, R_MARK);
                }
            }
        }
    }

    int len() {
        return treeSegments.sum(0, arraySize - 1);
    }

    int segCnt() {
        return treeEnds.sum(0, arraySize - 1) / 3;
    }
};

//void test() {
//    const int SHIFT = 10000;
//    const size_t MAX_SIZE = 2 * SHIFT + 100;
//
//    std::random_device rd;
//    std::mt19937 gen(rd());
//    std::uniform_int_distribution<> dis(-SHIFT, SHIFT);
//
//    SegmentTreeSolver segmentTreeSolver(MAX_SIZE);
//    NaiveSolver naiveSolver(MAX_SIZE);
//
//
//    for (int i = 0; i < 1000; ++i) {
//        int op = (dis(gen) + SHIFT) % 2;
////        std::cout << op << std::endl;
//        size_t from = (size_t) dis(gen) + SHIFT;
//        size_t len = (size_t) (dis(gen) + SHIFT) % (2 * SHIFT - from + 1);
//        size_t to = from + len;
////        std::cout << ((op == 0) ? 'W' : 'B') << " [ " << from << ", " << to << " ]" << std::endl;
//
//        segmentTreeSolver.paint(op, from, to);
//        naiveSolver.paint(op, from, to);
//
//        int lenA = segmentTreeSolver.len();
//        int lenE = naiveSolver.len();
//        if (lenA != lenE) {
//            std::cout << "(LEN) TEST ERROR!" << "[ " << lenA << " != " << lenE << " ] " << std::endl;
//        }
//
//        int segA = segmentTreeSolver.segCnt();
//        int segE = naiveSolver.segCnt();
//        if (segA != segE) {
//            std::cout << "(SEG) TEST ERROR!" << "[ " << segA << " != " << segE << " ] " << std::endl;
//        }
//    }
//
//    std::cout << "END" << std::endl;
//}

int main() {
	using namespace std;
	ios_base::sync_with_stdio(false);

//    test();

    const size_t SHIFT = 500010;
    const size_t MAX_ARRAY_SIZE = SHIFT * 2 + 1000;

    SegmentTreeSolver segmentTreeSolver(MAX_ARRAY_SIZE);
    NaiveSolver naiveSolver(MAX_ARRAY_SIZE);

    // reading and answering
    int qNum;
    char color;
    int left, len;

    scanf("%d", &qNum);
    for (int i = 0; i < qNum; ++i) {
        scanf("\n%c %d %d", &color, &left, &len);
//        cin >> color >> left >> len;
        size_t from = left + SHIFT;
        size_t to = from + len - 1;
        if (color == 'W') { // white == 0
            naiveSolver.paint(0, from, to);
            segmentTreeSolver.paint(0, from, to);
        } else if (color == 'B') {
            naiveSolver.paint(1, from, to);
            segmentTreeSolver.paint(1, from, to);
        } else {
            cout << "UPS" << endl;
            exit(1);
        }
        printf("%d %d\n", segmentTreeSolver.segCnt(), segmentTreeSolver.len());
    }

	return 0;
}