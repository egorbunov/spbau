#include <iostream>
#include <vector>
#include <algorithm>

using std::vector;

/**
 * T must have default contructor and (+), (-) operators must be defined
 */
template<typename T>
struct SegmentTree {
	SegmentTree(size_t size): root(0), arraySize(size), nodes(2 * size) {
	}

	T get(size_t idx) {
		return sum(idx, idx);
	}

	// sum on segment [from, to]
	T sum(size_t from, size_t to) {
		return sum(root, 0, arraySize - 1, from, to);
	}

	/**
	 * Assigns value to array[idx]
	 */
	void set(size_t idx, T value) {
		set(idx, idx, value);
	}

	/**
	 * Assigns value to array segment [from, to]
	 */
	void set(size_t from, size_t to, T value) {
		set(root, 0, arraySize - 1, from, to, value);
	}

private:
	struct Node {
		T val;
        T sum;
		bool isPushed;
		Node(): val(), isPushed(false) {}
	};

	size_t root;
	size_t arraySize;
	vector<Node> nodes; // segment tree nodes

	void push(size_t nodeId) {
		auto& node = nodes[nodeId];
		if (!node.isPushed) {
			node.isPushed = true;
			auto& nodeL = nodes[nodeId * 2 + 1];
			auto& nodeR = nodes[nodeId * 2 + 2];
			nodeL.val = nodeR.val = node.val;
			nodeL.isPushed = nodeR.isPushed = false;
		}
	}

	void set(size_t v, size_t vFrom, size_t vTo, size_t from, size_t to, T value) {
		if (from > to) {
			return;
		}
		if (vFrom == from && vTo == to) {
			nodes[v].isPushed = false;
			nodes[v].val = value;
            nodes[v].sum = (to - from + 1) * value;
		} else {
			push(v);
			size_t m = (vFrom + vTo) / 2;
			set(v * 2 + 1, vFrom, m, from, std::min(to, m), value);
			set(v * 2 + 2, m + 1, vTo, std::max(from, m + 1), to, value);
            nodes[v].sum = nodes[v * 2 + 1].sum + nodes[v * 2 + 2].sum;
		}
	}

	T sum(size_t v, size_t vFrom, size_t vTo, size_t from, size_t to) {
		if (from > to) {
			return 0;
		}
		if (vFrom == from && vTo == to) {
			return nodes[v].sum;
		} else {
			push(v);
			size_t m = (vFrom + vTo) / 2;
			return sum(v * 2 + 1, vFrom, m, from, std::min(to, m))
				 + sum(v * 2 + 2, m + 1, vTo, std::max(from, m + 1), to);
		}
	}
};

int main() {
	using namespace std;
	ios_base::sync_with_stdio(false);

    const int SHIFT = 500000;
    const int MAX_ARRAY_SIZE = 1000001;
//    const int LEFT_MART = 1;
//    const int RIGHT_MARK = 2;

	SegmentTree<int> treeSegments(MAX_ARRAY_SIZE);

    /**
     * segment tree below array like:
     *  0  1  2  3  4  5  6  7  8
     * [0, 0, 1, 0, 0, 2, 0, 1, 2]
     * means, that there is segment on painting [2, 5) and [7, 8)
     */
    SegmentTree<int> treeEnds(MAX_ARRAY_SIZE);

    // reading and answering
    int qNum;
    char color;
    int left, len;

    cin >> qNum;
    for (int i = 0; i < qNum; ++i) {
        cin >> color >> left >> len;
        // [from, to]
        int from = left + SHIFT;
        int to = from + len;

        if (color == 'W') { // white == 0
            treeSegments.set((size_t) from, (size_t) to, 0);
        } else if (color == 'B') {
            treeSegments.set((size_t) from, (size_t) to, 1);
        } else {
            cout << "UPS" << endl;
            exit(1);
        }

        cout << treeSegments.sum(0, MAX_ARRAY_SIZE) << endl;
    }

//    std::cout << treeSegments.get(2) << std::endl;
//    treeSegments.set(0, 1);
//    std::cout << treeSegments.sum(0, 3) << std::endl;
//    treeSegments.set(3, 2);
//    std::cout << treeSegments.sum(0, 3) << std::endl;
//    treeSegments.set(2, 3);
//	std::cout << treeSegments.sum(0, 3) << std::endl;
//    treeSegments.set(1, 4);
//    std::cout << treeSegments.sum(0, 3) << std::endl;


	return 0;
}