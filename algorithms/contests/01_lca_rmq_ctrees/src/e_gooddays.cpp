#include <iostream>
#include <vector>
#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <limits>
#include <iterator>
#include <random>

using std::vector;

template<typename  T>
struct mpair {
    size_t minIdx;
    T sum;

    bool operator==(const mpair& other) {
        return minIdx == other.minIdx && sum == other.sum;
    }
};

template<class T>
struct NaiveSegmentTree {
    const vector<T> &arr;
    NaiveSegmentTree(const vector<T> &arr): arr(arr) {
    }

    mpair<T> minIdxSum(size_t l, size_t r) {
        size_t idx = (size_t) std::distance(arr.begin(), std::min_element(arr.begin() + l, arr.begin() + r + 1));
        T sum = std::accumulate(arr.begin() + l, arr.begin() + r + 1, 0);
        return {idx , sum };
    }
};


/**
 * T must have default contructor and (+), (-) operators must be defined
 */
template<typename T>
struct SegmentTree {
    static const size_t INF = std::numeric_limits<size_t>::max();

    SegmentTree(const vector<T> &arr): arr(arr), root(0), arraySize(arr.size()) {
        nodes.reserve(2 * arraySize);
        nodes.push_back(Node(0, arraySize - 1));
        build(nodes[0]);
    }

    mpair<T> minIdxAndSum(size_t l, size_t r) const {
        assert(l <= r && l >= 0 && r < arraySize);
        return minIdxAndSum(nodes[root], l, r);
    }

private:
    struct Node {
        size_t l;
        size_t r;
        size_t lc;
        size_t rc;
        size_t minIdx;
        T sum;
        Node(size_t l, size_t r): l(l), r(r), lc(INF), rc(INF), minIdx(INF), sum(T()) {}
    };

    void build(Node &node) {
        if (node.l == node.r) {
            node.minIdx = node.l;
            node.sum = arr[node.minIdx];
            return;
        }
        size_t m = (node.l + node.r) / 2;
        nodes.push_back(Node(node.l, m));
        nodes.push_back(Node(m + 1, node.r));
        node.lc = (nodes.size() - 2);
        node.rc = (nodes.size() - 1);
        build(nodes[node.lc]);
        build(nodes[node.rc]);
        if (arr[nodes[node.lc].minIdx] < arr[nodes[node.rc].minIdx]) {
            node.minIdx = nodes[node.lc].minIdx;
        } else {
            node.minIdx = nodes[node.rc].minIdx;
        }
        node.sum = nodes[node.lc].sum + nodes[node.rc].sum;
    }

    const vector<T> &arr;
    size_t root;
    size_t arraySize;
    vector<Node> nodes; // segment tree nodes

    mpair<T> minIdxAndSum(const Node& node, size_t from, size_t to) const {
        if (node.l == from && node.r == to) {
            return {node.minIdx, node.sum };
        } else {
            size_t m = (size_t) ((node.l + node.r) / 2);
            if (std::max(from, m + 1) > to) {
                return minIdxAndSum(nodes[node.lc], from, std::min(to, m));
            }
            if (from > std::min(to, m)) {
                return minIdxAndSum(nodes[node.rc], std::max(from, m + 1), to);
            }
            assert(std::max(from, m + 1) <= to && from <= std::min(to, m));

            auto p1 = minIdxAndSum(nodes[node.lc], from, std::min(to, m));
            auto p2 = minIdxAndSum(nodes[node.rc], std::max(from, m + 1), to);
            T sum = p1.sum + p2.sum;
            if (arr[p1.minIdx] < arr[p2.minIdx]) {
                return {p1.minIdx, sum};
            } else {
                return {p2.minIdx, sum};
            }
        }
    }
};

//void test() {
//    const size_t SIZE = 100000;
//    std::random_device rd;
//    std::mt19937 gen(rd());
//    std::uniform_int_distribution<> dis(0, SIZE - 1);
//    std::uniform_int_distribution<> dis1(-(int) 1e6, (int) 1e6);
//
//    vector<long long> arr(SIZE);
//    std::generate(arr.begin(), arr.end(), [&dis1, &gen](){ return dis1(gen);});
//
//    SegmentTree<long long> stree(arr);
//    NaiveSegmentTree<long long> ntree(arr);
//
//    for (int i = 0; i < 1000; ++i) {
//        size_t l = (size_t) dis(gen);
//        size_t r = l + ((size_t) (dis(gen)) % (SIZE - l));
//
//        auto expected = ntree.minIdxSum(l, r);
//        auto actual = stree.minIdxSum(l, r);
//        if (expected == actual) {
//        } else {
//            std::cout << "ERROR" << std::endl;
//            if (expected.sum != actual.sum) {
//                std::cout << expected.sum << " != " << actual.sum << std::endl;
//            }
//            if (expected.minIdx != actual.minIdx) {
//                std::cout << expected.minIdx << " != " << actual.minIdx << std::endl;
//            }
//        }
//    }
//
//    std::cout << "END" << std::endl;
//}

size_t N;
vector<long long> array;

size_t gFrom, gTo;
long long gMax = std::numeric_limits<long long>::min();

// [from, to]
void solve(size_t from, size_t to, const SegmentTree<long long> &tree) {
    auto p = tree.minIdxAndSum(from, to);
    auto cMax = array[p.minIdx] * p.sum;
    if (cMax > gMax) {
        gMax = cMax;
        gFrom = from;
        gTo = to;
    }
    if (p.minIdx > 0 && p.minIdx > from) {
        solve(from, p.minIdx - 1, tree);
    }
    if (p.minIdx < to) {
        solve(p.minIdx + 1, to, tree);
    }
}


int main() {
    using namespace std;

    array.reserve(100000);
    scanf("%lu", &N);
    long long x;
    for (size_t i = 0; i < N; ++i) {
        scanf("%lli", &x);
        array.push_back(x);
    }

    SegmentTree<long long> tree(array);
    solve(0, N - 1, tree);

    cout << gMax << endl;
    cout << gFrom + 1 << " " << gTo + 1 << endl;

    return 0;
}