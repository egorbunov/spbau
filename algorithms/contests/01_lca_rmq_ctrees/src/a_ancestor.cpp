#include <iostream>
#include <vector>
#include <list>
#include <cstdio>

using namespace std;

struct tree_t {
    typedef int node_idx;
    struct node_t {
        static const int TIME_NOT_SET = -1;
        int in_time;
        int out_time;
        list<node_idx> children;

        node_t(): in_time(TIME_NOT_SET), out_time(TIME_NOT_SET) {}
    };

    vector<node_t> all_nodes;
    node_idx root;


    tree_t(size_t node_number): all_nodes(node_number) {}

    void set_child(node_idx parent, node_idx child) {
        parent -= 1;
        child -= 1;
        if (parent < 0) {
            root = child;
        } else {
            all_nodes[parent].children.push_back(child);
        }
    }

    void init() {
        dfs(root);
    }

    bool is_ancestor(node_idx a, node_idx b) {
        return all_nodes[b].in_time > all_nodes[a].in_time
               && all_nodes[b].out_time < all_nodes[a].out_time;
    }

private:
    void dfs(node_idx from) {
        static int t = 0;
        all_nodes[from].in_time = ++t;
        for (node_idx child : all_nodes[from].children) {
            dfs(child);
        }
        all_nodes[from].out_time = ++t;
    }
};

int main() {
    int n;
	scanf("%d", &n);

    tree_t tree(n);

    for (int c = 1; c <= n; ++c) {
        int p;
        scanf("%d", &p);
        tree.set_child(p, c);
    }

    tree.init();

    int m;
    scanf("%d", &m);
    for (int i = 0; i < m; ++i) {
        int a, b;
        scanf("%d %d", &a, &b);
        printf("%d\n", (tree.is_ancestor(a - 1, b - 1) ? 1 : 0));
    }
	return 0;
}