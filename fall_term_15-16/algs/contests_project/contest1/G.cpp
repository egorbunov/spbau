//
// Created by egorbunov on 26.10.15.
//

#include <iostream>
#include <algorithm>
#include <string>

/**
 * Task G
 */
namespace contest1G {
    struct Node {
        int parent;
        int a;
        std::vector<int> children;
        Node() : parent(-1), a(0), children(std::vector<int>()) {}
    };

    std::vector<Node> nodes;

    void dfs(int v) {
        int max = nodes[v].a;
        int sum = 0;
        for (int i = 0; i < nodes[v].children.size(); ++i) {
            dfs(nodes[v].children[i]);
            max = std::max(max, nodes[nodes[v].children[i]].a);
            sum += nodes[nodes[v].children[i]].a;
        }
        nodes[v].a = std::max(std::max(max, nodes[v].a), sum + nodes[v].a);
    }

    void solve() {
        int n;
        std::cin >> n;
        nodes.resize(n);
        std::cin >> nodes[0].a;

        for (int i = 1; i < n; ++i) {
            std::cin >> nodes[i].parent;
            nodes[i].parent -= 1;
            std::cin >> nodes[i].a;
            nodes[nodes[i].parent].children.push_back(i);
        }

        dfs(0);

        std::cout << nodes[0].a << std::endl;
    }
}