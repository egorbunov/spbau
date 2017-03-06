#include <iostream>
#include <queue>
#include <string>

namespace {
  using namespace std;

  struct Node {
    int key;
    char c;
    bool isLeaf;
    string code;
    Node *l;
    Node *r;
    Node(int key, char c) : key(key), c(c), isLeaf(true), l(NULL), r(NULL), code("") {}
    Node(int key, Node *l, Node *r) : key(key), c(-1), isLeaf(false), l(l), r(r), code("") {}
    inline bool operator<(const Node& node) const {
      return key < node.key;
    }
  };

  struct NodeComparator
  {
    bool operator () ( const Node* a, const Node* b ) const
    {
      return a->key >= b->key;
    }
  };

  void getCodes(Node *root, string code) {
    if (root->isLeaf) {
      if (code.empty())
        code = "0";
      root->code = code;
    }
    else {
      getCodes(root->l, code + "1");
      getCodes(root->r, code + "0");
    }
  }

  void huff_code(string s) {
    const int ALPHABET_SIZE = 26;
    int counts[ALPHABET_SIZE];
    for (int i = 0; i < ALPHABET_SIZE; ++i)
      counts[i] = 0;
    for (int i = 0; i < s.length(); ++i)
      counts[s[i] - 'a'] += 1;

    int letter_cnt = 0; // number of disctinct letters
    Node* leaves[ALPHABET_SIZE];
    priority_queue<Node*, vector<Node*>, NodeComparator> pqueue;
    for (int i = 0; i < ALPHABET_SIZE; ++i) {
      leaves[i] = NULL;
      if (counts[i] > 0) {
        letter_cnt += 1;
        leaves[i] = new Node(counts[i], static_cast<char>(i + 'a'));
        pqueue.push(leaves[i]);
      }
    }

    Node *l = NULL;
    Node *r = NULL;
    while (!pqueue.empty()) {
      l = pqueue.top();
      pqueue.pop();
      if (pqueue.empty())
        break;
      r = pqueue.top();
      pqueue.pop();
      pqueue.push(new Node(l->key + r->key, l, r));
    }

    getCodes(l, "");
    int fin_size = 0; // encoded line size
    for (int i = 0; i < s.length(); ++i) {
      fin_size += leaves[s[i] - 'a']->code.length();
    }

    cout << letter_cnt << " " << fin_size << endl;
    for (int i = 0; i < ALPHABET_SIZE; ++i) {
      if (leaves[i] != NULL) {
        cout << leaves[i]->c << ": " << leaves[i]->code << endl;
      }
    }
    for (int i = 0; i < s.length(); ++i) {
      cout << leaves[s[i] - 'a']->code;
    }

    // need to delete all nodes :)
  }
}